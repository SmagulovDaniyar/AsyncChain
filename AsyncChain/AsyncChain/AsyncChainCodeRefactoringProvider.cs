using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CodeActions;
using Microsoft.CodeAnalysis.CodeRefactorings;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.Editing;
using Microsoft.CodeAnalysis.FindSymbols;
using Microsoft.CodeAnalysis.Text;
using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Composition;
using System.Linq;
using System.Linq.Expressions;
using System.Threading;
using System.Threading.Tasks;

namespace AsyncChain
{
    [ExportCodeRefactoringProvider(LanguageNames.CSharp, Name = nameof(AsyncChainCodeRefactoringProvider)), Shared]
    internal sealed class AsyncChainCodeRefactoringProvider : CodeRefactoringProvider
    {
        public override async Task ComputeRefactoringsAsync(CodeRefactoringContext context)
        {
            SyntaxNode root = await context.Document.GetSyntaxRootAsync().ConfigureAwait(false);
            SemanticModel semanticModel = await context.Document.GetSemanticModelAsync().ConfigureAwait(false);
            SyntaxNode node = root.FindNode(context.Span);

            if (!(node is MethodDeclarationSyntax methodDeclaration)) return;

            IMethodSymbol methodSymbol = semanticModel.GetDeclaredSymbol(methodDeclaration);
            if (IsAwaitable(methodSymbol)) return;

            CodeAction action = CodeAction.Create("Build async chain", cancellationToken => BuildAsyncChain(context.Document, methodDeclaration, cancellationToken));

            context.RegisterRefactoring(action);
        }

        private static async Task<Solution> BuildAsyncChain(Document document, MethodDeclarationSyntax methodDeclaration, CancellationToken cancellationToken = default)
        {
            List<UpdateInfo> updateInfos = await GetUpdateInfo(document, methodDeclaration, cancellationToken);
            SolutionEditor solutionEditor = new SolutionEditor(document.Project.Solution);

            foreach (IGrouping<DocumentId, UpdateInfo> documentGroup in updateInfos.GroupBy(i => i.DocumentId))
            {
                DocumentEditor documentEditor = await solutionEditor.GetDocumentEditorAsync(documentGroup.Key, cancellationToken).ConfigureAwait(false);

                List<CallUpdateInfo> callUpdateInfos = documentGroup.OfType<CallUpdateInfo>().ToList();
                List<MethodUpdateInfo> methodUpdateInfos = documentGroup.OfType<MethodUpdateInfo>().ToList();

                Dictionary<MethodDeclarationSyntax, MethodDeclarationSyntax> transforms = new Dictionary<MethodDeclarationSyntax, MethodDeclarationSyntax>(); 

                foreach (MethodUpdateInfo methodUpdateInfo in methodUpdateInfos)
                {
                    List<CallUpdateInfo> invocations = callUpdateInfos.Where(i => SymbolEqualityComparer.Default.Equals(i.CallingMethod, methodUpdateInfo.Method)).ToList();

                    MethodDeclarationSyntax originalNode = documentEditor.OriginalRoot.FindNode(methodUpdateInfo.Span) as MethodDeclarationSyntax;
                    MethodDeclarationSyntax updatedNode = UpdateMethodDeclaration(documentEditor.SemanticModel, originalNode, methodUpdateInfo, invocations);

                    if (originalNode is null || updatedNode is null) continue;
                    if (originalNode == updatedNode || originalNode.IsEquivalentTo(updatedNode)) continue;

                    transforms[originalNode] = updatedNode;
                }

                SyntaxNode updatedRoot = documentEditor.OriginalRoot.ReplaceNodes(transforms.Keys, (original, _) => transforms[original]);
                updatedRoot = UpdateNamespaceDeclaration(updatedRoot);
                documentEditor.ReplaceNode(documentEditor.OriginalRoot, updatedRoot);
            }

            return solutionEditor.GetChangedSolution();
        }

        private static async Task<List<UpdateInfo>> GetUpdateInfo(Document document, MethodDeclarationSyntax methodDeclaration, CancellationToken cancellationToken = default)
        {
            Solution solution = document.Project.Solution;
            SemanticModel semanticModel = await document.GetSemanticModelAsync(cancellationToken).ConfigureAwait(false);

            IMethodSymbol startMethodSymbol = semanticModel.GetDeclaredSymbol(methodDeclaration, cancellationToken);
            if (startMethodSymbol is null) return new List<UpdateInfo>();

            var updateInfo = new List<UpdateInfo>();

            var methodsToProcess = new Queue<IMethodSymbol>();
            var processedSymbols = new HashSet<IMethodSymbol>(SymbolEqualityComparer.Default);

            methodsToProcess.Enqueue(startMethodSymbol);

            while (methodsToProcess.Count > 0)
            {
                IMethodSymbol currentSymbol = methodsToProcess.Dequeue();
                
                if (processedSymbols.Contains(currentSymbol)) continue;
                processedSymbols.Add(currentSymbol);

                updateInfo.AddRange(currentSymbol.Locations.Where(l => l.IsInSource).Select(l => new MethodUpdateInfo
                {
                    DocumentId = solution.GetDocument(l.SourceTree).Id,
                    Span = l.SourceSpan,
                    Method = currentSymbol
                }));

                IEnumerable<ISymbol> overrides = await SymbolFinder.FindOverridesAsync(currentSymbol, solution, null, cancellationToken).ConfigureAwait(false);

                foreach (ISymbol overrideSymbol in overrides)
                {
                    if (overrideSymbol is IMethodSymbol overrideMethod && !processedSymbols.Contains(overrideMethod))
                        methodsToProcess.Enqueue(overrideMethod);
                }

                if (currentSymbol.IsOverride && currentSymbol.OverriddenMethod != null)
                {
                    if (!processedSymbols.Contains(currentSymbol.OverriddenMethod))
                        methodsToProcess.Enqueue(currentSymbol.OverriddenMethod);
                }

                foreach (IMethodSymbol interfaceMethod in currentSymbol.ExplicitInterfaceImplementations)
                {
                    if (!processedSymbols.Contains(interfaceMethod))
                        methodsToProcess.Enqueue(interfaceMethod);
                }

                if (IsAwaitable(currentSymbol)) continue;

                IEnumerable<SymbolCallerInfo> callers = await SymbolFinder.FindCallersAsync(currentSymbol, solution, cancellationToken).ConfigureAwait(false);

                foreach (SymbolCallerInfo caller in callers)
                {
                    if (!(caller.CallingSymbol is IMethodSymbol callerMethodSymbol)) continue;

                    updateInfo.AddRange(caller.Locations.Where(l => l.IsInSource).Select(l => new CallUpdateInfo
                    {
                        DocumentId = solution.GetDocument(l.SourceTree).Id,
                        Span = l.SourceSpan,
                        CallingMethod = callerMethodSymbol,
                        CalledMethod = currentSymbol
                    }));

                    if (!processedSymbols.Contains(callerMethodSymbol)) methodsToProcess.Enqueue(callerMethodSymbol);
                }
            }

            return updateInfo;
        }

        private static ExpressionSyntax UpdateInvocationExpression(InvocationExpressionSyntax currentNode)
        {
            if (currentNode is null) return currentNode;

            InvocationExpressionSyntax newNode = currentNode;

            if (newNode.Expression is SimpleNameSyntax simpleNameSyntax)
                newNode = newNode.WithExpression(UpdateInvocationExpressionMethodName(simpleNameSyntax));
            else if (newNode.Expression is MemberAccessExpressionSyntax memberAccessSyntax)
                newNode = newNode.WithExpression(memberAccessSyntax.WithName(UpdateInvocationExpressionMethodName(memberAccessSyntax.Name)));
            else if (newNode.Expression is MemberBindingExpressionSyntax memberBindingExpressionSyntax)
                newNode = newNode.WithExpression(memberBindingExpressionSyntax.WithName(UpdateInvocationExpressionMethodName(memberBindingExpressionSyntax.Name)));

            if (!newNode.ArgumentList.Arguments.Any(a => a.Expression is IdentifierNameSyntax id && id.Identifier.Text == "cancellationToken"))
            {
                ArgumentSyntax tokenArg = SyntaxFactory
                    .Argument(SyntaxFactory.IdentifierName("cancellationToken"))
                    .WithNameColon(SyntaxFactory.NameColon(SyntaxFactory.IdentifierName("cancellationToken")));
                newNode = newNode.AddArgumentListArguments(tokenArg);
            }

            if (newNode.Parent is AwaitExpressionSyntax) return newNode;

            return SyntaxFactory.AwaitExpression(
                SyntaxFactory.Token(SyntaxKind.AwaitKeyword).WithTrailingTrivia(SyntaxFactory.Space), newNode.WithoutTrivia())
                    .WithLeadingTrivia(currentNode.GetLeadingTrivia())
                    .WithTrailingTrivia(currentNode.GetTrailingTrivia());
        }

        private static SimpleNameSyntax UpdateInvocationExpressionMethodName(SimpleNameSyntax node)
        {
            if (node.Identifier.Text.EndsWith("Async", StringComparison.OrdinalIgnoreCase)) return node;

            return node.WithIdentifier(
                SyntaxFactory.Identifier(
                    node.Identifier.LeadingTrivia,
                    node.Identifier.Text + "Async",
                    node.Identifier.TrailingTrivia));
        }

        private static ReturnStatementSyntax UpdateReturnStatementSyntax(ReturnStatementSyntax currentNode)
        {
            if (currentNode is null) return currentNode;

            ReturnStatementSyntax newNode = currentNode;

            if (newNode.Expression is null)
            {
                MemberAccessExpressionSyntax completedTaskExpression = SyntaxFactory.MemberAccessExpression(
                    SyntaxKind.SimpleMemberAccessExpression,
                    SyntaxFactory.IdentifierName("Task"),
                    SyntaxFactory.IdentifierName("CompletedTask")
                );

                return newNode
                    .WithExpression(completedTaskExpression)
                    .WithReturnKeyword(newNode.ReturnKeyword.WithTrailingTrivia(SyntaxFactory.Space));
            }

            if (newNode.Expression is MemberAccessExpressionSyntax memberAccessExpressionSyntax)
            {
                if (memberAccessExpressionSyntax.Expression.ToString() == nameof(Task) && memberAccessExpressionSyntax.Name.Identifier.Text == nameof(Task.CompletedTask)) return newNode;
            }

            if (newNode.Expression is InvocationExpressionSyntax invocation && invocation.Expression is MemberAccessExpressionSyntax taskMemberAccessExpressionSyntax)
            {
                if (taskMemberAccessExpressionSyntax.Expression.ToString() == nameof(Task) && taskMemberAccessExpressionSyntax.Name.Identifier.Text == nameof(Task.FromResult)) return newNode;
            }

            InvocationExpressionSyntax taskFromResultInvocation = SyntaxFactory.InvocationExpression(
                SyntaxFactory.MemberAccessExpression(
                    SyntaxKind.SimpleMemberAccessExpression,
                    SyntaxFactory.IdentifierName("Task"),
                    SyntaxFactory.IdentifierName("FromResult")
                ),
                SyntaxFactory.ArgumentList(
                    SyntaxFactory.SingletonSeparatedList(
                        SyntaxFactory.Argument(newNode.Expression.WithoutTrivia())
                    )
                )
            );

            return newNode.WithExpression(taskFromResultInvocation);
        }

        private static MethodDeclarationSyntax UpdateMethodDeclaration(SemanticModel semanticModel, MethodDeclarationSyntax currentNode, MethodUpdateInfo methodUpdateInfo, List<CallUpdateInfo> callUpdateInfos)
        {
            if (currentNode is null) return currentNode;

            MethodDeclarationSyntax newNode = currentNode;

            var invocationNodes = new List<InvocationExpressionSyntax>();
            List<IMethodSymbol> targetSymbols = callUpdateInfos.Select(cui => cui.CalledMethod).ToList();

            foreach (InvocationExpressionSyntax invocation in newNode.DescendantNodes().OfType<InvocationExpressionSyntax>())
            {
                if (!(semanticModel.GetSymbolInfo(invocation).Symbol is IMethodSymbol methodSymbol)) continue;
                string name = methodSymbol.ToDisplayString(SymbolDisplayFormat.FullyQualifiedFormat);
                if (targetSymbols.Any(ts => name == ts.ToDisplayString(SymbolDisplayFormat.FullyQualifiedFormat))) invocationNodes.Add(invocation);
            }

            newNode = newNode.ReplaceNodes(invocationNodes, (original, _) => UpdateInvocationExpression(original));

            if (!IsAwaitable(methodUpdateInfo.Method))
            {
                TypeSyntax returnType = newNode.ReturnType;
                TypeSyntax newReturnType = (returnType is PredefinedTypeSyntax predefined && predefined.Keyword.IsKind(SyntaxKind.VoidKeyword))
                    ? SyntaxFactory.ParseTypeName("Task")
                    : SyntaxFactory.ParseTypeName($"Task<{returnType.WithoutTrivia()}>");

                newNode = newNode.WithReturnType(newReturnType.WithTrailingTrivia(returnType.GetTrailingTrivia()));
            }

            if (!methodUpdateInfo.Method.Parameters.Any(p => p.Type.ToDisplayString().Contains("CancellationToken")))
            {
                ParameterSyntax tokenParameter = SyntaxFactory.Parameter(SyntaxFactory.Identifier("cancellationToken"))
                    .WithType(SyntaxFactory.ParseTypeName("CancellationToken"))
                    .WithDefault(SyntaxFactory.EqualsValueClause(
                        SyntaxFactory.LiteralExpression(SyntaxKind.DefaultLiteralExpression, SyntaxFactory.Token(SyntaxKind.DefaultKeyword))
                    ));

                newNode = newNode.AddParameterListParameters(tokenParameter);
            }

            if (invocationNodes.Count > 0)
            {
                if (!newNode.Modifiers.Any(m => m.IsKind(SyntaxKind.AsyncKeyword)))
                    newNode = newNode.AddModifiers(SyntaxFactory.Token(SyntaxKind.AsyncKeyword));
            }
            else
            {
                List<ReturnStatementSyntax> returnStatementSyntaxes = newNode.DescendantNodes().OfType<ReturnStatementSyntax>().ToList();
                if (returnStatementSyntaxes.Count > 0) newNode = newNode.ReplaceNodes(returnStatementSyntaxes, (original, _) => UpdateReturnStatementSyntax(original));
            }

            if (!newNode.Identifier.Text.EndsWith("Async", StringComparison.OrdinalIgnoreCase))
            {
                newNode = newNode.WithIdentifier(SyntaxFactory.Identifier(
                    newNode.Identifier.LeadingTrivia,
                    newNode.Identifier.Text + "Async",
                    newNode.Identifier.TrailingTrivia));
            }

            return newNode;
        }

        private static SyntaxNode UpdateNamespaceDeclaration(SyntaxNode currentNode)
        {
            if (!(currentNode is CompilationUnitSyntax newNode)) return currentNode;

            string[] namespacesToAdd = new[] { "System.Threading", "System.Threading.Tasks" };

            ImmutableHashSet<string> currentUsings = newNode.Usings.Select(u => u.Name.ToString()).ToImmutableHashSet();
            List<string> missingNamespaces = namespacesToAdd.Where(ns => !currentUsings.Contains(ns)).ToList();

            if (missingNamespaces.Count > 0) newNode = newNode.AddUsings(missingNamespaces.Select(ns => SyntaxFactory.UsingDirective(SyntaxFactory.ParseName(ns))).ToArray());

            return newNode;
        }


        private static bool IsAwaitable(IMethodSymbol methodSymbol)
        {
            if (methodSymbol is null) return false;

            string returnTypeName = methodSymbol.ReturnType.ToDisplayString();
            return returnTypeName.StartsWith("System.Threading.Tasks.Task");
        }


        private class UpdateInfo
        {
            public DocumentId DocumentId { get; set; }
            public TextSpan Span { get; set; }
        }

        private sealed class MethodUpdateInfo : UpdateInfo
        {
            public IMethodSymbol Method { get; set; }
        }

        private sealed class CallUpdateInfo : UpdateInfo
        {
            public IMethodSymbol CallingMethod { get; set; }
            public IMethodSymbol CalledMethod { get; set; }
        }
    }
}

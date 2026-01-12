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
using System.Threading;
using System.Threading.Tasks;

namespace AsyncChain
{
    [ExportCodeRefactoringProvider(LanguageNames.CSharp, Name = nameof(AsyncChainCodeRefactoringProvider)), Shared]
    internal sealed class AsyncChainCodeRefactoringProvider : CodeRefactoringProvider
    {
        public override async Task ComputeRefactoringsAsync(CodeRefactoringContext context)
        {
            SyntaxNode root = await context.Document.GetSyntaxRootAsync(context.CancellationToken).ConfigureAwait(false);
            SyntaxNode node = root.FindNode(context.Span);

            if (!(node is MethodDeclarationSyntax methodDeclaration)) return;

            SemanticModel semanticModel = await context.Document.GetSemanticModelAsync(context.CancellationToken).ConfigureAwait(false);
            bool isAwaitable = IsAwaitable(methodDeclaration, semanticModel);

            CodeAction action = CodeAction.Create("Build async caller chain", cancellationToken => BuildAsyncChain(context.Document, methodDeclaration, cancellationToken));

            context.RegisterRefactoring(action);
        }

        private static async Task<Solution> BuildAsyncChain(Document document, MethodDeclarationSyntax methodDeclaration, CancellationToken cancellationToken = default)
        {
            Solution currentSolution = document.Project.Solution;
            SemanticModel semanticModel = await document.GetSemanticModelAsync(cancellationToken).ConfigureAwait(false);

            IEnumerable<UpdateInfo> updateInfos = await GetUpdateInfo(document, methodDeclaration, cancellationToken);

            foreach (IGrouping<DocumentId, UpdateInfo> documentGroup in updateInfos.GroupBy(i => i.DocumentId))
            {
                Document currentDocument = currentSolution.GetDocument(documentGroup.Key);
                if (currentDocument == null) continue;

                SyntaxNode root = await currentDocument.GetSyntaxRootAsync(cancellationToken).ConfigureAwait(false);

                List<SyntaxNode> nodesToTrack = documentGroup.Select(i => root.FindNode(i.Span)).ToList();
                SyntaxNode trackedRoot = root.TrackNodes(nodesToTrack);

                Document trackedDocument = currentDocument.WithSyntaxRoot(trackedRoot);
                DocumentEditor editor = await DocumentEditor.CreateAsync(trackedDocument, cancellationToken).ConfigureAwait(false);

                SyntaxNode currentTrackedRoot = await trackedDocument.GetSyntaxRootAsync(cancellationToken).ConfigureAwait(false);

                List<UpdateInfo> updates = documentGroup.ToList();

                foreach (CallUpdateInfo callUpdateInfo in updates.OfType<CallUpdateInfo>().OrderByDescending(i => i.Span.Start))
                {
                    SyntaxNode originalNode = root.FindNode(callUpdateInfo.Span);
                    SyntaxNode currentNode = currentTrackedRoot.GetCurrentNode(originalNode);

                    if (currentNode != null)
                    {
                        UpdateCallSite(editor, currentNode, callUpdateInfo);
                    }
                }

                foreach (MethodUpdateInfo methodUpdateInfo in updates.OfType<MethodUpdateInfo>().OrderByDescending(i => i.Span.Start))
                {
                    SyntaxNode originalNode = root.FindNode(methodUpdateInfo.Span);
                    SyntaxNode currentNode = currentTrackedRoot.GetCurrentNode(originalNode);

                    if (currentNode != null)
                    {
                        UpdateMethodSignature(editor, currentNode, methodUpdateInfo);
                    }
                }

                Document updatedDocument = editor.GetChangedDocument();
                currentSolution = updatedDocument.Project.Solution;
            }

            return currentSolution;
        }

        private static async Task<IEnumerable<UpdateInfo>> GetUpdateInfo(Document document, MethodDeclarationSyntax methodDeclaration, CancellationToken cancellationToken = default)
        {
            Solution solution = document.Project.Solution;
            SemanticModel semanticModel = await document.GetSemanticModelAsync(cancellationToken).ConfigureAwait(false);

            IMethodSymbol startMethodSymbol = semanticModel.GetDeclaredSymbol(methodDeclaration, cancellationToken);
            if (startMethodSymbol is null) return Array.Empty<UpdateInfo>();

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

                IEnumerable<SymbolCallerInfo> callers = await SymbolFinder.FindCallersAsync(currentSymbol, solution, cancellationToken).ConfigureAwait(false);

                foreach (SymbolCallerInfo caller in callers)
                {
                    if (!(caller.CallingSymbol is IMethodSymbol callerMethodSymbol)) continue;

                    updateInfo.AddRange(caller.Locations.Where(l => l.IsInSource).Select(l => new CallUpdateInfo
                    {
                        DocumentId = solution.GetDocument(l.SourceTree)?.Id,
                        Span = l.SourceSpan
                    }));

                    if (!processedSymbols.Contains(callerMethodSymbol)) methodsToProcess.Enqueue(callerMethodSymbol);
                }
            }

            return updateInfo;
        }

        private static void UpdateCallSite(DocumentEditor editor, SyntaxNode currentNode, CallUpdateInfo update)
        {
            InvocationExpressionSyntax invocation = currentNode as InvocationExpressionSyntax ?? currentNode.FirstAncestorOrSelf<InvocationExpressionSyntax>();
            if (invocation is null) return;

            InvocationExpressionSyntax updatedInvocation = invocation;

            if (!invocation.ArgumentList.Arguments.Any(a => a.Expression is IdentifierNameSyntax id && id.Identifier.Text == "cancellationToken"))
            {
                ArgumentSyntax tokenArg = SyntaxFactory.Argument(SyntaxFactory.IdentifierName("cancellationToken"));
                updatedInvocation = invocation.AddArgumentListArguments(tokenArg);
            }

            SyntaxNode finalNode;

            if (invocation.Parent is AwaitExpressionSyntax)
            {
                finalNode = updatedInvocation;
            }
            else
            {
                finalNode = SyntaxFactory.AwaitExpression(
                    SyntaxFactory.Token(SyntaxKind.AwaitKeyword).WithTrailingTrivia(SyntaxFactory.Space),updatedInvocation.WithoutTrivia())
                        .WithLeadingTrivia(invocation.GetLeadingTrivia())
                        .WithTrailingTrivia(invocation.GetTrailingTrivia());
            }

            editor.ReplaceNode(invocation, finalNode);
        }

        private static void UpdateMethodSignature(DocumentEditor editor, SyntaxNode currentNode, MethodUpdateInfo update)
        {
            MethodDeclarationSyntax methodNode = currentNode as MethodDeclarationSyntax ?? currentNode.FirstAncestorOrSelf<MethodDeclarationSyntax>();
            if (methodNode is null) return;

            editor.ReplaceNode(methodNode, (original, generator) =>
            {
                MethodDeclarationSyntax method = (MethodDeclarationSyntax)original;

                if (!method.Modifiers.Any(SyntaxKind.AsyncKeyword))
                {
                    method = method.AddModifiers(SyntaxFactory.Token(SyntaxKind.AsyncKeyword));
                }

                if (!IsAwaitable(update.Method))
                {
                    TypeSyntax returnType = method.ReturnType;
                    TypeSyntax newReturnType = (returnType is PredefinedTypeSyntax predefined && predefined.Keyword.IsKind(SyntaxKind.VoidKeyword))
                        ? SyntaxFactory.ParseTypeName("Task")
                        : SyntaxFactory.ParseTypeName($"Task<{returnType.WithoutTrivia()}>");

                    method = method.WithReturnType(newReturnType.WithTrailingTrivia(returnType.GetTrailingTrivia()));
                }

                if (!update.Method.Parameters.Any(p => p.Type.ToDisplayString().Contains("CancellationToken")))
                {
                    ParameterSyntax tokenParameter = SyntaxFactory.Parameter(SyntaxFactory.Identifier("cancellationToken"))
                        .WithType(SyntaxFactory.ParseTypeName("CancellationToken"))
                        .WithDefault(SyntaxFactory.EqualsValueClause(
                            SyntaxFactory.LiteralExpression(SyntaxKind.DefaultLiteralExpression, SyntaxFactory.Token(SyntaxKind.DefaultKeyword))
                        ));

                    method = method.AddParameterListParameters(tokenParameter);
                }

                return method;
            });
        }


        private static bool IsAwaitable(IMethodSymbol methodSymbol)
        {
            if (methodSymbol is null) return false;

            string returnTypeName = methodSymbol.ReturnType.ToDisplayString();
            return returnTypeName.StartsWith("System.Threading.Tasks.Task");
        }

        private static bool IsAwaitable(MethodDeclarationSyntax methodDeclaration, SemanticModel semanticModel, CancellationToken cancellationToken = default)
        {
            if (semanticModel is null) return false;

            IMethodSymbol methodSymbol = semanticModel.GetDeclaredSymbol(methodDeclaration, cancellationToken);
            if (methodSymbol is null || methodSymbol.ReturnsVoid) return false;

            ITypeSymbol returnType = methodSymbol.ReturnType;
            ImmutableArray<ISymbol> awaiterMembers = semanticModel.LookupSymbols(methodDeclaration.SpanStart, returnType, "GetAwaiter");

            return awaiterMembers.Any(s => s is IMethodSymbol ms && ms.Parameters.Length == 0);
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

        }
    }
}

using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CodeActions;
using Microsoft.CodeAnalysis.CodeRefactorings;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.Editing;
using Microsoft.CodeAnalysis.FindSymbols;
using Microsoft.CodeAnalysis.Text;
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

            CodeAction action = CodeAction.Create("Build async chain", cancellationToken => BuildAsyncChain(context.Document, methodDeclaration, cancellationToken));

            context.RegisterRefactoring(action);
        }

        private static async Task<Solution> BuildAsyncChain(Document document, MethodDeclarationSyntax methodDeclaration, CancellationToken cancellationToken = default)
        {
            List<UpdateInfo> updateInfos = await GetUpdateInfo(document, methodDeclaration, cancellationToken);
            SolutionEditor solutionEditor = new SolutionEditor(document.Project.Solution);

            var documentChanges = new Dictionary<DocumentEditor, List<(MethodDeclarationSyntax Original, MethodDeclarationSyntax Changed)>>();

            foreach (IGrouping<DocumentId, UpdateInfo> documentGroup in updateInfos.GroupBy(i => i.DocumentId))
            {
                DocumentEditor documentEditor = await solutionEditor.GetDocumentEditorAsync(documentGroup.Key, cancellationToken).ConfigureAwait(false);
                documentChanges[documentEditor] = new List<(MethodDeclarationSyntax Original, MethodDeclarationSyntax Changed)>();

                ImmutableHashSet<CallUpdateInfo> callUpdateInfos = documentGroup.OfType<CallUpdateInfo>().ToImmutableHashSet();
                List<MethodUpdateInfo> methodUpdateInfos = documentGroup.OfType<MethodUpdateInfo>().ToList();

                foreach (MethodUpdateInfo methodUpdateInfo in methodUpdateInfos)
                {
                    List<CallUpdateInfo> invocations = callUpdateInfos.Where(i => SymbolEqualityComparer.Default.Equals(i.CallingMethod, methodUpdateInfo.Method)).ToList();

                    MethodDeclarationSyntax originalMethodNode = documentEditor.OriginalRoot.FindNode(methodUpdateInfo.Span) as MethodDeclarationSyntax;
                    MethodDeclarationSyntax replaceMethodNode = UpdateMethodDeclaration(documentEditor.SemanticModel, originalMethodNode, methodUpdateInfo, invocations);

                    if (originalMethodNode is null || replaceMethodNode is null) continue;

                    documentChanges[documentEditor].Add((originalMethodNode, replaceMethodNode));
                }
            }

            foreach (KeyValuePair<DocumentEditor, List<(MethodDeclarationSyntax Original, MethodDeclarationSyntax Changed)>> documentChange in documentChanges)
            {
                foreach ((MethodDeclarationSyntax original, MethodDeclarationSyntax changed) in documentChange.Value)
                {
                    documentChange.Key.ReplaceNode(original, changed);
                }

                AddRequiredNamespaces(documentChange.Key);
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

                    if (!processedSymbols.Contains(callerMethodSymbol) && !IsAwaitable(callerMethodSymbol)) methodsToProcess.Enqueue(callerMethodSymbol);
                }
            }

            return updateInfo;
        }

        private static SyntaxNode UpdateInvocationExpression(SemanticModel semanticModel, SyntaxNode currentNode)
        {
            InvocationExpressionSyntax newNode = currentNode as InvocationExpressionSyntax ?? currentNode.FirstAncestorOrSelf<InvocationExpressionSyntax>();
            if (newNode is null) return newNode;

            if (!newNode.ArgumentList.Arguments.Any(a => a.Expression is IdentifierNameSyntax id && id.Identifier.Text == "cancellationToken"))
            {
                ArgumentSyntax tokenArg = SyntaxFactory.Argument(SyntaxFactory.IdentifierName("cancellationToken"));
                newNode = newNode.AddArgumentListArguments(tokenArg);
            }

            if (newNode.Parent is AwaitExpressionSyntax) return newNode;

            return SyntaxFactory.AwaitExpression(
                SyntaxFactory.Token(SyntaxKind.AwaitKeyword).WithTrailingTrivia(SyntaxFactory.Space), newNode.WithoutTrivia())
                    .WithLeadingTrivia(currentNode.GetLeadingTrivia())
                    .WithTrailingTrivia(currentNode.GetTrailingTrivia());
        }

        private static MethodDeclarationSyntax UpdateMethodDeclaration(SemanticModel semanticModel, SyntaxNode currentNode, MethodUpdateInfo methodUpdateInfo, List<CallUpdateInfo> callUpdateInfos)
        {
            MethodDeclarationSyntax newNode = currentNode as MethodDeclarationSyntax ?? currentNode.FirstAncestorOrSelf<MethodDeclarationSyntax>();
            if (newNode is null) return null;

            var invocationNodes = new List<InvocationExpressionSyntax>();
            List<IMethodSymbol> targetSymbols = callUpdateInfos.Select(cui => cui.CalledMethod).ToList();

            foreach (InvocationExpressionSyntax invocation in newNode.DescendantNodes().OfType<InvocationExpressionSyntax>())
            {
                if (!(semanticModel.GetSymbolInfo(invocation).Symbol is IMethodSymbol methodSymbol)) continue;
                string name = methodSymbol.ToDisplayString(SymbolDisplayFormat.FullyQualifiedFormat);
                if (targetSymbols.Any(ts => name == ts.ToDisplayString(SymbolDisplayFormat.FullyQualifiedFormat))) invocationNodes.Add(invocation);
            }

            newNode = newNode.ReplaceNodes(invocationNodes, (original, _) => UpdateInvocationExpression(semanticModel, original));

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

            if (invocationNodes.Any())
            {
                if (!newNode.Modifiers.Any(m => m.IsKind(SyntaxKind.AsyncKeyword)))
                    newNode = newNode.AddModifiers(SyntaxFactory.Token(SyntaxKind.AsyncKeyword));
            }

            return newNode;
        }

        private static void AddRequiredNamespaces(DocumentEditor editor)
        {
            if (!(editor.OriginalRoot is CompilationUnitSyntax root)) return;

            string[] namespacesToAdd = new[] { "System.Threading", "System.Threading.Tasks" };

            ImmutableHashSet<string> currentUsings = root.Usings.Select(u => u.Name.ToString()).ToImmutableHashSet();
            List<string> missingNamespaces = namespacesToAdd.Where(ns => !currentUsings.Contains(ns)).ToList();

            if (missingNamespaces.Count > 0)
            {
                IEnumerable<SyntaxNode> importDeclarations = missingNamespaces.Select(ns => editor.Generator.NamespaceImportDeclaration(ns));
                editor.ReplaceNode(root, (currentRoot, _) => editor.Generator.AddNamespaceImports(currentRoot, importDeclarations));
            }
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

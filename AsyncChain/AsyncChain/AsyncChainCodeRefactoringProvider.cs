using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CodeActions;
using Microsoft.CodeAnalysis.CodeRefactorings;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.FindSymbols;
using System.Buffers;
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

            CodeAction action = CodeAction.Create("Build async chain", cancellationToken => BuildAsyncChain(context.Document, methodDeclaration, cancellationToken));

            context.RegisterRefactoring(action);
        }

        private async Task<Solution> BuildAsyncChain(Document document, MethodDeclarationSyntax methodDeclaration, CancellationToken cancellationToken = default)
        {
            Solution currentSolution = document.Project.Solution;
            SemanticModel semanticModel = await document.GetSemanticModelAsync(cancellationToken).ConfigureAwait(false);

            IMethodSymbol startMethodSymbol = semanticModel.GetDeclaredSymbol(methodDeclaration, cancellationToken);
            if (startMethodSymbol is null) return currentSolution;

            Queue<IMethodSymbol> methodsToProcess = new Queue<IMethodSymbol>();
            HashSet<IMethodSymbol> processedSymbols = new HashSet<IMethodSymbol>(SymbolEqualityComparer.Default);

            methodsToProcess.Enqueue(startMethodSymbol);

            while (methodsToProcess.Count > 0)
            {
                IMethodSymbol currentSymbol = methodsToProcess.Dequeue();
                if (processedSymbols.Contains(currentSymbol)) continue;
                processedSymbols.Add(currentSymbol);

                IEnumerable<SymbolCallerInfo> callers = await SymbolFinder.FindCallersAsync(currentSymbol, currentSolution, cancellationToken).ConfigureAwait(false);

                foreach (SymbolCallerInfo caller in callers)
                {
                    if (!(caller.CallingSymbol is IMethodSymbol callerMethodSymbol)) continue;

                    foreach (Location location in caller.Locations.Where(l => l.IsInSource))
                    {
                        currentSolution = await ApplyAwaitAndTokenToCallSite(currentSolution, location, cancellationToken);
                    }

                    Location referenceLocation = caller.Locations.FirstOrDefault(l => l.IsInSource);
                    if (referenceLocation is null || referenceLocation.SourceTree is null) continue;

                    Document callerDocument = currentSolution.GetDocument(referenceLocation.SourceTree);
                    if (callerDocument is null) continue;

                    SemanticModel callerModel = await callerDocument.GetSemanticModelAsync(cancellationToken).ConfigureAwait(false);
                    if (callerModel is null) continue;

                    if (IsAwaitable(callerMethodSymbol, callerModel, referenceLocation.SourceSpan.Start)) continue;

                    methodsToProcess.Enqueue(callerMethodSymbol);
                }

                currentSolution = await TransformMethodInSolution(currentSolution, currentSymbol, cancellationToken);
            }

            return currentSolution;
        }

        private async Task<Solution> TransformMethodInSolution(Solution solution, IMethodSymbol symbol, CancellationToken cancellationToken = default)
        {
            Location location = symbol.Locations.FirstOrDefault(l => l.IsInSource);
            if (location is null) return solution;

            Document doc = solution.GetDocument(location.SourceTree);
            SyntaxNode root = await location.SourceTree.GetRootAsync(cancellationToken).ConfigureAwait(false);
            MethodDeclarationSyntax methodNode = root.FindNode(location.SourceSpan).FirstAncestorOrSelf<MethodDeclarationSyntax>();

            if (methodNode == null) return solution;

            SemanticModel semanticModel = await doc.GetSemanticModelAsync(cancellationToken).ConfigureAwait(false);

            MethodDeclarationSyntax updatedMethod = MakeReturnTypeTask(methodNode, semanticModel, cancellationToken);
            updatedMethod = EnsureCancellationToken(updatedMethod, semanticModel, cancellationToken);
            updatedMethod = AddAsyncModifier(updatedMethod);

            SyntaxNode newRoot = root.ReplaceNode(methodNode, updatedMethod);
            return solution.WithDocumentSyntaxRoot(doc.Id, newRoot);
        }

        private async Task<Solution> ApplyAwaitAndTokenToCallSite(Solution solution, Location location, CancellationToken cancellationToken = default)
        {
            Document doc = solution.GetDocument(location.SourceTree);
            if (doc is null) return solution;

            SyntaxNode root = await doc.GetSyntaxRootAsync(cancellationToken).ConfigureAwait(false);

            InvocationExpressionSyntax invocation = root.FindNode(location.SourceSpan).FirstAncestorOrSelf<InvocationExpressionSyntax>();
            if (invocation is null) return solution;

            InvocationExpressionSyntax updatedInvocation = invocation;
            bool hasTokenArg = invocation.ArgumentList.Arguments.Any(a => a.Expression.ToString().Contains("cancellationToken"));

            if (!hasTokenArg)
            {
                ArgumentSyntax tokenArg = SyntaxFactory.Argument(SyntaxFactory.IdentifierName("cancellationToken"));
                updatedInvocation = invocation.AddArgumentListArguments(tokenArg);
            }

            SyntaxNode finalNode = null;

            if (updatedInvocation.Parent is AwaitExpressionSyntax)
            {
                finalNode = updatedInvocation;
            }
            else
            {
                finalNode = SyntaxFactory.AwaitExpression(updatedInvocation)
                    .WithLeadingTrivia(invocation.GetLeadingTrivia())
                    .WithTrailingTrivia(invocation.GetTrailingTrivia());
            }

            SyntaxNode newRoot = root.ReplaceNode(invocation, finalNode);
            return solution.WithDocumentSyntaxRoot(doc.Id, newRoot);
        }


        private static MethodDeclarationSyntax MakeReturnTypeTask(MethodDeclarationSyntax methodDeclaration, SemanticModel semanticModel, CancellationToken cancellationToken = default)
        {
            if (IsAwaitable(methodDeclaration, semanticModel, cancellationToken)) return methodDeclaration;

            TypeSyntax returnType = methodDeclaration.ReturnType;

            TypeSyntax newReturnType = returnType is PredefinedTypeSyntax predefined && predefined.Keyword.IsKind(SyntaxKind.VoidKeyword)
                ? SyntaxFactory.ParseTypeName("System.Threading.Tasks.Task")
                : SyntaxFactory.ParseTypeName($"System.Threading.Tasks.Task<{returnType}>");

            newReturnType = newReturnType.WithTrailingTrivia(returnType.GetTrailingTrivia());

            return methodDeclaration.WithReturnType(newReturnType);
        }

        private static MethodDeclarationSyntax EnsureCancellationToken(MethodDeclarationSyntax methodDeclaration, SemanticModel semanticModel, CancellationToken cancellationToken = default)
        {
            IMethodSymbol methodSymbol = semanticModel.GetDeclaredSymbol(methodDeclaration, cancellationToken);

            bool hasToken = methodSymbol?.Parameters.Any(p => p.Type.ToDisplayString() == "System.Threading.CancellationToken") ?? false;
            if (hasToken) return methodDeclaration;

            ParameterSyntax tokenParameter = SyntaxFactory.Parameter(SyntaxFactory.Identifier("cancellationToken"))
                .WithType(SyntaxFactory.ParseTypeName("System.Threading.CancellationToken"))
                .WithDefault(SyntaxFactory.EqualsValueClause(
                    SyntaxFactory.LiteralExpression(SyntaxKind.DefaultLiteralExpression, SyntaxFactory.Token(SyntaxKind.DefaultKeyword))
                ));

            return methodDeclaration.AddParameterListParameters(tokenParameter);
        }

        private static MethodDeclarationSyntax AddAsyncModifier(MethodDeclarationSyntax methodDeclaration)
        {
            if (methodDeclaration.Modifiers.Any(SyntaxKind.AsyncKeyword)) return methodDeclaration;
            return methodDeclaration.AddModifiers(SyntaxFactory.Token(SyntaxKind.AsyncKeyword));
        }


        private static bool IsAwaitable(IMethodSymbol methodSymbol, SemanticModel semanticModel, int position, CancellationToken cancellationToken = default)
        {
            if (semanticModel is null) return false;
            if (methodSymbol is null || methodSymbol.ReturnsVoid) return false;

            ITypeSymbol returnType = methodSymbol.ReturnType;
            ImmutableArray<ISymbol> awaiterMembers = semanticModel.LookupSymbols(position, returnType, "GetAwaiter");

            return awaiterMembers.Any(s => s is IMethodSymbol ms && ms.Parameters.Length == 0);
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
    }
}

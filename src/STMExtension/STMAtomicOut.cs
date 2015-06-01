using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using System.IO;
using Microsoft.CodeAnalysis.CSharp.Syntax.InternalSyntax;

namespace STMExtension
{
    public class STMAtomicOut
    {
        internal static CSharpCompilation HandleAtomicOutParameters(CSharpCompilation compilation, List<List<IdentifierNameSyntax>> skipLists)
        {
            var newTrees = compilation.SyntaxTrees.ToArray();
            for (int i = 0; i < compilation.SyntaxTrees.Length; i++)
            {
                var tree = compilation.SyntaxTrees[i];
                var root = tree.GetRoot();
                var semanticModel = compilation.GetSemanticModel(tree);
                //Find all method declarations with a atomic out parameter
                var originalMethods = root.DescendantNodes().OfType<MethodDeclarationSyntax>().Where(md => md.ParameterList.Parameters.Any(p => p.Modifiers.Any(SyntaxKind.OutKeyword) && p.Modifiers.Any(SyntaxKind.AtomicKeyword))).ToList();

                root = root.TrackNodes(originalMethods);
                var methods = root.GetCurrentNodes<MethodDeclarationSyntax>(originalMethods).ToList();

                //Apply handling
                root = root.ReplaceNodes(methods, (oldNode, newNode) => HandleAtomicOutParameter(oldNode));
                methods = root.GetCurrentNodes<MethodDeclarationSyntax>(originalMethods).ToList();

                //Find identifiers to skip when apply .Value property
                var idensToSkip = new List<IdentifierNameSyntax>();
                foreach (var methodDecl in methods)
                {
                    var outParameters = methodDecl.ParameterList.Parameters.Where(p => p.Modifiers.Any(SyntaxKind.OutKeyword) && p.Modifiers.Any(SyntaxKind.AtomicKeyword)).ToList();
                    var assignments = methodDecl.Body.Statements.OfType<ExpressionStatementSyntax>()
                            .Where(exprStatement => exprStatement.Expression is AssignmentExpressionSyntax && ((exprStatement.Expression as AssignmentExpressionSyntax).Left is IdentifierNameSyntax))
                            .Select(exprStatement => (exprStatement.Expression as AssignmentExpressionSyntax).Left as IdentifierNameSyntax);

                    foreach (var param in outParameters)
                    {
                        var res = assignments.Where(iden => iden.ToString() == param.Identifier.ToString()).First();
                        idensToSkip.Add(res);
                    }
                }

                root = root.TrackNodes(idensToSkip);
                skipLists[i].AddRange(idensToSkip);

                tree = SyntaxFactory.SyntaxTree(root, tree.Options, tree.FilePath);
                newTrees[i] = tree;
                compilation = CSharpCompilation.Create(compilation.AssemblyName, newTrees, compilation.References, compilation.Options);
            }
            return compilation;
        }

        private static MethodDeclarationSyntax HandleAtomicOutParameter(MethodDeclarationSyntax methodDecl)
        {

            var outParameters = methodDecl.ParameterList.Parameters.Where(p => p.Modifiers.Any(SyntaxKind.OutKeyword) && p.Modifiers.Any(SyntaxKind.AtomicKeyword)).ToList();

            if (outParameters.Count > 0)
            {
                var statementBuffer = new List<StatementSyntax>();
                foreach (var param in outParameters)
                {
                    var identifier = SyntaxFactory.IdentifierName(param.Identifier);
                    var initializer = STM.CreateObjectCreationExpression(STM.DetermineSTMType(param.Type), SyntaxFactory.ArgumentList());
                    var assigment = SyntaxFactory.ExpressionStatement(SyntaxFactory.AssignmentExpression(SyntaxKind.SimpleAssignmentExpression, identifier, initializer));
                    statementBuffer.Add(assigment);
                }

                foreach (var statement in methodDecl.Body.Statements)
                {
                    statementBuffer.Add(statement);
                }

                methodDecl = methodDecl.WithBody(SyntaxFactory.Block(statementBuffer));
            }

            return methodDecl;
        }
    }
}

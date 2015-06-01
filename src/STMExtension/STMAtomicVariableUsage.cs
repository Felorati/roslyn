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
    public class STMAtomicVariableUsage
    {
        internal static CSharpCompilation ReplaceAtomicVariableUsage(CSharpCompilation compilation, List<List<IdentifierNameSyntax>> skipLists)
        {
            var newTrees = new SyntaxTree[compilation.SyntaxTrees.Length];

            for (int i = 0; i < compilation.SyntaxTrees.Length; i++)
            {

                var tree = compilation.SyntaxTrees[i];
                var root = tree.GetRoot();
                var semanticModel = compilation.GetSemanticModel(tree);
                var skipList = skipLists[i].Select(iden => root.GetCurrentNode(iden)).ToList();

                var tmVarIdentifiers = root.DescendantNodes().OfType<IdentifierNameSyntax>()
                    .Where(iden => !skipList.Contains(iden) && STM.ReplaceCondition(iden, semanticModel) && STM.IsAtomicType(semanticModel.GetTypeInfo(iden)));
                root = root.ReplaceNodes(tmVarIdentifiers, (oldnode, newnode) => ReplaceIdentifier(oldnode));

                tree = SyntaxFactory.SyntaxTree(root, tree.Options, tree.FilePath);
                newTrees[i] = tree;
            }

            return CSharpCompilation.Create(compilation.AssemblyName, newTrees, compilation.References, compilation.Options);
        }

        private static MemberAccessExpressionSyntax ReplaceIdentifier(IdentifierNameSyntax iden)
        {
            var valueIden = SyntaxFactory.IdentifierName("Value");
            var newNode = SyntaxFactory.MemberAccessExpression(SyntaxKind.SimpleMemberAccessExpression, iden, SyntaxFactory.Token(SyntaxKind.DotToken), valueIden);
            return newNode;
        }
    }
}

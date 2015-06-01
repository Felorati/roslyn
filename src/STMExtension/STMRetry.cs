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
    public class STMRetry
    {
        internal static CSharpCompilation ReplaceRetryStatements(CSharpCompilation compilation)
        {
            var newTrees = compilation.SyntaxTrees.ToArray();
            for (int i = 0; i < compilation.SyntaxTrees.Length; i++)
            {
                var tree = compilation.SyntaxTrees[i];
                var root = tree.GetRoot();

                //replace retry's
                var retryNodes = root.DescendantNodes().OfType<RetryStatementSyntax>().ToList();
                root = root.ReplaceNodes(retryNodes, (oldnode, newnode) => ReplaceRetry(oldnode));

                tree = SyntaxFactory.SyntaxTree(root, tree.Options, tree.FilePath);
                newTrees[i] = tree;
            }

            return CSharpCompilation.Create(compilation.AssemblyName, newTrees, compilation.References, compilation.Options); ;
        }

        private static SyntaxNode ReplaceRetry(RetryStatementSyntax rNode)
        {
            return SyntaxFactory.ExpressionStatement(
                       SyntaxFactory.InvocationExpression(SyntaxFactory.ParseName(STM.PreprendNameSpace("STMSystem.Retry"))));
        }
    }
}

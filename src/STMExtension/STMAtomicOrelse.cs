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
    public class STMAtomicOrelse
    {
        internal static CSharpCompilation ReplaceAtomicOrElseBlocks(CSharpCompilation compilation)
        {
            var newTrees = new SyntaxTree[compilation.SyntaxTrees.Length];

            for (int i = 0; i < compilation.SyntaxTrees.Length; i++)
            {
                var tree = compilation.SyntaxTrees[i];
                var root = tree.GetRoot();

                //replace atomic and orelse blocks
                while (GetAtomics(root).Any())
                {
                    List<AtomicStatementSyntax> atomicNodes = root.DescendantNodes().
                        Where(
                            node => node.IsKind(SyntaxKind.AtomicStatement) && //is atomic node
                            !GetAtomics(node).Any()) //does not have any inner atomic
                            .Cast<AtomicStatementSyntax>().ToList();
                    root = root.ReplaceNodes(atomicNodes, (oldnode, newnode) => ReplaceAtomicOrElse(oldnode));
                }

                tree = SyntaxFactory.SyntaxTree(root, tree.Options, tree.FilePath);
                newTrees[i] = tree;
            }

            return CSharpCompilation.Create(compilation.AssemblyName, newTrees, compilation.References, compilation.Options);
        }

        private static StatementSyntax ReplaceAtomicOrElse(AtomicStatementSyntax anAtomic)
        {
            //Build up arguments to library call
            List<ArgumentSyntax> aArguments = new List<ArgumentSyntax>();

            //Atomic arg
            StatementSyntax aBlock = anAtomic.Statement;
            var aLambda = SyntaxFactory.ParenthesizedLambdaExpression(aBlock);
            var atomicArg = SyntaxFactory.Argument(aLambda);
            aArguments.Add(atomicArg);

            //OrElse args
            var aOrElses = anAtomic.Orelses;
            foreach (var oe in aOrElses)
            {
                var oeLambda = SyntaxFactory.ParenthesizedLambdaExpression(oe.Statement);
                var oeArg = SyntaxFactory.Argument(oeLambda);
                aArguments.Add(oeArg);
            }

            //Create library call  
            var expression = SyntaxFactory.InvocationExpression(
                SyntaxFactory.ParseName(STM.PreprendNameSpace("STMSystem.Atomic")),
                SyntaxFactory.ArgumentList(
                    arguments: SyntaxFactory.SeparatedList<ArgumentSyntax>(aArguments)));

            //Add return statement if there is return in the atomic block, else make it an expression statement
            var allButLambdas = aBlock.DescendantNodes((node) => !node.IsKind(SyntaxKind.ParenthesizedLambdaExpression));
            var allReturns = allButLambdas.Where(node => node.IsKind(SyntaxKind.ReturnStatement)).ToList();

            StatementSyntax atomicInvoNode = (allReturns.Count > 0) ?
                atomicInvoNode = SyntaxFactory.ReturnStatement(SyntaxFactory.Token(SyntaxKind.ReturnKeyword), expression, SyntaxFactory.Token(SyntaxKind.SemicolonToken)) :
                atomicInvoNode = SyntaxFactory.ExpressionStatement(expression);


            return atomicInvoNode;
        }

        public static IEnumerable<AtomicStatementSyntax> GetAtomics(SyntaxNode root)
        {
            return root.DescendantNodes().OfType<AtomicStatementSyntax>();
        }
    }
}

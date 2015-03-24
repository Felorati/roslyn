using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;

namespace STMExtension
{
    public class STM
    {
        public static void Extend(ref SyntaxTree[] trees)
        {
            if(trees.Count() > 1)
            {
                throw new Exception("There are more than one syntax tree in trees");
            }

            var tree = trees.First();
            var root = tree.GetRoot();
            var rootDesNodes = root.DescendantNodes().ToList();
            var atomicNodes = rootDesNodes.Where(node => node.IsKind(SyntaxKind.AtomicStatement)).ToList();

            //List<ExpressionStatementSyntax> newAtomicNodes = new List<ExpressionStatementSyntax>();
            //TODO: Lav i stedet et diconary, som nye noder og gamle atomic noder tilføjes til, og til sidst en gang lave et nyt 
                //syntax træ (efter foreach'en)

            foreach (var aNode in atomicNodes)
            {
                //var adesnodes = anode.descendantnodes().tolist();
                //var ablock = adesnodes.where(node => node.iskind(syntaxkind.block)).tolist();
                //var aablock = anode.childnodes().tolist();
                //var stuffintran = aablock.first().childnodes().tolist();
                //var ades1 = anode.childnodes().tolist();
                //var kind = ades1.first().iskind(syntaxkind.block);
                //var ades2 = ades1.first().childnodes().tolist();

                ////byg lamda expressions op ud fra de noder der er i atomic blocken.
                ////var lambda = syntaxfactory.simplelambdaexpression

                var arg1 = SyntaxFactory.Argument(SyntaxFactory.LiteralExpression(
                    SyntaxKind.StringLiteralExpression,
                    SyntaxFactory.Literal("hello")));

                var newNode = SyntaxFactory.ExpressionStatement(
                SyntaxFactory.InvocationExpression(
                    SyntaxFactory.ParseName("System.Console.WriteLine"),
                    SyntaxFactory.ArgumentList(
                        arguments: SyntaxFactory.SeparatedList<ArgumentSyntax>(
                            new List<ArgumentSyntax>() { arg1 }))));
                var newRoot = tree.GetRoot().ReplaceNode(aNode, newNode);
                var text = newRoot.GetText().ToString();
                SyntaxTree oldTree = trees[0];
                SyntaxTree newTree = SyntaxFactory.SyntaxTree(newRoot, oldTree.Options, oldTree.FilePath);
                var ntL = newTree.GetRoot().DescendantNodes().ToList();
                var atomicNodesInNew = newTree.GetRoot().DescendantNodes().Where(node => node.IsKind(SyntaxKind.AtomicStatement)).ToList();
                trees[0] = newTree;
                var atomicNodesInNew2 = trees.First().GetRoot().DescendantNodes().Where(node => node.IsKind(SyntaxKind.AtomicStatement)).ToList();
                var textAfter = newTree.GetText().ToString();
            }
        }
    }
}

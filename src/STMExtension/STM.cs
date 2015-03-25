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
                throw new Exception("There are more than one syntax tree in trees: Undefined behaviour.");
            }

            var tree = trees.First();
            var root = tree.GetRoot();
            var rootDesNodes = root.DescendantNodes().ToList();
            var atomicNodes = rootDesNodes.Where(node => node.IsKind(SyntaxKind.AtomicStatement)).ToList();

            var replaceDic = new Dictionary<AtomicStatementSyntax, ExpressionStatementSyntax>(); //TODO: Replace the second generic type (ExpressionStatementSyntax), when we know it
            //var textBefore = root.GetText().ToString();

            int helloInc = 0; //this is just for testing incrementing atomics
            foreach (AtomicStatementSyntax aNode in atomicNodes)
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

                var arg = SyntaxFactory.Argument(SyntaxFactory.LiteralExpression(
                    SyntaxKind.StringLiteralExpression,
                    SyntaxFactory.Literal("hello" + helloInc)));

                var newNode = SyntaxFactory.ExpressionStatement(
                SyntaxFactory.InvocationExpression(
                    SyntaxFactory.ParseName("System.Console.WriteLine"),
                    SyntaxFactory.ArgumentList(
                        arguments: SyntaxFactory.SeparatedList<ArgumentSyntax>(
                            new List<ArgumentSyntax>() { arg }))));

                replaceDic.Add(aNode, newNode);
                helloInc++;
            }

            var newRoot = tree.GetRoot().ReplaceNodes(replaceDic.Keys, (oldnode, newnode) => replaceDic[oldnode]);
            SyntaxTree newTree = SyntaxFactory.SyntaxTree(newRoot, tree.Options, tree.FilePath);
            trees[0] = newTree;
            //var textAfter = newTree.GetText().ToString();
        }
    }
}

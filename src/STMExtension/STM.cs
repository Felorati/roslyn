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
        public static void Lam(Func<int> lam)
        {
            Console.WriteLine("Lambda Test: " + lam());
        }

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
            //var textBefore = root.GetText().ToString(); //Get source text before transformation

            foreach (AtomicStatementSyntax aNode in atomicNodes)
            {
                var desNodes = aNode.DescendantNodes().ToList(); //Can be used to check that descendant nodes does not use an illegal construct
                var childNodes = aNode.ChildNodes().ToList();
                if(childNodes.Count() > 1)
                {
                    throw new Exception("There are more than one child nodes: Undefined behaviour.");
                }
                BlockSyntax aBlock = (BlockSyntax) childNodes.ElementAt(0);

                var lambda = SyntaxFactory.ParenthesizedLambdaExpression(
                    aBlock
                );

                var arg = SyntaxFactory.Argument(lambda);

                var newNode = SyntaxFactory.ExpressionStatement(
                SyntaxFactory.InvocationExpression(
                    SyntaxFactory.ParseName("STM.Lam"), //TODO: Insert name fra STM library instead
                    SyntaxFactory.ArgumentList(
                        arguments: SyntaxFactory.SeparatedList<ArgumentSyntax>(
                            new List<ArgumentSyntax>() { arg }))));

                replaceDic.Add(aNode, newNode);
            }

            var newRoot = tree.GetRoot().ReplaceNodes(replaceDic.Keys, (oldnode, newnode) => replaceDic[oldnode]);
            SyntaxTree newTree = SyntaxFactory.SyntaxTree(newRoot, tree.Options, tree.FilePath);
            trees[0] = newTree;
            var textAfter = newTree.GetText().ToString(); //Get source text after transformation
        }
    }
}

//***Hello world invocation example (replaces atomic with hello world)***
//var arg = SyntaxFactory.Argument(SyntaxFactory.LiteralExpression(
//    SyntaxKind.StringLiteralExpression,
//    SyntaxFactory.Literal("hello")));

//var newNode = SyntaxFactory.ExpressionStatement(
//SyntaxFactory.InvocationExpression(
//    SyntaxFactory.ParseName("System.Console.WriteLine"),
//    SyntaxFactory.ArgumentList(
//        arguments: SyntaxFactory.SeparatedList<ArgumentSyntax>(
//            new List<ArgumentSyntax>() { arg }))));
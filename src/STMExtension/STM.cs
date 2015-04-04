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
            //var textBefore = root.GetText().ToString(); //Get source text before transformation (for testing and debugging)

            //replace atomics
            var atomicNodes = root.DescendantNodes().Where(node => node.IsKind(SyntaxKind.AtomicStatement)).ToList();
            var atomicReplaceDic = new Dictionary<AtomicStatementSyntax, ExpressionStatementSyntax>();

            foreach (AtomicStatementSyntax aNode in atomicNodes)
            {
                //var desNodes = aNode.DescendantNodes().ToList(); //Can be used to check that descendant nodes does not use an illegal construct
                var childNodes = aNode.ChildNodes().ToList();
                if (childNodes.Count() > 1)
                {
                    throw new Exception("There are more than one child nodes: Undefined behaviour.");
                }
                
                BlockSyntax aBlock = (BlockSyntax)childNodes.ElementAt(0);

                var lambda = SyntaxFactory.ParenthesizedLambdaExpression(aBlock);
                var arg = SyntaxFactory.Argument(lambda);

                var atomicInvoNode = SyntaxFactory.ExpressionStatement(
                SyntaxFactory.InvocationExpression(
                    SyntaxFactory.ParseName("STMSystem.Atomic"), //
                    SyntaxFactory.ArgumentList(
                        arguments: SyntaxFactory.SeparatedList<ArgumentSyntax>(
                            new List<ArgumentSyntax>() { arg }))));

                atomicReplaceDic.Add(aNode, atomicInvoNode);
            }

            root = root.ReplaceNodes(atomicReplaceDic.Keys, (oldnode, newnode) => atomicReplaceDic[oldnode]);

            //replace retry's
            var retryReplaceDic = new Dictionary<RetryStatementSyntax, ExpressionStatementSyntax>();
            var retryNodes = root.DescendantNodes().Where(node => node.IsKind(SyntaxKind.RetryStatement)).ToList();
            foreach (RetryStatementSyntax rNode in retryNodes)
            {
                var retryInvoNode = SyntaxFactory.ExpressionStatement(
                    SyntaxFactory.InvocationExpression(SyntaxFactory.ParseName("STMSystem.Retry")));
                retryReplaceDic.Add(rNode, retryInvoNode);
            }
            root = root.ReplaceNodes(retryReplaceDic.Keys, (oldnode, newnode) => retryReplaceDic[oldnode]);

            //create new syntax tree (based on new root), and update it as the current tree in the trees array
            SyntaxTree newTree = SyntaxFactory.SyntaxTree(root, tree.Options, tree.FilePath);
            trees[0] = newTree;
            var textAfter = newTree.GetText().ToString(); //Get source text after transformation (for testing)
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
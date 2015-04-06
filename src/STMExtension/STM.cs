using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using System.IO;

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
            for (int i = 0; i < trees.Length; i++)
            {
                var tree = trees[i];
                var root = tree.GetRoot();

                //var textBefore = root.GetText().ToString(); //Get source text before transformation (for testing and debugging) TestRef.cs

                //replace atomic field types
                List<FieldDeclarationSyntax> allFields = root.DescendantNodes().Where(node => node.IsKind(SyntaxKind.FieldDeclaration)).Cast<FieldDeclarationSyntax>().ToList();
                var atomicFields = allFields.Where(node => node.Modifiers.Any(SyntaxKind.AtomicKeyword));
                var fieldDclReplaceDic = new Dictionary<FieldDeclarationSyntax, FieldDeclarationSyntax>();

                foreach (FieldDeclarationSyntax aField in atomicFields)
                {
                    //Remove atomic from modifier list
                    var newModifierList = aField.Modifiers.Where(mod => !mod.IsKind(SyntaxKind.AtomicKeyword)).ToList();
                    var newFieldDcl = aField.WithModifiers(SyntaxFactory.TokenList(newModifierList));

                    //Change declaration type to our TMVar type (or specific like TMInt)
                    VariableDeclarationSyntax aFVarDcl = aField.Declaration;
                    TypeSyntax aFType = aFVarDcl.Type; //original type node

                    string aFTypeStr = "";
                    string aFFullTypeStr = "";

                    if (aFType.IsKind(SyntaxKind.IdentifierName)) //userdefined types
                    {
                        aFTypeStr = ((IdentifierNameSyntax)aFType).Identifier.Text;
                        aFFullTypeStr = "TMVar<" + aFTypeStr + ">";
                    }
                    else if (aFType.IsKind(SyntaxKind.PredefinedType))
                    {
                        aFTypeStr = ((PredefinedTypeSyntax)aFType).Keyword.Text;
                        string aFTypeStrUp = FirstCharToUpper(aFTypeStr);

                        switch (aFTypeStr)
                        {
                            case "int":
                            case "long":
                            case "double":
                            case "float":
                            case "uint":
                            case "ulong":
                                aFFullTypeStr = "TM" + aFTypeStrUp;
                                break;
                            case "string":
                            default:
                                aFFullTypeStr = "TMVar<" + aFTypeStrUp + ">";
                                break;
                        }
                    }
                    else
                    {
                        throw new Exception("This declaration type is unknown");
                    }

                    var newTypeDcl = SyntaxFactory.ParseName(aFFullTypeStr + " "); //whitespace needed to seperate type from name

                    //Build new field declaration and add to dic
                    newFieldDcl = newFieldDcl.WithDeclaration(SyntaxFactory.VariableDeclaration(newTypeDcl, aFVarDcl.Variables));
                    fieldDclReplaceDic.Add(aField, newFieldDcl);
                }
                root = root.ReplaceNodes(fieldDclReplaceDic.Keys, (oldnode, newnode) => fieldDclReplaceDic[oldnode]);

                //replace atomics and orelses
                var atomicNodes = root.DescendantNodes().Where(node => node.IsKind(SyntaxKind.AtomicStatement)).ToList();
                var atomicReplaceDic = new Dictionary<AtomicStatementSyntax, ExpressionStatementSyntax>();

                foreach (AtomicStatementSyntax aNode in atomicNodes)
                {
                    //Build up arguments to library call
                    List<ArgumentSyntax> aArguments = new List<ArgumentSyntax>();

                    //Atomic arg
                    StatementSyntax aBlock = aNode.Statement; //(BlockSyntax)childNodes.ElementAt(0);
                    var aLambda = SyntaxFactory.ParenthesizedLambdaExpression(aBlock);
                    var atomicArg = SyntaxFactory.Argument(aLambda);
                    aArguments.Add(atomicArg);

                    //OrElse args
                    var aOrElses = aNode.Orelses;
                    foreach (var oe in aOrElses)
                    {
                        var oeLambda = SyntaxFactory.ParenthesizedLambdaExpression(oe.Statement);
                        var oeArg = SyntaxFactory.Argument(oeLambda);
                        aArguments.Add(oeArg);
                    }

                    //Create library call
                    var atomicInvoNode = SyntaxFactory.ExpressionStatement(
                    SyntaxFactory.InvocationExpression(
                        SyntaxFactory.ParseName("STMSystem.Atomic"),
                        SyntaxFactory.ArgumentList(
                            arguments: SyntaxFactory.SeparatedList<ArgumentSyntax>(aArguments))));

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

                //Create new syntax tree (based on new root), and update it as the current tree in the trees array
                SyntaxTree newTree = SyntaxFactory.SyntaxTree(root, tree.Options, tree.FilePath);
                trees[i] = newTree;
                //Get source text after transformation (for testing and debug purposes)
                var textAfter = newTree.GetText().ToString();
                var appendText = "File: " + tree.FilePath + "\n" + textAfter + "\n\n";
                File.AppendAllText(AppDomain.CurrentDomain.BaseDirectory + "TextAfterCompilation.txt", appendText);
                //File.WriteAllText(AppDomain.CurrentDomain.BaseDirectory + "TextAfterCompilation.txt", textAfter);
            }

           
        }
        private static string FirstCharToUpper(string s)
        {
            // Check for empty string.
            if (string.IsNullOrEmpty(s))
            {
                return string.Empty;
            }
            // Return char and concat substring.
            return char.ToUpper(s[0]) + s.Substring(1);
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
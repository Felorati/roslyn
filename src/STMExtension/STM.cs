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

            //Cleaning debug and testing file upon each compilation
            File.WriteAllText(AppDomain.CurrentDomain.BaseDirectory + "TextAfterCompilation.txt", "");

            for (int i = 0; i < trees.Length; i++)
            {
                var tree = trees[i];
                var root = tree.GetRoot();

                //var textBefore = root.GetText().ToString(); //Get source text before transformation (for testing and debugging) TestRef.cs

                //replace atomic parameter types
                List<ParameterSyntax> allParams = root.DescendantNodes().Where(node => node.IsKind(SyntaxKind.Parameter)).Cast<ParameterSyntax>().ToList();
                var atomicParams = allParams.Where(node => node.Modifiers.Any(SyntaxKind.AtomicKeyword));
                root = root.ReplaceNodes(atomicParams, (oldnode, newnode) => ReplaceParams(oldnode));

                //replace atomic field types
                List<FieldDeclarationSyntax> allFields = root.DescendantNodes().Where(node => node.IsKind(SyntaxKind.FieldDeclaration)).Cast<FieldDeclarationSyntax>().ToList();
                var atomicFields = allFields.Where(node => node.Modifiers.Any(SyntaxKind.AtomicKeyword));
                root = root.ReplaceNodes(atomicFields, (oldnode, newnode) => ReplaceFieldDecl(oldnode));

                //Replace local vars
                root = ReplaceLocalVars(root);

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

        private static NameSyntax DetermineSTMType(TypeSyntax aFType)
        {
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
                        aFFullTypeStr = "TMVar<" + aFTypeStr + ">";
                        break;
                }
            }
            else
            {
                throw new Exception("This declaration type is unknown");
            }

            var newTypeDcl = SyntaxFactory.ParseName(aFFullTypeStr + " "); //whitespace needed to seperate type from name
            return newTypeDcl;
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

        private static ParameterSyntax ReplaceParams(ParameterSyntax aParam) //TODO: Der skal nok laves noget specielt med params, ref og out
        {
            //Remove atomic from modifier
            var newParam = aParam.WithModifiers(RemoveAtomicMod(aParam.Modifiers));
            //Replace type and initializers
            var newTypeDcl = DetermineSTMType(newParam.Type);
            newParam = newParam.WithType(newTypeDcl);
            return newParam;
        }

        private static FieldDeclarationSyntax ReplaceFieldDecl(FieldDeclarationSyntax aField)
        {
            //Remove atomic from modifier list
            var newFieldDcl = aField.WithModifiers(RemoveAtomicMod(aField.Modifiers));
            //Replace type and initializers
            newFieldDcl = newFieldDcl.WithDeclaration(ConstructVariableDeclaration(newFieldDcl.Declaration));
            return newFieldDcl;
        }

        private static SyntaxNode ReplaceLocalVars(SyntaxNode root)
        {
            //replace atomic local var dcl types
            var allLocals = root.DescendantNodes().Where(node => node.IsKind(SyntaxKind.LocalDeclarationStatement)).Cast<LocalDeclarationStatementSyntax>().ToList();
            var atomicLocals = allLocals.Where(node => node.Modifiers.Any(SyntaxKind.AtomicKeyword));

            root = root.ReplaceNodes(atomicLocals, (oldnode, newnode) => ReplaceLocalVar(oldnode));
            return root;
        }

        private static LocalDeclarationStatementSyntax ReplaceLocalVar(LocalDeclarationStatementSyntax aLocal)
        {
            //Remove atomic from modifier list
            var newLocalDecl = aLocal.WithModifiers(RemoveAtomicMod(aLocal.Modifiers));

            //Change declaration type to our TMVar type (or specific like TMInt)
            newLocalDecl = newLocalDecl.WithDeclaration(ConstructVariableDeclaration(aLocal.Declaration));
            return newLocalDecl;
        }

        private static SyntaxTokenList RemoveAtomicMod(SyntaxTokenList list)
        {
            var newModifierList = list.Where(mod => !mod.IsKind(SyntaxKind.AtomicKeyword)).ToList();
            return SyntaxFactory.TokenList(newModifierList);
        }

        private static VariableDeclarationSyntax ConstructVariableDeclaration(VariableDeclarationSyntax aVarDcl)
        {
            var newTypeDcl = DetermineSTMType(aVarDcl.Type);

            var buffer = new List<VariableDeclaratorSyntax>();
            foreach (var variable in aVarDcl.Variables)
            {
                if (variable.Initializer != null)
                {
                    var argListContent = SyntaxFactory.SeparatedList<ArgumentSyntax>(new List<ArgumentSyntax> { SyntaxFactory.Argument(variable.Initializer.Value) });
                    var argList = SyntaxFactory.ArgumentList(SyntaxFactory.ParseToken("("), argListContent, SyntaxFactory.ParseToken(")"));
                    var initExpression = SyntaxFactory.ObjectCreationExpression(SyntaxFactory.ParseToken("new "), newTypeDcl, argList, null);
                    var newVarDeclarator = variable.WithInitializer(variable.Initializer.WithValue(initExpression));
                    buffer.Add(newVarDeclarator);
                }
                else
                {
                    buffer.Add(variable);
                }
            }

            return SyntaxFactory.VariableDeclaration(newTypeDcl, SyntaxFactory.SeparatedList<VariableDeclaratorSyntax>(buffer));
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
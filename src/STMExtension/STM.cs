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

        public static void ExtendCompilation(ref CSharpCompilation compilation)
        {
            for (int i = 0; i < compilation.SyntaxTrees.Length ; i++)
            {
                var tree = compilation.SyntaxTrees[i];
                var semanticModel = compilation.GetSemanticModel(tree);
                var atomicFields = tree.GetRoot().DescendantNodes().OfType<FieldDeclarationSyntax>().Where(fDecl => IsAtomicType(semanticModel, fDecl.Declaration.Type));
                foreach (var item in atomicFields)
                {
                    foreach (var vardcl in item.Declaration.Variables)
                    {
                        ISymbol symbol = semanticModel.GetDeclaredSymbol(vardcl);
                    }
                    
                }

                var expressions = tree.GetRoot().DescendantNodes().OfType<IdentifierNameSyntax>();
                var exprCount = expressions.Count();
                foreach (var expr in expressions)
                {
                    var typeinfo = semanticModel.GetTypeInfo(expr);
                    var dataflow = semanticModel.AnalyzeDataFlow(expr);
                }

            }
        }

        private static bool IsAtomicType(SemanticModel semanticModel, TypeSyntax type)
        {
            bool isAtomic = false;
            var typeInfo = semanticModel.GetTypeInfo(type);
            if (typeInfo.Type.ContainingNamespace.ToString() == "STM.Implementation.Lockbased")
            {
                switch (typeInfo.Type.Name)
                {
                    case "TMInt":
                    case "TMLong":
                    case "TMDouble":
                    case "TMFloat":
                    case "TMUlong":
                    case "TMUint":
                    case "TMVar":
                        isAtomic = true;
                        break;
                    default:
                        break;
                }
            }

            return isAtomic;
        }

        public static void Extend(ref SyntaxTree[] trees)
        {
            for (int i = 0; i < trees.Length; i++)
            {
                var tree = trees[i];
                var root = tree.GetRoot();

                //var textBefore = root.GetText().ToString(); //Get source text before transformation (for testing and debugging) TestRef.cs

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
            var aFTypeStr = aFType.GetTypeString();
            var aFTypeStrUp = FirstCharToUpper(aFTypeStr);
            string aFFullTypeStr = "";
            switch (aFTypeStr)
            {
                case "int":
                case "long":
                case "double":
                case "float":
                case "uint":
                case "ulong":
                    aFFullTypeStr = "TM" + FirstCharToUpper(aFTypeStr);
                    break;
                case "string":
                default:
                    aFFullTypeStr = "TMVar<" + aFTypeStr + ">";
                    break;
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
                    var argListContent = SyntaxFactory.SeparatedList(new List<ArgumentSyntax> { SyntaxFactory.Argument(variable.Initializer.Value) });
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
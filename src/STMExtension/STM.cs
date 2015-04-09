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
            var newTrees = new SyntaxTree[compilation.SyntaxTrees.Length];

            for (int i = 0; i < compilation.SyntaxTrees.Length ; i++)
            {
                var tree = compilation.SyntaxTrees[i];
                var root = tree.GetRoot();
                var semanticModel = compilation.GetSemanticModel(tree);

                var tmVarIdentifiers = tree.GetRoot().DescendantNodes().OfType<IdentifierNameSyntax>()
                    .Where(iden => ReplaceCondition(iden)  && IsAtomicType(semanticModel.GetTypeInfo(iden)));
                var list = tmVarIdentifiers.ToList();


                root = root.ReplaceNodes(tmVarIdentifiers, (oldnode, newnode) => ReplaceIdentifier(oldnode));

                var newTree = SyntaxFactory.SyntaxTree(root, tree.Options, tree.FilePath);
                newTrees[i] = newTree;
                PrintDebugSource(newTree);
            }

            compilation = CSharpCompilation.Create(compilation.AssemblyName, newTrees, compilation.References, compilation.Options);
        }

        private static bool ReplaceCondition(IdentifierNameSyntax iden)
        {
            if (iden.Parent is VariableDeclarationSyntax || iden.Parent is ParameterSyntax)
            {
                return false;
            }

            if (iden.Parent is PrefixUnaryExpressionSyntax)
            {
                var parent = iden.Parent as PrefixUnaryExpressionSyntax;
                if (parent.OperatorToken.IsKind(SyntaxKind.PlusPlusToken))
                {
                    return false;
                }
            }

            if (iden.Parent is PostfixUnaryExpressionSyntax)
            {
                var parent = iden.Parent as PostfixUnaryExpressionSyntax;
                if (parent.OperatorToken.IsKind(SyntaxKind.PlusPlusToken))
                {
                    return false;
                }
            }

            return true; 
        }

        private static MemberAccessExpressionSyntax ReplaceIdentifier(IdentifierNameSyntax iden)
        {
            var valueIden = SyntaxFactory.IdentifierName("Value");
            return SyntaxFactory.MemberAccessExpression(SyntaxKind.SimpleMemberAccessExpression, iden, SyntaxFactory.Token(SyntaxKind.DotToken), valueIden);
        }

        private static bool IsAtomicType(TypeInfo typeInfo)
        {
            bool isAtomic = false;
            if (typeInfo.Type != null && typeInfo.Type.ContainingNamespace.ToString() == "STM.Implementation.Lockbased")
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
			//Cleaning debug and testing file upon each compilation
            File.WriteAllText(AppDomain.CurrentDomain.BaseDirectory + "TextAfterCompilation.txt", "");

            for (int i = 0; i < trees.Length; i++)
            {
                var tree = trees[i];
                var root = tree.GetRoot();

                root = ReplaceProperties(root);

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
                PrintDebugSource(newTree);
            }
        }

        private static void PrintDebugSource(SyntaxTree tree)
        {
            var textAfter = tree.GetText().ToString();
            var appendText = "File: " + tree.FilePath + "\n" + textAfter + "\n\n";
            File.AppendAllText(AppDomain.CurrentDomain.BaseDirectory + "TextAfterCompilation.txt", appendText);
        }

        private static SyntaxNode ReplaceProperties(SyntaxNode root)
        {
            //Generates a manual property
            var allProperties = root.DescendantNodes().Where(node => node.IsKind(SyntaxKind.PropertyDeclaration)).Cast<PropertyDeclarationSyntax>().ToList();
            var atomicProperties = allProperties.Where(node => node.Modifiers.Any(SyntaxKind.AtomicKeyword));
            
            foreach(var atomicProperty in atomicProperties)
            {
                var modifiersWithoutAtomic = RemoveAtomicMod(atomicProperty.Modifiers);
                var backingFieldIdentifier = GenerateFieldName(atomicProperty.Identifier);

                var getModifier = SyntaxFactory.TokenList();
                var setModifier = SyntaxFactory.TokenList();      
                GetPropertyModifier(atomicProperty, ref getModifier, ref setModifier);

                var returnStatement = SyntaxFactory.ReturnStatement(SyntaxFactory.Token(SyntaxKind.ReturnKeyword), SyntaxFactory.IdentifierName(backingFieldIdentifier), SyntaxFactory.Token(SyntaxKind.SemicolonToken));
                var getBlock = SyntaxFactory.Block(SyntaxFactory.Token(SyntaxKind.OpenBraceToken), SyntaxFactory.List<StatementSyntax>().Add(returnStatement), SyntaxFactory.Token(SyntaxKind.CloseBraceToken));
                var getAccessorDeclaration = SyntaxFactory.AccessorDeclaration(SyntaxKind.GetAccessorDeclaration, atomicProperty.AttributeLists, getModifier, SyntaxFactory.Token(SyntaxKind.GetKeyword), getBlock, SyntaxFactory.Token(SyntaxKind.None));

                var expressionStatement = SyntaxFactory.ExpressionStatement(SyntaxFactory.AssignmentExpression(SyntaxKind.SimpleAssignmentExpression, SyntaxFactory.IdentifierName(backingFieldIdentifier), SyntaxFactory.Token(SyntaxKind.EqualsToken), SyntaxFactory.IdentifierName("value")), SyntaxFactory.Token(SyntaxKind.SemicolonToken));
                var setBlock = SyntaxFactory.Block(SyntaxFactory.Token(SyntaxKind.OpenBraceToken), SyntaxFactory.List<StatementSyntax>().Add(expressionStatement), SyntaxFactory.Token(SyntaxKind.CloseBraceToken));
                var setAccessorDeclaration = SyntaxFactory.AccessorDeclaration(SyntaxKind.SetAccessorDeclaration, atomicProperty.AttributeLists, setModifier, SyntaxFactory.Token(SyntaxKind.SetKeyword), setBlock, SyntaxFactory.Token(SyntaxKind.None));

                var accessors = SyntaxFactory.List<AccessorDeclarationSyntax>().Add(getAccessorDeclaration).Add(setAccessorDeclaration);
                var accessorList = SyntaxFactory.AccessorList(SyntaxFactory.Token(SyntaxKind.OpenBraceToken), accessors, SyntaxFactory.Token(SyntaxKind.CloseBraceToken));
                var manuelProperty = SyntaxFactory.PropertyDeclaration(atomicProperty.AttributeLists, modifiersWithoutAtomic, atomicProperty.Type, null, atomicProperty.Identifier, accessorList);

                root = root.InsertNodesAfter(atomicProperty, SyntaxFactory.List<PropertyDeclarationSyntax>().Add(manuelProperty));
            }

            //Converts atomic property to atomic backing field
            allProperties = root.DescendantNodes().Where(node => node.IsKind(SyntaxKind.PropertyDeclaration)).Cast<PropertyDeclarationSyntax>().ToList();
            atomicProperties = allProperties.Where(node => node.Modifiers.Any(SyntaxKind.AtomicKeyword));
            root = root.ReplaceNodes(atomicProperties, (oldnode, newnode) => ReplaceProperty(oldnode));
                        
            return root;
        }

        private static void GetPropertyModifier(PropertyDeclarationSyntax atomicProperty, ref SyntaxTokenList getModifier, ref SyntaxTokenList setModifier)
        {
            foreach (var mod in atomicProperty.AccessorList.Accessors)
            {
                var keyword = mod.Keyword;
                var modifier = keyword.GetPreviousToken();
                if (modifier.IsKeyword())
                {
                    if (keyword.IsKind(SyntaxKind.GetKeyword))
                    {
                        getModifier = getModifier.Add(modifier);
                    }
                    else if (keyword.IsKind(SyntaxKind.SetKeyword))
                    {
                        setModifier = setModifier.Add(modifier);
                    }
                }
            }
        }

        private static SyntaxNode ReplaceProperty(PropertyDeclarationSyntax aPropertyDcl)
        {
            // Generate backing field
            var modifiers = SyntaxFactory.TokenList(SyntaxFactory.Token(SyntaxKind.PrivateKeyword), SyntaxFactory.Token(SyntaxKind.AtomicKeyword));
            var identifier = SyntaxFactory.Identifier(GenerateFieldName(aPropertyDcl.Identifier));
            var variableDeclarator = SyntaxFactory.VariableDeclarator(identifier);
            var variableDeclarators = SyntaxFactory.SeparatedList(new List<VariableDeclaratorSyntax>() { variableDeclarator });
            var variableDeclaration = SyntaxFactory.VariableDeclaration(aPropertyDcl.Type, variableDeclarators);
            var replacingField = SyntaxFactory.FieldDeclaration(aPropertyDcl.AttributeLists, modifiers, variableDeclaration, SyntaxFactory.Token(SyntaxKind.SemicolonToken));
            return replacingField;
        }

        private static string GenerateFieldName(SyntaxToken identifier)
        {
            var nameAsString = new StringBuilder(identifier.ToString());
            nameAsString.Insert(0, "_", 1);
            nameAsString.Remove(1, 1);
            nameAsString.Insert(1, char.ToLower(identifier.ToString()[0]));
            return nameAsString.ToString();
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
                ArgumentListSyntax argList;
                if (variable.Initializer != null)
                {
                    var argListContent = SyntaxFactory.SeparatedList(new List<ArgumentSyntax> { SyntaxFactory.Argument(variable.Initializer.Value) });
                    argList = SyntaxFactory.ArgumentList(SyntaxFactory.ParseToken("("), argListContent, SyntaxFactory.ParseToken(")"));
                }
                else
                {
                    argList = SyntaxFactory.ArgumentList();
                }

                var initExpression = SyntaxFactory.ObjectCreationExpression(SyntaxFactory.ParseToken("new "), newTypeDcl, argList, null);
                var newVarDeclarator = variable.WithInitializer(SyntaxFactory.EqualsValueClause(initExpression));
                buffer.Add(newVarDeclarator);
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
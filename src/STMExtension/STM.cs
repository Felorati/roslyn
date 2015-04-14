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
        private static readonly string STMNameSpace = "STM.Implementation.Lockbased";

        public static void Extend(ref SyntaxTree[] trees)
        {
            //Cleaning debug and testing file upon each compilation
            File.WriteAllText(AppDomain.CurrentDomain.BaseDirectory + "TextAfterCompilation.txt", "");

            for (int i = 0; i < trees.Length; i++)
            {
                var tree = trees[i];
                var root = tree.GetRoot();

                root = ReplaceProperties(root);
                root = ReplaceFieldTypes(root); //replace atomic field types
                root = ReplaceAtomicOrElseBlocks(root);
                root = ReplaceRetry(root);

                //Create new syntax tree (based on new root), and update it as the current tree in the trees array
                SyntaxTree newTree = SyntaxFactory.SyntaxTree(root, tree.Options, tree.FilePath);
                trees[i] = newTree;
                //Get source text after transformation (for testing and debug purposes)
                PrintDebugSource(newTree);
            }
        }

        public static void ExtendCompilation(ref CSharpCompilation compilation)
        {
            compilation = ReplaceLocalVars(compilation);
            compilation = ReplaceAtomicRefOut(compilation);
            compilation = ReplaceMethodArguments(compilation);
            compilation = ReplaceConstructorArguments(compilation);
            compilation = ReplaceAtomicVariableUsage(compilation);
            compilation = ReplaceMemberAccesses(compilation);
            compilation = ReplaceParameters(compilation);
            CheckMethodSignatures(compilation); //Ensure two overloaded methods does not have a TMInt and int param at the same position

            foreach (var tree in compilation.SyntaxTrees)
            {
                PrintDebugSource(tree);
            }

        }

        private static CSharpCompilation ReplaceAtomicRefOut(CSharpCompilation compilation)
        {
            var newTrees = new SyntaxTree[compilation.SyntaxTrees.Length];

            for (int i = 0; i < compilation.SyntaxTrees.Length; i++)
            {
                var tree = compilation.SyntaxTrees[i];
                var root = tree.GetRoot();
                var semanticModel = compilation.GetSemanticModel(tree);


                var methodCalls = root.DescendantNodes().OfType<InvocationExpressionSyntax>();
                foreach (var ive in methodCalls)
                {
                    var info = semanticModel.GetSymbolInfo(ive);
                    if (info.Symbol != null)
                    {
                        IMethodSymbol methodInfo = (IMethodSymbol)info.Symbol;
                        var localDecls = new List<LocalDeclarationStatementSyntax>();
                        var assignments = new List<AssignmentExpressionSyntax>();
                        var args = new List<ArgumentSyntax>();

                        for (int j = 0; j < methodInfo.Parameters.Count(); j++)
                        {
                            var parameter = methodInfo.Parameters[j];
                            var arg = ive.ArgumentList.Arguments[j];
                            if (parameter.IsAtomic && (parameter.RefKind == RefKind.Ref || parameter.RefKind == RefKind.Out))
                            {
                                //Generate local variable
                                var typeString = parameter.Type.ToString();
                                var type = DetermineSTMType(typeString);
                                var initializer = SyntaxFactory.EqualsValueClause(CreateObjectCreationExpression(type, CreateArgList(arg.Expression)));
                                var identifier = SyntaxFactory.Identifier("_" + Guid.NewGuid().ToString().Replace("-", string.Empty));
                                var varDecltor = SyntaxFactory.VariableDeclarator(identifier, null, initializer);
                                var varDecl = SyntaxFactory.VariableDeclaration(type, SyntaxFactory.SeparatedList(new List<VariableDeclaratorSyntax>() { varDecltor }));
                                var localDecl = SyntaxFactory.LocalDeclarationStatement(varDecl);
                                localDecls.Add(localDecl);

                                //Generate new argument
                                arg = SyntaxFactory.Argument(SyntaxFactory.IdentifierName(identifier));

                                //Generate assignment to actual parameter from local var;
                                var memberAccess = CreatePropertyAccess(SyntaxFactory.IdentifierName(identifier), "Value");
                                var assigment = SyntaxFactory.AssignmentExpression(SyntaxKind.SimpleAssignmentExpression, arg.Expression, memberAccess);
                                assignments.Add(assigment);
                            }

                            args.Add(arg);
                        }
                    }
                }

                tree = SyntaxFactory.SyntaxTree(root, tree.Options, tree.FilePath);
                newTrees[i] = tree;
            }

            return CSharpCompilation.Create(compilation.AssemblyName, newTrees, compilation.References, compilation.Options);
        }

        public struct MethodSignature
        {
            public string name;
            public List<string> paramTypes;
            public MethodSignature(string name, List<string> paramTypes)
            {
                this.name = name;
                this.paramTypes = paramTypes;
            }
            public override string ToString()
            {
                StringBuilder strB = new StringBuilder();
                strB.Append("Methodname: " + name + ", Parameter types: ");
                foreach (var pt in paramTypes)
                {
                    strB.Append(pt + " ");
                }
                return strB.ToString();
            }
        }

        private static void CheckMethodSignatures(Microsoft.CodeAnalysis.CSharp.CSharpCompilation compilation)
        {
            var newTrees = compilation.SyntaxTrees.ToArray();
            foreach(var tree in newTrees)
            {
                var classDcls = tree.GetRoot().DescendantNodes().OfType<TypeDeclarationSyntax>(); //class and struct

                //check method signatures for each typedeclaration
                foreach(var cDcl in classDcls)
                {
                    var methodDcls = cDcl.DescendantNodes().OfType<MethodDeclarationSyntax>().ToList();
                    List<MethodSignature> methodSigs = new List<MethodSignature>();
                    
                    //Build method signatures
                    foreach(var mDcl in methodDcls)
                    {
                        string mName = mDcl.Identifier.ValueText;
                        var mParams = mDcl.ParameterList.Parameters;
                        var mTypeParams = new List<string>();

                        foreach(var mPar in mParams)
                        {
                            string mParType = mPar.Type.GetTypeString();
                            mTypeParams.Add(mParType);
                        }

                        MethodSignature m = new MethodSignature(mName, mTypeParams);
                        methodSigs.Add(m);
                    }

                    //Check for methodsigs with same name does not have same paramtypes
                    methodSigs = methodSigs.Where(m => methodSigs.Where(m2 => m.name == m2.name).Count() > 1).ToList();
                    Dictionary<string, List<MethodSignature>> groupedMethodSigs = new Dictionary<string, List<MethodSignature>>();
                    foreach(var ms in methodSigs)
                    {
                        List<MethodSignature> value;
                        if (groupedMethodSigs.ContainsKey(ms.name))
                        {
                            value = groupedMethodSigs[ms.name];
                            value.Add(ms);
                            groupedMethodSigs[ms.name] = value;
                        }
                        else
                        {
                            value = new List<MethodSignature>() { ms };
                            groupedMethodSigs.Add(ms.name, value);
                        }
                    }

                    //alternative method til at udregne groupedMethodSigs
                    //var groupedMethodSigs = methodSigs.GroupBy(ms => ms.name, ms => ms, (name, ms) => new { Name = name, MethodSigs = ms }).ToList();

                    //Replace transactional types to original types
                    var copyDic = new Dictionary<string, List<MethodSignature>>(groupedMethodSigs); //copy needed inorder to update dic while looping
                    foreach (var kvpair in copyDic)
                    {
                        List<MethodSignature> newMsSignatures = new List<MethodSignature>(); //replace 
                        foreach(var ms in kvpair.Value)
                        {
                            List<string> newMsParams = new List<string>();
                            foreach(var msParamType in ms.paramTypes)
                            {
                                newMsParams.Add(DetermineOriginalType(msParamType));
                            }
                            newMsSignatures.Add(new MethodSignature(ms.name, newMsParams));
                        }
                        groupedMethodSigs[kvpair.Key] = newMsSignatures;
                    }

                    //Check if there are identical methodsignatures
                    foreach(List<MethodSignature> msList in groupedMethodSigs.Values)
                    {
                        List<MethodSignature> identicalMethodSigs = msList.Where(ms => msList.Where(ms2 => ms.name == ms2.name && ms.paramTypes.SequenceEqual(ms2.paramTypes)).Count() > 1).ToList();
                        if(identicalMethodSigs.Any())
                        {
                            StringBuilder strB = new StringBuilder();
                            strB.Append("Identical method signatures exists:");
                            int i = 0;
                            while(i < identicalMethodSigs.Count)
                            {
                                strB.AppendLine();
                                strB.Append("[" + i + "]: " + identicalMethodSigs[i].ToString());
                                i++;
                            }
                            throw new Exception(strB.ToString());
                        }
                    }
                }
            }
        }

        private static string DetermineOriginalType(string typeString)
        {
            string originalType = "";

            if(typeString.StartsWith("TMVar<"))
            {
                originalType = typeString.Substring(6, typeString.Length - 7);
            }
            else
            {
                switch (typeString)
                {
                    case "TMInt":
                    case "TMLong":
                    case "TMDouble":
                    case "TMFloat":
                    case "TMUint":
                    case "TMUlong":
                        originalType = FirstCharToLower(typeString.Substring(2));
                        break;
                    default:
                        originalType = typeString;
                        break;
                }
            }
            
            return originalType;
        }

        private static CSharpCompilation ReplaceMemberAccesses(CSharpCompilation compilation)
        {
            var newTrees = compilation.SyntaxTrees.ToArray();
            for (int i = 0; i < compilation.SyntaxTrees.Length; i++)
            {
                var replacedNodes = new HashSet<int>();
                var replaced = true;
                var currentPos = -1;
                while (replaced)
                {
                    var tree = compilation.SyntaxTrees[i];
                    var root = tree.GetRoot();
                    var semanticModel = compilation.GetSemanticModel(tree);

                    var memberAccess = root.DescendantNodes().OfType<MemberAccessExpressionSyntax>().FirstOrDefault((ma) => currentPos < ma.Span.End && IsAtomicType(semanticModel.GetTypeInfo(ma)));
                    if (memberAccess != null)
                    {
                        root = root.ReplaceNode(memberAccess, ReplaceMemberAccess(memberAccess));
                        currentPos = memberAccess.Span.End;
                    }
                    else
                    {
                        replaced = false;
                    }

                    tree = SyntaxFactory.SyntaxTree(root, tree.Options, tree.FilePath);
                    newTrees[i] = tree;
                    compilation = CSharpCompilation.Create(compilation.AssemblyName, newTrees, compilation.References, compilation.Options);
                }
            }
            return compilation;
        }

        private static bool ReplaceMemberAccessCondition(MemberAccessExpressionSyntax ma)
        {
            return !(ma.Parent is MemberAccessExpressionSyntax && ((MemberAccessExpressionSyntax)ma.Parent).Name.Identifier.ValueText == "Value");
        }

        private static MemberAccessExpressionSyntax ReplaceMemberAccess(MemberAccessExpressionSyntax ma)
        {
            var replacement = CreatePropertyAccess(ma, "Value");
            return replacement;
        }

        private static MemberAccessExpressionSyntax CreatePropertyAccess(ExpressionSyntax expression, string property)
        {
            return SyntaxFactory.MemberAccessExpression(SyntaxKind.SimpleMemberAccessExpression, expression, SyntaxFactory.IdentifierName(property));
        }

        private static CSharpCompilation ReplaceConstructorArguments(CSharpCompilation compilation)
        {
            var newTrees = new SyntaxTree[compilation.SyntaxTrees.Length];

            for (int i = 0; i < compilation.SyntaxTrees.Length; i++)
            {
                var tree = compilation.SyntaxTrees[i];
                var root = tree.GetRoot();
                var semanticModel = compilation.GetSemanticModel(tree);


                var methodCalls = root.DescendantNodes().OfType<ObjectCreationExpressionSyntax>();
                root = root.ReplaceNodes(methodCalls, (oldnode, newnode) => ReplaceConstructorArgument(semanticModel, oldnode));

                tree = SyntaxFactory.SyntaxTree(root, tree.Options, tree.FilePath);
                newTrees[i] = tree;
            }

            return CSharpCompilation.Create(compilation.AssemblyName, newTrees, compilation.References, compilation.Options);
        }


        private static ObjectCreationExpressionSyntax ReplaceConstructorArgument(SemanticModel semanticModel, ObjectCreationExpressionSyntax oce)
        {
            var info = semanticModel.GetSymbolInfo(oce);
            if (info.Symbol != null)
            {
                IMethodSymbol methodInfo = (IMethodSymbol)info.Symbol;
                List<ArgumentSyntax> args;
                bool hasAtomicParam;
                CreateReplacementArgList(oce.ArgumentList, methodInfo, out args, out hasAtomicParam);

                if (hasAtomicParam)
                {
                    oce = oce.WithArgumentList(CreateArgList(args));
                }
            }

            return oce;
        }

        private static void CreateReplacementArgList(ArgumentListSyntax arglist, IMethodSymbol methodInfo, out List<ArgumentSyntax> args, out bool hasAtomicParam)
        {
            args = new List<ArgumentSyntax>();
            hasAtomicParam = false;
            for (int i = 0; i < methodInfo.Parameters.Count(); i++)
            {
                var parameter = methodInfo.Parameters[i];
                var arg = arglist.Arguments[i];
                if (parameter.IsAtomic)
                {
                    hasAtomicParam = true;
                    var typeString = parameter.Type.ToString();
                    var type = DetermineSTMType(typeString);
                    arg = SyntaxFactory.Argument(CreateObjectCreationExpression(type, CreateArgList(arg.Expression)));
                }

                args.Add(arg);
            }
        }

        private static CSharpCompilation ReplaceParameters(CSharpCompilation compilation)
        {
            var newTrees = new SyntaxTree[compilation.SyntaxTrees.Length];
            for (int i = 0; i < compilation.SyntaxTrees.Length; i++)
            {
                var tree = compilation.SyntaxTrees[i];
                var root = tree.GetRoot();
                //replace atomic parameter types
                List<ParameterSyntax> allParams = root.DescendantNodes().Where(node => node.IsKind(SyntaxKind.Parameter)).Cast<ParameterSyntax>().ToList();
                var atomicParams = allParams.Where(node => node.Modifiers.Any(SyntaxKind.AtomicKeyword));
                root = root.ReplaceNodes(atomicParams, (oldnode, newnode) => ReplaceParam(oldnode));

                tree = SyntaxFactory.SyntaxTree(root, tree.Options, tree.FilePath);
                newTrees[i] = tree;
            }

            return CSharpCompilation.Create(compilation.AssemblyName, newTrees, compilation.References, compilation.Options);
        }

        private static CSharpCompilation ReplaceMethodArguments(CSharpCompilation compilation)
        {
            var newTrees = new SyntaxTree[compilation.SyntaxTrees.Length];

            for (int i = 0; i < compilation.SyntaxTrees.Length; i++)
            {
                var tree = compilation.SyntaxTrees[i];
                var root = tree.GetRoot();
                var semanticModel = compilation.GetSemanticModel(tree);

                var methodCalls = root.DescendantNodes().OfType<InvocationExpressionSyntax>();
                root = root.ReplaceNodes(methodCalls, (oldnode, newnode) => ReplaceMethodArgument(semanticModel, oldnode));

                tree = SyntaxFactory.SyntaxTree(root, tree.Options, tree.FilePath);
                newTrees[i] = tree;
            }

            return CSharpCompilation.Create(compilation.AssemblyName, newTrees, compilation.References, compilation.Options);
        }

        private static CSharpCompilation ReplaceAtomicVariableUsage(CSharpCompilation compilation)
        {
            var newTrees = new SyntaxTree[compilation.SyntaxTrees.Length];

            for (int i = 0; i < compilation.SyntaxTrees.Length; i++)
            {
                var tree = compilation.SyntaxTrees[i];
                var root = tree.GetRoot();
                var semanticModel = compilation.GetSemanticModel(tree);

                var tmVarIdentifiers = root.DescendantNodes().OfType<IdentifierNameSyntax>()
                    .Where(iden => ReplaceCondition(iden) && IsAtomicType(semanticModel.GetTypeInfo(iden)));
                root = root.ReplaceNodes(tmVarIdentifiers, (oldnode, newnode) => ReplaceIdentifier(oldnode));

                tree = SyntaxFactory.SyntaxTree(root, tree.Options, tree.FilePath);
                newTrees[i] = tree;
            }

            return CSharpCompilation.Create(compilation.AssemblyName, newTrees, compilation.References, compilation.Options);
        }

        private static InvocationExpressionSyntax ReplaceMethodArgument(SemanticModel semanticModel, InvocationExpressionSyntax ive)
        {
            var info = semanticModel.GetSymbolInfo(ive);
            if (info.Symbol != null)
            {
                IMethodSymbol methodInfo = (IMethodSymbol)info.Symbol;
                List<ArgumentSyntax> args;
                bool hasAtomicParam;
                CreateReplacementArgList(ive.ArgumentList, methodInfo, out args, out hasAtomicParam);

                if (hasAtomicParam)
                {
                    ive = ive.WithArgumentList(CreateArgList(args));
                }
            }

            return ive;
        }

        private static bool ReplaceCondition(IdentifierNameSyntax iden)
        {
            if (iden.Parent is VariableDeclarationSyntax 
                || iden.Parent is ParameterSyntax 
                || iden.Parent is MemberAccessExpressionSyntax)
            {
                return false;
            }

            if (iden.Parent is QualifiedNameSyntax && iden.Parent.Parent != null && 
                (iden.Parent.Parent is VariableDeclarationSyntax 
                || iden.Parent.Parent is ParameterSyntax 
                || iden.Parent.Parent is MemberAccessExpressionSyntax))
            {
                return false;
            }

            if (iden.Parent is PrefixUnaryExpressionSyntax)
            {
                var parent = iden.Parent as PrefixUnaryExpressionSyntax;
                if (parent.OperatorToken.IsKind(SyntaxKind.PlusPlusToken) || parent.OperatorToken.IsKind(SyntaxKind.MinusMinusToken))
                {
                    return false;
                }
            }

            if (iden.Parent is PostfixUnaryExpressionSyntax)
            {
                var parent = iden.Parent as PostfixUnaryExpressionSyntax;
                if (parent.OperatorToken.IsKind(SyntaxKind.PlusPlusToken) || parent.OperatorToken.IsKind(SyntaxKind.MinusMinusToken))
                {
                    return false;
                }
            }

            return true;
        }

        private static MemberAccessExpressionSyntax ReplaceIdentifier(IdentifierNameSyntax iden)
        {
            var valueIden = SyntaxFactory.IdentifierName("Value");
            var newNode = SyntaxFactory.MemberAccessExpression(SyntaxKind.SimpleMemberAccessExpression, iden, SyntaxFactory.Token(SyntaxKind.DotToken), valueIden);
            return newNode;
        }

        private static bool IsAtomicType(TypeInfo typeInfo)
        {
            bool isAtomic = false;
            if (typeInfo.Type != null && typeInfo.Type.ContainingNamespace != null && typeInfo.Type.ContainingNamespace.ToString() == STMNameSpace)
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

            foreach (var atomicProperty in atomicProperties)
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



        private static NameSyntax DetermineSTMType(string typeString)
        {
            string aFFullTypeStr = "";
            switch (typeString)
            {
                case "int":
                case "long":
                case "double":
                case "float":
                case "uint":
                case "ulong":
                    aFFullTypeStr = "TM" + FirstCharToUpper(typeString);
                    break;
                case "string":
                default:
                    aFFullTypeStr = "TMVar<" + typeString + ">";
                    break;
            }

            var newTypeDcl = SyntaxFactory.ParseName(PreprendNameSpace(aFFullTypeStr) + " "); //whitespace needed to seperate type from name
            return newTypeDcl;
        }

        private static NameSyntax DetermineSTMType(TypeSyntax aFType, SemanticModel semanticModel)
        {
            string aFTypeString = aFType.GetTypeString();
            if (aFTypeString == "var")
            {
                aFTypeString = InferType(aFType, semanticModel).Type.ToString();
            }
            return DetermineSTMType(aFTypeString);
        }

        private static NameSyntax DetermineSTMType(TypeSyntax aFType)
        {
            string aFTypeString = aFType.GetTypeString();
            return DetermineSTMType(aFTypeString);
        }

        private static TypeInfo InferType(TypeSyntax typeString, SemanticModel semanticModel)
        {
            return semanticModel.GetTypeInfo(typeString);
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
        private static string FirstCharToLower(string s)
        {
            // Check for empty string.
            if (string.IsNullOrEmpty(s))
            {
                return string.Empty;
            }
            // Return char and concat substring.
            return char.ToLower(s[0]) + s.Substring(1);
        }


        private static ParameterSyntax ReplaceParam(ParameterSyntax aParam) //TODO: Der skal nok laves noget specielt med params, ref og out
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

        private static SyntaxNode ReplaceRetry(SyntaxNode root)
        {
            //replace retry's
            var retryReplaceDic = new Dictionary<RetryStatementSyntax, ExpressionStatementSyntax>();
            var retryNodes = root.DescendantNodes().Where(node => node.IsKind(SyntaxKind.RetryStatement)).ToList();
            foreach (RetryStatementSyntax rNode in retryNodes)
            {
                var retryInvoNode = SyntaxFactory.ExpressionStatement(
                    SyntaxFactory.InvocationExpression(SyntaxFactory.ParseName(PreprendNameSpace("STMSystem.Retry"))));
                retryReplaceDic.Add(rNode, retryInvoNode);
            }
            return root.ReplaceNodes(retryReplaceDic.Keys, (oldnode, newnode) => retryReplaceDic[oldnode]);
        }

        private static IEnumerable<AtomicStatementSyntax> GetAtomics(SyntaxNode root)
        {
            return root.DescendantNodes().OfType<AtomicStatementSyntax>();
        }

        private static SyntaxNode ReplaceAtomicOrElseBlocks(SyntaxNode root)
        {
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
            return root;
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
                SyntaxFactory.ParseName(PreprendNameSpace("STMSystem.Atomic")),
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

        private static SyntaxNode ReplaceFieldTypes(SyntaxNode root)
        {
            //replace atomic field types
            List<FieldDeclarationSyntax> allFields = root.DescendantNodes().Where(node => node.IsKind(SyntaxKind.FieldDeclaration)).Cast<FieldDeclarationSyntax>().ToList();
            var atomicFields = allFields.Where(node => node.Modifiers.Any(SyntaxKind.AtomicKeyword));
            return root.ReplaceNodes(atomicFields, (oldnode, newnode) => ReplaceFieldDecl(oldnode));
        }

        private static CSharpCompilation ReplaceLocalVars(CSharpCompilation compilation)
        {
            var newTrees = new SyntaxTree[compilation.SyntaxTrees.Length];

            for (int i = 0; i < compilation.SyntaxTrees.Length; i++)
            {
                var tree = compilation.SyntaxTrees[i];
                var root = tree.GetRoot();
                var semanticModel = compilation.GetSemanticModel(tree);

                //replace atomic local var dcl types
                var allLocals = root.DescendantNodes().Where(node => node.IsKind(SyntaxKind.LocalDeclarationStatement)).Cast<LocalDeclarationStatementSyntax>().ToList();
                var atomicLocals = allLocals.Where(node => node.Modifiers.Any(SyntaxKind.AtomicKeyword));

                root = root.ReplaceNodes(atomicLocals, (oldnode, newnode) => ReplaceLocalVar(oldnode, semanticModel));
                tree = SyntaxFactory.SyntaxTree(root, tree.Options, tree.FilePath);
                newTrees[i] = tree;
            }
            return CSharpCompilation.Create(compilation.AssemblyName, newTrees, compilation.References, compilation.Options);
        }

        private static LocalDeclarationStatementSyntax ReplaceLocalVar(LocalDeclarationStatementSyntax aLocal, SemanticModel semanticModel)
        {
            //Remove atomic from modifier list
            var newLocalDecl = aLocal.WithModifiers(RemoveAtomicMod(aLocal.Modifiers));

            //Change declaration type to our TMVar type (or specific like TMInt)
            newLocalDecl = newLocalDecl.WithDeclaration(ConstructVariableDeclaration(aLocal.Declaration, semanticModel));
            return newLocalDecl;
        }

        private static SyntaxTokenList RemoveAtomicMod(SyntaxTokenList list)
        {
            var newModifierList = list.Where(mod => !mod.IsKind(SyntaxKind.AtomicKeyword)).ToList();
            return SyntaxFactory.TokenList(newModifierList);
        }

        private static VariableDeclarationSyntax ConstructVariableDeclaration(VariableDeclarationSyntax aVarDcl, SemanticModel semanticModel = null)
        {
            var newTypeDcl = DetermineSTMType(aVarDcl.Type, semanticModel);
            
            var buffer = new List<VariableDeclaratorSyntax>();
            foreach (var variable in aVarDcl.Variables)
            {
                ArgumentListSyntax argList;
                if (variable.Initializer != null)
                {
                    argList = CreateArgList(variable.Initializer.Value);
                }
                else
                {
                    argList = SyntaxFactory.ArgumentList();
                }

                var initExpression = CreateObjectCreationExpression(newTypeDcl, argList);
                var newVarDeclarator = variable.WithInitializer(SyntaxFactory.EqualsValueClause(initExpression));
                buffer.Add(newVarDeclarator);
            }

            return SyntaxFactory.VariableDeclaration(newTypeDcl, SyntaxFactory.SeparatedList<VariableDeclaratorSyntax>(buffer));
        }

        private static ArgumentListSyntax CreateArgList(ExpressionSyntax expr)
        {
            return CreateArgList(new List<ArgumentSyntax> { SyntaxFactory.Argument(expr) });
        }

        private static ArgumentListSyntax CreateArgList(IEnumerable<ArgumentSyntax> args)
        {
            var argListContent = SyntaxFactory.SeparatedList(args);
            return SyntaxFactory.ArgumentList(SyntaxFactory.Token(SyntaxKind.OpenParenToken), argListContent, SyntaxFactory.Token(SyntaxKind.CloseParenToken));
        }

        private static ObjectCreationExpressionSyntax CreateObjectCreationExpression(TypeSyntax typeSyntax, ArgumentListSyntax argList)
        {
            return SyntaxFactory.ObjectCreationExpression(SyntaxFactory.Token(SyntaxKind.NewKeyword), typeSyntax, argList, null);
        }

        private static string PreprendNameSpace(string str)
        {
            return STMNameSpace + "." + str;
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
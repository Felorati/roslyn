using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using System.IO;
using Microsoft.CodeAnalysis.CSharp.Syntax.InternalSyntax;
using System.Collections.Immutable;

namespace STMExtension
{
    public class STM
    {
        public static readonly string STMNameSpace = "STM.Implementation.Lockbased";

        public static void ExtendCompilation(ref CSharpCompilation compilation, string stmIntermediateOutputPath)
        {
            List<Diagnostic> stmDiagnostics = new List<Diagnostic>();
            var skipLists = new List<List<IdentifierNameSyntax>>();
            for (int i = 0; i < compilation.SyntaxTrees.Length; i++)
            {
                skipLists.Add(new List<IdentifierNameSyntax>());
            }

            CheckRetryPlacement(compilation, stmDiagnostics);

            compilation = STMAtomicOrelse.ReplaceAtomicOrElseBlocks(compilation);
            compilation = STMRetry.ReplaceRetryStatements(compilation);
            compilation = STMIdentifyVariables.IdentifyVariables(compilation);
            compilation = STMProperties.ReplaceProperties(compilation);
            compilation = STMMethodArgs.ReplaceMethodArguments(compilation);
            compilation = STMRefOut.ReplaceAtomicRefOut(compilation, stmDiagnostics, skipLists);
            compilation = STMAtomicOut.HandleAtomicOutParameters(compilation, skipLists);
            compilation = STMLocalVars.ReplaceLocalVars(compilation);
            compilation = STMFields.ReplaceFieldTypes(compilation);
            compilation = STMConstructor.ReplaceConstructorArguments(compilation);
            compilation = STMParameters.ReplaceParameters(compilation);
            compilation = STMAtomicVariableUsage.ReplaceAtomicVariableUsage(compilation, skipLists);
            compilation = STMMemberAccess.ReplaceMemberAccesses(compilation);
            compilation = STMAtomicModifiers.RemoveAtomicModifiers(compilation);

            CheckMethodSignatures(compilation, stmDiagnostics); //Ensure two overloaded methods fx. does not have a TMInt and int param at the same position

            if (stmIntermediateOutputPath != null)
            {
                if (File.Exists(stmIntermediateOutputPath)) //Cleaning file before appending to it
                    File.WriteAllText(stmIntermediateOutputPath, "");

                foreach (var tree in compilation.SyntaxTrees)
                {
                    PrintDebugSource(tree, stmIntermediateOutputPath);
                }
            }

            compilation.AddSTMDiagnostics(stmDiagnostics.ToImmutableArray());
        }

        internal static FieldDeclarationSyntax RemoveAtomicModifier(FieldDeclarationSyntax aField)
        {
            //Remove atomic from modifier list
            var newFieldDcl = aField.WithModifiers(RemoveAtomicMod(aField.Modifiers));
            return newFieldDcl;
        }

        internal static LocalDeclarationStatementSyntax RemoveAtomicModifier(LocalDeclarationStatementSyntax aLocal)
        {
            //Remove atomic from modifier list
            var newLocalDecl = aLocal.WithModifiers(RemoveAtomicMod(aLocal.Modifiers));
            return newLocalDecl;
        }

        internal static ParameterSyntax RemoveAtomicModifier(ParameterSyntax aParam) //TODO: Der skal nok laves noget specielt med params, ref og out
        {
            //Remove atomic from modifier
            var newParam = aParam.WithModifiers(RemoveAtomicMod(aParam.Modifiers));
            return newParam;
        }

        internal static bool IsVariableSymbol(ISymbol symbol)
        {
            if (symbol != null)
            {
                return symbol is IParameterSymbol || symbol is ILocalSymbol || symbol is IFieldSymbol;
            }

            return false;
        }

        internal static bool IsAtomicSymbol(ISymbol symbol)
        {
            if (symbol != null)
            {
                if (symbol is IParameterSymbol)
                {
                    return ((IParameterSymbol)symbol).IsAtomic;
                }

                if (symbol is ILocalSymbol)
                {
                    return ((ILocalSymbol)symbol).IsAtomic;
                }

                if (symbol is IFieldSymbol)
                {
                    return ((IFieldSymbol)symbol).IsAtomic;
                }
            }

            return false;
        }

        internal  static LocalDeclarationStatementSyntax CreateLocalDeclaration(TypeSyntax type, ExpressionSyntax initExpr, out string identifierString, out SyntaxToken identifier)
        {
            var initializer = SyntaxFactory.EqualsValueClause(initExpr);
            identifierString = "_" + Guid.NewGuid().ToString().Replace("-", string.Empty);
            identifier = SyntaxFactory.Identifier(identifierString);
            var varDecltor = SyntaxFactory.VariableDeclarator(identifier, null, initializer);
            var varDecl = SyntaxFactory.VariableDeclaration(type, SyntaxFactory.SeparatedList(new List<VariableDeclaratorSyntax>() { varDecltor }));
            return SyntaxFactory.LocalDeclarationStatement(varDecl);
        }

        private static IEnumerable<MethodDeclarationSyntax> FilterOutDuplicates(IEnumerable<MethodDeclarationSyntax> methodDecls)
        {
            HashSet<string> passedValues = new HashSet<string>();

            foreach (var item in methodDecls)
            {
                var name = item.Identifier.ToString();
                if (passedValues.Contains(name))
                {
                    continue;
                }
                else
                {
                    passedValues.Add(name);
                    yield return item;
                }
            }
        }

        private static int TotalSpanLength(IEnumerable<SyntaxNode> nodes)
        {
            return nodes.Aggregate<SyntaxNode, int>(0, (acc, cur) => acc + cur.Span.Length);
        }


        public class MethodSignature
        {
            public string name;
            public List<MsParam> paramTypes;
            public TypeDeclarationSyntax definingType;
            public MethodSignature(string name, List<MsParam> paramTypes, TypeDeclarationSyntax definingTypeLoc)
            {
                this.name = name;
                this.paramTypes = paramTypes;
                this.definingType = definingTypeLoc;
            }
            public override string ToString()
            {
                StringBuilder strB = new StringBuilder();
                strB.Append("Methodname: " + name + ", Parameter types: ");
                foreach (var pt in paramTypes)
                {
                    if(pt.isRefOrOut)
                        strB.Append("ref/out ");
                    strB.Append(pt.type + " ");
                }
                return strB.ToString();
            }
        }
        public class MsParam
        {
            public string type;
            public bool isRefOrOut;

            public MsParam(string type, bool isRefOrOut)
            {
                this.type = type;
                this.isRefOrOut = isRefOrOut;
            }

            public override bool Equals(object obj)
            {
                var item = obj as MsParam;

                if (item == null)
                {
                    return false;
                }
                return this.type.Equals(item.type) && this.isRefOrOut.Equals(item.isRefOrOut);
            }

            public override int GetHashCode()
            {
                int res = 1337;
                res = 31 * res + type.GetHashCode();
                res = 31 * res + Convert.ToInt32(isRefOrOut);
                return res;
            }
        }

        private static void CheckRetryPlacement(CSharpCompilation compilation, List<Diagnostic> stmDiagnostics)
        {
            foreach (var tree in compilation.SyntaxTrees)
            {
                var retryStatements = tree.GetRoot().DescendantNodes().OfType<RetryStatementSyntax>();

                foreach (var rs in retryStatements)
                {
                    var node = rs.AttemptToGetParent<AtomicStatementSyntax, OrelseSyntax>();
                    if (node == null)
                    {
                        stmDiagnostics.Add(Diagnostic.Create(STMErrorDescriptors.DD_INVALID_RETRY_PLACEMENT, rs.GetLocation()));
                    }
                }
            }
        }

        private static void CheckMethodSignatures(CSharpCompilation compilation, List<Diagnostic> stmDiagnostics)
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
                        var mTypeParams = new List<MsParam>();

                        foreach(var mPar in mParams)
                        {
                            string mParType = mPar.Type.GetTypeString();
                            bool isRefOut = mPar.Modifiers.Any(SyntaxKind.OutKeyword) || mPar.Modifiers.Any(SyntaxKind.RefKeyword);
                            MsParam msPar = new MsParam(mParType, isRefOut);
                            mTypeParams.Add(msPar);
                        }

                        MethodSignature m = new MethodSignature(mName, mTypeParams, cDcl);
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

                    //alternative method to produce groupedMethodSigs
                    //var groupedMethodSigs = methodSigs.GroupBy(ms => ms.name, ms => ms, (name, ms) => new { Name = name, MethodSigs = ms }).ToList();

                    //Replace transactional types to original types (for checking if some are equal)
                    var copyDic = new Dictionary<string, List<MethodSignature>>(groupedMethodSigs); //copy needed inorder to update dic while looping
                    foreach (var kvpair in copyDic)
                    {
                        List<MethodSignature> newMsSignatures = new List<MethodSignature>();
                        foreach(var ms in kvpair.Value)
                        {
                            List<MsParam> newMsParams = new List<MsParam>();
                            foreach(var msParamType in ms.paramTypes)
                            {
                                var msParam = new MsParam(DetermineOriginalType(msParamType.type), msParamType.isRefOrOut);
                                newMsParams.Add(msParam);
                            }
                            newMsSignatures.Add(new MethodSignature(ms.name, newMsParams, ms.definingType));
                        }
                        groupedMethodSigs[kvpair.Key] = newMsSignatures;
                    }

                    //Check if there are identical methodsignatures
                    foreach (List<MethodSignature> msList in groupedMethodSigs.Values)
                    {
                        List<MethodSignature> identicalMethodSigs = msList.Where(ms => msList.Where(ms2 => ms.name == ms2.name && ms.paramTypes.SequenceEqual(ms2.paramTypes)).Count() > 1).ToList();
                        if(identicalMethodSigs.Any())
                        {
                            //Build 
                            StringBuilder strB = new StringBuilder();
                            string declaringType = identicalMethodSigs.First().definingType.Identifier.Text;
                            strB.Append("The type " + declaringType + " cannot define identical overloaded methods (atomic int and int does not differ):");
                            int i = 0;
                            while(i < identicalMethodSigs.Count)
                            {
                                strB.AppendLine();
                                strB.Append("[" + i + "]: " + identicalMethodSigs[i].ToString());
                                i++;
                            }

                            //Build diagnostics error
                            DiagnosticDescriptor dDes = new DiagnosticDescriptor("IdenticalMethods", "Identical method overloads not allowed", strB.ToString(), "Typing", DiagnosticSeverity.Error, true);
                            Diagnostic dia = Diagnostic.Create(dDes, identicalMethodSigs.First().definingType.GetLocation());
                            stmDiagnostics.Add(dia);
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


        internal static MemberAccessExpressionSyntax CreatePropertyAccess(ExpressionSyntax expression, string property)
        {
            return SyntaxFactory.MemberAccessExpression(SyntaxKind.SimpleMemberAccessExpression, expression, SyntaxFactory.IdentifierName(property));
        }

        internal static void CreateReplacementArgList(ArgumentListSyntax arglist, IMethodSymbol methodInfo, out List<ArgumentSyntax> args, out bool hasAtomicParam)
        {
            args = new List<ArgumentSyntax>();
            hasAtomicParam = false;
            for (int i = 0; i < arglist.Arguments.Count; i++)
            {
                var arg = arglist.Arguments[i];
                var parameter = GetParameterForArg(methodInfo, arg, i);
                if (parameter != null && parameter.IsAtomic && !parameter.IsRefOrOut())
                {
                    hasAtomicParam = true;
                    var typeString = parameter.Type.ToString();
                    var type = DetermineSTMType(typeString);
                    arg = SyntaxFactory.Argument(CreateObjectCreationExpression(type, CreateArgList(arg.Expression)));
                }

                args.Add(arg);
            }
        }

        internal static IParameterSymbol GetParameterForArg(IMethodSymbol methodSymbol, ArgumentSyntax arg, int index)
        {
            if (arg.NameColon != null)
            {
                IParameterSymbol param = null;
                int i = 0;
                bool found = false;
                while (!found && i < methodSymbol.Parameters.Length)
                {
                    var temp = methodSymbol.Parameters[i];
                    if (temp.Name != null & temp.Name == arg.NameColon.ToString())
                    {
                        param = temp;
                        found = true;
                    }
                    i++;
                }

                return param;
            }
            else
            {
                if (index < methodSymbol.Parameters.Length)
                {
                    return methodSymbol.Parameters[index];
                }
                else
                {
                    return null;
                }
               
            }
        }

        internal static bool ReplaceCondition(IdentifierNameSyntax iden, SemanticModel model)
        {
            var symbolInfo = model.GetSymbolInfo(iden);
            if (symbolInfo.Symbol != null)
            {
                if (symbolInfo.Symbol is IParameterSymbol || symbolInfo.Symbol is ILocalSymbol || symbolInfo.Symbol is IFieldSymbol)
                {
                    if (!IsAtomicSymbol(symbolInfo.Symbol))
                    {
                        return false;
                    }

                    if (iden.Parent is VariableDeclarationSyntax
                    || iden.Parent is ParameterSyntax
                    || (iden.Parent is MemberAccessExpressionSyntax && (iden.Parent as MemberAccessExpressionSyntax).Name == iden)
                    || iden.Parent is InvocationExpressionSyntax)
                    {
                        return false;
                    }

                    var nonQualifiedParent = iden.AttemptToGetParentNoOfType<QualifiedNameSyntax>();
                    if (iden.Parent != nonQualifiedParent && nonQualifiedParent != null &&
                        (nonQualifiedParent is VariableDeclarationSyntax
                        || nonQualifiedParent is ParameterSyntax
                        || (nonQualifiedParent is MemberAccessExpressionSyntax && (nonQualifiedParent as MemberAccessExpressionSyntax).Name == iden)
                        || nonQualifiedParent is InvocationExpressionSyntax))
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

                    /*
                    var ive = iden.AttemptToGetParentStop<InvocationExpressionSyntax,StatementSyntax>();
                    if (ive != null)
                    {
                        var methodInfo = model.GetSymbolInfo(ive);
                        if (methodInfo.Symbol != null)
                        {
                            var index = GetArgumentIndex(iden);
                            if (index != -1)
                            {
                                var symbol = (IMethodSymbol)methodInfo.Symbol;
                                var param = symbol.Parameters[index];
                                if (param.IsRefOrOut())
                                {
                                    return false;
                                }
                            }
                        }
                    }*/

                    return true;
                }
            }

            return false;
        }

        private static int GetArgumentIndex(IdentifierNameSyntax iden)
        {
            var arg = iden.AttemptToGetParentStop<ArgumentSyntax,StatementSyntax>();
            if (arg == null || arg.Parent.IsKind(SyntaxKind.BracketedArgumentList))
            {
                return -1;
            }

            var arglist = (ArgumentListSyntax)arg.Parent;
            for (int i = 0; i < arglist.Arguments.Count; i++)
            {
                var possibleArg = arglist.Arguments[i];
                if (arg == possibleArg)
                {
                    return i;
                }
            }

            return -1;
        }

        internal static bool IsAtomicType(TypeInfo typeInfo)
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

        private static void PrintDebugSource(SyntaxTree tree, string stmIntermediateOutputPath)
        {
            var formattedRoot = tree.GetRoot().NormalizeWhitespace();
            var textAfter = formattedRoot.GetText().ToString();
            //var appendText = "File: " + tree.FilePath + "\n" + textAfter + "\n\n";
            File.AppendAllText(stmIntermediateOutputPath, textAfter);
        }

        internal static NameSyntax DetermineSTMType(string typeString)
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

        internal static NameSyntax DetermineSTMType(TypeSyntax aFType, SemanticModel semanticModel)
        {
            string aFTypeString = aFType.GetTypeString();
            if (aFTypeString == "var")
            {
                aFTypeString = InferType(aFType, semanticModel).Type.ToString();
            }
            return DetermineSTMType(aFTypeString);
        }

        internal static NameSyntax DetermineSTMType(TypeSyntax aFType)
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

        internal static SyntaxTokenList RemoveAtomicMod(SyntaxTokenList list)
        {
            return RemoveModifiers(list, SyntaxKind.AtomicKeyword);
        }

        internal static SyntaxTokenList RemoveModifiers(SyntaxTokenList list, params SyntaxKind[] mods)
        {
            var newModifierList = list.Where(mod => !mods.Contains(mod.Kind()));
            return SyntaxFactory.TokenList(newModifierList);
        }

        internal static VariableDeclarationSyntax ConstructVariableDeclaration(VariableDeclarationSyntax aVarDcl, SemanticModel semanticModel = null)
        {
            var newTypeDcl = DetermineSTMType(aVarDcl.Type, semanticModel);
            
            var buffer = new List<VariableDeclaratorSyntax>();
            foreach (var variable in aVarDcl.Variables)
            {
                var argList = variable.Initializer != null ? CreateArgList(variable.Initializer.Value) : SyntaxFactory.ArgumentList();

                var initExpression = CreateObjectCreationExpression(newTypeDcl, argList);
                var newVarDeclarator = variable.WithInitializer(SyntaxFactory.EqualsValueClause(initExpression));
                buffer.Add(newVarDeclarator);
            }

            return SyntaxFactory.VariableDeclaration(newTypeDcl, SyntaxFactory.SeparatedList<VariableDeclaratorSyntax>(buffer));
        }

        internal static ArgumentListSyntax CreateArgList(ExpressionSyntax expr)
        {
            return CreateArgList(new List<ArgumentSyntax> { SyntaxFactory.Argument(expr) });
        }

        internal static ArgumentListSyntax CreateArgList(IEnumerable<ArgumentSyntax> args)
        {
            var argListContent = SyntaxFactory.SeparatedList(args);
            return SyntaxFactory.ArgumentList(SyntaxFactory.Token(SyntaxKind.OpenParenToken), argListContent, SyntaxFactory.Token(SyntaxKind.CloseParenToken));
        }

        internal static ObjectCreationExpressionSyntax CreateObjectCreationExpression(TypeSyntax typeSyntax, ArgumentListSyntax argList)
        {
            return SyntaxFactory.ObjectCreationExpression(SyntaxFactory.Token(SyntaxKind.NewKeyword), typeSyntax, argList, null);
        }

        internal static string PreprendNameSpace(string str)
        {
            return STMNameSpace + "." + str;
        }

        internal static bool HasAtomicModifier(SyntaxTokenList modifiers)
        {
            return HasModifier(modifiers, SyntaxKind.AtomicKeyword);
        }

        internal static bool HasModifier(SyntaxTokenList modifiers, SyntaxKind kind)
        {
            foreach (var mod in modifiers)
            {
                if (mod.IsKind(kind))
                {
                    return true;
                }
            }

            return false;
        }
    }
}

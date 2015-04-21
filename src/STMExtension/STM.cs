using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
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
                root = ReplaceAtomicOrElseBlocks(root);
                root = ReplaceRetry(root);

                //Create new syntax tree (based on new root), and update it as the current tree in the trees array
                SyntaxTree newTree = SyntaxFactory.SyntaxTree(root, tree.Options, tree.FilePath);
                trees[i] = newTree;
                //Get source text after transformation (for testing and debug purposes)
                //PrintDebugSource(newTree);
            }
        }

        public static void ExtendCompilation(ref CSharpCompilation compilation)
        {
            List<Diagnostic> stmDiagnostics = new List<Diagnostic>();

            compilation = ReplaceMethodArguments(compilation);
            List<List<IdentifierNameSyntax>> skipLists;
            compilation = ReplaceAtomicRefOut(compilation, stmDiagnostics, out skipLists);
            compilation = ReplaceLocalVars(compilation);
            compilation = ReplaceFieldTypes(compilation);
            compilation = ReplaceConstructorArguments(compilation);
            compilation = ReplaceParameters(compilation);
            compilation = ReplaceAtomicVariableUsage(compilation, skipLists);
            compilation = ReplaceMemberAccesses(compilation);

            CheckMethodSignatures(compilation, stmDiagnostics); //Ensure two overloaded methods does not have a TMInt and int param at the same position

            foreach (var tree in compilation.SyntaxTrees)
            {
                PrintDebugSource(tree);
            }

            compilation.AddSTMDiagnostics(stmDiagnostics.ToImmutableArray());
        }

        private class CompilationState
        {
            public CSharpCompilation Compilation { get; set; }
            public SyntaxTree[] NewTrees { get; set; }
            public SyntaxTree Tree { get; set; }
            public SyntaxNode Root { get; set; }
            public SemanticModel SemanticModel { get; set; }
            public CompilationState(CSharpCompilation compilation)
            {
                NewTrees = compilation.SyntaxTrees.ToArray();
                Compilation = compilation;
            }

            public void PrepIteration(int i)
            {
                Tree = Compilation.SyntaxTrees[i];
                Root = Tree.GetRoot();
            }

            public void UpdateState(int i)
            {
                Tree = SyntaxFactory.SyntaxTree(Root, Tree.Options, Tree.FilePath);
                NewTrees[i] = Tree;
                Compilation = CSharpCompilation.Create(Compilation.AssemblyName, NewTrees, Compilation.References, Compilation.Options);
                NewTrees = Compilation.SyntaxTrees.ToArray();
                Root = Tree.GetRoot();
                SemanticModel = Compilation.GetSemanticModel(Tree);
            }

        }

        private static CSharpCompilation ReplaceAtomicRefOut(CSharpCompilation compilation, List<Diagnostic> diagnostics, out List<List<IdentifierNameSyntax>> skipsLists)
        {
            var newTrees = compilation.SyntaxTrees.ToArray();
            var state = new CompilationState(compilation);
            skipsLists = new List<List<IdentifierNameSyntax>>();

            for (int i = 0; i < state.Compilation.SyntaxTrees.Length; i++)
            {
                var skipListBuffer = new List<string>();
                state.PrepIteration(i);

                var methodCalls = state.Root.DescendantNodes().OfType<InvocationExpressionSyntax>().ToList();
                state.Root  = state.Root.TrackNodes(methodCalls);
                state.UpdateState(i);

                var methods = new List<MethodDeclarationSyntax>(); 

                foreach (var item in methodCalls)
                {
                    var ive = state.Root.GetCurrentNode(item);
                    var info = state.SemanticModel.GetSymbolInfo(ive);
                    if (info.Symbol != null)
                    {
                        IMethodSymbol methodInfo = (IMethodSymbol)info.Symbol;
                        var localDecls = new List<LocalDeclarationStatementSyntax>();
                        var assignments = new List<ExpressionStatementSyntax>();
                        var args = new List<ArgumentSyntax>();
                        var replace = false;
                        for (int j = 0; j < methodInfo.Parameters.Count(); j++)
                        {
                            var parameter = methodInfo.Parameters[j];
                            var arg = ive.ArgumentList.Arguments[j];
                            if (parameter.IsRefOrOut())
                            {
                                var symbolInfo = state.SemanticModel.GetSymbolInfo(arg.Expression);
                                if ((parameter.IsAtomic || IsAtomicSymbol(symbolInfo.Symbol)) && !IsVariableSymbol(symbolInfo.Symbol))
                                {
                                    DiagnosticDescriptor dDes = new DiagnosticDescriptor("Invalid ref/out argument", "Invalid ref/out argument", "ref/out arguments must be either a field, local variable or parameter", "Typing", DiagnosticSeverity.Error, true);
                                    Diagnostic dia = Diagnostic.Create(dDes, arg.GetLocation());
                                    diagnostics.Add(dia);
                                }

                                if (parameter.IsAtomic)
                                {
                                    //Generate local variable
                                    var typeString = parameter.Type.ToString();
                                    var type = DetermineSTMType(typeString);
                                    string identifierString;
                                    SyntaxToken identifier;
                                    var localDecl = CreateLocalDeclaration(type, CreateObjectCreationExpression(type, CreateArgList(arg.Expression)), out identifierString, out identifier);
                                    localDecls.Add(localDecl);

                                    //Generate assignment to actual parameter from local var;
                                    var memberAccess = CreatePropertyAccess(SyntaxFactory.IdentifierName(identifier), "Value");
                                    var assigment = SyntaxFactory.ExpressionStatement(SyntaxFactory.AssignmentExpression(SyntaxKind.SimpleAssignmentExpression, arg.Expression, memberAccess));
                                    assignments.Add(assigment);

                                    var argIdentifier = SyntaxFactory.IdentifierName(identifier);
                                    skipListBuffer.Add(identifierString);
                                    //Generate new argument
                                    arg = arg.WithExpression(argIdentifier);
                                }
                                else if (IsAtomicSymbol(symbolInfo.Symbol))
                                {
                                    //Generate local variable
                                    var typeString = parameter.Type.ToString();
                                    var type = SyntaxFactory.ParseTypeName(typeString + " ");
                                    string identifierString;
                                    SyntaxToken identifier;
                                    var localDecl = CreateLocalDeclaration(type, arg.Expression, out identifierString, out identifier);
                                    localDecls.Add(localDecl);

                                    //Generate assignment to actual parameter from local var;
                                    var argIdentifier = SyntaxFactory.IdentifierName(identifier);
                                    var assigment = SyntaxFactory.ExpressionStatement(SyntaxFactory.AssignmentExpression(SyntaxKind.SimpleAssignmentExpression, arg.Expression, argIdentifier));
                                    assignments.Add(assigment);

                                    skipListBuffer.Add(identifierString);
                                    //Generate new argument
                                    arg = arg.WithExpression(argIdentifier);

                                }

                                replace = true;
                            }

                            args.Add(arg);
                        }

                        if (replace)
                        {
                            var methodSyntax = (MethodDeclarationSyntax)methodInfo.DeclaringSyntaxReferences[0].GetSyntax();
                            methods.Add(methodSyntax);
                            var originalStatement = ive.GetClosestStatementSyntax();
                            var statement = originalStatement;

                            state.Root = state.Root.TrackNodes(originalStatement, methodSyntax);
                            statement = state.Root.GetCurrentNode(originalStatement);
                            if (localDecls.Count > 0)
                            {
                                state.Root = state.Root.InsertNodesBefore(statement, localDecls);
                                statement = state.Root.GetCurrentNode(originalStatement);
                            }

                            if (assignments.Count > 0)
                            {
                                state.Root = state.Root.InsertNodesAfter(statement, assignments);
                                statement = state.Root.GetCurrentNode(originalStatement);
                            }
                           
                            var toReplace = state.Root.GetCurrentNode(item);
                            state.Root = state.Root.ReplaceNode(toReplace, ive.WithArgumentList(CreateArgList(args)));

                            state.UpdateState(i);
                        }
                    }

                }

                methods = FilterOutDuplicates(methods).ToList();
                var currentMethods = methods.Select(method => state.Root.GetCurrentNode(method)).ToList();
                state.Root = state.Root.ReplaceNodes(currentMethods, (oldNode, newNode) => ReplaceAtomicRefOutParams(oldNode));
                state.UpdateState(i);

                currentMethods = methods.Select(method => state.Root.GetCurrentNode(method)).ToList();

                var idensToSkip = new List<IdentifierNameSyntax>();
                foreach (var methodDecl in currentMethods)
                {
                    var outParameters = methodDecl.ParameterList.Parameters.Where(p => p.Modifiers.Any(SyntaxKind.OutKeyword) && p.Modifiers.Any(SyntaxKind.AtomicKeyword)).ToList();
                    var assignments = methodDecl.Body.Statements.OfType<ExpressionStatementSyntax>()
                            .Where(exprStatement => exprStatement.Expression is AssignmentExpressionSyntax && ((exprStatement.Expression as AssignmentExpressionSyntax).Left is IdentifierNameSyntax))
                            .Select(exprStatement => (exprStatement.Expression as AssignmentExpressionSyntax).Left as IdentifierNameSyntax);

                    foreach (var param in outParameters)
                    {

                        var res = assignments.Where(iden => iden.ToString() == param.Identifier.ToString()).First();
                        idensToSkip.Add(res);
                    }
                   
                }

                var refOutArgs = state.Root.DescendantNodes().OfType<IdentifierNameSyntax>().Where(iden => skipListBuffer.Contains(iden.Identifier.Text) && ReplaceCondition(iden,state.SemanticModel)).ToList();
                idensToSkip.AddRange(refOutArgs);
                state.Root = state.Root.TrackNodes(idensToSkip);
                skipsLists.Add(idensToSkip);
                
                state.UpdateState(i);
            }

            return state.Compilation;
        }

        private static bool IsVariableSymbol(ISymbol symbol)
        {
            if (symbol != null)
            {
                return symbol is IParameterSymbol || symbol is ILocalSymbol || symbol is IFieldSymbol;
            }

            return false;
        }

        private static bool IsAtomicSymbol(ISymbol symbol)
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

        private  static LocalDeclarationStatementSyntax CreateLocalDeclaration(TypeSyntax type, ExpressionSyntax initExpr, out string identifierString, out SyntaxToken identifier)
        {
            var initializer = SyntaxFactory.EqualsValueClause(initExpr);
            identifierString = "_" + Guid.NewGuid().ToString().Replace("-", string.Empty);
            identifier = SyntaxFactory.Identifier(identifierString);
            var varDecltor = SyntaxFactory.VariableDeclarator(identifier, null, initializer);
            var varDecl = SyntaxFactory.VariableDeclaration(type, SyntaxFactory.SeparatedList(new List<VariableDeclaratorSyntax>() { varDecltor }));
            return SyntaxFactory.LocalDeclarationStatement(varDecl);
        }

        private static MethodDeclarationSyntax ReplaceAtomicRefOutParams(MethodDeclarationSyntax methodDecl)
        {
            /*
            var parameters = new List<ParameterSyntax>();
            var outParameters = new List<ParameterSyntax>();

            foreach (var param in methodDecl.ParameterList.Parameters)
            {
                var hasOutMod = param.Modifiers.Any(SyntaxKind.OutKeyword);
                var hasRefMod = param.Modifiers.Any(SyntaxKind.RefKeyword);
                var hasAtomicMod = param.Modifiers.Any(SyntaxKind.AtomicKeyword);
                if (hasAtomicMod && (hasOutMod || hasRefMod))
                {
                    var newMods = RemoveModifiers(param.Modifiers, SyntaxKind.RefKeyword, SyntaxKind.OutKeyword);
                    parameters.Add(param.WithModifiers(newMods));
                    if (hasOutMod)
                    {
                        outParameters.Add(param);
                    }
                }
                else
                {
                    parameters.Add(param);
                }
            }



            var parameterList = methodDecl.ParameterList.WithParameters(SyntaxFactory.SeparatedList(parameters));
            var newDecl = methodDecl.WithParameterList(parameterList);
            */

            var outParameters = methodDecl.ParameterList.Parameters.Where(p => p.Modifiers.Any(SyntaxKind.OutKeyword) && p.Modifiers.Any(SyntaxKind.AtomicKeyword)).ToList();

            if (outParameters.Count > 0)
            {
                var statementBuffer = new List<StatementSyntax>();
                foreach (var param in outParameters)
                {
                    var identifier = SyntaxFactory.IdentifierName(param.Identifier);
                    var initializer = CreateObjectCreationExpression(DetermineSTMType(param.Type), SyntaxFactory.ArgumentList());
                    var assigment = SyntaxFactory.ExpressionStatement(SyntaxFactory.AssignmentExpression(SyntaxKind.SimpleAssignmentExpression, identifier, initializer));
                    statementBuffer.Add(assigment);
                }

                foreach (var statement in methodDecl.Body.Statements)
                {
                    statementBuffer.Add(statement);
                }

                methodDecl = methodDecl.WithBody(SyntaxFactory.Block(statementBuffer));
            }

            return methodDecl;
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
                if (parameter.IsAtomic && !parameter.IsRefOrOut())
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

        private static CSharpCompilation ReplaceAtomicVariableUsage(CSharpCompilation compilation, List<List<IdentifierNameSyntax>> skipLists)
        {
            var newTrees = new SyntaxTree[compilation.SyntaxTrees.Length];

            for (int i = 0; i < compilation.SyntaxTrees.Length; i++)
            {
                
                var tree = compilation.SyntaxTrees[i];
                var root = tree.GetRoot();
                var semanticModel = compilation.GetSemanticModel(tree);
                var skipList = skipLists[i].Select(iden => root.GetCurrentNode(iden)).ToList();
                /*
                var list = root.DescendantNodes().OfType<IdentifierNameSyntax>().ToList();
                foreach (var item in list)
                {
                    if (item.ToString() == "test")
                    {
                        var typeIndo = semanticModel.GetTypeInfo(item);
                        bool atomic = IsAtomicType(typeIndo);
                        bool condition = ReplaceCondition(item);
                    }
                }*/

                var tmVarIdentifiers = root.DescendantNodes().OfType<IdentifierNameSyntax>()
                    .Where(iden => !skipList.Contains(iden) && ReplaceCondition(iden, semanticModel) && IsAtomicType(semanticModel.GetTypeInfo(iden)));
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

        private static bool ReplaceCondition(IdentifierNameSyntax iden, SemanticModel model)
        {
            var symbolInfo = model.GetSymbolInfo(iden);
            if (symbolInfo.Symbol != null)
            {
                if (symbolInfo.Symbol is IParameterSymbol || symbolInfo.Symbol is ILocalSymbol || symbolInfo.Symbol is IFieldSymbol)
                {
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

                    var ive = iden.AttemptToGetParent<InvocationExpressionSyntax>();
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
                    }

                    return true;
                }
            }

            return false;
        }

        private static int GetArgumentIndex(IdentifierNameSyntax iden)
        {
            var arg = iden.AttemptToGetParent<ArgumentSyntax>();
            if (arg == null)
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
            var atomicProperties = allProperties.Where(node => node.Modifiers.Any(SyntaxKind.AtomicKeyword)).ToList();
            root = root.TrackNodes(atomicProperties);

            foreach (var item in atomicProperties)
            {
                var atomicProperty = root.GetCurrentNode(item);
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
            atomicProperties = allProperties.Where(node => node.Modifiers.Any(SyntaxKind.AtomicKeyword)).ToList();
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

        private static CSharpCompilation ReplaceFieldTypes(CSharpCompilation compilation)
        {
            var newTrees = new SyntaxTree[compilation.SyntaxTrees.Length];

            for (int i = 0; i < compilation.SyntaxTrees.Length; i++)
            {
                var tree = compilation.SyntaxTrees[i];
                var root = tree.GetRoot();

                //replace atomic field types
                List<FieldDeclarationSyntax> allFields = root.DescendantNodes().Where(node => node.IsKind(SyntaxKind.FieldDeclaration)).Cast<FieldDeclarationSyntax>().ToList();
                var atomicFields = allFields.Where(node => node.Modifiers.Any(SyntaxKind.AtomicKeyword));
                root = root.ReplaceNodes(atomicFields, (oldnode, newnode) => ReplaceFieldDecl(oldnode));

                tree = SyntaxFactory.SyntaxTree(root, tree.Options, tree.FilePath);
                newTrees[i] = tree;
            }

            return CSharpCompilation.Create(compilation.AssemblyName, newTrees, compilation.References, compilation.Options);
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
            return RemoveModifiers(list, SyntaxKind.AtomicKeyword);
        }

        private static SyntaxTokenList RemoveModifiers(SyntaxTokenList list, params SyntaxKind[] mods)
        {
            var newModifierList = list.Where(mod => !mods.Contains(mod.Kind()));
            return SyntaxFactory.TokenList(newModifierList);
        }

        private static VariableDeclarationSyntax ConstructVariableDeclaration(VariableDeclarationSyntax aVarDcl, SemanticModel semanticModel = null)
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

        private static string optinalTest(string myPar = "hej")
        {
            return null;
        }
    }
}

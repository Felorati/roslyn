using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace STMExtension
{
    internal class STMIdentifyVariables
    {
        public static readonly string ATOMIC_METHOD_FULL = STM.STMNameSpace + ".STMSystem.Atomic";

        internal static CSharpCompilation IdentifyVariables(CSharpCompilation compilation)
        {
            var newTrees = compilation.SyntaxTrees.ToArray();
            var state = new CompilationState(compilation);

            for (int i = 0; i < state.Compilation.SyntaxTrees.Length; i++)
            {
                state.PrepIteration(i);
                state.UpdateState(i);
                var atomics = state.Root.DescendantNodes().OfType<InvocationExpressionSyntax>()
                    .Where(ive => AtomicCondition(ive)).ToList();
                var variables = new HashSet<SyntaxNode>();
                //var atomics = STMAtomicOrelse.GetAtomics(state.Root);
                
                foreach (var atomic in atomics)
                {
                    var idens = atomic.ArgumentList.DescendantNodes().OfType<IdentifierNameSyntax>().ToList();
                    foreach (var iden in idens)
                    {
                        var info = state.SemanticModel.GetSymbolInfo(iden);
                        if (info.Symbol != null && info.Symbol.DeclaringSyntaxReferences.Length > 0)
                        {
                            if (info.Symbol is ILocalSymbol)
                            {
                                var local = info.Symbol.DeclaringSyntaxReferences[0].GetSyntax();
                                if (local.Parent is LocalDeclarationStatementSyntax)
                                {
                                    variables.Add(local.Parent);
                                }
                                else if (local.Parent.Parent is LocalDeclarationStatementSyntax)
                                {
                                    variables.Add(local.Parent.Parent);
                                }
                                //variables.Add((LocalDeclarationStatementSyntax)local.Parent.Parent);
                            }
                            else if (info.Symbol is IPropertySymbol)
                            {
                                var prop = info.Symbol.DeclaringSyntaxReferences[0].GetSyntax();
                                variables.Add(prop);
                            }
                            else if (info.Symbol is IFieldSymbol)
                            {
                                var field = info.Symbol.DeclaringSyntaxReferences[0].GetSyntax();
                                variables.Add((FieldDeclarationSyntax)field.Parent.Parent);
                            }/*
                            else if (info.Symbol is IParameterSymbol)
                            {
                                var parameter = info.Symbol.DeclaringSyntaxReferences[0].GetSyntax();
                                variables.Add((ParameterSyntax)parameter);
                            }*/
                        }
                    }
                }

                state.Root = state.Root.ReplaceNodes(variables, (oldnode, newnode) => ReplaceVariableDecl(oldnode));
                state.UpdateState(i);
            }

            return state.Compilation;
        }

        private static SyntaxNode ReplaceVariableDecl(SyntaxNode decl)
        {
            if (decl is FieldDeclarationSyntax)
            {
                return ReplaceFieldDecl((FieldDeclarationSyntax)decl);
            }
            else if (decl is LocalDeclarationStatementSyntax)
            {
                return ReplaceLocalDecl((LocalDeclarationStatementSyntax)decl);
            }
            else if (decl is ParameterSyntax)
            {
                return ReplaceParamDecl((ParameterSyntax)decl);
            }
            else if (decl is PropertyDeclarationSyntax)
            {
                return ReplacePropDecl((PropertyDeclarationSyntax)decl);
            }
            else
            {
                throw new Exception("Unsupported type");
            }
        }

        private static PropertyDeclarationSyntax ReplacePropDecl(PropertyDeclarationSyntax propDcl)
        {
            if (!STM.HasAtomicModifier(propDcl.Modifiers) && IsManual(propDcl))
            {
                var newMods = propDcl.Modifiers.Add(SyntaxFactory.Token(SyntaxKind.AtomicKeyword));
                propDcl = propDcl.WithModifiers(newMods);
            }

            return propDcl;
        }
        
        private static bool IsManual(PropertyDeclarationSyntax propDecl)
        {
            foreach (var item in propDecl.AccessorList.Accessors)
            {
                if (item.Body != null)
                {
                    return false;
                }
            }

            return true;
        }

        private static FieldDeclarationSyntax ReplaceFieldDecl(FieldDeclarationSyntax fieldDcl)
        {
            if (!STM.HasAtomicModifier(fieldDcl.Modifiers))
            {
                var newMods = fieldDcl.Modifiers.Add(SyntaxFactory.Token(SyntaxKind.AtomicKeyword));
                fieldDcl = fieldDcl.WithModifiers(newMods);
            }

            return fieldDcl;
        }

        private static LocalDeclarationStatementSyntax ReplaceLocalDecl(LocalDeclarationStatementSyntax localDecl)
        {
            var ive = localDecl.AttemptToGetParent<InvocationExpressionSyntax>();
            if (!STM.HasAtomicModifier(localDecl.Modifiers) && (ive == null || !AtomicCondition(ive)))
            {
                var newMods = localDecl.Modifiers.Add(SyntaxFactory.Token(SyntaxKind.AtomicKeyword));
                localDecl = localDecl.WithModifiers(newMods);
            }

            return localDecl;
        }

        private static ParameterSyntax ReplaceParamDecl(ParameterSyntax paramDecl)
        {
            if (!STM.HasAtomicModifier(paramDecl.Modifiers))
            {
                var newMods = paramDecl.Modifiers.Add(SyntaxFactory.Token(SyntaxKind.AtomicKeyword));
                paramDecl = paramDecl.WithModifiers(newMods);
            }

            return paramDecl;
        }

        private static bool IsVariable(ISymbol symbol)
        {
            return symbol != null 
                && (symbol is IParameterSymbol || symbol is ILocalSymbol || symbol is IFieldSymbol);
        }

        private static bool AtomicCondition(InvocationExpressionSyntax ive)
        {
            return ive.Expression.ToFullString() == ATOMIC_METHOD_FULL;
        }
    }
}

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

namespace STMExtension
{
    public class STMConstructor
    {
        internal static CSharpCompilation ReplaceConstructorArguments(CSharpCompilation compilation)
        {
            var state = new CompilationState(compilation);

            for (int i = 0; i < compilation.SyntaxTrees.Length; i++)
            {
                state.PrepIteration(i);
                var methodCalls = state.Root.DescendantNodes().OfType<ObjectCreationExpressionSyntax>().ToList();
                state.Root = state.Root.TrackNodes(methodCalls);
                state.UpdateState(i);

                for (var j = 0; j < methodCalls.Count; j++)
                {
                    var ive = methodCalls[j];
                    ive = state.Root.GetCurrentNode(ive);
                    var replacement = ReplaceConstructorArgument(state.SemanticModel, ive);
                    if (replacement != null)
                    {
                        state.Root = state.Root.ReplaceNode(ive, replacement);
                        state.UpdateState(i);
                    }
                }
            }

            return state.Compilation;
        }


        private static ObjectCreationExpressionSyntax ReplaceConstructorArgument(SemanticModel semanticModel, ObjectCreationExpressionSyntax oce)
        {
            var info = semanticModel.GetSymbolInfo(oce);
            if (info.Symbol != null)
            {
                IMethodSymbol methodInfo = (IMethodSymbol)info.Symbol;
                List<ArgumentSyntax> args;
                bool hasAtomicParam;
                STM.CreateReplacementArgList(oce.ArgumentList, methodInfo, out args, out hasAtomicParam);

                if (hasAtomicParam)
                {
                    oce = oce.WithArgumentList(STM.CreateArgList(args));
                    return oce;
                }
            }

            return null;
        }
    }
}

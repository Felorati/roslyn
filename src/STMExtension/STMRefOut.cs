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
    public class STMRefOut
    {
        internal static CSharpCompilation ReplaceAtomicRefOut(CSharpCompilation compilation, List<Diagnostic> diagnostics, List<List<IdentifierNameSyntax>> skipsLists)
        {
            var newTrees = compilation.SyntaxTrees.ToArray();
            var state = new CompilationState(compilation);

            for (int i = 0; i < state.Compilation.SyntaxTrees.Length; i++)
            {
                var skipListBuffer = new List<string>();
                state.PrepIteration(i);

                var methodCalls = state.Root.DescendantNodes().OfType<InvocationExpressionSyntax>().ToList();
                state.Root = state.Root.TrackNodes(methodCalls);
                state.UpdateState(i);

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
                        var nodeSkipBuffer = new List<int>();
                        for (int j = 0; j < ive.ArgumentList.Arguments.Count; j++)
                        {
                            var arg = ive.ArgumentList.Arguments[j];
                            var parameter = STM.GetParameterForArg(methodInfo, arg, j);

                            if (parameter != null)
                            {
                                if (parameter.IsRefOrOut())
                                {
                                    var symbolInfo = state.SemanticModel.GetSymbolInfo(arg.Expression);
                                    var isAtomicSymbol = STM.IsAtomicSymbol(symbolInfo.Symbol);
                                    if ((parameter.IsAtomic || STM.IsAtomicSymbol(symbolInfo.Symbol)) && !STM.IsVariableSymbol(symbolInfo.Symbol))
                                    {
                                        diagnostics.Add(Diagnostic.Create(STMErrorDescriptors.DD_INVALID_REF_OUT_ARG, arg.GetLocation()));
                                    }

                                    if (parameter.IsAtomic)
                                    {
                                        if (!isAtomicSymbol)
                                        {
                                            //Generate local variable
                                            var typeString = parameter.Type.ToString();
                                            var type = STM.DetermineSTMType(typeString);
                                            string identifierString;
                                            SyntaxToken identifier;
                                            var localDecl = STM.CreateLocalDeclaration(type, STM.CreateObjectCreationExpression(type, STM.CreateArgList(arg.Expression)), out identifierString, out identifier);
                                            localDecls.Add(localDecl);

                                            //Generate assignment to actual parameter from local var;
                                            //var memberAccess = CreatePropertyAccess(SyntaxFactory.IdentifierName(identifier), "Value");
                                            //var assigment = SyntaxFactory.ExpressionStatement(SyntaxFactory.AssignmentExpression(SyntaxKind.SimpleAssignmentExpression, arg.Expression, memberAccess));
                                            var argIdentifier = SyntaxFactory.IdentifierName(identifier);
                                            var assigment = SyntaxFactory.ExpressionStatement(SyntaxFactory.AssignmentExpression(SyntaxKind.SimpleAssignmentExpression, arg.Expression, argIdentifier));
                                            assignments.Add(assigment);

                                            skipListBuffer.Add(identifierString);
                                            //Generate new argument
                                            arg = arg.WithExpression(argIdentifier);

                                            diagnostics.Add(Diagnostic.Create(STMErrorDescriptors.DD_INVALID_REF_ATOMIC_PARAMETER, arg.GetLocation(), parameter.Type.ToString()));
                                        }
                                        else
                                        {
                                            nodeSkipBuffer.Add(j);
                                        }

                                    }
                                    else if (isAtomicSymbol)
                                    {
                                        //Generate local variable
                                        var typeString = parameter.Type.ToString();
                                        var type = SyntaxFactory.ParseTypeName(typeString + " ");
                                        string identifierString;
                                        SyntaxToken identifier;
                                        var localDecl = STM.CreateLocalDeclaration(type, arg.Expression, out identifierString, out identifier);
                                        localDecls.Add(localDecl);

                                        //Generate assignment to actual parameter from local var;
                                        var argIdentifier = SyntaxFactory.IdentifierName(identifier);
                                        var assigment = SyntaxFactory.ExpressionStatement(SyntaxFactory.AssignmentExpression(SyntaxKind.SimpleAssignmentExpression, arg.Expression, argIdentifier));
                                        assignments.Add(assigment);

                                        skipListBuffer.Add(identifierString);
                                        //Generate new argument
                                        arg = arg.WithExpression(argIdentifier);

                                        diagnostics.Add(Diagnostic.Create(STMErrorDescriptors.DD_INVALID_ATOMIC_REF_ARG, arg.GetLocation(), parameter.Type.ToString()));
                                    }

                                    replace = true;
                                }
                            }

                            args.Add(arg);
                        }

                        if (replace)
                        {
                            var originalStatement = ive.GetClosestStatementSyntax();
                            var statement = originalStatement;

                            var trackNodes = new List<SyntaxNode>(nodeSkipBuffer.Count);
                            trackNodes.Add(originalStatement);

                            state.Root = state.Root.TrackNodes(trackNodes);
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
                            state.Root = state.Root.ReplaceNode(toReplace, ive.WithArgumentList(STM.CreateArgList(args)));
                            var newIve = state.Root.GetCurrentNode(item);

                            if (nodeSkipBuffer.Count > 0)
                            {
                                var arguments = new List<ArgumentSyntax>();
                                for (int k = 0; k < newIve.ArgumentList.Arguments.Count; k++)
                                {
                                    if (nodeSkipBuffer.Contains(k))
                                    {
                                        arguments.Add(newIve.ArgumentList.Arguments[k]);
                                    }
                                }

                                var atomicSkip = arguments.Select(arg => arg.Expression is IdentifierNameSyntax ? (IdentifierNameSyntax)arg.Expression : arg.Expression.DescendantNodes().OfType<IdentifierNameSyntax>().Last());
                                state.Root = state.Root.TrackNodes(atomicSkip);
                                skipsLists[i].AddRange(atomicSkip);
                            }

                            state.UpdateState(i);
                        }
                    }

                }

                var idensToSkip = new List<IdentifierNameSyntax>();
                var refOutArgs = state.Root.DescendantNodes().OfType<IdentifierNameSyntax>().Where(iden => skipListBuffer.Contains(iden.Identifier.Text) && iden.Parent.IsKind(SyntaxKind.Argument)).ToList();
                idensToSkip.AddRange(refOutArgs);
                state.Root = state.Root.TrackNodes(idensToSkip);
                skipsLists[i].AddRange(idensToSkip);

                state.UpdateState(i);
            }

            return state.Compilation;
        }
    }
}

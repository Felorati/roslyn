using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis;

namespace STMExtension
{
    public static class ExtensionMethods
    {
        public static string GetTypeString(this TypeSyntax typesyntax)
        {
            if (typesyntax.IsKind(SyntaxKind.IdentifierName)) //userdefined types
            {
                return ((IdentifierNameSyntax)typesyntax).Identifier.Text;
            }
            else if (typesyntax.IsKind(SyntaxKind.PredefinedType))
            {
                return ((PredefinedTypeSyntax)typesyntax).Keyword.Text;
            }
            else if (typesyntax.IsKind(SyntaxKind.QualifiedName))
            {
                return ((QualifiedNameSyntax)typesyntax).Right.ToString();
            }
            else
            {
                throw new Exception(string.Format("Unexcepted type: {0} can not return type string", typesyntax.GetType().FullName));
            }
        }


        public static StatementSyntax GetClosestStatementSyntax(this SyntaxNode node)
        {
            return node.AttemptToGetParent<StatementSyntax>();
        }

        public static bool IsRefOrOut(this IParameterSymbol param)
        {
            return param.RefKind == RefKind.Out || param.RefKind == RefKind.Ref;
        }

        public static T AttemptToGetParent<T>(this SyntaxNode iden) where T : SyntaxNode
        {
            return AttemptToGetParentWorker<T>(iden.Parent);
        }

        private static T AttemptToGetParentWorker<T>(SyntaxNode node) where T : SyntaxNode
        {
            if (node == null)
            {
                return null;
            }
            else if (node is T)
            {
                return (T)node;
            }
            else
            {
                return AttemptToGetParentWorker<T>(node.Parent); ;
            }
        }

        public static T AttemptToGetParent<T,V>(this SyntaxNode iden) where T : SyntaxNode where V : SyntaxNode
        {
            return AttemptToGetParentWorker<T,V>(iden.Parent);
        }

        private static T AttemptToGetParentWorker<T,V>(SyntaxNode node) where T : SyntaxNode where V : SyntaxNode
        {
            if (node == null)
            {
                return null;
            }
            else if (node is T || node is V)
            {
                return (T)node;
            }
            else
            {
                return AttemptToGetParentWorker<T>(node.Parent); ;
            }
        }

        public static T AttemptToGetParentStop<T,V>(this SyntaxNode iden) where T : SyntaxNode where V : SyntaxNode
        {
            return AttemptToGetParentStopWorker<T,V>(iden.Parent);
        }

        private static T AttemptToGetParentStopWorker<T,V>(SyntaxNode node) where T : SyntaxNode where V : SyntaxNode
        {
            if (node == null)
            {
                return null;
            }
            else if (node is T)
            {
                return (T)node;
            }
            else if (node is V)
            {
                return null;
            }
            else
            {
                return AttemptToGetParentStopWorker<T,V>(node.Parent); ;
            }
        }

        public static SyntaxNode AttemptToGetParentNoOfType<T>(this SyntaxNode node) where T : SyntaxNode
        {
            return AttemptToGetParentNoOfTypeWorker<T>(node.Parent);
        }

        private static SyntaxNode AttemptToGetParentNoOfTypeWorker<T>(SyntaxNode node) where T : SyntaxNode
        {
            if (node == null)
            {
                return node;
            }
            else if (node is T)
            {
                return AttemptToGetParentNoOfTypeWorker<T>(node.Parent);
            }
            else
            {
                return node;
            }
        }
    }
}

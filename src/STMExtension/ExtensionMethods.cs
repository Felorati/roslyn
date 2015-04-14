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
    }
}

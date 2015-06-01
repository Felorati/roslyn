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
    public class STMProperties
    {
        internal static CSharpCompilation ReplaceProperties(CSharpCompilation compilation)
        {
            var newTrees = new SyntaxTree[compilation.SyntaxTrees.Length];

            for (int i = 0; i < compilation.SyntaxTrees.Length; i++)
            {
                var tree = compilation.SyntaxTrees[i];
                var root = tree.GetRoot();
                //Generates a manual property
                var allProperties = root.DescendantNodes().Where(node => node.IsKind(SyntaxKind.PropertyDeclaration)).Cast<PropertyDeclarationSyntax>().ToList();
                var atomicProperties = allProperties.Where(node => node.Modifiers.Any(SyntaxKind.AtomicKeyword)).ToList();
                root = root.TrackNodes(atomicProperties);

                foreach (var item in atomicProperties)
                {
                    var atomicProperty = root.GetCurrentNode(item);
                    var modifiersWithoutAtomic = STM.RemoveAtomicMod(atomicProperty.Modifiers);
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

                tree = SyntaxFactory.SyntaxTree(root, tree.Options, tree.FilePath);
                newTrees[i] = tree;
            }

            return CSharpCompilation.Create(compilation.AssemblyName, newTrees, compilation.References, compilation.Options);
        }

        private static string GenerateFieldName(SyntaxToken identifier)
        {
            var nameAsString = new StringBuilder(identifier.ToString());
            nameAsString.Insert(0, "_", 1);
            nameAsString.Remove(1, 1);
            nameAsString.Insert(1, char.ToLower(identifier.ToString()[0]));
            return nameAsString.ToString();
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

    }
}

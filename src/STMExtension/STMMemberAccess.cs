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
    public class STMMemberAccess
    {
        internal static CSharpCompilation ReplaceMemberAccesses(CSharpCompilation compilation)
        {
            var newTrees = compilation.SyntaxTrees.ToArray();
            for (int i = 0; i < compilation.SyntaxTrees.Length; i++)
            {
                var replacedNodes = new HashSet<int>();
                var replaced = true;
                var visited = new List<MemberAccessExpressionSyntax>();
                while (replaced)
                {
                    var tree = compilation.SyntaxTrees[i];
                    var root = tree.GetRoot();
                    var semanticModel = compilation.GetSemanticModel(tree);

                    var memberAccesses = GetMemberAccesses(root, semanticModel, root.GetCurrentNodes<MemberAccessExpressionSyntax>(visited));
                    if (memberAccesses.Count > 0)
                    {
                        visited.AddRange(memberAccesses);
                        root = root.TrackNodes(memberAccesses);
                        memberAccesses = root.GetCurrentNodes<MemberAccessExpressionSyntax>(memberAccesses).ToList();
                        root = root.ReplaceNodes(memberAccesses, (oldNode, newNode) => ReplaceMemberAccess(oldNode));
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

        private static List<MemberAccessExpressionSyntax> GetMemberAccesses(SyntaxNode root, SemanticModel semanticModel, IEnumerable<MemberAccessExpressionSyntax> visited)
        {
            return root.DescendantNodes().OfType<MemberAccessExpressionSyntax>().Where(ma => !visited.Contains(ma) && STM.IsAtomicType(semanticModel.GetTypeInfo(ma))).ToList();
        }

        private static bool ReplaceMemberAccessCondition(MemberAccessExpressionSyntax ma)
        {
            return !(ma.Parent is MemberAccessExpressionSyntax && ((MemberAccessExpressionSyntax)ma.Parent).Name.Identifier.ValueText == "Value");
        }

        private static MemberAccessExpressionSyntax ReplaceMemberAccess(MemberAccessExpressionSyntax ma)
        {
            var replacement = STM.CreatePropertyAccess(ma, "Value");
            return replacement;
        }

    }
}

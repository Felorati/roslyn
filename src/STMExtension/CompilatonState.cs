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
    public class CompilationState
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
}

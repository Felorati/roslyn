using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using Microsoft.CodeAnalysis;

namespace STMExtension
{
    internal class STMErrorDescriptors
    {
        internal static readonly DiagnosticDescriptor DD_INVALID_RETRY_PLACEMENT = new DiagnosticDescriptor("RetryPlacement", "Invalid retry statement placement", "The retry statement can only be used inside atomic or orelse statements", "Typing", DiagnosticSeverity.Error, true);
        internal static readonly DiagnosticDescriptor DD_INVALID_REF_OUT_ARG = new DiagnosticDescriptor("Invalid ref/out argument", "Invalid ref/out argument", " ref/out arguments must be either a field, local variable or parameter", "Typing", DiagnosticSeverity.Error, true);
        internal static readonly DiagnosticDescriptor DD_INVALID_ATOMIC_REF_ARG = new DiagnosticDescriptor("Changed semantics", "Changed semantics", " When a atomic argument of type {0} is passed into a paramter of type {0} by ref/out assignments to the paramter first take effect when the method terminates", "Typing", DiagnosticSeverity.Warning, true); 
        internal static readonly DiagnosticDescriptor DD_INVALID_REF_ATOMIC_PARAMETER = new DiagnosticDescriptor("Changed semantics", "Changed semantics", " When a argument of type {0} is passed into a atomic paramter of type {0} by ref/out assignments to the paramter first take effect when the method terminates", "Typing", DiagnosticSeverity.Warning, true); 
    }
}

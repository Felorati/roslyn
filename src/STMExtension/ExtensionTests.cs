using NUnit.Framework;
using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
using System.Linq;
using System.Reflection;
using System.Text;
using System.Threading.Tasks;

namespace STMExtension
{
    [TestFixture]
    public class ExtensionTests
    {
        private void runCmd(string argStr)
        {
            // Start the child process.
            Process p = new Process();
            // Redirect the output stream of the child process.
            p.StartInfo.UseShellExecute = false;
            p.StartInfo.RedirectStandardOutput = true;
            p.StartInfo.FileName = "CMD.exe";
            p.StartInfo.Arguments = "/C " + argStr; ///C Carries out the command specified by string and then terminates
            p.StartInfo.CreateNoWindow = true;
            p.Start();
            string output = p.StandardOutput.ReadToEnd();
            int exitCode = p.ExitCode;
            p.WaitForExit();
        }

        private void runCsc(string argStr)
        {
            string wanted_path = Path.GetDirectoryName(Path.GetDirectoryName(System.IO.Directory.GetCurrentDirectory()));
            string rootPath = System.AppDomain.CurrentDomain.BaseDirectory;
            //string test = System.IO.Path.
            string path = System.IO.Path.GetDirectoryName(System.Reflection.Assembly.GetExecutingAssembly().GetName().CodeBase); //source http://msdn.microsoft.com/en-us/library/aa457089.aspx
            string executableLocation = Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location);

            string cscPath = Path.Combine(System.Environment.CurrentDirectory, "csc.exe");
            runCmd(cscPath + " " + argStr);
        }

        [Test]
        public void cmdTest()
        {
            runCsc("TestRef.cs");
            runCmd("echo hej");
            int i = 30;
            int a = 50;
            Assert.AreEqual(i, a);
        }


    }
}

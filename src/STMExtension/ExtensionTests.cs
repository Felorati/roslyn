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
    [SetUpFixture]
    public class ExtensionTestsCleanup
    {
        [SetUp]
        public void RunBeforeAnyTest()
        {
            if (!Directory.Exists(ExtensionTests.testFilesFolderName))
            {
                string exePath = Path.GetDirectoryName(System.Reflection.Assembly.GetExecutingAssembly().GetName().CodeBase); //source http://msdn.microsoft.com/en-us/library/aa457089.aspx
                ExtensionTests.exePath = exePath;
                Directory.CreateDirectory(ExtensionTests.testFilesFolderName);
            }
        }
    }

    [TestFixture]
    public class ExtensionTests
    {
        public static int csFilesCount;
        public static string exePath;
        public static string currentCsFile;
        readonly public static string testFilesFolderName = "CSTestFiles";

        [SetUp]
        public void SetupTestFile()
        {
            csFilesCount++;
            currentCsFile = testFilesFolderName + Path.DirectorySeparatorChar + "TestFile" + csFilesCount + ".cs";
        }

        [TestFixture]
        public class CmdRes //Result from a commandline execution
        {
            public string output;//Warnings and errors
            public string error; //Exceptions
            public int exitCode; //0 represents all went well
            public CmdRes(string output, string error, int exitCode)
            {
                this.output = output;
                this.error = error;
                this.exitCode = exitCode;
            }
        }

        private CmdRes RunCmd(string argStr)
        {
            // Start the child process.
            Process p = new Process();
            // Redirect the output stream of the child process.
            p.StartInfo.UseShellExecute = false;
            p.StartInfo.RedirectStandardOutput = true;
            p.StartInfo.RedirectStandardError = true;
            p.StartInfo.FileName = "CMD.exe";
            p.StartInfo.Arguments = "/C " + argStr; ///'C' Carries out the command specified by string and then terminates
            p.StartInfo.CreateNoWindow = true;
            p.Start();
            string output = p.StandardOutput.ReadToEnd();
            string error =  p.StandardError.ReadToEnd();
            int exitCode = p.ExitCode;
            p.WaitForExit();
            return new CmdRes(output, error, exitCode);
        }

        private CmdRes RunCsc(string argStr)
        {
            //Other dir obtainings
            //string rootPath = System.AppDomain.CurrentDomain.BaseDirectory;
            //string path = System.IO.Path.GetDirectoryName(System.Reflection.Assembly.GetExecutingAssembly().GetName().CodeBase); //source http://msdn.microsoft.com/en-us/library/aa457089.aspx
            //string executableLocation = Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location);

            //Cannot use one of the three methods above because the reference ExtensionsTests bin folder, and not the out Binaries/Debug folder where csc.exe and TestRef.cs is in.
            string debugPath = Path.GetDirectoryName(Path.GetDirectoryName(Directory.GetCurrentDirectory()));
            DirectoryInfo dirI = new DirectoryInfo(debugPath);
            dirI = dirI.Parent.Parent;
            string binariesDebugPath = Path.Combine(dirI.FullName, "Binaries" + Path.DirectorySeparatorChar + "Debug");

            string cscPath = Path.Combine(binariesDebugPath, "csc.exe");
            return RunCmd(cscPath + " " + argStr);
        }

        private void WrtStrToTestFile(string str)
        {
            File.WriteAllText(currentCsFile, str);
        }

        private string InsertStrInSkeletonWithMain(string str)
        {
            StringBuilder strBuilder = new StringBuilder();
            strBuilder.AppendLine("static void Main() \n\t\t{\n\t\t} \n");          
            strBuilder.AppendLine("\t\t" + str);

            return InsertStrInSkeletonWithoutMain(strBuilder.ToString());
        }

        private string InsertStrInSkeletonWithoutMain(string str)
        {
            StringBuilder strBuilder = new StringBuilder();
            strBuilder.Append("using System;\n\n");
            strBuilder.Append("namespace TestFileNamespace\n{");
            strBuilder.Append("\n\tpublic class TestFile \n\t{ \n\t\t");
            strBuilder.Append(str);
            strBuilder.Append("\n\t} \n}");

            return strBuilder.ToString();
        }

        [Test]
        public void VariableDcl()
        {
            string finalStr = InsertStrInSkeletonWithMain(
                "public int var1; \n\t\t" + 
                "public atomic int var2;");
            WrtStrToTestFile(finalStr);
            CmdRes res = RunCsc(currentCsFile); //TODO: lav en metode der tjekker for at der ikke er warinings i output string og der ikke er andre exitkoder en 0

            string expecStr = "";
            string compiledStr = ""; //TODO: Obtain maybe from a file that is printed under compilation or through the CMD via CSC.exe (dunno if possible)

            Assert.AreEqual(expecStr, compiledStr);
        }

        [Test]
        public void MedthodDclWithWarning()
        {
            string finalStr = InsertStrInSkeletonWithMain(
                "public int myMethod(){ \n\t\t\t" +
                "return 5;\n\t\t\t" +
                "return 3;\n}");
            WrtStrToTestFile(finalStr);
            CmdRes res = RunCsc(currentCsFile); //TODO: lav en metode der tjekker for at der ikke er warinings i output string og der ikke er andre exitkoder en 0

            Assert.IsTrue(res.output.Contains("warning CS0162: Unreachable code detected"));
        }

        [Test]
        public void CmdTest()
        {
            RunCmd("echo hej");
        }


    }
}

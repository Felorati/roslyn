﻿using NUnit.Framework;
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
            else
            {
                Array.ForEach(Directory.GetFiles(ExtensionTests.testFilesFolderName), File.Delete);
            }
        }
    }

    [TestFixture]
    public class ExtensionTests
    {
        public static int csFilesCount;
        public static string exePath;
        public static string currentCsFile; //without extension
        public static string currentCompiledCsFile; //with extension
        readonly public static string testFilesFolderName = "CSTestFiles";

        [SetUp]
        public void SetupTestFile()
        {
            csFilesCount++;
            currentCsFile = testFilesFolderName + Path.DirectorySeparatorChar + "TestFile" + csFilesCount; //Without extension ie. ".cs"
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
            //Have to navigate to Binaries/Debug folder where csc.exe and TestRef.cs is in. (different from currentdir which is STMExtension\Debug\Bin)
            string currentDir = AppDomain.CurrentDomain.BaseDirectory;
            var dirI = new DirectoryInfo(currentDir);
            dirI = dirI.Parent.Parent.Parent.Parent;
     
            string binariesDebugPath = Path.Combine(dirI.FullName, "Binaries" + Path.DirectorySeparatorChar + "Debug");
            string cscPath = Path.Combine(binariesDebugPath, "csc.exe");
            return RunCmd(cscPath + " " + argStr);
        }

        private CmdRes RunCscWithOutAndStmIOut()
        {
            currentCompiledCsFile = currentCsFile + "Compiled.cs";
            StringBuilder strB = new StringBuilder();
            strB.Append("/out:\"" + currentCsFile + ".exe\"");
            strB.Append(" ");
            strB.Append("/stmiout:\"" + currentCompiledCsFile + "\"");
            strB.Append(" ");
            strB.Append(currentCsFile + ".cs");
            return RunCsc(strB.ToString());
        }

        #region Read and write to files
        private void StringToTestFile(string str)
        {
            File.WriteAllText(currentCsFile + ".cs", str);
        }

        private String TestFileToString(string filepath)
        {
            StringBuilder sb = new StringBuilder();
            using (StreamReader sr = new StreamReader(filepath))
            {
                String line;
                // Read and display lines from the file until the end of 
                // the file is reached.
                while ((line = sr.ReadLine()) != null)
                {
                    sb.AppendLine(line);
                }
            }
            string allines = sb.ToString();

            return allines;
        }
        #endregion

        #region Skeletons for creating examples

        private string MakeSkeletonWithMain(string strInClass, string strInMain = "")
        {
            StringBuilder strBuilder = new StringBuilder();
            strBuilder.AppendLine("static void Main() \n\t\t{\n\t\t");
            strBuilder.Append(strInMain + "\n\t\t}\n");
            strBuilder.AppendLine("\t\t" + strInClass);

            return MakeSkeletonWithoutMain(strBuilder.ToString());
        }

        private string MakeSkeletonWithoutMain(string strInClass)
        {
            StringBuilder strBuilder = new StringBuilder();
            strBuilder.Append("using System;\n\n");
            strBuilder.Append("namespace TestFileNamespace\n{");
            strBuilder.Append("\n\tpublic class TestFile \n\t{ \n\t\t");
            strBuilder.Append(strInClass);
            strBuilder.Append("\n\t} \n}");

            return strBuilder.ToString();
        }
        #endregion

        /// <summary>
        /// Removes the unnecessary parts in the strings given, ie. whitespace, tabs and newlines and carrige return.
        /// </summary>
        private string RemoveUnnecessaryParts(string str)
        {
            StringBuilder strB = new StringBuilder(str);
            strB.Replace(" ", "");
            strB.Replace("\n", "");
            strB.Replace("\t", "");
            strB.Replace("\r", "");
            return strB.ToString();
        }

        private void AssertEqualStrings(string str1, string str2)
        {
            var cleanStr1 = RemoveUnnecessaryParts(str1);
            var cleanStr2 = RemoveUnnecessaryParts(str2);
            Assert.AreEqual(cleanStr1, cleanStr2);
        }

        private void AssertCmdRes(CmdRes cRes)
        {
            bool res = true;

            if(cRes.exitCode != 0)
                res = false;
            if(cRes.error.Length != 0)
                res = false;
            if(cRes.output.Contains("error"))
                res = false;

            Assert.IsTrue(res);
        }

        [Test]
        public void VariableDcl()
        {
            string finalStr = MakeSkeletonWithMain(
                "public int var1; \n\t\t" + 
                "public atomic int var2;");
            StringToTestFile(finalStr);
            CmdRes res = RunCscWithOutAndStmIOut();
            AssertCmdRes(res);

            string expecStr = MakeSkeletonWithMain(
                "public int var1; \n\t\t" +
                "public STM.Implementation.Lockbased.TMInt var2 = new STM.Implementation.Lockbased.TMInt();");
            string compiledStr = TestFileToString(currentCompiledCsFile);

            AssertEqualStrings(expecStr, compiledStr);
        }

        [Test]
        public void MedthodDclWithWarning()
        {
            string finalStr = MakeSkeletonWithMain(
                "public int myMethod(){ \n\t\t\t" +
                "return 5;\n\t\t\t" +
                "return 3;\n}");
            StringToTestFile(finalStr);
            CmdRes res = RunCscWithOutAndStmIOut();
            AssertCmdRes(res);

            Assert.IsTrue(res.output.Contains("warning CS0162: Unreachable code detected"));
        }

    }
}

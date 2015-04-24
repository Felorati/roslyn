using NUnit.Framework;
using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
using System.Linq;
using System.Reflection;
using System.Text;
using System.Threading.Tasks;
using System.Text.RegularExpressions;

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
            strBuilder.AppendLine("static void Main() \n\t\t{");
            strBuilder.AppendLine("\t\t\t" + strInMain + "\n\t\t}\n");
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

            if (cRes.exitCode != 0 || cRes.error.Length != 0 || cRes.output.Contains("error"))
                res = false;

            Assert.IsTrue(res, "CmdRes is invalid.\nExitcode: " + cRes.exitCode + "\nError: " + cRes.error + "\nOutput: " + cRes.output );
        }

        private HashSet<string> GetGUIDsFromFile(string fileStr)
        {
            var regRes = Regex.Matches(fileStr, "_[A-Za-z0-9]+");
            var guidIds = new HashSet<string>();
            foreach (var r in regRes)
            {
                if(r.ToString().Length == 33)
                    guidIds.Add(r.ToString());
            }
            return guidIds;
        }

        [Test]
        public void FieldDeclaration()
        {
            string finalStr = MakeSkeletonWithMain(
                "public int var1; \n\t\t" + 
                "public atomic int var2; \n\t\t" +
                "public atomic string var3;");
            StringToTestFile(finalStr);
            CmdRes res = RunCscWithOutAndStmIOut();
            AssertCmdRes(res);

            string expecStr = MakeSkeletonWithMain(
                "public int var1; \n\t\t" +
                "public " + STM.STMNameSpace + ".TMInt var2 = new " + STM.STMNameSpace + ".TMInt();" +
                "public " + STM.STMNameSpace + ".TMVar<string> var3 = new " + STM.STMNameSpace + ".TMVar<string>();");
            string compiledStr = TestFileToString(currentCompiledCsFile);

            AssertEqualStrings(expecStr, compiledStr);
        }

        [Test]
        public void FieldDeclarationAndUsage()
        {
            string finalStr = MakeSkeletonWithMain(
                strInClass:
                "public static atomic int var1; \n\t\t" +
                "public static atomic string var2;",
                strInMain:
                "var1 = 2; \n\t\t\t" +
                "var2 = \"val\"; \n\t\t\t" +
                "int newVar1 = var1; \n\t\t\t" +
                "string newVar2 = var2;");
            StringToTestFile(finalStr);
            CmdRes res = RunCscWithOutAndStmIOut();
            AssertCmdRes(res);

            string expecStr = MakeSkeletonWithMain(
                strInClass:
                "public static " + STM.STMNameSpace + ".TMInt var1 = new " + STM.STMNameSpace + ".TMInt();" +
                "public static " + STM.STMNameSpace + ".TMVar<string> var2 = new " + STM.STMNameSpace + ".TMVar<string>();",
                strInMain:
                "var1.Value = 2;" +
                "var2.Value = \"val\";" +
                "int newVar1 = var1.Value;" +
                "string newVar2 = var2.Value;");
            string compiledStr = TestFileToString(currentCompiledCsFile);

            AssertEqualStrings(expecStr, compiledStr);
        }

        [Test]
        public void PropertyDeclarationAndUsage()
        {
            string finalStr = MakeSkeletonWithMain(
                strInClass:
                "public atomic int TestProp { get; set; } \n\t\t" +
                "private atomic string TestProp2 { get; set; }",
                strInMain:
                "var objRef = new TestFile(); \n\t\t\t" +
                "int readVar = objRef.TestProp;\n\t\t\t" +
                "objRef.TestProp = 5;"
                );
            StringToTestFile(finalStr);
            CmdRes res = RunCscWithOutAndStmIOut();
            AssertCmdRes(res);

            string expecStr = MakeSkeletonWithMain(
                strInClass:
                "private " + STM.STMNameSpace + ".TMInt _testProp = new " + STM.STMNameSpace + ".TMInt();" +
                @"public int TestProp
                {
                    get
                    {
                        return _testProp.Value;
                    }

                    set
                    {
                    _testProp.Value = value;
                    }
                }" +
                "private " + STM.STMNameSpace + ".TMVar<string> _testProp2 = new " + STM.STMNameSpace + ".TMVar<string>();" +
                @"private string TestProp2
                {
                    get
                    {
                        return _testProp2.Value;
                    }

                    set
                    {
                        _testProp2.Value = value;
                    }
                }",
                strInMain:
                "var objRef = new TestFile();" +
                "int readVar = objRef.TestProp;" +
                "objRef.TestProp = 5;");
            string compiledStr = TestFileToString(currentCompiledCsFile);
            AssertEqualStrings(expecStr, compiledStr);
        }

        [Test]
        public void LocalVarDeclaration()
        {
            string finalStr = MakeSkeletonWithMain(
                "public void TestMethod()\n\t\t"+ 
                "{\n\t\t\t" +
                    "atomic int var1; \n\t\t\t" +
                    "atomic string var2; \n\t\t" +
                "}"
                );
            StringToTestFile(finalStr);
            CmdRes res = RunCscWithOutAndStmIOut();
            AssertCmdRes(res);

            string expecStr = MakeSkeletonWithMain(
                "public void TestMethod()" +
                "{" +
                    STM.STMNameSpace + ".TMInt var1 = new " + STM.STMNameSpace + ".TMInt();" +
                    STM.STMNameSpace + ".TMVar<string> var2 = new " + STM.STMNameSpace + ".TMVar<string>();" +
                "}");
            string compiledStr = TestFileToString(currentCompiledCsFile);

            AssertEqualStrings(expecStr, compiledStr);
        }

        [Test]
        public void LocalVarDeclarationInstantiation()
        {
            string finalStr = MakeSkeletonWithMain(
                "public void TestMethod()\n\t\t" +
                "{\n\t\t\t" +
                    "atomic int var1 = 1337; \n\t\t\t" +
                    "atomic string var2 = \"myString\"; \n\t\t" +
                "}"
                );
            StringToTestFile(finalStr);
            CmdRes res = RunCscWithOutAndStmIOut();
            AssertCmdRes(res);

            string expecStr = MakeSkeletonWithMain(
                "public void TestMethod()" +
                "{" +
                    STM.STMNameSpace + ".TMInt var1 = new " + STM.STMNameSpace + ".TMInt(1337);" +
                    STM.STMNameSpace + ".TMVar<string> var2 = new " + STM.STMNameSpace + ".TMVar<string>(\"myString\");" +
                "}");
            string compiledStr = TestFileToString(currentCompiledCsFile);

            AssertEqualStrings(expecStr, compiledStr);
        }

        [Test]
        public void LocalVarDeclarationAndUsage()
        {
            string finalStr = MakeSkeletonWithMain("",
                "atomic int var1; \n\t\t\t" +
                "atomic string var2; \n\t\t\t" +
                "var1 = 2; \n\t\t\t" +
                "var2 = \"val\"; \n\t\t\t" +
                "int newVar1 = var1; \n\t\t\t" +
                "string newVar2 = var2;"
                );
            StringToTestFile(finalStr);
            CmdRes res = RunCscWithOutAndStmIOut();
            AssertCmdRes(res);

            string expecStr = MakeSkeletonWithMain("",
                    STM.STMNameSpace + ".TMInt var1 = new " + STM.STMNameSpace + ".TMInt();" +
                    STM.STMNameSpace + ".TMVar<string> var2 = new " + STM.STMNameSpace + ".TMVar<string>();" +
                    "var1.Value = 2;" +
                    "var2.Value = \"val\";" +
                    "int newVar1 = var1.Value;" +
                    "string newVar2 = var2.Value;"
                    );
            string compiledStr = TestFileToString(currentCompiledCsFile);

            AssertEqualStrings(expecStr, compiledStr);
        }


        [Test]
        public void AtomicBlockEmpty()
        {
            string finalStr = MakeSkeletonWithMain("",
                "atomic{ \n\t\t\t"+
                "}");
            StringToTestFile(finalStr);
            CmdRes res = RunCscWithOutAndStmIOut();
            AssertCmdRes(res);

            string expecStr = MakeSkeletonWithMain("",
                STM.STMNameSpace + @".STMSystem.Atomic(
                    () =>
                    {
                    });");
            string compiledStr = TestFileToString(currentCompiledCsFile);

            AssertEqualStrings(expecStr, compiledStr);
        }

        [Test]
        public void AtomicBlockReturn()
        {
            string finalStr = MakeSkeletonWithMain(
                "public int TestMethod()\n\t\t" +
                "{\n\t\t\t" +
                    "atomic{\n\t\t\t\t" +
                    "return 10;\n\t\t\t" +
                    "} \n\t\t" +
                "}");
            StringToTestFile(finalStr);
            CmdRes res = RunCscWithOutAndStmIOut();
            AssertCmdRes(res);

            string expecStr = MakeSkeletonWithMain(
                "public int TestMethod()" +
                "{" +
                    "return " + STM.STMNameSpace + @".STMSystem.Atomic(
                        () =>
                        {
                            return 10;
                        });
                }");
            string compiledStr = TestFileToString(currentCompiledCsFile);

            AssertEqualStrings(expecStr, compiledStr);
        }

        [Test]
        public void AtomicBlockRetry()
        {
            string finalStr = MakeSkeletonWithMain("",
                "atomic{ \n\t\t\t\t" +
                "retry;\n\t\t\t" +
                "}");
            StringToTestFile(finalStr);
            CmdRes res = RunCscWithOutAndStmIOut();
            AssertCmdRes(res);

            string expecStr = MakeSkeletonWithMain("",
                STM.STMNameSpace + ".STMSystem.Atomic(" +
                    "() =>" +
                    "{" +
                        STM.STMNameSpace + ".STMSystem.Retry();" +
                    "});");
            string compiledStr = TestFileToString(currentCompiledCsFile);

            AssertEqualStrings(expecStr, compiledStr);
        }

        [Test]
        public void AtomicOrelseBlock()
        {
            string finalStr = MakeSkeletonWithMain("",
                "atomic{ \n\t\t\t" +
                "}\n\t\t\t" +
                "orelse{ \n\t\t\t" +
                "}");
            StringToTestFile(finalStr);
            CmdRes res = RunCscWithOutAndStmIOut();
            AssertCmdRes(res);

            string expecStr = MakeSkeletonWithMain("",
                STM.STMNameSpace + @".STMSystem.Atomic(
                    () =>
                    {
                    },
                    () =>
                    {
                    });");
            string compiledStr = TestFileToString(currentCompiledCsFile);

            AssertEqualStrings(expecStr, compiledStr);
        }

        [Test]
        public void AtomicMultipleOrelseBlock()
        {
            string finalStr = MakeSkeletonWithMain("",
                "atomic{ \n\t\t\t" +
                "}\n\t\t\t" +
                "orelse{ \n\t\t\t" +
                "}\n\t\t\t" +
                "orelse{ \n\t\t\t" +
                "}\n\t\t\t" +
                "orelse{ \n\t\t\t" +
                "}");
            StringToTestFile(finalStr);
            CmdRes res = RunCscWithOutAndStmIOut();
            AssertCmdRes(res);

            string expecStr = MakeSkeletonWithMain("",
                STM.STMNameSpace + @".STMSystem.Atomic(
                    () =>
                    {
                    },
                    () =>
                    {
                    },
                    () =>
                    {
                    },
                    () =>
                    {
                    });");
            string compiledStr = TestFileToString(currentCompiledCsFile);

            AssertEqualStrings(expecStr, compiledStr);
        }

        [Test]
        public void NestedAtomicBlocks()
        {
            string finalStr = MakeSkeletonWithMain("",
                "atomic{ \n\t\t\t\t"+
                    "atomic{ \n\t\t\t\t\t" +
                        "atomic{ \n\t\t\t\t\t" +
                        "}\n\t\t\t\t" +
                    "}\n\t\t\t" +
                "}");
            StringToTestFile(finalStr);
            CmdRes res = RunCscWithOutAndStmIOut();
            AssertCmdRes(res);

            string expecStr = MakeSkeletonWithMain("",
                STM.STMNameSpace + ".STMSystem.Atomic(() =>" +
                "{" +
                    STM.STMNameSpace + ".STMSystem.Atomic(() =>" +
                    "{" +
                        STM.STMNameSpace + ".STMSystem.Atomic(() =>" +
                        "{" +
                        "});" +
                    "});" +
                "});");
            string compiledStr = TestFileToString(currentCompiledCsFile);

            AssertEqualStrings(expecStr, compiledStr);
        }

        [Test]
        public void NestedAtomicBlocksReturn()
        {
            string finalStr = MakeSkeletonWithMain(
                "public int TestMethod() \n\t\t" +
                "{ \n\t\t\t" +
                    "atomic{ \n\t\t\t\t" +
                        "atomic{ \n\t\t\t\t\t" +
                            "atomic{ \n\t\t\t\t\t\t" +
                                "return 10; \n\t\t\t\t\t" +
                            "}\n\t\t\t\t" +
                        "}\n\t\t\t" +
                    "}\n\t\t" +
                "}");
            StringToTestFile(finalStr);
            CmdRes res = RunCscWithOutAndStmIOut();
            AssertCmdRes(res);

            string expecStr = MakeSkeletonWithMain(
                "public int TestMethod()" +
                "{" +
                    "return" + STM.STMNameSpace + ".STMSystem.Atomic(() =>" +
                    "{" +
                        "return" + STM.STMNameSpace + ".STMSystem.Atomic(() =>" +
                        "{" +
                            "return" + STM.STMNameSpace + ".STMSystem.Atomic(() =>" +
                            "{" +
                                "return 10;" +
                            "});" +
                        "});" +
                    "});" +
                "}");
            string compiledStr = TestFileToString(currentCompiledCsFile);

            AssertEqualStrings(expecStr, compiledStr);
        }

        [Test]
        public void NestedAtomicOrelseBlocks()
        {
            string finalStr = MakeSkeletonWithMain("",
                "atomic{ \n\t\t\t\t" +
                    "atomic{ \n\t\t\t\t\t" +
                        "atomic{ \n\t\t\t\t\t" +
                        "}\n\t\t\t\t\t" +
                        "orelse{ \n\t\t\t\t\t" +
                        "}\n\t\t\t\t\t" +
                        "orelse{ \n\t\t\t\t\t" +
                        "}\n\t\t\t\t" +
                    "}\n\t\t\t\t" +
                    "orelse{ \n\t\t\t\t" +
                    "}\n\t\t\t" +
                "} \n\t\t\t" +
                "orelse{ \n\t\t\t" +
                "}");
            StringToTestFile(finalStr);
            CmdRes res = RunCscWithOutAndStmIOut();
            AssertCmdRes(res);

            string expecStr = MakeSkeletonWithMain("",
                STM.STMNameSpace + ".STMSystem.Atomic(() =>" +
                "{" +
                    STM.STMNameSpace + ".STMSystem.Atomic(() =>" +
                    "{" +
                        STM.STMNameSpace + ".STMSystem.Atomic(() =>" +
                        "{" +
                        "}, () =>" +
                        "{" +
                        "}, () =>" +
                        "{" +
                        "});" +
                    "}, () =>" +
                    "{" +
                    "});" +
                "}, () =>" +
                "{" +
                "});");
            string compiledStr = TestFileToString(currentCompiledCsFile);

            AssertEqualStrings(expecStr, compiledStr);
        }

        [Test]
        public void AtomicParameter()
        {
            string finalStr = MakeSkeletonWithMain(
                "public void TestMethod(int param1, atomic int param2, atomic string param3) \n\t\t" +
                "{ \n\t\t" +
                "}");
            StringToTestFile(finalStr);
            CmdRes res = RunCscWithOutAndStmIOut();
            AssertCmdRes(res);

            string expecStr = MakeSkeletonWithMain(
                "public void TestMethod(int param1," + STM.STMNameSpace + ".TMInt param2," + STM.STMNameSpace + ".TMVar<string> param3)" +
                "{" +
                "}");
            string compiledStr = TestFileToString(currentCompiledCsFile);

            AssertEqualStrings(expecStr, compiledStr);
        }

        [Test]
        public void AtomicParameterOut()
        {
            string finalStr = MakeSkeletonWithMain(
                "private void TestMethod(atomic out int param1) \n\t\t" +
                "{ \n\t\t\t" +
                    "param1 = 5; \n\t\t" +
                "}");
            StringToTestFile(finalStr);
            CmdRes res = RunCscWithOutAndStmIOut();
            AssertCmdRes(res);

            string expecStr = MakeSkeletonWithMain(
                "private void TestMethod(out " + STM.STMNameSpace + ".TMInt param1)" +
                "{" +
                    "param1 = new " + STM.STMNameSpace + ".TMInt();" +
                    "param1.Value = 5;" +
                "}");
            string compiledStr = TestFileToString(currentCompiledCsFile);

            AssertEqualStrings(expecStr, compiledStr);
        }

        [Test]
        public void AtomicParameterRef()
        {
            string finalStr = MakeSkeletonWithMain(
                "private void TestMethod(atomic ref int param1) \n\t\t" +
                "{ \n\t\t\t" +
                    "param1 = 5; \n\t\t" +
                "}");
            StringToTestFile(finalStr);
            CmdRes res = RunCscWithOutAndStmIOut();
            AssertCmdRes(res);

            string expecStr = MakeSkeletonWithMain(
                "private void TestMethod(ref " + STM.STMNameSpace + ".TMInt param1)" +
                "{" +
                    "param1.Value = 5;" +
                "}");
            string compiledStr = TestFileToString(currentCompiledCsFile);

            AssertEqualStrings(expecStr, compiledStr);
        }

        [Test]
        public void AtomicParameterUsage()
        {
            string finalStr = MakeSkeletonWithMain(
                strInClass:
                "public static void TestMethod(atomic int param1, atomic string param2) \n\t\t" +
                "{ \n\t\t" +
                "}",
                strInMain:
                "TestMethod(1, \"str\");\n\t\t\t" +
                "int val1 = 1; \n\t\t\t" +
                "string val2 = \"str\"; \n\t\t\t" +
                "TestMethod(val1, val2);\n\t\t\t" +
                "atomic int atomicVal1 = 1; \n\t\t\t" +
                "atomic string atomicVal2 = \"str\"; \n\t\t\t" +
                "TestMethod(atomicVal1, atomicVal2);");
            StringToTestFile(finalStr);
            CmdRes res = RunCscWithOutAndStmIOut();
            AssertCmdRes(res);

            string expecStr = MakeSkeletonWithMain(
                strInClass:
                "public static void TestMethod(" + STM.STMNameSpace + ".TMInt param1," + STM.STMNameSpace + ".TMVar<string> param2)" +
                "{" +
                "}",
                strInMain:
                "TestMethod(new " + STM.STMNameSpace + ".TMInt(1), new " + STM.STMNameSpace + ".TMVar<string>(\"str\"));" +
                "int val1 = 1;" +
                "string val2 = \"str\";" +
                "TestMethod(new " + STM.STMNameSpace + ".TMInt(val1), new " + STM.STMNameSpace + ".TMVar<string>(val2));" +
                STM.STMNameSpace + ".TMInt atomicVal1 = new " + STM.STMNameSpace + ".TMInt(1);" +
                STM.STMNameSpace + ".TMVar<string> atomicVal2 = new " + STM.STMNameSpace + ".TMVar<string>(\"str\");" +
                "TestMethod(new " + STM.STMNameSpace + ".TMInt(atomicVal1.Value), new " + STM.STMNameSpace + ".TMVar<string>(atomicVal2.Value));"
                );
            string compiledStr = TestFileToString(currentCompiledCsFile);

            AssertEqualStrings(expecStr, compiledStr);
        }

        [Test]
        public void AtomicOutParameterUsage() //TODO: Er ikke sikker på den virker 100% endnu (lav igen efter Kasper har fikset)
        {
            string finalStr = MakeSkeletonWithMain(
                strInClass:
                "public static void TestMethod(atomic out int param1, atomic out string param2) \n\t\t" +
                "{ \n\t\t\t" +
                    "param1 = 5; \n\t\t\t" +
                    "param2 = \"val\"; \n\t\t" +
                "}",
                strInMain:
                "int val1 = 1; \n\t\t\t" +
                "string val2 = \"str\"; \n\t\t\t" +
                "TestMethod(out val1, out val2);\n\t\t\t" +
                "atomic int atomicVal1 = 1; \n\t\t\t" +
                "atomic string atomicVal2 = \"str\"; \n\t\t\t" +
                "TestMethod(out atomicVal1, out atomicVal2);");
            StringToTestFile(finalStr);
            CmdRes res = RunCscWithOutAndStmIOut();
            AssertCmdRes(res);

            string compiledStr = TestFileToString(currentCompiledCsFile);
            HashSet<string> guids = GetGUIDsFromFile(compiledStr);  //Fetch guid's (global unique id's)

            string expecStr = MakeSkeletonWithMain(
                strInClass:
                "public static void TestMethod(out " + STM.STMNameSpace + ".TMInt param1, out" + STM.STMNameSpace + ".TMVar<string> param2)" +
                "{" +
                    "param1 = new STM.Implementation.Lockbased.TMInt();" +
                    "param2 = new STM.Implementation.Lockbased.TMVar<string>();" +
                    "param1.Value = 5;" +
                    "param2.Value = \"val\";" +
                "}",
                strInMain:
                "int val1 = 1;" +
                "string val2 = \"str\";" +
                STM.STMNameSpace +".TMInt " + guids.ElementAt(0) +  " = new "+ STM.STMNameSpace+".TMInt(val1);" +
                STM.STMNameSpace + ".TMVar<string> " + guids.ElementAt(1) + " = new " + STM.STMNameSpace + ".TMVar<string>(val2);" +
                "TestMethod(out "+ guids.ElementAt(0) + ", out "+ guids.ElementAt(1) + ");" +
                "val1 = " + guids.ElementAt(0) + ".Value;" +
                "val2 = " + guids.ElementAt(1) + ".Value; " +
                STM.STMNameSpace + ".TMInt atomicVal1 = new " + STM.STMNameSpace + ".TMInt(1);" +
                STM.STMNameSpace + ".TMVar<string> atomicVal2 = new " + STM.STMNameSpace + ".TMVar<string>(\"str\");" +
                "TestMethod(out atomicVal1, out atomicVal2);"
                );

            AssertEqualStrings(expecStr, compiledStr);
        }

        [Test]
        public void AtomicRefParameterUsage()
        {
            string finalStr = MakeSkeletonWithMain(
                strInClass:
                "public static void TestMethod(atomic ref int param1, atomic ref string param2) \n\t\t" +
                "{ \n\t\t\t" +
                    "param1 = 5; \n\t\t\t" +
                    "param2 = \"val\"; \n\t\t" +
                "}",
                strInMain:
                "int val1 = 1; \n\t\t\t" +
                "string val2 = \"str\"; \n\t\t\t" +
                "TestMethod(ref val1, ref val2);\n\t\t\t" +
                "atomic int atomicVal1 = 1; \n\t\t\t" +
                "atomic string atomicVal2 = \"str\"; \n\t\t\t" +
                "TestMethod(ref atomicVal1, ref atomicVal2);");
            StringToTestFile(finalStr);
            CmdRes res = RunCscWithOutAndStmIOut();
            AssertCmdRes(res);

            string compiledStr = TestFileToString(currentCompiledCsFile);
            HashSet<string> guids = GetGUIDsFromFile(compiledStr);  //Fetch guid's (global unique id's)

            string expecStr = MakeSkeletonWithMain(
                strInClass:
                "public static void TestMethod(ref " + STM.STMNameSpace + ".TMInt param1, ref" + STM.STMNameSpace + ".TMVar<string> param2)" +
                "{" +
                    "param1 = new STM.Implementation.Lockbased.TMInt();" +
                    "param2 = new STM.Implementation.Lockbased.TMVar<string>();" +
                    "param1.Value = 5;" +
                    "param2.Value = \"val\";" +
                "}",
                strInMain:
                "int val1 = 1;" +
                "string val2 = \"str\";" +
                STM.STMNameSpace + ".TMInt " + guids.ElementAt(0) + " = new " + STM.STMNameSpace + ".TMInt(val1);" +
                STM.STMNameSpace + ".TMVar<string> " + guids.ElementAt(1) + " = new " + STM.STMNameSpace + ".TMVar<string>(val2);" +
                "TestMethod(ref " + guids.ElementAt(0) + ", ref " + guids.ElementAt(1) + ");" +
                "val1 = " + guids.ElementAt(0) + ".Value;" +
                "val2 = " + guids.ElementAt(1) + ".Value; " +
                STM.STMNameSpace + ".TMInt atomicVal1 = new " + STM.STMNameSpace + ".TMInt(1);" +
                STM.STMNameSpace + ".TMVar<string> atomicVal2 = new " + STM.STMNameSpace + ".TMVar<string>(\"str\");" +
                "TestMethod(ref atomicVal1, ref atomicVal2);"
                );

            AssertEqualStrings(expecStr, compiledStr);
        }

        //TODO: MORE
        //Flere vores generings fejl? - sæt det nede i fejl test region (evt. spørg kasper efter hans)

        //Evt. tilføj de nedenstående i hvor metoder med deklæreret også: (ELLER lav nye - fordi, så tjekker vi at de ikke behøves at kaldes, for at skulle laves)
        //Kald af funktioner: atomic ref og atomic out.


        #region Tests for our own generated errors
        [Test]
        public void AtomicDefaultParameter()
        {
            string finalStr = MakeSkeletonWithMain(
                "public void TestMethod(atomic int par1 = 10) \n\t\t" +
                "{ \n\t\t" +
                "}");
            StringToTestFile(finalStr);
            CmdRes res = RunCscWithOutAndStmIOut();
            //AssertCmdRes(res);
            Assert.IsTrue(res.output.Contains("error CS8105: Atomic keyword cannot be used with default parameters") && res.exitCode != 0 && res.error.Length == 0, "Default parameter error was not raised.");
        }

        [Test]
        public void IdenticalMethodOverloadsWithAtomicParameterInt()
        {
            string finalStr = MakeSkeletonWithMain(
                "public void TestMethod(int param1, string param2) \n\t\t" +
                "{ \n\t\t" +
                "} \n\t\t" +
                "public void TestMethod(atomic int param1, string param2) \n\t\t" +
                "{ \n\t\t" +
                "}");
            StringToTestFile(finalStr);
            CmdRes res = RunCscWithOutAndStmIOut();
            //AssertCmdRes(res);
            Assert.IsTrue(res.output.Contains("error IdenticalMethods:") && res.exitCode != 0 && res.error.Length == 0, "Did not generate identical methods error");
        }

        [Test]
        public void IdenticalMethodOverloadsWithAtomicParameterString()
        {
            string finalStr = MakeSkeletonWithMain(
                "public void TestMethod(int param1, string param2) \n\t\t" +
                "{ \n\t\t" +
                "} \n\t\t" +
                "public void TestMethod(int param1, atomic string param2) \n\t\t" +
                "{ \n\t\t" +
                "}");
            StringToTestFile(finalStr);
            CmdRes res = RunCscWithOutAndStmIOut();
            //AssertCmdRes(res);
            Assert.IsTrue(res.output.Contains("error IdenticalMethods:") && res.exitCode != 0 && res.error.Length == 0, "Did not generate identical methods error");
        }

        [Test]
        public void IdenticalMethodOverloadsWithAtomicParameterRef()
        {
            string finalStr = MakeSkeletonWithMain(
                "public void TestMethod(int param1) \n\t\t" +
                "{ \n\t\t" +
                "} \n\t\t" +
                "public void TestMethod(atomic ref int param1) \n\t\t" +
                "{ \n\t\t" +
                "}");
            StringToTestFile(finalStr);
            CmdRes res = RunCscWithOutAndStmIOut();
            AssertCmdRes(res);
        }

        [Test]
        public void IdenticalMethodOverloadsWithParameterRef()
        {
            string finalStr = MakeSkeletonWithMain(
                "public void TestMethod(int param1) \n\t\t" +
                "{ \n\t\t" +
                "} \n\t\t" +
                "public void TestMethod(ref int param1) \n\t\t" +
                "{ \n\t\t" +
                "}");
            StringToTestFile(finalStr);
            CmdRes res = RunCscWithOutAndStmIOut();
            AssertCmdRes(res);
        }


        #endregion

        //[Test]
        //public void WarningTest()
        //{
        //    string finalStr = MakeSkeletonWithMain(
        //        "public int myMethod(){ \n\t\t\t" +
        //        "return 5;\n\t\t\t" +
        //        "return 3;\n}");
        //    StringToTestFile(finalStr);
        //    CmdRes res = RunCscWithOutAndStmIOut();
        //    AssertCmdRes(res);

        //    Assert.IsTrue(res.output.Contains("warning CS0162: Unreachable code detected"));
        //}
    }
}

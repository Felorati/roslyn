using System;
using STM.Implementation.Lockbased;

namespace TestRefNamespace
{
    class TestRef
    {
        /*
        private atomic int i = 0;
        public atomic const int test1 = 1;
        private static atomic int test2 = 2;
        private atomic readonly int test3 = 3;
        */

        /*public atomic int TestProp { get { return true; } set; }

        public static atomic void AtomicMethod()
        {
            atomic int x = 0;
            atomic var y = 1;
        }*/

        private static void AtomicParamTest(atomic int i)
        {

        }

        /*
        private static void RefRefTest(ref ref int x)
        {

        }

        private static void TestMethod(atomic ref int i1, atomic out int i2)
        {

        }

        private static void TestMethod(ref atomic int i1, out atomic int i2)
        {

        }


        private static void ThisTest(this atomic int i3)
        {

        }

        private static void ThisTest2(this ref int i3)
        {
            
        }

        private static void ThisTest3(this out int i3)
        {

        }

        private static void ThisParams(this params string[] s2)
        {

        }


        private static void RefOutTest(ref out int x)
        {

        }

        private static void ParamsTest(atomic params string[] s1)
        {

        }

        private static void ParamsOut(params out string[] s2)
        {

        }

        private static void ParamsOut(params ref string[] s2)
        {

        }*/
        
        private static void Add(int i, int result)
        {
            
            atomic{
				return 5 + 5;
                //Console.WriteLine("Atomic works");
				//retry;
				//Console.WriteLine("Atomic works");
				//int hello;
            }
			atomic{
				return 1+2;
			}
            /*
			orelse
			{
				Console.WriteLine("Orelse works");
				Console.WriteLine("Orelse works");
			}
			orelse
			{
				Console.WriteLine("Orelse 2 works");
				Console.WriteLine("Orelse 2 works");
				
			}*/
            
            if (true){ 
                Console.WriteLine("If3 works");
				Console.WriteLine("If4 works");
                Console.WriteLine("If5 works");
            }
            result += 1;
            return;
        }

        static void Main()
        {
            int total = 20;
            System.Console.WriteLine("Original value of 'total': {0}", total);

            Add(10, total);
            System.Console.WriteLine("Value after calling Add(): {0}", total);
        }
    }
}
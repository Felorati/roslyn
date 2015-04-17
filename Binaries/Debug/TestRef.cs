using System;
//using STM.Implementation.Lockbased;

namespace TestRefNamespace
{
    class TestRef
    {
        public static void OptionalParTest(atomic string par1 = "Dr. Pjuskebusk")
        {

        }

        public atomic int TestProp1 { get; set; }
        public atomic int TestProp2 { get; set; }

        private static void AtomicRefTest(atomic out int test)
        {
            test = 12;
        }

        static void Main()
        {
            atomic int j = 11;
            //AtomicRefTest(out j);

            int x = 10;
            AtomicRefTest(out x);

            int i = 13;
            //AtomicRefTest(out i);

            var test = new TestRef(1);
            var test2 = new TestRef(2,test);
            var test3 = new TestRef(3,test2);
            var test4 = new TestRef(4,test3);

            Console.WriteLine(test4.next.next.x);
            int total = 20;
            System.Console.WriteLine("Original value of 'total': {0}", total);

            Add(10, total);
            System.Console.WriteLine("Value after calling Add(): {0}", total);
            System.Console.WriteLine("Yey atomicvars!");
            NestAtomic();
            System.Console.WriteLine("test3");
            
        }

        private atomic int x;
        private atomic TestRef next;
		
		private static string AtomicTest(atomic int i, atomic TestRef extra)
        {
            return "teststring: " + i;
        }
		
		/*public void MethodOverload(int par, atomic int par2)
		{

		}
        
		public void MethodOverload(int par, int par2)
		{

		}*/
        
        private TestRef GetNext()
        {
            return next;
        }

        public TestRef(atomic int i)
        {
            x = i;
        }
        public TestRef(atomic int i, TestRef next)
        {
            x = i;
            this.next = next;
        }

        private static string AtomicTest(atomic int i)
        {
            return "teststring: " + i;
        }
		
        private static string TestMethod()
        {
            var s = "hej";
            Expressiontest(s);
            int i = 12;
            return AtomicTest(i);
        }
        
        private static string Expressiontest(string s)
        {
            ++iName;
            iName++;
            sName = "test";
            return sName + iName;
        }


		private static atomic string sName = "hejj";
        private static atomic int iName = 5;

        public atomic string TestProp {get; set;}

    	//private atomic TestRef k = null;
		/*
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
		
		private static void AtomicParamTest2(atomic string i2)
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
        
        private static int Add(int i, int result)
        {
            atomic int x = 0, y = 1, z = 2;
            atomic int someInt;
            atomic TestRef tr = null;
            atomic{
				if(i == 5)
				{
					retry;
				}
				
				if(i == 9)
				{
					retry;
				}
				return 5 + 5;
                //Console.WriteLine("Atomic works");
				//retry;
				//Console.WriteLine("Atomic works");
				//int hello;
            }
			atomic{
				retry;
				return 1+2;
			}
			orelse
			{
				Console.WriteLine("Orelse works");
				Console.WriteLine("Orelse works");
				return 1336 + 1;
			}
			/*
			orelse
			{
				Console.WriteLine("Orelse 2 works");
				Console.WriteLine("Orelse 2 works");
				
			}*/
            
            if (true){ 
                Console.WriteLine("If3 works");
				Console.WriteLine("If4 works");
                Console.WriteLine("If5 works");
                Console.WriteLine("If6 works");
            }
            result += 1;
            return result;
        }
		
		private static int NestAtomic()
		{
			atomic dynamic myDyn = 5;
			int x = (int) myDyn;
			myDyn = 10;

            atomic{
				int i = 5;
				atomic{
					i = 1337;
					atomic{
						System.Console.WriteLine("Triple nesting...Wow");
						return i;
					}
				}
			
			}
		
		}
        
    }
}
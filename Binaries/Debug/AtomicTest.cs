using System;
//using STM.Implementation.Lockbased;

namespace TestRefNamespace
{
    class AtomicTest
    {
        private static void AtomicRefTest(atomic out int test)
        {
            test = 16;
        }

        private static void RefTest(out int test)
        {
            test = 16;
        }
		
		private static void DefaultTest(atomic int x, int y = 1, int z = 2)
		{
            Console.WriteLine(x + y + z);
		}

        static void Main()
        {
            TestClass t = new TestClass();
            RefTest(out t.TestField);
            Console.WriteLine("Res-1: " + t.TestField);

            atomic int k = 11;
            RefTest(out k);
            Console.WriteLine("Res0: " + k);

            atomic int j = 11;
            AtomicRefTest(out j);
            Console.WriteLine("Res1: " + j);
            int x = 11;
            AtomicRefTest(out x);
            Console.WriteLine("Res2: "+x);

            DefaultTest(1,z:3);
			DefaultTest(1,y:3);
			DefaultTest(1,2);
        }

        
    }

    public class TestClass
    {
        public atomic int TestField;

        public TestClass()
        {
            TestField = 11;
        }

        public int TestMethod()
        {
            return 11;
        }
    } 
}
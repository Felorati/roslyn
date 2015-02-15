using System;

namespace TestRefNamespace
{
    class TestRef
    {
        private static void Add(int i, int result)
        {
            /*
            atomic{ 
                Console.WriteLine("Atomic works");
            }*/
            /*
            if (true){ 
                Console.WriteLine("If3 works");
            }
            result += 1;*/
            return;
        }

        static void Main()
        {
            int total = 20
            System.Console.WriteLine("Original value of 'total': {0}", total);

            Add(10, total);
            System.Console.WriteLine("Value after calling Add(): {0}", total);
        }
    }
}
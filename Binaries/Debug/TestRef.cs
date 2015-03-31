using System;
using STMExtension; 

namespace TestRefNamespace
{
    class TestRef
    {
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
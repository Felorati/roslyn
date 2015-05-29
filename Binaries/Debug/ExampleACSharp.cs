using System;

namespace ExampleACSharp
{
    class ExampleACSharp
    {
		public atomic int AtomicProp1 { get; set; }
        public atomic string AtomicProp2 { get; set; }
		
		static void Main()
		{
			Console.WriteLine(SpecialIncrementMethod(5)); //TODO: Slet og udkommenter længere ned når de er fixed
			Console.WriteLine(SpecialIncrementMethod(-5)); //gælder samme
			Console.WriteLine("Start of Main");
			atomic int TestVar1;
			atomic int TestVar2 = 100;
			atomic string TestVar3;
			atomic string TestVar4 = "some_str";
			
			NormalMethod(5);
			AtomicParamMethod(10);
			AtomicOutParamMethod(TestVar1);
			AtomicRefParamMethod(TestVar2);
			//Console.WriteLine(SpecialIncrementMethod(5));
			//Console.WriteLine(SpecialIncrementMethod(-5));
			NestedAtomicMethod();
			Console.WriteLine("End of Main");
		}
		
		private static void NormalMethod(int par)
		{
		}
		
		private static void AtomicParamMethod(atomic int par)
		{
			int = 5;
		}
		
		private static void AtomicOutParamMethod(atomic out int par)
        {
            par = 10;
        }
		
		private static void AtomicRefParamMethod(atomic ref int par)
        {
            par = 20;
        }
		
		private static int SpecialIncrementMethod(atomic int par)
		{
			atomic{
				if(par < 0)
					retry;
				return par + 1;
			}
			orelse{
				return par + 2;
			}
		}

		private static void NestedAtomicMethod(atomic int par)
		{
			atomic{
				atomic{
					atomic{
						Console.WriteLine("Triple nesting");
					}
				}
			}
		}
	}
}
        
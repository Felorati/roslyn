using System;
using System.Threading;
//using STM.Implementation.Lockbased;

namespace TestRefNamespace
{
    class AtomicTest
    {
        public static atomic Holder _testField = new Holder();
		
		private  static void TestMethod()
		{
			var backing = _testField;
            Thread.MemoryBarrier();
            //Thread.MemoryBarrier();  Forces the compiler to not move the local variable into the loop header
            //This is important as the iterator will otherwise start iterating over a resized backing array 
            // if a resize happes during iteration.
            //Result if allowed could be the same key value pair being iterated over more than once or not at all
            //This way the iterator only iterates over one backing array if a resize occurs those changes are not taken into account
            //Additions or removals are still possible during iteration => same guarantee as System.Collections.Concurrent.ConcurrentDictionary
            for (var i = 0; i < backing.Length; i++)
            {
				Console.WriteLine(i);
            }
		}
        
		public static void Main()
		{
			TestMethod();
		}
    }
	
	public class Holder
	{
		public atomic int Length;
		
		public Holder()
		{
			Length = 5;
		}
	}

}
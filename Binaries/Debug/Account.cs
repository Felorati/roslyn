using System;

namespace ExampleACSharp
{
    class Account
    {
		public atomic long Balance { get; set; }
		
		static void Main()
		{
			Console.WriteLine("Start of Main");
			var budget = new Account(1000);
			var carLoan = new Account(-50000);
			TransferMoney(1000, budget, carLoan);
			Console.WriteLine("End of Main");
		}
		
		public Account(long balance)
		{
			Balance = balance;
		}
		
		public static TransferMoney(long amount, Account from, Account to)
		{
			atomic{
				from - amount;
				to + amount;
			}
		}
		
		public void Add(long amount)
		{
			Balance += amount;
		}
		
		public void Subtract(long amount)
		{
			atomic{
				if(Balance - amount >= 0)
				{
					Balance -= amount;
				}
				else
				{
					throw new Exception("Insufficient funds!");
				}
			}
		}
		
		public static void operator +(Account account, long amount)
		{
			account.Add(amount);
		}
		
		public static void operator -(Account account, long amount)
		{
			account.Subtract(amount);
		}
	}
}
        
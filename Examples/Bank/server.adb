with Bank; use Bank;

package body Server is

   function Balance
     (Customer : in Customer_Type;
      Password : in Password_Type)
      return Integer is
   begin
      return Bank.Balance (Customer, Password);
   end Balance;

   procedure Deposit
     (Customer : in Customer_Type;
      Amount   : in Positive) is
   begin
      Bank.Deposit (Customer, Amount);
   end Deposit;

   procedure Register
     (Terminal : in Terminal_Access;
      Customer : in Customer_Type;
      Password : in Password_Type) is
   begin
      Bank.Register (Terminal, Customer, Password);
   end Register;

   procedure Transfer
     (Donator  : in Customer_Type;
      Password : in Password_Type;
      Amount   : in Positive;
      Customer : in Customer_Type) is
   begin
      Bank.Transfer (Donator, Password, Amount, Customer);
   end Transfer;

   procedure Withdraw
     (Customer : in Customer_Type;
      Password : in Password_Type;
      Amount   : in Positive) is
   begin
      Bank.Withdraw (Customer, Password, Amount);
   end Withdraw;

end Server;

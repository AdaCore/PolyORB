with Types; use Types;
with Alarm; use Alarm;

package Server is

   pragma Remote_Call_Interface;

   type Terminal_Access is access all Terminal_Type'Class;

   procedure Register
     (Terminal : in Terminal_Access;
      Customer : in Customer_Type;
      Password : in Password_Type);

   function Balance
     (Customer : in Customer_Type;
      Password : in Password_Type)
      return Integer;

   procedure Deposit
     (Customer : in Customer_Type;
      Amount   : in Positive);

   procedure Withdraw
     (Customer : in Customer_Type;
      Password : in Password_Type;
      Amount   : in Positive);

   procedure Transfer
     (Donator  : in Customer_Type;
      Password : in Password_Type;
      Amount   : in Positive;
      Customer : in Customer_Type);

end Server;

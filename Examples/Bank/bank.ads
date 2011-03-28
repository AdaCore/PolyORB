with Types; use Types;
with Server; use Server;

package Bank is

   function Balance
     (Customer : in Customer_Type;
      Password : in Password_Type)
      return Integer;

   procedure Create
     (Customer : in Customer_Type;
      Password : in Password_Type;
      Deposit  : in Positive);

   procedure Deposit
     (Customer : in Customer_Type;
      Amount   : in Positive);

   procedure Register
     (Terminal : in Terminal_Access;
      Customer : in Customer_Type;
      Password : in Password_Type);

   procedure Transfer
     (Donator  : in Customer_Type;
      Password : in Password_Type;
      Amount   : in Positive;
      Customer : in Customer_Type);

   procedure Withdraw
     (Customer : in Customer_Type;
      Password : in Password_Type;
      Amount   : in Positive);

   function Is_Activated (ID : Customer_ID) return Boolean;
   function Get_Customer (ID : Customer_ID) return Customer_Type;
   function Get_Password (ID : Customer_ID) return Password_Type;
   function Get_Balance  (ID : Customer_ID) return Integer;

end Bank;

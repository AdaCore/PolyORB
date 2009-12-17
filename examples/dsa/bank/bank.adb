with Text_IO; use Text_IO;
with Types; use Types;
with Server; use Server;
with Alarm; use Alarm;

package body Bank is

   Null_Customer : constant Customer_Type := "";

   type Customer_Access is access Customer_Type;
   type Password_Access is access Password_Type;

   type ID_Type is range 0 .. N_Customer_IDs;

   Null_ID  : Customer_ID := 0;
   First_ID : Customer_ID := 1;
   Last_ID  : Customer_ID := N_Customer_IDs;

   function Find
     (Customer : in Customer_Type)
      return Customer_ID;

   protected type Account_Type is

      function Balance
        return Integer;

      function Check
        (Password : in Password_Type)
         return Boolean;

      function Customer
        return Customer_Type;

      procedure Deposit
        (Amount : in Integer);

      procedure Initialize
        (Customer  : in Customer_Type;
         Password  : in Password_Type;
         Deposit   : in Positive;
         Available : out Boolean);

      function Password
        return Password_Type;

      procedure Register
        (Terminal : in Terminal_Access);

      function Terminal return Terminal_Access;

   private

      My_Customer : Customer_Access;
      My_Password : Password_Access;
      My_Balance  : Integer := 0;
      My_Terminal : Terminal_Access := null;

   end Account_Type;

   ------------------
   -- Account_Type --
   ------------------

   protected body Account_Type is

      function Balance return Integer is
      begin
         return My_Balance;
      end Balance;

      function Check
        (Password : in Password_Type)
         return Boolean is
      begin
         return My_Password /= null and then
                My_Password.all = Password;
      end Check;

      function Customer return Customer_Type is
      begin
         if My_Customer = null then
            return Null_Customer;
         else
            return My_Customer.all;
         end if;
      end Customer;

      procedure Deposit
        (Amount : in Integer) is
      begin
         My_Balance := My_Balance + Amount;
      end Deposit;

      procedure Initialize
        (Customer  : in Customer_Type;
         Password  : in Password_Type;
         Deposit   : in Positive;
         Available : out Boolean) is
      begin
         if Customer = Null_Customer then
            raise Wrong_Customer;
         end if;
         if My_Customer = null then
            My_Customer := new Customer_Type'(Customer);
            My_Password := new Password_Type'(Password);
            My_Balance  := Deposit;
            Available   := True;
         else
            Available   := False;
         end if;
      end Initialize;

      function Password return Password_Type is
      begin
         if My_Customer = null then
            return "";
         else
            return My_Password.all;
         end if;
      end Password;

      procedure Register
        (Terminal : in Terminal_Access) is
      begin
         My_Terminal := Terminal;
      end Register;

      function Terminal return Terminal_Access is
      begin
         return My_Terminal;
      end Terminal;

   end Account_Type;

   Accounts      : array (First_ID .. Last_ID) of Account_Type;

   -------------
   -- Balance --
   -------------

   function Balance
     (Customer : Customer_Type;
      Password : Password_Type)
      return Integer is
      ID : Customer_ID := Find (Customer);
      Balance   : Integer        := 0;
   begin
      if ID = Null_ID then
         raise Wrong_Customer;
      end if;
      if Accounts (ID).Check (Password) then
         return Accounts (ID).Balance;
      else
         raise Wrong_Password;
      end if;
   end Balance;

   ------------
   -- Create --
   ------------

   procedure Create
     (Customer : in Customer_Type;
      Password : in Password_Type;
      Deposit  : in Positive) is
      Done     : Boolean;
   begin
      if Find (Customer) /= Null_ID then
         raise Wrong_Customer;
      end if;
      for N in Accounts'range loop
         Accounts (N).Initialize (Customer, Password, Deposit, Done);
         if Done then
            return;
         end if;
      end loop;
      raise No_More_IDs;
   end Create;

   ---------------
   --  Deposit  --
   ---------------

   procedure Deposit
     (Customer : in Customer_Type;
      Amount   : in Positive) is
      ID : Customer_ID := Find (Customer);
   begin
      if ID = Null_ID then
         raise Wrong_Customer;
      end if;
      Accounts (ID).Deposit (Amount);
   end Deposit;

   -----------
   --  Find --
   -----------

   function Find
     (Customer : Customer_Type)
      return Customer_ID is
   begin
      for N in Accounts'range loop
         if Accounts (N).Customer = Customer then
            return N;
         end if;
      end loop;
      return Null_ID;
   end Find;

   ------------------
   -- Get_Customer --
   ------------------

   function Get_Customer (ID : Customer_ID) return Customer_Type is
   begin
      return Accounts (ID).Customer;
   end Get_Customer;

   ------------------
   -- Get_Password --
   ------------------

   function Get_Password (ID : Customer_ID) return Password_Type is
   begin
      return Accounts (ID).Password;
   end Get_Password;

   -----------------
   -- Get_Balance --
   -----------------

   function Get_Balance  (ID : Customer_ID) return Integer is
   begin
      return Accounts (ID).Balance;
   end Get_Balance;

   ------------------
   -- Is_Activated --
   ------------------

   function Is_Activated (ID : Customer_ID) return Boolean is
      Customer : Customer_Type := Accounts (ID).Customer;
   begin
      return Customer /= Null_Customer;
   end Is_Activated;

   --------------
   -- Register --
   --------------

   procedure Register
     (Terminal : in Terminal_Access;
      Customer : in Customer_Type;
      Password : in Password_Type) is
      ID : Customer_ID := Find (Customer);
   begin
      if ID = Null_ID then
         raise Wrong_Customer;
      end if;
      if Accounts (ID).Check (Password) then
         Accounts (ID).Register (Terminal);
      else
         raise Wrong_Password;
      end if;
   end Register;

   ---------------
   -- Transfert --
   ---------------

   procedure Transfer
     (Donator  : in Customer_Type;
      Password : in Password_Type;
      Amount   : in Positive;
      Customer : in Customer_Type) is
      ID_1 : Customer_ID := Find (Donator);
      ID_2 : Customer_ID := Find (Customer);
      Term : Terminal_Access;
      Ok   : Boolean := False;
   begin
      if ID_1 = Null_ID then
         raise Wrong_Donator;
      end if;
      if ID_2 = Null_ID then
         raise Wrong_Customer;
      end if;
      if Accounts (ID_1).Check (Password) then
         Accounts (ID_1).Deposit (-Amount);
         Accounts (ID_2).Deposit (Amount);
         Term := Accounts (ID_2).Terminal;
         if Term /= null then
            begin
               Notify (Term, Donator, Amount);
               Ok := True;
            exception when others => null;
            end;
         end if;
         if not Ok then
            New_Line;
            New_Line;
            Put_Line ("=> Warning: couldn't notify client of a transfer");
            New_Line;
         end if;
      else
         raise Wrong_Password;
      end if;
   end Transfer;

   ----------------
   --  Withdraw  --
   ----------------

   procedure Withdraw
     (Customer : in Customer_Type;
      Password : in Password_Type;
      Amount   : in Positive) is
      ID : Customer_ID := Find (Customer);
   begin
      if ID = Null_ID then
         raise Wrong_Customer;
      end if;
      if Accounts (ID).Check (Password) then
         Accounts (ID).Deposit (-Amount);
      else
         raise Wrong_Password;
      end if;
   end Withdraw;

end Bank;

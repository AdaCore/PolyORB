with Bank; use Bank;
with Types; use Types;
with Text_IO; use Text_IO;

procedure Client is

   function Get_Password return Password_Type;
   function Get_Customer return Customer_Type;

   function Get_Customer return Customer_Type is
      C : Customer_Type (1 .. 16);
      L : Natural;
   begin
      Put ("Customer : ");
      Get_Line (String (C), L);
      return C (1 .. L);
   end Get_Customer;

   function Get_Password return Password_Type is
      P : Password_Type (1 .. 8);
      L : Natural;
   begin
      Put ("Password : ");
      Get_Line (String (P), L);
      return P (1 .. L);
   end Get_Password;

   Customer : Customer_Type := Get_Customer;
   Password : Password_Type := Get_Password;
   Request  : String (1 .. 16);
   Length   : Natural;
   Balance  : Integer;
   Shortcut : Character;
   Amount   : Integer;

begin

   loop
      New_Line;
      Balance := Bank.Balance (Customer, Password);
      Put_Line ("Balance :" & Integer'Image (Balance));
      New_Line;

      Put_Line ("Menu :");
      New_Line;
      Put_Line ("   Balance");
      Put_Line ("   Deposit");
      Put_Line ("   Quit");
      Put_Line ("   Withdraw");
      New_Line;

      Put ("Request : ");
      Get_Line (Request, Length);
      New_Line;

      if Length > 0 then
         Shortcut := Request (1);

         case Shortcut is
            when 'B' | 'b' =>
               null;

            when 'D' | 'd' =>
               Put_Line ("=> Deposit");
               New_Line;
               Put ("   Amount : ");
               Get_Line (Request, Length);
               Amount := Integer'Value (Request (1 .. Length));
               Deposit (Customer, Amount);

            when 'W' | 'w' =>
               Put_Line ("=> Withdraw");
               New_Line;
               Put ("   Amount : ");
               Get_Line (Request, Length);
               Amount := Integer'Value (Request (1 .. Length));
               Withdraw (Customer, Password, Amount);

            when 'Q' | 'q'=>
              exit;

            when others =>
               Put_Line ("Illegal operation");

         end case;

      end if;

   end loop;

exception
   when Wrong_Customer =>
      Put ("Wrong customer ");
      Put_Line (String (Customer));
   when Wrong_Password =>
      Put ("Wrong Password ");
      Put_Line (String (Password));
end Client;

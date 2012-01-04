------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                               C L I E N T                                --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 1997-2012, Free Software Foundation, Inc.          --
--                                                                          --
-- This is free software;  you can redistribute it  and/or modify it  under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for  more details.                                               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
--                  PolyORB is maintained by AdaCore                        --
--                     (email: sales@adacore.com)                           --
--                                                                          --
------------------------------------------------------------------------------

with Server; use Server;
with Types; use Types;
with Text_IO; use Text_IO;
with Message; use Message;
with Terminal; use Terminal;

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

   Register (My_Terminal'Access, Customer, Password);

   loop
      New_Line;
      Balance := Server.Balance (Customer, Password);
      Put_Line ("Balance :" & Integer'Image (Balance));
      New_Line;

      Put_Line ("Menu :");
      New_Line;
      Put_Line ("   Balance");
      Put_Line ("   Deposit");
      Put_Line ("   Quit");
      Put_Line ("   Transfer");
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

            when 'T' | 't' =>
               Put_Line ("=> Transfer");
               New_Line;
               Put ("   Amount : ");
               Get_Line (Request, Length);
               Amount := Integer'Value (Request (1 .. Length));
               Put ("   Customer : ");
               Get_Line (Request, Length);
               begin
                  Transfer (Customer, Password, Amount,
                            Customer_Type (Request (1 .. Length)));
               exception
                  when Wrong_Donator =>
                     Put ("Wrong Donator ");
                     Put_Line (Request (1 .. Length));
               end;

            when 'W' | 'w' =>
               Put_Line ("=> Withdraw");
               New_Line;
               Put ("   Amount : ");
               Get_Line (Request, Length);
               Amount := Integer'Value (Request (1 .. Length));
               Withdraw (Customer, Password, Amount);

            when 'Q' | 'q' =>
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

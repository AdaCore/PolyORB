------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                               S E R V E R                                --
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

with Bank; use Bank;

package body Server is

   function Balance
     (Customer : Customer_Type;
      Password : Password_Type)
      return Integer is
   begin
      return Bank.Balance (Customer, Password);
   end Balance;

   procedure Deposit
     (Customer : Customer_Type;
      Amount   : Positive) is
   begin
      Bank.Deposit (Customer, Amount);
   end Deposit;

   procedure Register
     (Terminal : Terminal_Access;
      Customer : Customer_Type;
      Password : Password_Type) is
   begin
      Bank.Register (Terminal, Customer, Password);
   end Register;

   procedure Transfer
     (Donator  : Customer_Type;
      Password : Password_Type;
      Amount   : Positive;
      Customer : Customer_Type) is
   begin
      Bank.Transfer (Donator, Password, Amount, Customer);
   end Transfer;

   procedure Withdraw
     (Customer : Customer_Type;
      Password : Password_Type;
      Amount   : Positive) is
   begin
      Bank.Withdraw (Customer, Password, Amount);
   end Withdraw;

end Server;

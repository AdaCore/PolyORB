------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                                 B A N K                                  --
--                                                                          --
--                                 S p e c                                  --
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

with Types; use Types;
with Server; use Server;

package Bank is

   function Balance
     (Customer : Customer_Type;
      Password : Password_Type)
      return Integer;

   procedure Create
     (Customer : Customer_Type;
      Password : Password_Type;
      Deposit  : Positive);

   procedure Deposit
     (Customer : Customer_Type;
      Amount   : Positive);

   procedure Register
     (Terminal : Terminal_Access;
      Customer : Customer_Type;
      Password : Password_Type);

   procedure Transfer
     (Donator  : Customer_Type;
      Password : Password_Type;
      Amount   : Positive;
      Customer : Customer_Type);

   procedure Withdraw
     (Customer : Customer_Type;
      Password : Password_Type;
      Amount   : Positive);

   function Is_Activated (ID : Customer_ID) return Boolean;
   function Get_Customer (ID : Customer_ID) return Customer_Type;
   function Get_Password (ID : Customer_ID) return Password_Type;
   function Get_Balance  (ID : Customer_ID) return Integer;

end Bank;

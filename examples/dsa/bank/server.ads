------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                               S E R V E R                                --
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
with Alarm; use Alarm;

package Server is

   pragma Remote_Call_Interface;

   type Terminal_Access is access all Terminal_Type'Class;

   procedure Register
     (Terminal : Terminal_Access;
      Customer : Customer_Type;
      Password : Password_Type);

   function Balance
     (Customer : Customer_Type;
      Password : Password_Type)
      return Integer;

   procedure Deposit
     (Customer : Customer_Type;
      Amount   : Positive);

   procedure Withdraw
     (Customer : Customer_Type;
      Password : Password_Type;
      Amount   : Positive);

   procedure Transfer
     (Donator  : Customer_Type;
      Password : Password_Type;
      Amount   : Positive;
      Customer : Customer_Type);

end Server;

------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                              M E S S A G E                               --
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

with Types; use Types;
with Text_Io; use Text_Io;
with Alarm; use Alarm;

package body Message is

   procedure Notify
     (Terminal : access Alarm_Terminal;
      Donator  : Customer_Type;
      Amount   : Integer)
   is
   begin
      New_Line;
      New_Line;
      Put ("=> Receive");
      Put (Integer'Image (Amount));
      Put (" from ");
      Put (String (Donator));
      New_Line;
   end Notify;

end Message;

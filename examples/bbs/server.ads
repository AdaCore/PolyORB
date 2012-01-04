------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                               S E R V E R                                --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2002-2012, Free Software Foundation, Inc.          --
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

with Common; use Common;

package Server is

   pragma Remote_Call_Interface;
   --  This package can be called remotely

   procedure Post_Message
     (Sender  : String;
      Message : String);
   --  Add a message to the BBS service. Sender_Error will be raised if the
   --  sender's name is empty, Message_Error if the message is empty.

   function Number_Of_Messages return Natural;
   --  Return a number of messages that were posted to the BBS

   function Get_Sender (N : Positive) return String;
   --  Return the name of the sender of a particular message. No_Such_Message
   --  will be raised if there is no such message.

   function Get_Message (N : Positive) return String;
   --  Return the content of a particular message. No_Such_Message will be
   --  raised if there is no such message.

   type Penpal_Pointer is access all Penpal_Type'Class;
   --  A Penpal_Pointer can designated any descendent of the Penpal_Type type

   procedure Register (Penpal : Penpal_Pointer);
   --  Register a penpal in the connected users database. Sender_Error will
   --  be raised if the penpal has not been correctly initialized. If a
   --  penpal with this name has been registered already, then it will be
   --  replaced with the new one (to cover the case where a penpal moves
   --  to another machine for example).

   function Get_Penpal (Name : String) return Penpal_Pointer;
   --  Return the object representing a penpal of a given type, or raise
   --  No_Such_Penpal if no penpal by this name has been registered.

   procedure Broadcast (Sender : String; Message : String);
   --  Broadcast a message to every registered penpal

end Server;

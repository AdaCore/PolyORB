------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                            M A I L B O X E S                             --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2009-2012, Free Software Foundation, Inc.          --
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

package Mailboxes is
   pragma Remote_Types;

   subtype Message_Type is String;

   type Mailbox is limited interface;
   procedure Send_Message (Recipient : access Mailbox; Message : Message_Type)
     is abstract;

   type Remote_Mailbox is access all Mailbox'Class;
   pragma Asynchronous (Remote_Mailbox);
   --  Remote access to mailbox

end Mailboxes;

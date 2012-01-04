------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                                  H U B                                   --
--                                                                          --
--                                 B o d y                                  --
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

package body Hub is
   All_Mailboxes : array (0 .. 7) of Mailboxes.Remote_Mailbox;

   procedure Register_Mailbox (Id : Integer; Ptr : Mailboxes.Remote_Mailbox) is
   begin
      All_Mailboxes (Id) := Ptr;
   end Register_Mailbox;

   function Get_Mailbox (Id : Integer) return Mailboxes.Remote_Mailbox is
   begin
      return All_Mailboxes (Id);
   end Get_Mailbox;

end Hub;

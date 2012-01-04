------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--          D S A _ C O M M O N . P E N P A L _ T Y P E . I M P L           --
--                                                                          --
--                                 B o d y                                  --
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

with Ada.Text_IO;

with DSA_Common.Penpal_Type.Skel;
pragma Warnings (Off, DSA_Common.Penpal_Type.Skel);

package body DSA_Common.Penpal_Type.Impl is

   procedure Initialize
     (Self : access Object;
      Name : CORBA.String) is
   begin
      Self.Name := Name;
   end Initialize;

   function Name_Of
     (Self : access Object)
     return CORBA.String
   is
   begin
      return Self.Name;
   end Name_Of;

   procedure New_Message
     (Self : access Object;
      Sender : CORBA.String;
      Message : CORBA.String)
   is
      pragma Unreferenced (Self);
      use CORBA;

      A_Sender : constant String := To_Standard_String (Sender);
      A_Message : constant String := To_Standard_String (Message);
   begin
--        if A_Sender = "" then
--           raise Sender_Error;
--        elsif A_Message = "" then
--           raise Message_Error;
--        else
         Ada.Text_IO.Put_Line
           ("New message: <" & A_Sender & "> " & A_Message);
--        end if;
   end New_Message;

end DSA_Common.Penpal_Type.Impl;

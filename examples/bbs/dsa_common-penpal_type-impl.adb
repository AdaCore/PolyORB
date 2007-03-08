------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--          D S A _ C O M M O N . P E N P A L _ T Y P E . I M P L           --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--            Copyright (C) 2002 Free Software Foundation, Inc.             --
--                                                                          --
-- PolyORB is free software; you  can  redistribute  it and/or modify it    --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. PolyORB is distributed  in the hope that it will be  useful,    --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with PolyORB; see file COPYING. If    --
-- not, write to the Free Software Foundation, 59 Temple Place - Suite 330, --
-- Boston, MA 02111-1307, USA.                                              --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
--                PolyORB is maintained by ACT Europe.                      --
--                    (email: sales@act-europe.fr)                          --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Text_IO;

with DSA_Common.Penpal_Type.Skel;
pragma Warnings (Off, DSA_Common.Penpal_Type.Skel);

package body DSA_Common.Penpal_Type.Impl is

   procedure Initialize
     (Self : access Object;
      Name : in CORBA.String) is
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
      Sender : in CORBA.String;
      Message : in CORBA.String)
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

------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                               C O M M O N                                --
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

with Ada.Text_IO; use Ada.Text_IO;
with Exceptions;  use Exceptions;

package body Common is

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Penpal : in out Penpal_Type;
      Name   : in String)
   is
   begin
      if Name = "" then
         raise Sender_Error;
      end if;
      Penpal.Name := new String'(Name);
   end Initialize;

   -------------
   -- Name_Of --
   -------------

   function Name_Of (Penpal : access Penpal_Type) return String is
   begin
      if Penpal.Name = null then
         raise Sender_Error;
      else
         return Penpal.Name.all;
      end if;
   end Name_Of;

   -----------------
   -- New_Message --
   -----------------

   procedure New_Message
     (Sender    : in String;
      Recipient : access Penpal_Type;
      Message   : in String)
   is
      pragma Warnings (Off);
      pragma Unreferenced (Recipient);
      pragma Warnings (On);
   begin
      if Sender = "" then
         raise Sender_Error;
      elsif Message = "" then
         raise Message_Error;
      else
         Put_Line ("New message: <" & Sender & "> " & Message);
      end if;
   end New_Message;

   ----------
   -- Read --
   ----------

   procedure Read
     (Stream : access Ada.Streams.Root_Stream_Type'Class;
      Penpal : out String_Access)
   is
   begin
      --  No need to use this

      raise Program_Error;
   end Read;

   -----------
   -- Write --
   -----------

   procedure Write
     (Stream : access Ada.Streams.Root_Stream_Type'Class;
      Penpal : in String_Access)
   is
   begin
      --  No need to use this

      raise Program_Error;
   end Write;

end Common;

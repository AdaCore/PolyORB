------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                               C O M M O N                                --
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

with Ada.Text_IO; use Ada.Text_IO;
with Exceptions;  use Exceptions;

package body Common is

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Penpal : in out Penpal_Type;
      Name   : String)
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
     (Sender    : String;
      Recipient : access Penpal_Type;
      Message   : String)
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
      Penpal : String_Access)
   is
   begin
      --  No need to use this

      raise Program_Error;
   end Write;

end Common;

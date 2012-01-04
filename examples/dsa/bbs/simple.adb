------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                               S I M P L E                                --
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

with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_IO;      use Ada.Text_IO;
with Exceptions;       use Exceptions;
with Server;           use Server;
with Utils;            use Utils;

--  This client lets someone post a simple message to the BBS or retrieve
--  all the messages that have been posted on the BBS.
--
--  Syntax: simple post "pseudo" "message"
--              add a message onto the BBS
--          simple read
--              get the messages from the BBS

procedure Simple is

   procedure Post (Sender : String; Message : String);
   --  Post a message

   procedure Read;
   --  Read messages

   procedure Usage;
   --  Print usage

   ----------
   -- Post --
   ----------

   procedure Post (Sender : String; Message : String) is
   begin
      Post_Message (Sender, Message);
   exception
      when Sender_Error =>
         Put_Line ("Invalid sender name");
         Set_Exit_Status (2);
      when Message_Error =>
         Put_Line ("Invalid message");
         Set_Exit_Status (3);
   end Post;

   ----------
   -- Read --
   ----------

   procedure Read is
   begin
      for I in 1 .. Number_Of_Messages loop
         Put_Line ("Message " & Integer_To_String (I) & ": <" &
                   Get_Sender (I) & "> " & Get_Message (I));
      end loop;
   end Read;

   -----------
   -- Usage --
   -----------

   procedure Usage is
   begin
      Put_Line ("Usage: simple post ""nickname"" ""message""");
      Put_Line ("  or   simple read");
      Set_Exit_Status (1);
   end Usage;

begin
   if Argument_Count = 1 and then Argument (1) = "read" then
      Read;
   elsif Argument_Count = 3 and then Argument (1) = "post" then
      Post (Argument (2), Argument (3));
   else
      Usage;
   end if;
end Simple;

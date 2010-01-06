------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                         E V O L U T E D _ P K G                          --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2002-2008, Free Software Foundation, Inc.          --
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

with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.IO_Exceptions;
with Ada.Text_IO;             use Ada.Text_IO;
with Exceptions;              use Exceptions;
with Server;                  use Server;
with Utils;                   use Utils;

package body Evoluted_Pkg is

   Want_To_Quit : Boolean := False;
   --  Set this to True if you want to leave the program

   procedure Cmd_Help;
   --  HELP command

   procedure Cmd_Post;
   --  POST command

   procedure Cmd_Quit;
   --  QUIT command

   procedure Cmd_Read;
   --  READ command

   procedure Cmd_Page;
   --  PAGE commend

   type Command is access procedure;
   type String_Access is access String;

   type Binding is record
      Command_Name : String_Access;
      Real_Command : Command;
      Help_String  : String_Access;
   end record;

   Commands : constant array (Positive range <>) of Binding :=

     ((Command_Name => new String'("help"),
       Real_Command => Cmd_Help'Access,
       Help_String  => new String'("List of commands (this screen)")),

      (Command_Name => new String'("post"),
       Real_Command => Cmd_Post'Access,
       Help_String  => new String'("Post a message to the BBS")),

      (Command_Name => new String'("quit"),
       Real_Command => Cmd_Quit'Access,
       Help_String  => new String'("Quit the program")),

      (Command_Name => new String'("read"),
       Real_Command => Cmd_Read'Access,
       Help_String  => new String'("Read messages posted on the BBS")),

      (Command_Name => new String'("page"),
       Real_Command => Cmd_Page'Access,
       Help_String  => new String'
         ("Send a private message to a connected user.")));

   --------------
   -- Cmd_Help --
   --------------

   procedure Cmd_Help is
   begin
      Put_Line ("List of commands:");
      for I in Commands'Range loop
         Put_Line (Commands (I).Command_Name.all & ": " &
                   Commands (I).Help_String.all);
      end loop;
   end Cmd_Help;

   --------------
   -- Cmd_Post --
   --------------

   procedure Cmd_Post is
   begin
      Post_Message (Sender  => Name_Of (Penpal'Access),
                    Message => Get_Line ("   Message> "));
   exception
      when Message_Error =>
         Put_Line ("Invalid message");
      when Ada.IO_Exceptions.End_Error =>
         Put_Line ("Control-D pressed, aborting post operation");
   end Cmd_Post;

   --------------
   -- Cmd_Page --
   --------------

   procedure Cmd_Page is
   begin
      New_Message
        (Sender    => Name_Of (Penpal'Access),
         Recipient => Get_Penpal (Get_Line ("   Penpal> ")),
         Message   => Get_Line ("   Message> "));
   exception
      when Message_Error =>
         Put_Line ("Invalid message");
      when Ada.IO_Exceptions.End_Error =>
         Put_Line ("Control-D pressed, aborting post operation");
   end Cmd_Page;

   --------------
   -- Cmd_Quit --
   --------------

   procedure Cmd_Quit is
   begin
      Want_To_Quit := True;
   end Cmd_Quit;

   --------------
   -- Cmd_Read --
   --------------

   procedure Cmd_Read is
   begin
      for I in 1 .. Number_Of_Messages loop
         Put_Line ("Message " & Integer_To_String (I) & ": <" &
                   Get_Sender (I) & "> " & Get_Message (I));
      end loop;
   end Cmd_Read;

   --------------
   -- Mainloop --
   --------------

   procedure Mainloop is
      Found    : Boolean;
   begin
      loop
         New_Line;
         declare
            Command : constant String :=
              To_Lower (Get_Line ("Command (type ""help"" if needed)> "));
         begin
            Found := False;
            for I in Commands'Range loop
               if Commands (I) .Command_Name.all = Command then
                  Found := True;
                  Commands (I) .Real_Command.all;
                  exit;
               end if;
            end loop;
            if Command /= "" and then not Found then
               Put_Line ("Unknown command, type ""help"" for help");
            end if;
         end;
         if Want_To_Quit then
            Put_Line ("Exiting");
            exit;
         end if;
      end loop;
   exception
      when Ada.IO_Exceptions.End_Error =>
         Put_Line ("Control-D pressed, exiting");
   end Mainloop;

   protected body Received_Counter is
      procedure Set_Expected (N : Integer) is
      begin
         Expected := N;
      end Set_Expected;

      procedure Message_Received is
      begin
         Expected := Expected - 1;
         if Expected = 0 then
            Put_Line ("Recv done.");
         end if;
      end Message_Received;

      entry Wait_For_Completion when Expected = 0 is
      begin
         null;
      end Wait_For_Completion;
   end Received_Counter;

   procedure New_Message
     (Sender    : String;
      Recipient : access Instrumented_Penpal;
      Message   : String)
   is
   begin
      Received_Counter.Message_Received;
      Common.New_Message
        (Sender, Penpal_Type (Recipient.all)'Access, Message);
   end New_Message;

end Evoluted_Pkg;

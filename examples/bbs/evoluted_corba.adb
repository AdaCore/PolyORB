with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.IO_Exceptions;
with Ada.Text_IO;             use Ada.Text_IO;
with Exceptions;              use Exceptions;
with Utils;                   use Utils;
with CORBA;

package body Evoluted_CORBA is

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
   --  READ command

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
      use DSA_Common.Penpal_Type.Impl;
   begin
      DSA_Server.Post_Message
        (Self    => My_Server,
         Sender  => Name_Of (Penpal'Access),
         Message => CORBA.To_CORBA_String (Get_Line ("   Message> ")));
   exception
      when Message_Error =>
         Put_Line ("Invalid message");
      when Ada.IO_Exceptions.End_Error =>
         Put_Line ("Control-D pressed, aborting post operation");
   end Cmd_Post;

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
      use DSA_Server;
   begin
      for I in 1 .. Number_Of_Messages (My_Server) loop
         Put_Line ("Message " & Integer_To_String (Integer (I)) & ": <"
                   & CORBA.To_Standard_String (Get_Sender (My_Server, I))
                   & "> "
                   & CORBA.To_Standard_String (Get_Message (My_Server, I)));
      end loop;
   end Cmd_Read;

   --------------
   -- Cmd_Read --
   --------------

   procedure Cmd_Page is
      use DSA_Common.Penpal_Type;
      use DSA_Common.Penpal_Type.Impl;
   begin
      new_message (Self    => DSA_Server.Get_Penpal
                   (My_Server, CORBA.To_CORBA_String (Get_Line ("Penpal> "))),
                   Sender  => Name_Of (Penpal'Access),
                   Message => CORBA.To_CORBA_String (Get_Line ("Message> ")));
   end Cmd_Page;

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

end Evoluted_CORBA;

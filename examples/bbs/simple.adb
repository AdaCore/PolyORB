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

   procedure Post (Sender : in String; Message : in String);
   --  Post a message

   procedure Read;
   --  Read messages

   procedure Usage;
   --  Print usage

   ----------
   -- Post --
   ----------

   procedure Post (Sender : in String; Message : in String) is
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

with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_IO;      use Ada.Text_IO;
with Exceptions;       use Exceptions;
with Evoluted_Pkg;     use Evoluted_Pkg;
with Server;           use Server;
with Common;
with Utils;
with GNAT.OS_Lib;

procedure Evoluted is

   --  This program is launched using: evoluted "pseudo"

   procedure Usage;
   --  Print usage

   -----------
   -- Usage --
   -----------

   procedure Usage is
   begin
      Put_Line ("Usage: evoluted ""nickname""");
      Set_Exit_Status (1);
   end Usage;

   procedure Is_Test_Run
     (Name    : in String;
      Is_Test : out Boolean;
      Nmax    : out Integer);

   procedure Is_Test_Run
     (Name    : in String;
      Is_Test : out Boolean;
      Nmax    : out Integer)
   is
      I : Integer;
   begin
      Is_Test := False;
      if Name (Name'First .. Name'First + 3) = "TEST" then
         I := Name'First + 4;
         while I in Name'Range and then Name (I) /= '/' loop
            I := I + 1;
         end loop;
         I := I + 1;
         begin
            Nmax := Integer'Value (Name (I .. Name'Last));
            Is_Test := True;
         exception
            when others =>
               null;
         end;
      end if;
   end Is_Test_Run;

   Is_Test : Boolean;
   Nmax : Integer;

   function Image (I : Integer) return String;
   function Image (I : Integer) return String
   is
      S : constant String := Integer'Image (I);
      First : Integer := S'First;
   begin
      if S (First) = ' ' then
         First := First + 1;
      end if;
      return S (First .. S'Last);
   end Image;

begin
   if Argument_Count = 1 then
      Put ("Initializing local penpal...");
      Initialize (Penpal, Argument (1));
      Put (" registering...");
      Register (Penpal'Access);
      Put_Line (" done.");
      Is_Test_Run (Argument (1), Is_Test, Nmax);
      if Is_Test then
         Expected_Messages := 2 * Nmax;
         Put_Line ("Expecting"
                     & Expected_Messages'Img & " messages.");
         declare
            Dummy : constant String := Utils.Get_Line ("Ready>");
            pragma Unreferenced (Dummy);
         begin
            null;
         end;

         Ada.Text_IO.Put_Line ("Sending bcast");
         Post_Message (Sender  => Name_Of (Penpal'Access),
                       Message => "BCAST from " & Argument (1));

         for J in 0 .. Nmax - 1 loop
            Ada.Text_IO.Put ("Sending page" & J'Img & "...");
            declare
               To : constant String := "TEST_" & Image (J)
                 & "/" & Image (Nmax);
            begin
               Common.New_Message
                 (Sender    => Name_Of (Penpal'Access),
                  Recipient => Get_Penpal (To),
                  Message   => "PAGE from " & Argument (1)
                    & " to " & To);
            end;
            Ada.Text_IO.Put_Line (" done.");
         end loop;
         Send_Done := True;
         Ada.Text_IO.Put_Line ("Send done.");

         while not Recv_Done loop
            delay 0.1;
         end loop;

         GNAT.OS_Lib.OS_Exit (0);
      else
         Mainloop;
      end if;
   else
      Usage;
      Set_Exit_Status (1);
   end if;
exception
   when Sender_Error =>
      Put_Line ("Invalid nickname");
      Set_Exit_Status (2);
end Evoluted;

with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_IO;      use Ada.Text_IO;
with Common;           use Common;
with Exceptions;       use Exceptions;
with Evoluted_Pkg;     use Evoluted_Pkg;
with Server;           use Server;

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

begin
   if Argument_Count = 1 then
      Put ("Initializing local penpal...");
      Initialize (Penpal, Argument (1));
      Put (" registering...");
      Register (Penpal'Access);
      Put_Line (" done.");
      Mainloop;
   else
      Usage;
      Set_Exit_Status (1);
   end if;
exception
   when Sender_Error =>
      Put_Line ("Invalid nickname");
      Set_Exit_Status (2);
end Evoluted;

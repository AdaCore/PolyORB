--
--  main.adb,v 1.2 1996/04/10 15:42:57 tardieu Exp
--

with Ada.Text_IO; use Ada.Text_IO;
with Remote;

procedure Main is

   --  This procedure tests that a remote Text_IO doesn't crash everything.

begin
   Put_Line ("Text_IO here (main) works"); Flush;
   select
      delay 20.0;
      Put_Line ("No answer... Probably a crash at the other side, bad !");
   then abort
      if Remote.Double (3) = 6 then
         Put_Line ("Everything OK");
      else
         Put_Line ("Argh, bad result !");
      end if;
   end select;
end Main;

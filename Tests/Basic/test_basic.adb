--
--  test_basic.adb,v 1.4 1996/04/16 15:25:43 tardieu Exp
--

with Interfaces.C;
with Remote; use Remote;
with Text_IO; use Text_IO;

procedure Test_Basic is

begin
   select
      delay 2000.0;
      Put_Line ("Failed (couldn't connect the remote partition)");
      Flush;
   then abort
      Put_Line (Revert ("!!! enif dekrow sihT"));
   end select;
end Test_Basic;

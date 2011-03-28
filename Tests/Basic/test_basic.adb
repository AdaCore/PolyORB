--
--  test_basic.adb,v 1.4 1996/04/16 15:25:43 tardieu Exp
--

with Interfaces.C;
with Remote; use Remote;
with Text_IO; use Text_IO;

procedure Test_Basic is

begin
   Put_Line ("Local partition is" & Integer'Image (Text_IO'Partition_ID));
   Put_Line ("Remote partition is" & Integer'Image (Remote'Partition_ID));
   select
      delay 2000.0;
      Put_Line ("Failed (couldn't connect the remote partition)");
      Flush;
   then abort
      Put_Line (Revert ("!!! enif dekrow sihT"));
   end select;
end Test_Basic;

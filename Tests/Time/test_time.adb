--
--  test_time.adb,v 1.2 1996/04/10 15:43:02 tardieu Exp
--

with Ada.Real_Time; use Ada.Real_Time;
with Test_Time_Remote; use Test_Time_Remote;
with Text_IO; use Text_IO;

procedure Test_Time is

   Before, After : Time;

   procedure Test is
   begin
      Before := Clock;
      Do_Nothing;
      After := Clock;
      Put_Line ("Done in " & Duration'Image (To_Duration (After - Before)) &
                "seconds");
   end Test;

begin
   Put_Line ("Waiting for the remote partition");
   Do_Nothing;
   Put_Line ("Starting");
   for I in 1 .. 20 loop
      Test;
   end loop;
   Put_Line ("Starting second test (5 seconds)");
   Before := Clock;
   Wait_5_Seconds;
   After := Clock;
   Put_Line ("Done in " & Duration'Image (To_Duration (After - Before)) &
             "seconds");
end Test_Time;

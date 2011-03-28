--
--  test_time.adb,v 1.2 1996/04/10 15:43:02 tardieu Exp
--

with Ada.Real_Time; use Ada.Real_Time;
with Test_Time_Remote; use Test_Time_Remote;
with Text_IO; use Text_IO;

procedure Test_Time is

   Count : constant := 1_000;
   Before, After : Time;
   Table : Table_Type := (others => 0.0);

begin
   Put_Line ("Waiting for the remote partition");
   Do_Nothing (Table);
   Put_Line ("Starting");
   Before := Clock;
   for I in 1 .. Count loop
      Do_Nothing (Table);
   end loop;
   After := Clock;
   Put_Line
      ("Avg Time : " &
       Duration'Image (To_Duration (After - Before) / Duration (Count)) &
       "seconds");
end Test_Time;

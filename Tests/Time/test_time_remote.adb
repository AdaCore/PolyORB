--
--  test_time_remote.adb,v 1.2 1996/04/10 15:43:03 tardieu Exp
--

package body Test_Time_Remote is

   procedure Do_Nothing is begin null; end Do_Nothing;

   procedure Wait_5_Seconds is
   begin
      delay 5.0;
   end Wait_5_Seconds;

end Test_Time_Remote;

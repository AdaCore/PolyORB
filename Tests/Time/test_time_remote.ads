--
--  test_time_remote.ads,v 1.2 1996/04/10 15:43:04 tardieu Exp
--

package Test_Time_Remote is

   pragma Remote_Call_Interface;

   procedure Do_Nothing;
   --  Do nothing :)

   procedure Wait_5_Seconds;
   --  Wait 5 seconds... :/

end Test_Time_Remote;

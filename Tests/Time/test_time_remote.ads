--
--  test_time_remote.ads,v 1.2 1996/04/10 15:43:04 tardieu Exp
--

package Test_Time_Remote is

   pragma Remote_Call_Interface;

   type Table_Type is array (1 .. 1_000) of Float;

   procedure Do_Nothing (Table : in out Table_Type);
   --  Do nothing :)

   procedure Wait_5_Seconds;
   --  Wait 5 seconds... :/

end Test_Time_Remote;

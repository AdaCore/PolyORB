--
--  test_abort.adb,v 1.3 1996/04/10 15:42:44 tardieu Exp
--
--  This program tests for availibility of abortion in distributed mode.
--

with Remote_Abort; use Remote_Abort;
with Text_IO; use Text_IO;

procedure Main_Abort is

begin
   select
      delay 15.0;
      Put_Line ("The delay has expired"); Flush;
   then abort
      Put_Line ("Executing"); Flush;
      Execute;
      Put_Line ("End of execution. I didn't get interrupted"); Flush;
   end select;
   if Status then
      Put_Line ("This worked fine !");
   else
      Put_Line ("Something went wrong");
   end if;
end Main_Abort;

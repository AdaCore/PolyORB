--
--  remote_abort.adb,v 1.2 1996/04/10 15:42:42 tardieu Exp
--

package body Remote_Abort is

   Execute_Has_Been_Called : Integer := 0;
   Execute_Has_Been_Terminated  : Integer := 0;

   procedure Execute is
   begin
      Execute_Has_Been_Called := Execute_Has_Been_Called + 1;
      delay 4.0;
      Execute_Has_Been_Terminated  := Execute_Has_Been_Terminated + 1;
   end Execute;

   procedure Status (Started, Aborted : out Integer) is
   begin
      Started := Execute_Has_Been_Called;
      Aborted := Execute_Has_Been_Called - Execute_Has_Been_Terminated;
   end Status;

end Remote_Abort;

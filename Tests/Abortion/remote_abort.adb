--
--  remote_abort.adb,v 1.2 1996/04/10 15:42:42 tardieu Exp
--

package body Remote_Abort is

   Execute_Has_Been_Called : Boolean := False;
   Execute_Has_Terminated  : Boolean := False;

   procedure Execute is
   begin
      Execute_Has_Been_Called := True;
      delay 20.0;
      Execute_Has_Terminated  := True;
   end Execute;

   function Status return Boolean is
   begin
      return Execute_Has_Been_Called and not Execute_Has_Terminated;
   end Status;

end Remote_Abort;

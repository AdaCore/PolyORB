--
--  remote_abort.ads,v 1.2 1996/04/10 15:42:43 tardieu Exp
--

package Remote_Abort is

   pragma Remote_Call_Interface;

   procedure Execute;
   --  This procedure takes at least 20 seconds to execute.

   function Status return Boolean;
   --  If Execute is called but aborted before the end, Status will return
   --  True. Otherwise, Status will return False.

end Remote_Abort;

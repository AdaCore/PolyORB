--
--  $Id$
--

package body System.RPC.Util is

   ---------------
   -- Deep_Free --
   ---------------

   procedure Deep_Free (Stream : in out Params_Stream_Access) is
      Next : Node_Ptr;
   begin
      if Stream = null then
         return;
      end if;
      while Stream.First /= null loop
         Next := Stream.First.Next;
         Free (Stream.First);
         Stream.First := Next;
      end loop;
      Free (Stream);
   end Deep_Free;

end System.RPC.Util;

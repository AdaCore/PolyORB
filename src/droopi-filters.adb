--  A communication filter (a transport Data_Unit handler/forwarder).

--  $Id$

package body Droopi.Filters is

   function Lower (F : access Filter) return Component_Access is
   begin
      return F.Lower;
   end Lower;

   function Server_Of (F : access Filter) return Schedulers.Server_Access is
   begin
      return F.Server;
   end Server_Of;

   function Create_Filter_Chain (FChain : Factory_Chain_Access)
     return Filter_Access
   is
      F : Filter_Access;
   begin
      pragma Assert (FChain /= null);

      Create (Fact => FChain.This, Filt => F);
      --  Create new filter.

      if FChain.Upper /= null then
         declare
            Upper : constant Filter_Access
              := Create_Filter_Chain (FChain.Upper);
         begin
            Connect (F.Upper, Component_Access (Upper));
            Connect (Upper.Lower, Component_Access (F));
         end;
      end if;
      return F;
   end Create_Filter_Chain;

end Droopi.Filters;

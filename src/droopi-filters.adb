--  A communication filter (a transport SDU handler/forwarder).

--  $Id$

package body Droopi.Filters is

   function Lower (F : access Filter) return Filter_Access is
   begin
      return F.Lower;
   end Lower;

   function Server_Of (F : access Filter) return Servers.Server_Access is
   begin
      return F.Server;
   end Server_Of;

   procedure Create_Filter_Chain
     (Lower  : Filter_Access;
      FChain : Factory_Chain_Access) is
   begin
      pragma Assert (FChain /= null);

      Create (Fact => FChain.This,
              Filt => Lower.Upper);
      Lower.Upper.Lower := Lower;
      --  Instanciate the upper layer and link.

      if FChain.Upper /= null then
         Create_Filter_Chain (Lower.Upper, FChain.Upper);
         --  Continue chaining for upper layers.
      end if;
   end Create_Filter_Chain;

end Droopi.Filters;

with weapon.Impl ;
with weapon.Marshal ;
with Netbufferedstream ;
with Membufferedstream ;
with Omniropeandkey ;
with Giop ;
with Corba ;
use weapon.Impl ;
use weapon.Marshal ;
use Netbufferedstream ;
use Membufferedstream ;
use Omniropeandkey ;
use Giop ;
use Corba ;
use Netbufferedstream ;
use Membufferedstream ;
use type Corba.Unsigned_Long ;

package body weapon.Skeleton is

   procedure Dispatch (Myself : in Omniobject.Implemented_Object_Ptr ;
                       Orls : in out Giop_S.Object ;
                       Orl_Op : in Standard.String ;
                       Orl_Response_Expected : in Corba.Boolean ;
                       Dispatch_Returns : out Corba.Boolean) is
      Self : weapon.Impl.Object_Ptr := weapon.Impl.Object_Ptr(Myself) ;
   begin
   -----------------------
   -- IDL definitions   --
   -----------------------

      if Orl_Op = "shoot" then
         declare
            ranges : dist ;
            Mesg_Size : Corba.Unsigned_Long ;
         begin
            -- unmarshalls arguments
            UnMarshall(ranges, Orls) ;
            -- change state
            Giop_S.Request_Received(Orls) ;
            -- call the implementation
            weapon.Impl.shoot(Self, ranges) ;
            -- compute the size of the replied message
            Mesg_Size := Giop_S.Reply_Header_Size ;
            -- Initialisation of the reply
            Giop_S.Initialize_Reply (Orls, Giop.NO_EXCEPTION, Mesg_Size) ;
            -- Marshall the arguments
            -- inform the orb
            Giop_S.Reply_Completed (Orls) ;
            Dispatch_Returns := True ;
            return ;
         end ;
      end if ;



      Dispatch_Returns := false ;
   end ;

end weapon.Skeleton  ;

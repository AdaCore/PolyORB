with Echo.Impl ;
with Echo.Marshal ;
with Netbufferedstream ;
with Membufferedstream ;
with Omniropeandkey ;
with Giop ;
with Corba ;
use Echo.Impl ;
use Echo.Marshal ;
use Netbufferedstream ;
use Membufferedstream ;
use Omniropeandkey ;
use Giop ;
use Corba ;
use Netbufferedstream ;
use Membufferedstream ;
use type Corba.Unsigned_Long ;

package body Echo.Skeleton is

   procedure Dispatch (Myself : in Omniobject.Implemented_Object_Ptr ;
                       Orls : in out Giop_S.Object ;
                       Orl_Op : in Standard.String ;
                       Orl_Response_Expected : in Corba.Boolean ;
                       Returns : out Corba.Boolean) is
      Self : Echo.Impl.Object_Ptr := Echo.Impl.Object_Ptr(Myself) ;
   begin
   -----------------------
   -- IDL definitions   --
   -----------------------

      if Orl_Op = "echoString" then
         declare
            mesg : Corba.String ;
            Result : Corba.String ;
            Mesg_Size : Corba.Unsigned_Long ;
         begin
            -- unmarshalls arguments
            UnMarshall(mesg, Orls) ;
            -- change state
            Giop_S.Request_Received(Orls) ;
            -- call the implementation
            Result := Echo.Impl.echoString(Self, mesg) ;
            -- compute the size of the replied message
            Mesg_Size := Giop_S.Reply_Header_Size ;
            Mesg_Size := Align_Size (Result, Mesg_Size) ;
            -- Initialisation of the reply
            Giop_S.Initialize_Reply (Orls, Giop.NO_EXCEPTION, Mesg_Size) ;
            -- Marshall the arguments
            Marshall (Result, Orls) ;
            -- inform the orb
            Giop_S.Reply_Completed (Orls) ;
            Returns := True ;
            return ;
         end ;
      end if ;



      Returns := false ;
   end ;

end Echo.Skeleton  ;

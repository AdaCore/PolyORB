with vehicle.Impl ;
with vehicle.Marshal ;
with Netbufferedstream ;
with Membufferedstream ;
with Omniropeandkey ;
with Giop ;
with Corba ;
with Corba.Object ;
use vehicle.Impl ;
use vehicle.Marshal ;
use Netbufferedstream ;
use Membufferedstream ;
use Omniropeandkey ;
use Giop ;
use Corba ;
use Corba.Object ;
package body vehicle.Skeleton is

   procedure Dispatch (Myself : in Omniobject.Implemented_Object_Ptr ;
                       Orls : in out Giop_S.Object ;
                       Orl_Op : in Standard.String ;
                       Orl_Response_Expected : in Corba.Boolean ;
                       Dispatch_Returns : out Corba.Boolean) is
      Self : vehicle.Impl.Object_Ptr := vehicle.Impl.Object_Ptr(Myself) ;
   begin
   -----------------------
   -- IDL definitions   --
   -----------------------

      if Orl_Op = "_get_mark" then
         declare
            Result : Corba.String ;
            Mesg_Size : Corba.Unsigned_Long ;
         begin
            -- change state
            Giop_S.Request_Received(Orls) ;
            -- call the implementation
            Result := vehicle.Impl.Get_mark(Self) ;
            -- compute the size of the replied message
            Mesg_Size := Giop_S.Reply_Header_Size ;
            Mesg_Size := Align_Size (Result, Mesg_Size) ;
            -- Initialisation of the reply
            Giop_S.Initialize_Reply (Orls, Giop.NO_EXCEPTION, Mesg_Size) ;
            -- Marshall the arguments
            Marshall (Result, Orls) ;
            -- inform the orb
            Giop_S.Reply_Completed (Orls) ;
            Dispatch_Returns := True ;
            return ;
         end ;
      end if ;

      if Orl_Op = "_set_mark" then
         declare
            Mesg : Corba.String ;
            Mesg_Size : Corba.Unsigned_Long ;
         begin
            -- unmarshalls arguments
            Unmarshall (Mesg,Orls) ;
            -- change state
            Giop_S.Request_Received(Orls) ;
            -- call the implementation
            vehicle.Impl.Set_mark(Self,Mesg) ;
            -- compute the size of the replied message
            Mesg_Size := Giop_S.Reply_Header_Size ;
            -- Initialisation of the reply
            Giop_S.Initialize_Reply (Orls, Giop.NO_EXCEPTION, Mesg_Size) ;
            -- inform the orb
            Giop_S.Reply_Completed (Orls) ;
            Dispatch_Returns := True ;
            return ;
         end ;
      end if ;

      -- vehicle.can_drive
      -----------------------
      if Orl_Op = "can_drive" then
         declare
            age : Corba.Unsigned_Short ;
            Returns : Corba.Boolean ;
            Mesg_Size : Corba.Unsigned_Long ;
         begin
            -- unmarshalls arguments
            UnMarshall(age, Orls) ;
            -- change state
            Giop_S.Request_Received(Orls) ;
            -- call the implementation
            Returns := vehicle.Impl.can_drive(Self, age) ;
            -- compute the size of the replied message
            Mesg_Size := Giop_S.Reply_Header_Size ;
            Mesg_Size := Align_Size (Returns, Mesg_Size) ;
            -- Initialisation of the reply
            Giop_S.Initialize_Reply (Orls, Giop.NO_EXCEPTION, Mesg_Size) ;
            -- Marshall the arguments
            Marshall (Returns, Orls) ;
            -- inform the orb
            Giop_S.Reply_Completed (Orls) ;
            Dispatch_Returns := True ;
            return ;
         end ;
      end if ;



      Dispatch_Returns := false ;
   end ;

end vehicle.Skeleton  ;

--  $Id$

package body CORBA.Impl is

   function Handle_Message
     (Self : access Object;
      Msg  : Droopi.Components.Message'Class)
     return Droopi.Components.Message'Class
   is
      use Droopi.Components;

      Res : Null_Message;
   begin
      raise Unhandled_Message;
      return Res;
   end Handle_Message;

   function Handle_Message
     (Self : access Implementation;
      Msg  : Droopi.Components.Message'Class)
     return Droopi.Components.Message'Class is
   begin
      return Handle_Message (Self.As_Object, Msg);
   end Handle_Message;

   function To_Droopi_Servant (S : access Object)
     return Droopi.Objects.Servant_Access is
   begin
      return S.As_Component'Access;
   end To_Droopi_Servant;

   function "=" (X, Y : Implementation) return Boolean
   is
   begin
      raise Program_Error;
      return False;
   end "=";

end CORBA.Impl;

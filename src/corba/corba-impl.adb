--  $Id$

package body CORBA.Impl is

   function Handle_Message
     (Self : access Object;
      Msg  : PolyORB.Components.Message'Class)
     return PolyORB.Components.Message'Class
   is
      use PolyORB.Components;

      Res : Null_Message;
   begin
      raise Unhandled_Message;
      return Res;
   end Handle_Message;

   function Handle_Message
     (Self : access Implementation;
      Msg  : PolyORB.Components.Message'Class)
     return PolyORB.Components.Message'Class is
   begin
      return Handle_Message (Self.As_Object, Msg);
   end Handle_Message;

   function To_PolyORB_Servant (S : access Object)
     return PolyORB.Objects.Servant_Access is
   begin
      return S.Neutral_View'Access;
   end To_PolyORB_Servant;

   function "=" (X, Y : Implementation) return Boolean
   is
   begin
      raise Program_Error;
      return False;
   end "=";

end CORBA.Impl;

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

end CORBA.Impl;

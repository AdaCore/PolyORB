--  $Id$

with Droopi.Components;
with Droopi.Smart_Pointers;

package CORBA.Impl is

   pragma Elaborate_Body;

   type Object is abstract new Droopi.Smart_Pointers.Entity
     with private;
   type Object_Ptr is access all Object'Class;

   --  procedure Inc_Usage (Obj : in Object_Ptr);
   --  procedure Dec_Usage (Obj : in out Object_Ptr);

   function Handle_Message
     (Self : access Object;
      Msg  : Droopi.Components.Message'Class)
     return Droopi.Components.Message'Class;

private

   type Implementation (As_Object : access Object'Class)
   is new Droopi.Components.Component with null record;

   function Handle_Message
     (Self : access Implementation;
      Msg  : Droopi.Components.Message'Class)
     return Droopi.Components.Message'Class;

   type Object is abstract new Droopi.Smart_Pointers.Entity
     with record
        As_Component : aliased Implementation (Object'Access);
     end record;

end CORBA.Impl;

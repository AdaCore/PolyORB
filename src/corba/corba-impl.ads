--  $Id$

with Droopi.Components;
with Droopi.Objects;
with Droopi.POA_Types;
with Droopi.Smart_Pointers;

package CORBA.Impl is

   pragma Elaborate_Body;

   type Object is abstract new Droopi.Smart_Pointers.Entity
     with private;
   --  type Object_Ptr is access all Object'Class;
   subtype Object_Ptr is Droopi.Smart_Pointers.Entity_Ptr;
   --  Object_Ptr is the return type of CORBA.AbstractBase.Object_Of.
   --  It may either designate an actual local object
   --  (a CORBA.Impl.Object'Class), or a surrogate thereof
   --  (a D.SP.Entity'Class not derived from CORBA.Impl.Object).

   --  procedure Inc_Usage (Obj : in Object_Ptr);
   --  procedure Dec_Usage (Obj : in out Object_Ptr);

   function Handle_Message
     (Self : access Object;
      Msg  : Droopi.Components.Message'Class)
     return Droopi.Components.Message'Class;

   function To_Droopi_Servant (S : access Object)
     return Droopi.Objects.Servant_Access;

private

   type Implementation (As_Object : access Object'Class)
   is new Droopi.POA_Types.Servant with null record;
   --  The CORBA personality is based on the Portable Object Adapter.

   function "=" (X, Y : Implementation) return Boolean;
   --  ??? XXX Why does the compiler require the presence of this operator?
   --  As a descendant of Component, Implementation is a limited type!
   function Handle_Message
     (Self : access Implementation;
      Msg  : Droopi.Components.Message'Class)
     return Droopi.Components.Message'Class;

   type Object is abstract new Droopi.Smart_Pointers.Entity with
   record
      Neutral_View : aliased Implementation (Object'Access);
      --  The Droopi (personality-neutral) view of this servant.
   end record;

end CORBA.Impl;

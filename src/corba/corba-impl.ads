--  $Id$

with PolyORB.Components;
with PolyORB.Objects;
with PolyORB.POA_Types;
with PolyORB.Smart_Pointers;

package CORBA.Impl is

   pragma Elaborate_Body;

   type Object is abstract new PolyORB.Smart_Pointers.Entity
     with private;
   --  type Object_Ptr is access all Object'Class;
   subtype Object_Ptr is PolyORB.Smart_Pointers.Entity_Ptr;
   --  Object_Ptr is the return type of CORBA.AbstractBase.Object_Of.
   --  It may either designate an actual local object
   --  (a CORBA.Impl.Object'Class), or a surrogate thereof
   --  (a D.SP.Entity'Class not derived from CORBA.Impl.Object).

   function Handle_Message
     (Self : access Object;
      Msg  : PolyORB.Components.Message'Class)
     return PolyORB.Components.Message'Class;

   function To_PolyORB_Servant (S : access Object)
     return PolyORB.Objects.Servant_Access;

private

   type Implementation (As_Object : access Object'Class)
   is new PolyORB.POA_Types.Servant with null record;
   --  The CORBA personality is based on the Portable Object Adapter.

   function "=" (X, Y : Implementation) return Boolean;
   --  ??? XXX Why does the compiler require the presence of this operator?
   --  As a descendant of Component, Implementation is a limited type!
   function Handle_Message
     (Self : access Implementation;
      Msg  : PolyORB.Components.Message'Class)
     return PolyORB.Components.Message'Class;

   type Object is abstract new PolyORB.Smart_Pointers.Entity with
   record
      Neutral_View : aliased Implementation (Object'Access);
      --  The PolyORB (personality-neutral) view of this servant.
   end record;

end CORBA.Impl;

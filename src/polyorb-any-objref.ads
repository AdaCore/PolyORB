--  PolyORB components

--  $Id$

--  Any's that contain object references.

with PolyORB.References;

package PolyORB.Any.ObjRef is

   pragma Elaborate_Body;

   function To_Any (Item : in PolyORB.References.Ref) return Any;
   function From_Any (Item : in Any) return PolyORB.References.Ref;
   procedure Set_Any_Value
     (Any_Value : in out Any;
      Value     : in     PolyORB.References.Ref);

end PolyORB.Any.ObjRef;

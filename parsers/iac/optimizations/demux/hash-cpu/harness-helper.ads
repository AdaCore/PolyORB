with CORBA.Object;
with PolyORB.Any;
with CORBA;

package Harness.Helper is

   function To_Ref
     (The_Ref : in CORBA.Object.Ref'Class)
     return Harness.Ref;

   function Unchecked_To_Ref
     (The_Ref : in CORBA.Object.Ref'Class)
     return Harness.Ref;

   TC_Harness : CORBA.TypeCode.Object :=
     CORBA.TypeCode.Internals.To_CORBA_Object
        (PolyORB.Any.TypeCode.TC_Object);

   function From_Any
     (Item : in CORBA.Any)
     return Harness.Ref;

   function To_Any
     (Item : in Harness.Ref)
     return CORBA.Any;

end Harness.Helper;

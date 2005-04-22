
with CORBA.Object;
with PolyORB.Any;

package DynamicAny.DynArray.Helper is

   TC_DynArray : CORBA.TypeCode.Object
     := CORBA.TypeCode.Internals.To_CORBA_Object
     (PolyORB.Any.TypeCode.TC_Object);

   function Unchecked_To_Local_Ref
     (The_Ref : in CORBA.Object.Ref'Class)
      return DynamicAny.DynArray.Local_Ref;

   function To_Local_Ref
     (The_Ref : in CORBA.Object.Ref'Class)
      return DynamicAny.DynArray.Local_Ref;

end DynamicAny.DynArray.Helper;

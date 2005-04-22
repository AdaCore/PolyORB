
with CORBA.Object;
with PolyORB.Any;

package DynamicAny.DynValue.Helper is

   TC_DynValue : CORBA.TypeCode.Object
     := CORBA.TypeCode.Internals.To_CORBA_Object
     (PolyORB.Any.TypeCode.TC_Object);

   function Unchecked_To_Local_Ref
     (The_Ref : in CORBA.Object.Ref'Class)
      return DynamicAny.DynValue.Local_Ref;

   function To_Local_Ref
     (The_Ref : in CORBA.Object.Ref'Class)
      return DynamicAny.DynValue.Local_Ref;

end DynamicAny.DynValue.Helper;

-------------------------------------------------
--  This file has been generated automatically
--  by IDLAC (http://libre.act-europe.fr/polyorb/)
--
--  Do NOT hand-modify this file, as your
--  changes will be lost when you re-run the
--  IDL to Ada compiler.
-------------------------------------------------
pragma Style_Checks (Off);

with PolyORB.Any;
with CORBA;
with CORBA.Object;

package EnumTests.Helper is

   pragma Elaborate_Body;

   function Unchecked_To_Ref
     (The_Ref : in CORBA.Object.Ref'Class)
     return EnumTests.Ref;
   function To_Ref
     (The_Ref : in CORBA.Object.Ref'Class)
     return EnumTests.Ref;

   TC_EnumTests : CORBA.TypeCode.Object
     := CORBA.TypeCode.Internals.To_CORBA_Object (PolyORB.Any.TypeCode.TC_Object);

   function From_Any (Item : in CORBA.Any)
      return EnumTests.Ref;

   function To_Any
     (Item : in EnumTests.Ref)
     return CORBA.Any;

   TC_Color : CORBA.TypeCode.Object :=
      CORBA.TypeCode.Internals.To_CORBA_object (PolyORB.Any.TypeCode.TC_Enum);

   function From_Any (Item : in CORBA.Any)
      return EnumTests.Color;

   function To_Any
     (Item : in EnumTests.Color)
     return CORBA.Any;

end EnumTests.Helper;

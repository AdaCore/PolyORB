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
pragma Elaborate_All (CORBA);
with CORBA.Object;

package mod1.Int1.Helper is

   pragma Elaborate_Body;

   function Unchecked_To_Ref
     (The_Ref : in CORBA.Object.Ref'Class)
     return mod1.Int1.Ref;
   function To_Ref
     (The_Ref : in CORBA.Object.Ref'Class)
     return mod1.Int1.Ref;

   TC_Int1 : CORBA.TypeCode.Object
     := CORBA.TypeCode.Internals.To_CORBA_Object (PolyORB.Any.TypeCode.TC_Object);

   function From_Any (Item : in CORBA.Any)
      return mod1.Int1.Ref;

   function To_Any
     (Item : in mod1.Int1.Ref)
     return CORBA.Any;

   TC_New_Float : CORBA.TypeCode.Object := 
   CORBA.TypeCode.Internals.To_CORBA_Object (
   PolyORB.Any.TypeCode.TC_Alias);

   function From_Any (Item : in CORBA.Any)
      return mod1.Int1.New_Float;

   function To_Any
     (Item : in mod1.Int1.New_Float)
     return CORBA.Any;

   TC_Color : CORBA.TypeCode.Object :=
      CORBA.TypeCode.Internals.To_CORBA_object (PolyORB.Any.TypeCode.TC_Enum);

   function From_Any (Item : in CORBA.Any)
      return mod1.Int1.Color;

   function To_Any
     (Item : in mod1.Int1.Color)
     return CORBA.Any;

end mod1.Int1.Helper;

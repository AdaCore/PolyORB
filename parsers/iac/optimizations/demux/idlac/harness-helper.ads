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

package Harness.Helper is

   pragma Elaborate_Body;

   function Unchecked_To_Ref
     (The_Ref : in CORBA.Object.Ref'Class)
     return Harness.Ref;
   function To_Ref
     (The_Ref : in CORBA.Object.Ref'Class)
     return Harness.Ref;

   TC_Harness : CORBA.TypeCode.Object
     := CORBA.TypeCode.Internals.To_CORBA_Object (PolyORB.Any.TypeCode.TC_Object);

   function From_Any (Item : in CORBA.Any)
      return Harness.Ref;

   function To_Any
     (Item : in Harness.Ref)
     return CORBA.Any;

end Harness.Helper;

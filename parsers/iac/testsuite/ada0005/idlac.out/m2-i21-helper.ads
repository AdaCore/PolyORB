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

package m2.I21.Helper is

   pragma Elaborate_Body;

   function Unchecked_To_Ref
     (The_Ref : in CORBA.Object.Ref'Class)
     return m2.I21.Ref;
   function To_Ref
     (The_Ref : in CORBA.Object.Ref'Class)
     return m2.I21.Ref;

   TC_I21 : CORBA.TypeCode.Object
     := CORBA.TypeCode.Internals.To_CORBA_Object (PolyORB.Any.TypeCode.TC_Object);

   function From_Any (Item : in CORBA.Any)
      return m2.I21.Ref;

   function To_Any
     (Item : in m2.I21.Ref)
     return CORBA.Any;

   TC_new_bool : CORBA.TypeCode.Object := 
   CORBA.TypeCode.Internals.To_CORBA_Object (
   PolyORB.Any.TypeCode.TC_Alias);

   function From_Any (Item : in CORBA.Any)
      return m2.I21.new_bool;

   function To_Any
     (Item : in m2.I21.new_bool)
     return CORBA.Any;

end m2.I21.Helper;

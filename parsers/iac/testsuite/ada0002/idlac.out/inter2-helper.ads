-------------------------------------------------
--  This file has been generated automatically
--  by IDLAC (http://libre.adacore.com/polyorb/)
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

package Inter2.Helper is

   function Unchecked_To_Ref
     (The_Ref : in CORBA.Object.Ref'Class)
     return Inter2.Ref;
   function To_Ref
     (The_Ref : in CORBA.Object.Ref'Class)
     return Inter2.Ref;

   TC_Inter2 : CORBA.TypeCode.Object
     := CORBA.TypeCode.Internals.To_CORBA_Object (PolyORB.Any.TypeCode.TC_Object);

   function From_Any (Item : in CORBA.Any)
      return Inter2.Ref;

   function To_Any
     (Item : in Inter2.Ref)
     return CORBA.Any;

   TC_New_Float : CORBA.TypeCode.Object := 
   CORBA.TypeCode.Internals.To_CORBA_Object (
   PolyORB.Any.TypeCode.TC_Alias);

   function From_Any (Item : in CORBA.Any)
      return Inter2.New_Float;

   function To_Any
     (Item : in Inter2.New_Float)
     return CORBA.Any;

end Inter2.Helper;

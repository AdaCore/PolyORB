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
with CORBA.Object;

package Inter1.Helper is

   function Unchecked_To_Ref
     (The_Ref : in CORBA.Object.Ref'Class)
     return Inter1.Ref;
   function To_Ref
     (The_Ref : in CORBA.Object.Ref'Class)
     return Inter1.Ref;

   TC_Inter1 : CORBA.TypeCode.Object
     := CORBA.TypeCode.Internals.To_CORBA_Object (PolyORB.Any.TypeCode.TC_Object);

   function From_Any (Item : in CORBA.Any)
      return Inter1.Ref;

   function To_Any
     (Item : in Inter1.Ref)
     return CORBA.Any;

end Inter1.Helper;

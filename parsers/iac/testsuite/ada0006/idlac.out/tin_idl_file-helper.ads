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

package tin_IDL_File.Helper is

   TC_New_Float : CORBA.TypeCode.Object := 
   CORBA.TypeCode.Internals.To_CORBA_Object (
   PolyORB.Any.TypeCode.TC_Alias);

   function From_Any (Item : in CORBA.Any)
      return tin_IDL_File.New_Float;

   function To_Any
     (Item : in tin_IDL_File.New_Float)
     return CORBA.Any;

   TC_New_Boolean : CORBA.TypeCode.Object := 
   CORBA.TypeCode.Internals.To_CORBA_Object (
   PolyORB.Any.TypeCode.TC_Alias);

   function From_Any (Item : in CORBA.Any)
      return tin_IDL_File.New_Boolean;

   function To_Any
     (Item : in tin_IDL_File.New_Boolean)
     return CORBA.Any;

end tin_IDL_File.Helper;

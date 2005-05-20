-------------------------------------------------
--  This file has been generated automatically
--  by IDLAC (http://libre.adacore.com/polyorb/)
--
--  Do NOT hand-modify this file, as your
--  changes will be lost when you re-run the
--  IDL to Ada compiler.
-------------------------------------------------
pragma Style_Checks (Off);

with CORBA;
pragma Elaborate_All (CORBA);
with m1.i1;
with CORBA.Object;

package m1.i3 is

   type Ref is new CORBA.Object.Ref with null record;

   type new_float is
     new m1.i1.t1;

   new_float_Repository_Id : constant Standard.String
     := "IDL:m1/i3/new_float:1.0";

   Repository_Id : constant Standard.String
     := "IDL:m1/i3:1.0";

   function Is_A
     (Self : Ref;
      Logical_Type_Id : Standard.String)
     return CORBA.Boolean;

private

   function Is_A
     (Logical_Type_Id : Standard.String)
     return CORBA.Boolean;

end m1.i3;

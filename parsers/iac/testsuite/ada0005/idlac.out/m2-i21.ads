-------------------------------------------------
--  This file has been generated automatically
--  by IDLAC (http://libre.act-europe.fr/polyorb/)
--
--  Do NOT hand-modify this file, as your
--  changes will be lost when you re-run the
--  IDL to Ada compiler.
-------------------------------------------------
pragma Style_Checks (Off);

with CORBA;
pragma Elaborate_All (CORBA);
with CORBA.Object;

package m2.I21 is

   type Ref is new CORBA.Object.Ref with null record;

   type new_bool is
     new CORBA.Boolean;

   new_bool_Repository_Id : constant Standard.String
     := "IDL:m2/I21/new_bool:1.0";

   function is_greater
     (Self : in Ref;
      f1 : in CORBA.Float;
      f2 : in CORBA.Float)
     return m2.I21.new_bool;

   is_greater_Repository_Id : constant Standard.String
     := "IDL:m2/I21/is_greater:1.0";

   Repository_Id : constant Standard.String
     := "IDL:m2/I21:1.0";

   function Is_A
     (Self : Ref;
      Logical_Type_Id : Standard.String)
     return CORBA.Boolean;

private

   function Is_A
     (Logical_Type_Id : Standard.String)
     return CORBA.Boolean;

end m2.I21;

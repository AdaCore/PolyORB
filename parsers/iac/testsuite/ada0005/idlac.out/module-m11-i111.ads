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

package module.m11.I111 is

   type Ref is new CORBA.Object.Ref with null record;

   value_Repository_Id : constant Standard.String
     := "IDL:module/m11/I111/value:1.0";

   function get_value
     (Self : in Ref)
     return CORBA.Float;

   procedure set_value
     (Self : in Ref;
      To : in CORBA.Float);

   Repository_Id : constant Standard.String
     := "IDL:module/m11/I111:1.0";

   function Is_A
     (Self : Ref;
      Logical_Type_Id : Standard.String)
     return CORBA.Boolean;

private

   function Is_A
     (Logical_Type_Id : Standard.String)
     return CORBA.Boolean;

end module.m11.I111;

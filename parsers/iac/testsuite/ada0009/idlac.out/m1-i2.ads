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
with m1.i1;
with CORBA.Object;

package m1.i2 is

   type Ref is new CORBA.Object.Ref with null record;

   attr1_Repository_Id : constant Standard.String
     := "IDL:m1/i2/attr1:1.0";

   function get_attr1
     (Self : in Ref)
     return m1.i1.t1;

   procedure set_attr1
     (Self : in Ref;
      To : in m1.i1.t1);

   bool_Repository_Id : constant Standard.String
     := "IDL:m1/i2/bool:1.0";

   function get_bool
     (Self : in Ref)
     return m1.b1;

   procedure set_bool
     (Self : in Ref;
      To : in m1.b1);

   Repository_Id : constant Standard.String
     := "IDL:m1/i2:1.0";

   function Is_A
     (Self : Ref;
      Logical_Type_Id : Standard.String)
     return CORBA.Boolean;

private

   function Is_A
     (Logical_Type_Id : Standard.String)
     return CORBA.Boolean;

end m1.i2;

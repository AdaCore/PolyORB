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
with tin_IDL_File;
with CORBA.Object;

package m1.int1 is

   type Ref is new CORBA.Object.Ref with null record;

   attr1_Repository_Id : constant Standard.String
     := "IDL:m1/int1/attr1:1.0";

   function get_attr1
     (Self : in Ref)
     return tin_IDL_File.New_Float;

   procedure set_attr1
     (Self : in Ref;
      To : in tin_IDL_File.New_Float);

   bool1_Repository_Id : constant Standard.String
     := "IDL:m1/int1/bool1:1.0";

   function get_bool1
     (Self : in Ref)
     return CORBA.Boolean;

   Repository_Id : constant Standard.String
     := "IDL:m1/int1:1.0";

   function Is_A
     (Self : Ref;
      Logical_Type_Id : Standard.String)
     return CORBA.Boolean;

private

   function Is_A
     (Logical_Type_Id : Standard.String)
     return CORBA.Boolean;

end m1.int1;

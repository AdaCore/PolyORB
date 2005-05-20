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
with CORBA.Object;

package Inter2 is

   type Ref is new CORBA.Object.Ref with null record;

   type New_Float is
     new CORBA.Float;

   New_Float_Repository_Id : constant Standard.String
     := "IDL:Inter2/New_Float:1.0";

   attr1_Repository_Id : constant Standard.String
     := "IDL:Inter2/attr1:1.0";

   function get_attr1
     (Self : in Ref)
     return Inter2.New_Float;

   procedure set_attr1
     (Self : in Ref;
      To : in Inter2.New_Float);

   function ConvertNew
     (Self : in Ref;
      N : in CORBA.Float)
     return Inter2.New_Float;

   ConvertNew_Repository_Id : constant Standard.String
     := "IDL:Inter2/ConvertNew:1.0";

   Repository_Id : constant Standard.String
     := "IDL:Inter2:1.0";

   function Is_A
     (Self : Ref;
      Logical_Type_Id : Standard.String)
     return CORBA.Boolean;

private

   function Is_A
     (Logical_Type_Id : Standard.String)
     return CORBA.Boolean;

end Inter2;

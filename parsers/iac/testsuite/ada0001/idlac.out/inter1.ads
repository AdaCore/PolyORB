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

package Inter1 is

   type Ref is new CORBA.Object.Ref with null record;

   Attr1_Repository_Id : constant Standard.String
     := "IDL:Inter1/Attr1:1.0";

   function get_Attr1
     (Self : in Ref)
     return CORBA.Float;

   procedure set_Attr1
     (Self : in Ref;
      To : in CORBA.Float);

   Attr2_Repository_Id : constant Standard.String
     := "IDL:Inter1/Attr2:1.0";

   function get_Attr2
     (Self : in Ref)
     return CORBA.Boolean;

   procedure set_Attr2
     (Self : in Ref;
      To : in CORBA.Boolean);

   Attr3_Repository_Id : constant Standard.String
     := "IDL:Inter1/Attr3:1.0";

   function get_Attr3
     (Self : in Ref)
     return CORBA.Long;

   Attr4_Repository_Id : constant Standard.String
     := "IDL:Inter1/Attr4:1.0";

   function get_Attr4
     (Self : in Ref)
     return CORBA.Long_Long;

   procedure set_Attr4
     (Self : in Ref;
      To : in CORBA.Long_Long);

   function Name
     (Self : in Ref;
      code : in CORBA.Short)
     return CORBA.String;

   Name_Repository_Id : constant Standard.String
     := "IDL:Inter1/Name:1.0";

   procedure SName
     (Self : in Ref;
      code : in CORBA.Short;
      str : in CORBA.String);

   SName_Repository_Id : constant Standard.String
     := "IDL:Inter1/SName:1.0";

   Repository_Id : constant Standard.String
     := "IDL:Inter1:1.0";

   function Is_A
     (Self : Ref;
      Logical_Type_Id : Standard.String)
     return CORBA.Boolean;

private

   function Is_A
     (Logical_Type_Id : Standard.String)
     return CORBA.Boolean;

end Inter1;

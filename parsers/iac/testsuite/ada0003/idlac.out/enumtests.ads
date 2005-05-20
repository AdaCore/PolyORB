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

package EnumTests is

   type Ref is new CORBA.Object.Ref with null record;

   type Color is
     (Red,
      Blue,
      Green);

   Color_Repository_Id : constant Standard.String
     := "IDL:EnumTests/Color:1.0";

   use type EnumTests.Color;
   Rouge : constant EnumTests.Color
     := EnumTests.Red;

   attr_enum_Repository_Id : constant Standard.String
     := "IDL:EnumTests/attr_enum:1.0";

   function get_attr_enum
     (Self : in Ref)
     return EnumTests.Color;

   procedure set_attr_enum
     (Self : in Ref;
      To : in EnumTests.Color);

   procedure modif_enum
     (Self : in Ref;
      C : in out EnumTests.Color;
      Returns : out EnumTests.Color);

   modif_enum_Repository_Id : constant Standard.String
     := "IDL:EnumTests/modif_enum:1.0";

   Repository_Id : constant Standard.String
     := "IDL:EnumTests:1.0";

   function Is_A
     (Self : Ref;
      Logical_Type_Id : Standard.String)
     return CORBA.Boolean;

private

   function Is_A
     (Logical_Type_Id : Standard.String)
     return CORBA.Boolean;

end EnumTests;

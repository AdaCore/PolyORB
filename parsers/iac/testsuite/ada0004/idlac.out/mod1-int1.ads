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

package mod1.Int1 is

   type Ref is new CORBA.Object.Ref with null record;

   type New_Float is
     new CORBA.Float;

   New_Float_Repository_Id : constant Standard.String
     := "IDL:mod1/Int1/New_Float:1.0";

   type Color is
     (Red,
      Blue,
      Green);

   Color_Repository_Id : constant Standard.String
     := "IDL:mod1/Int1/Color:1.0";

   Real_Number_Repository_Id : constant Standard.String
     := "IDL:mod1/Int1/Real_Number:1.0";

   function get_Real_Number
     (Self : in Ref)
     return mod1.Int1.New_Float;

   procedure set_Real_Number
     (Self : in Ref;
      To : in mod1.Int1.New_Float);

   couleur_Repository_Id : constant Standard.String
     := "IDL:mod1/Int1/couleur:1.0";

   function get_couleur
     (Self : in Ref)
     return mod1.Int1.Color;

   procedure set_couleur
     (Self : in Ref;
      To : in mod1.Int1.Color);

   b1_Repository_Id : constant Standard.String
     := "IDL:mod1/Int1/b1:1.0";

   function get_b1
     (Self : in Ref)
     return mod1.bool;

   procedure set_b1
     (Self : in Ref;
      To : in mod1.bool);

   Repository_Id : constant Standard.String
     := "IDL:mod1/Int1:1.0";

   function Is_A
     (Self : Ref;
      Logical_Type_Id : Standard.String)
     return CORBA.Boolean;

private

   function Is_A
     (Logical_Type_Id : Standard.String)
     return CORBA.Boolean;

end mod1.Int1;

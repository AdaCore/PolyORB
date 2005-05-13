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

package i1 is

   type Ref is new CORBA.Object.Ref with null record;

   type New_Float is
     new CORBA.Float;

   New_Float_Repository_Id : constant Standard.String
     := "IDL:i1/New_Float:1.0";

   type Tab_Float is
     array (0 .. 10 - 1) of
      CORBA.Float;

   Tab_Float_Repository_Id : constant Standard.String
     := "IDL:i1/Tab_Float:1.0";

   val1_Repository_Id : constant Standard.String
     := "IDL:i1/val1:1.0";

   val2_Repository_Id : constant Standard.String
     := "IDL:i1/val2:1.0";

   tab_val_Repository_Id : constant Standard.String
     := "IDL:i1/tab_val:1.0";

   function get_val1
     (Self : in Ref)
     return CORBA.Float;

   procedure set_val1
     (Self : in Ref;
      To : in CORBA.Float);

   function get_val2
     (Self : in Ref)
     return CORBA.Float;

   procedure set_val2
     (Self : in Ref;
      To : in CORBA.Float);

   function get_tab_val
     (Self : in Ref)
     return CORBA.Float;

   procedure set_tab_val
     (Self : in Ref;
      To : in CORBA.Float);

   tab_Repository_Id : constant Standard.String
     := "IDL:i1/tab:1.0";

   function get_tab
     (Self : in Ref)
     return i1.Tab_Float;

   procedure set_tab
     (Self : in Ref;
      To : in i1.Tab_Float);

   Repository_Id : constant Standard.String
     := "IDL:i1:1.0";

   function Is_A
     (Self : Ref;
      Logical_Type_Id : Standard.String)
     return CORBA.Boolean;

private

   function Is_A
     (Logical_Type_Id : Standard.String)
     return CORBA.Boolean;

end i1;

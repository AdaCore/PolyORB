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

   type new_string is
     new CORBA.String;

   new_string_Repository_Id : constant Standard.String
     := "IDL:i1/new_string:1.0";

   type New_Float is
     new CORBA.Float;

   New_Float_Repository_Id : constant Standard.String
     := "IDL:i1/New_Float:1.0";

   type n2 is
     new i1.new_string;

   n2_Repository_Id : constant Standard.String
     := "IDL:i1/n2:1.0";

   type tab is
     array (0 .. 4 - 1, 0 .. 2 - 1) of
      i1.new_string;

   tab_Repository_Id : constant Standard.String
     := "IDL:i1/tab:1.0";

   type f33 is
     array (0 .. 120 - 1) of
      CORBA.Float;

   f33_Repository_Id : constant Standard.String
     := "IDL:i1/f33:1.0";

   type f45 is
     array (0 .. 4 - 1, 0 .. 5 - 1, 0 .. 6 - 1) of
      CORBA.Float;

   f45_Repository_Id : constant Standard.String
     := "IDL:i1/f45:1.0";

   str_Repository_Id : constant Standard.String
     := "IDL:i1/str:1.0";

   function get_str
     (Self : in Ref)
     return CORBA.String;

   procedure set_str
     (Self : in Ref;
      To : in CORBA.String);

   S_Repository_Id : constant Standard.String
     := "IDL:i1/S:1.0";

   function get_S
     (Self : in Ref)
     return i1.new_string;

   procedure set_S
     (Self : in Ref;
      To : in i1.new_string);

   procedure min
     (Self : in Ref;
      f1 : in i1.New_Float);

   min_Repository_Id : constant Standard.String
     := "IDL:i1/min:1.0";

   procedure Add
     (Self : in Ref;
      f1 : in out i1.New_Float;
      f2 : in CORBA.Float;
      Returns : out CORBA.Float);

   Add_Repository_Id : constant Standard.String
     := "IDL:i1/Add:1.0";

   procedure minus
     (Self : in Ref;
      f1 : in CORBA.Float;
      f2 : in CORBA.Float;
      r : out CORBA.Float;
      Returns : out i1.New_Float);

   minus_Repository_Id : constant Standard.String
     := "IDL:i1/minus:1.0";

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

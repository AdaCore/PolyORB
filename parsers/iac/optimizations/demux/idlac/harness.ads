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

package Harness is

   type Ref is new CORBA.Object.Ref with null record;

   function echoULong1
     (Self : in Ref;
      arg : in CORBA.Unsigned_Long)
     return CORBA.Unsigned_Long;

   echoULong1_Repository_Id : constant Standard.String
     := "IDL:Harness/echoULong1:1.0";

   function echoULong2
     (Self : in Ref;
      arg : in CORBA.Unsigned_Long)
     return CORBA.Unsigned_Long;

   echoULong2_Repository_Id : constant Standard.String
     := "IDL:Harness/echoULong2:1.0";

   function echoULong3
     (Self : in Ref;
      arg : in CORBA.Unsigned_Long)
     return CORBA.Unsigned_Long;

   echoULong3_Repository_Id : constant Standard.String
     := "IDL:Harness/echoULong3:1.0";

   function echoULong4
     (Self : in Ref;
      arg : in CORBA.Unsigned_Long)
     return CORBA.Unsigned_Long;

   echoULong4_Repository_Id : constant Standard.String
     := "IDL:Harness/echoULong4:1.0";

   function echoULong5
     (Self : in Ref;
      arg : in CORBA.Unsigned_Long)
     return CORBA.Unsigned_Long;

   echoULong5_Repository_Id : constant Standard.String
     := "IDL:Harness/echoULong5:1.0";

   function echoULong6
     (Self : in Ref;
      arg : in CORBA.Unsigned_Long)
     return CORBA.Unsigned_Long;

   echoULong6_Repository_Id : constant Standard.String
     := "IDL:Harness/echoULong6:1.0";

   function echoULong7
     (Self : in Ref;
      arg : in CORBA.Unsigned_Long)
     return CORBA.Unsigned_Long;

   echoULong7_Repository_Id : constant Standard.String
     := "IDL:Harness/echoULong7:1.0";

   function echoULong8
     (Self : in Ref;
      arg : in CORBA.Unsigned_Long)
     return CORBA.Unsigned_Long;

   echoULong8_Repository_Id : constant Standard.String
     := "IDL:Harness/echoULong8:1.0";

   function echoULong9
     (Self : in Ref;
      arg : in CORBA.Unsigned_Long)
     return CORBA.Unsigned_Long;

   echoULong9_Repository_Id : constant Standard.String
     := "IDL:Harness/echoULong9:1.0";

   function echoULong10
     (Self : in Ref;
      arg : in CORBA.Unsigned_Long)
     return CORBA.Unsigned_Long;

   echoULong10_Repository_Id : constant Standard.String
     := "IDL:Harness/echoULong10:1.0";

   function echoULong11
     (Self : in Ref;
      arg : in CORBA.Unsigned_Long)
     return CORBA.Unsigned_Long;

   echoULong11_Repository_Id : constant Standard.String
     := "IDL:Harness/echoULong11:1.0";

   function echoULong12
     (Self : in Ref;
      arg : in CORBA.Unsigned_Long)
     return CORBA.Unsigned_Long;

   echoULong12_Repository_Id : constant Standard.String
     := "IDL:Harness/echoULong12:1.0";

   function echoULong13
     (Self : in Ref;
      arg : in CORBA.Unsigned_Long)
     return CORBA.Unsigned_Long;

   echoULong13_Repository_Id : constant Standard.String
     := "IDL:Harness/echoULong13:1.0";

   function echoULong14
     (Self : in Ref;
      arg : in CORBA.Unsigned_Long)
     return CORBA.Unsigned_Long;

   echoULong14_Repository_Id : constant Standard.String
     := "IDL:Harness/echoULong14:1.0";

   function echoULong15
     (Self : in Ref;
      arg : in CORBA.Unsigned_Long)
     return CORBA.Unsigned_Long;

   echoULong15_Repository_Id : constant Standard.String
     := "IDL:Harness/echoULong15:1.0";

   function echoULong16
     (Self : in Ref;
      arg : in CORBA.Unsigned_Long)
     return CORBA.Unsigned_Long;

   echoULong16_Repository_Id : constant Standard.String
     := "IDL:Harness/echoULong16:1.0";

   function echoULong17
     (Self : in Ref;
      arg : in CORBA.Unsigned_Long)
     return CORBA.Unsigned_Long;

   echoULong17_Repository_Id : constant Standard.String
     := "IDL:Harness/echoULong17:1.0";

   function echoULong18
     (Self : in Ref;
      arg : in CORBA.Unsigned_Long)
     return CORBA.Unsigned_Long;

   echoULong18_Repository_Id : constant Standard.String
     := "IDL:Harness/echoULong18:1.0";

   function echoULong19
     (Self : in Ref;
      arg : in CORBA.Unsigned_Long)
     return CORBA.Unsigned_Long;

   echoULong19_Repository_Id : constant Standard.String
     := "IDL:Harness/echoULong19:1.0";

   function echoULong20
     (Self : in Ref;
      arg : in CORBA.Unsigned_Long)
     return CORBA.Unsigned_Long;

   echoULong20_Repository_Id : constant Standard.String
     := "IDL:Harness/echoULong20:1.0";

   Repository_Id : constant Standard.String
     := "IDL:Harness:1.0";

   function Is_A
     (Self : Ref;
      Logical_Type_Id : Standard.String)
     return CORBA.Boolean;

private

   function Is_A
     (Logical_Type_Id : Standard.String)
     return CORBA.Boolean;

end Harness;

-------------------------------------------------
--  This file has been generated automatically
--  by IDLAC version 2.3.0w.
--
--  Do NOT hand-modify this file, as your
--  changes will be lost when you re-run the
--  IDL to Ada compiler.
-------------------------------------------------
pragma Style_Checks ("NM32766");

with PolyORB.Std;
with CORBA.Object;

package RTCORBA.ProtocolProperties is

   type Local_Ref is new CORBA.Object.Ref with null record;

   Repository_Id : constant PolyORB.Std.String
     := "IDL:omg.org/RTCORBA/ProtocolProperties:1.0";

   package Convert_Forward is
     new RTCORBA.ProtocolProperties_Forward.Convert (Local_Ref);
end RTCORBA.ProtocolProperties;

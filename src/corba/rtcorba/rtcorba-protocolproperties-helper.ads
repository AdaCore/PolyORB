-------------------------------------------------
--  This file has been generated automatically
--  by IDLAC version 2.3.0w.
--
--  Do NOT hand-modify this file, as your
--  changes will be lost when you re-run the
--  IDL to Ada compiler.
-------------------------------------------------
pragma Style_Checks ("NM32766");

with CORBA;
pragma Elaborate_All (CORBA);
with CORBA.Object;

package RTCORBA.ProtocolProperties.Helper is

   function Unchecked_To_Local_Ref
     (The_Ref : CORBA.Object.Ref'Class) return RTCORBA.ProtocolProperties.Local_Ref;

   function To_Local_Ref
     (The_Ref : CORBA.Object.Ref'Class) return RTCORBA.ProtocolProperties.Local_Ref;

   TC_ProtocolProperties : CORBA.TypeCode.Object;

end RTCORBA.ProtocolProperties.Helper;

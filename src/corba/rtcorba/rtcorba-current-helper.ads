-------------------------------------------------
--  This file has been generated automatically
--  by IDLAC (http://libre.act-europe.fr/polyorb/)
--
--  Do NOT hand-modify this file, as your
--  changes will be lost when you re-run the
--  IDL to Ada compiler.
-------------------------------------------------
pragma Style_Checks (Off);

with CORBA.Object;

package RTCORBA.Current.Helper is

   pragma Elaborate_Body;

   function Unchecked_To_Local_Ref
     (The_Ref : in CORBA.Object.Ref'Class)
     return RTCORBA.Current.Local_Ref;
   function To_Local_Ref
     (The_Ref : in CORBA.Object.Ref'Class)
     return RTCORBA.Current.Local_Ref;

end RTCORBA.Current.Helper;

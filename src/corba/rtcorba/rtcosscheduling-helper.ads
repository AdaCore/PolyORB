pragma Style_Checks ("NM32766");
pragma Wide_Character_Encoding (Brackets);
pragma Warnings (Off, "use of an anonymous access type allocator");

---------------------------------------------------
--  This file has been generated automatically from
--  /Users/heathdorn/Documents/Playground/Agents/RefactorTeam/code_refactor/PolyORB/idls/RTCORBA/RTCosScheduling.idl
--  by IAC (IDL to Ada Compiler) 20.0w (rev. 41a9b833).
---------------------------------------------------
--  NOTE: If you modify this file by hand, your
--  changes will be lost when you re-run the
--  IDL to Ada compiler.
---------------------------------------------------

with CORBA;
pragma Elaborate_All (CORBA);

package RTCosScheduling.Helper is

   TC_UnknownName : CORBA.TypeCode.Object;

   function From_Any
     (Item : CORBA.Any)
     return RTCosScheduling.UnknownName_Members;

   function To_Any
     (Item : RTCosScheduling.UnknownName_Members)
     return CORBA.Any;

   procedure Raise_UnknownName
     (Members : RTCosScheduling.UnknownName_Members);

   pragma No_Return (Raise_UnknownName);

   
   package Internals is

      procedure Initialize_UnknownName;

   end Internals;

end RTCosScheduling.Helper;

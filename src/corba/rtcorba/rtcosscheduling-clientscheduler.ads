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

with CORBA.Object;
with PolyORB.Std;
with CORBA;
pragma Elaborate_All (CORBA);

package RTCosScheduling.ClientScheduler is

   type Local_Ref is
     new CORBA.Object.Ref with null record;

   Repository_Id : constant PolyORB.Std.String :=
     "IDL:RTCosScheduling/ClientScheduler:1.0";

   procedure schedule_activity
     (Self : Local_Ref;
      activity_name : CORBA.String);

   schedule_activity_Repository_Id : constant PolyORB.Std.String :=
     "IDL:RTCosScheduling/ClientScheduler/schedule_activity:1.0";

end RTCosScheduling.ClientScheduler;

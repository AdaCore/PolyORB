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
with PortableServer;
with PortableServer.POA;
with CORBA;
pragma Elaborate_All (CORBA);
with PortableServer.POAManager;
with CORBA.Policy;

package RTCosScheduling.ServerScheduler is

   type Local_Ref is
     new CORBA.Object.Ref with null record;

   Repository_Id : constant PolyORB.Std.String :=
     "IDL:RTCosScheduling/ServerScheduler:1.0";

   function create_POA
     (Self : Local_Ref;
      parent : PortableServer.POA.Local_Ref;
      adapter_name : CORBA.String;
      a_POAManager : PortableServer.POAManager.Local_Ref;
      policies : CORBA.Policy.PolicyList)
     return PortableServer.POA.Local_Ref;

   create_POA_Repository_Id : constant PolyORB.Std.String :=
     "IDL:RTCosScheduling/ServerScheduler/create_POA:1.0";

   procedure schedule_object
     (Self : Local_Ref;
      obj : CORBA.Object.Ref;
      name : CORBA.String);

   schedule_object_Repository_Id : constant PolyORB.Std.String :=
     "IDL:RTCosScheduling/ServerScheduler/schedule_object:1.0";

end RTCosScheduling.ServerScheduler;

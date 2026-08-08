pragma Style_Checks ("NM32766");
pragma Warnings (Off, "use of an anonymous access type allocator");
pragma Warnings (Off, "unnecessary with of ancestor");

---------------------------------------------------
--  This file has been generated automatically from
--  /Users/heathdorn/Documents/Playground/Agents/RefactorTeam/code_refactor/PolyORB/idls/RTCORBA/RTCosScheduling.idl
--  by IAC (IDL to Ada Compiler) 20.0w (rev. 41a9b833).
---------------------------------------------------
--  NOTE: If you modify this file by hand, your
--  changes will be lost when you re-run the
--  IDL to Ada compiler.
---------------------------------------------------

with RTCosScheduling.ClientScheduler.Impl;

package body RTCosScheduling.ClientScheduler is

   -----------------------
   -- schedule_activity --
   -----------------------

   procedure schedule_activity
     (Self : Local_Ref;
      activity_name : CORBA.String)
   is
   begin
      if CORBA.Object.Is_Nil
        (CORBA.Object.Ref
           (Self))
      then
         CORBA.Raise_Inv_Objref
           (CORBA.Default_Sys_Member);
      end if;
      RTCosScheduling.ClientScheduler.Impl.schedule_activity
        (RTCosScheduling.ClientScheduler.Impl.Object_Ptr
           (Entity_Of
              (Self)),
         activity_name);
   end schedule_activity;

end RTCosScheduling.ClientScheduler;

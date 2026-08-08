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

with RTCosScheduling.ServerScheduler.Impl;

package body RTCosScheduling.ServerScheduler is

   ----------------
   -- create_POA --
   ----------------

   function create_POA
     (Self : Local_Ref;
      parent : PortableServer.POA.Local_Ref;
      adapter_name : CORBA.String;
      a_POAManager : PortableServer.POAManager.Local_Ref;
      policies : CORBA.Policy.PolicyList)
     return PortableServer.POA.Local_Ref
   is
   begin
      if CORBA.Object.Is_Nil
        (CORBA.Object.Ref
           (Self))
      then
         CORBA.Raise_Inv_Objref
           (CORBA.Default_Sys_Member);
      end if;
      return RTCosScheduling.ServerScheduler.Impl.create_POA
        (RTCosScheduling.ServerScheduler.Impl.Object_Ptr
           (Entity_Of
              (Self)),
         parent,
         adapter_name,
         a_POAManager,
         policies);
   end create_POA;

   ---------------------
   -- schedule_object --
   ---------------------

   procedure schedule_object
     (Self : Local_Ref;
      obj : CORBA.Object.Ref;
      name : CORBA.String)
   is
   begin
      if CORBA.Object.Is_Nil
        (CORBA.Object.Ref
           (Self))
      then
         CORBA.Raise_Inv_Objref
           (CORBA.Default_Sys_Member);
      end if;
      RTCosScheduling.ServerScheduler.Impl.schedule_object
        (RTCosScheduling.ServerScheduler.Impl.Object_Ptr
           (Entity_Of
              (Self)),
         obj,
         name);
   end schedule_object;

end RTCosScheduling.ServerScheduler;

pragma Style_Checks ("NM32766");
pragma Warnings (Off, "use of an anonymous access type allocator");
pragma Warnings (Off, "unnecessary with of ancestor");

---------------------------------------------------
--  This file has been generated automatically from
--  /Users/heathdorn/Documents/Playground/Agents/RefactorTeam/code_refactor/PolyORB/idls/Misc/PortableInterceptor.idl
--  by IAC (IDL to Ada Compiler) 20.0w (rev. 41a9b833).
---------------------------------------------------
--  NOTE: If you modify this file by hand, your
--  changes will be lost when you re-run the
--  IDL to Ada compiler.
---------------------------------------------------

with PortableInterceptor.ClientRequestInfo.Impl;

package body PortableInterceptor.ClientRequestInfo is

   ----------------
   -- get_target --
   ----------------

   function get_target
     (Self : Local_Ref)
     return CORBA.Object.Ref
   is
   begin
      if CORBA.Object.Is_Nil
        (CORBA.Object.Ref
           (Self))
      then
         CORBA.Raise_Inv_Objref
           (CORBA.Default_Sys_Member);
      end if;
      return PortableInterceptor.ClientRequestInfo.Impl.get_target
        (PortableInterceptor.ClientRequestInfo.Impl.Object_Ptr
           (Entity_Of
              (Self)));
   end get_target;

   --------------------------
   -- get_effective_target --
   --------------------------

   function get_effective_target
     (Self : Local_Ref)
     return CORBA.Object.Ref
   is
   begin
      if CORBA.Object.Is_Nil
        (CORBA.Object.Ref
           (Self))
      then
         CORBA.Raise_Inv_Objref
           (CORBA.Default_Sys_Member);
      end if;
      return PortableInterceptor.ClientRequestInfo.Impl.get_effective_target
        (PortableInterceptor.ClientRequestInfo.Impl.Object_Ptr
           (Entity_Of
              (Self)));
   end get_effective_target;

   ---------------------------
   -- get_effective_profile --
   ---------------------------

   function get_effective_profile
     (Self : Local_Ref)
     return IOP.TaggedProfile
   is
   begin
      if CORBA.Object.Is_Nil
        (CORBA.Object.Ref
           (Self))
      then
         CORBA.Raise_Inv_Objref
           (CORBA.Default_Sys_Member);
      end if;
      return PortableInterceptor.ClientRequestInfo.Impl.get_effective_profile
        (PortableInterceptor.ClientRequestInfo.Impl.Object_Ptr
           (Entity_Of
              (Self)));
   end get_effective_profile;

   ----------------------------
   -- get_received_exception --
   ----------------------------

   function get_received_exception
     (Self : Local_Ref)
     return CORBA.Any
   is
   begin
      if CORBA.Object.Is_Nil
        (CORBA.Object.Ref
           (Self))
      then
         CORBA.Raise_Inv_Objref
           (CORBA.Default_Sys_Member);
      end if;
      return PortableInterceptor.ClientRequestInfo.Impl.get_received_exception
        (PortableInterceptor.ClientRequestInfo.Impl.Object_Ptr
           (Entity_Of
              (Self)));
   end get_received_exception;

   -------------------------------
   -- get_received_exception_id --
   -------------------------------

   function get_received_exception_id
     (Self : Local_Ref)
     return CORBA.RepositoryId
   is
   begin
      if CORBA.Object.Is_Nil
        (CORBA.Object.Ref
           (Self))
      then
         CORBA.Raise_Inv_Objref
           (CORBA.Default_Sys_Member);
      end if;
      return PortableInterceptor.ClientRequestInfo.Impl.get_received_exception_id
        (PortableInterceptor.ClientRequestInfo.Impl.Object_Ptr
           (Entity_Of
              (Self)));
   end get_received_exception_id;

   -----------------------------
   -- get_effective_component --
   -----------------------------

   function get_effective_component
     (Self : Local_Ref;
      id : IOP.ComponentId)
     return IOP.TaggedComponent
   is
   begin
      if CORBA.Object.Is_Nil
        (CORBA.Object.Ref
           (Self))
      then
         CORBA.Raise_Inv_Objref
           (CORBA.Default_Sys_Member);
      end if;
      return PortableInterceptor.ClientRequestInfo.Impl.get_effective_component
        (PortableInterceptor.ClientRequestInfo.Impl.Object_Ptr
           (Entity_Of
              (Self)),
         id);
   end get_effective_component;

   ------------------------------
   -- get_effective_components --
   ------------------------------

   function get_effective_components
     (Self : Local_Ref;
      id : IOP.ComponentId)
     return IOP.TaggedComponentSeq
   is
   begin
      if CORBA.Object.Is_Nil
        (CORBA.Object.Ref
           (Self))
      then
         CORBA.Raise_Inv_Objref
           (CORBA.Default_Sys_Member);
      end if;
      return PortableInterceptor.ClientRequestInfo.Impl.get_effective_components
        (PortableInterceptor.ClientRequestInfo.Impl.Object_Ptr
           (Entity_Of
              (Self)),
         id);
   end get_effective_components;

   ------------------------
   -- get_request_policy --
   ------------------------

   function get_request_policy
     (Self : Local_Ref;
      IDL_type : CORBA.PolicyType)
     return CORBA.Policy.Ref
   is
   begin
      if CORBA.Object.Is_Nil
        (CORBA.Object.Ref
           (Self))
      then
         CORBA.Raise_Inv_Objref
           (CORBA.Default_Sys_Member);
      end if;
      return PortableInterceptor.ClientRequestInfo.Impl.get_request_policy
        (PortableInterceptor.ClientRequestInfo.Impl.Object_Ptr
           (Entity_Of
              (Self)),
         IDL_type);
   end get_request_policy;

   ---------------------------------
   -- add_request_service_context --
   ---------------------------------

   procedure add_request_service_context
     (Self : Local_Ref;
      service_context : IOP.ServiceContext;
      replace : CORBA.Boolean)
   is
   begin
      if CORBA.Object.Is_Nil
        (CORBA.Object.Ref
           (Self))
      then
         CORBA.Raise_Inv_Objref
           (CORBA.Default_Sys_Member);
      end if;
      PortableInterceptor.ClientRequestInfo.Impl.add_request_service_context
        (PortableInterceptor.ClientRequestInfo.Impl.Object_Ptr
           (Entity_Of
              (Self)),
         service_context,
         replace);
   end add_request_service_context;

end PortableInterceptor.ClientRequestInfo;

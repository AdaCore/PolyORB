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

with CORBA.Object;
with PortableInterceptor.ServerRequestInfo.Impl;

package body PortableInterceptor.ServerRequestInfo is

   ---------------------------
   -- get_sending_exception --
   ---------------------------

   function get_sending_exception
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
      return PortableInterceptor.ServerRequestInfo.Impl.get_sending_exception
        (PortableInterceptor.ServerRequestInfo.Impl.Object_Ptr
           (Entity_Of
              (Self)));
   end get_sending_exception;

   -------------------
   -- get_server_id --
   -------------------

   function get_server_id
     (Self : Local_Ref)
     return PortableInterceptor.ServerId
   is
   begin
      if CORBA.Object.Is_Nil
        (CORBA.Object.Ref
           (Self))
      then
         CORBA.Raise_Inv_Objref
           (CORBA.Default_Sys_Member);
      end if;
      return PortableInterceptor.ServerRequestInfo.Impl.get_server_id
        (PortableInterceptor.ServerRequestInfo.Impl.Object_Ptr
           (Entity_Of
              (Self)));
   end get_server_id;

   ----------------
   -- get_orb_id --
   ----------------

   function get_orb_id
     (Self : Local_Ref)
     return PortableInterceptor.ORBId
   is
   begin
      if CORBA.Object.Is_Nil
        (CORBA.Object.Ref
           (Self))
      then
         CORBA.Raise_Inv_Objref
           (CORBA.Default_Sys_Member);
      end if;
      return PortableInterceptor.ServerRequestInfo.Impl.get_orb_id
        (PortableInterceptor.ServerRequestInfo.Impl.Object_Ptr
           (Entity_Of
              (Self)));
   end get_orb_id;

   ----------------------
   -- get_adapter_name --
   ----------------------

   function get_adapter_name
     (Self : Local_Ref)
     return PortableInterceptor.AdapterName
   is
   begin
      if CORBA.Object.Is_Nil
        (CORBA.Object.Ref
           (Self))
      then
         CORBA.Raise_Inv_Objref
           (CORBA.Default_Sys_Member);
      end if;
      return PortableInterceptor.ServerRequestInfo.Impl.get_adapter_name
        (PortableInterceptor.ServerRequestInfo.Impl.Object_Ptr
           (Entity_Of
              (Self)));
   end get_adapter_name;

   -------------------
   -- get_object_id --
   -------------------

   function get_object_id
     (Self : Local_Ref)
     return PortableInterceptor.ObjectId
   is
   begin
      if CORBA.Object.Is_Nil
        (CORBA.Object.Ref
           (Self))
      then
         CORBA.Raise_Inv_Objref
           (CORBA.Default_Sys_Member);
      end if;
      return PortableInterceptor.ServerRequestInfo.Impl.get_object_id
        (PortableInterceptor.ServerRequestInfo.Impl.Object_Ptr
           (Entity_Of
              (Self)));
   end get_object_id;

   --------------------
   -- get_adapter_id --
   --------------------

   function get_adapter_id
     (Self : Local_Ref)
     return CORBA.IDL_Sequences.OctetSeq
   is
   begin
      if CORBA.Object.Is_Nil
        (CORBA.Object.Ref
           (Self))
      then
         CORBA.Raise_Inv_Objref
           (CORBA.Default_Sys_Member);
      end if;
      return PortableInterceptor.ServerRequestInfo.Impl.get_adapter_id
        (PortableInterceptor.ServerRequestInfo.Impl.Object_Ptr
           (Entity_Of
              (Self)));
   end get_adapter_id;

   ---------------------------------------
   -- get_target_most_derived_interface --
   ---------------------------------------

   function get_target_most_derived_interface
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
      return PortableInterceptor.ServerRequestInfo.Impl.get_target_most_derived_interface
        (PortableInterceptor.ServerRequestInfo.Impl.Object_Ptr
           (Entity_Of
              (Self)));
   end get_target_most_derived_interface;

   -----------------------
   -- get_server_policy --
   -----------------------

   function get_server_policy
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
      return PortableInterceptor.ServerRequestInfo.Impl.get_server_policy
        (PortableInterceptor.ServerRequestInfo.Impl.Object_Ptr
           (Entity_Of
              (Self)),
         IDL_type);
   end get_server_policy;

   --------------
   -- set_slot --
   --------------

   procedure set_slot
     (Self : Local_Ref;
      id : PortableInterceptor.SlotId;
      data : CORBA.Any)
   is
   begin
      if CORBA.Object.Is_Nil
        (CORBA.Object.Ref
           (Self))
      then
         CORBA.Raise_Inv_Objref
           (CORBA.Default_Sys_Member);
      end if;
      PortableInterceptor.ServerRequestInfo.Impl.set_slot
        (PortableInterceptor.ServerRequestInfo.Impl.Object_Ptr
           (Entity_Of
              (Self)),
         id,
         data);
   end set_slot;

   -----------------
   -- target_is_a --
   -----------------

   function target_is_a
     (Self : Local_Ref;
      id : CORBA.RepositoryId)
     return CORBA.Boolean
   is
   begin
      if CORBA.Object.Is_Nil
        (CORBA.Object.Ref
           (Self))
      then
         CORBA.Raise_Inv_Objref
           (CORBA.Default_Sys_Member);
      end if;
      return PortableInterceptor.ServerRequestInfo.Impl.target_is_a
        (PortableInterceptor.ServerRequestInfo.Impl.Object_Ptr
           (Entity_Of
              (Self)),
         id);
   end target_is_a;

   -------------------------------
   -- add_reply_service_context --
   -------------------------------

   procedure add_reply_service_context
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
      PortableInterceptor.ServerRequestInfo.Impl.add_reply_service_context
        (PortableInterceptor.ServerRequestInfo.Impl.Object_Ptr
           (Entity_Of
              (Self)),
         service_context,
         replace);
   end add_reply_service_context;

end PortableInterceptor.ServerRequestInfo;

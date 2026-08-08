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

with PortableInterceptor.RequestInfo.Impl;

package body PortableInterceptor.RequestInfo is

   --------------------
   -- get_request_id --
   --------------------

   function get_request_id
     (Self : Local_Ref)
     return CORBA.Unsigned_Long
   is
   begin
      if CORBA.Object.Is_Nil
        (CORBA.Object.Ref
           (Self))
      then
         CORBA.Raise_Inv_Objref
           (CORBA.Default_Sys_Member);
      end if;
      return PortableInterceptor.RequestInfo.Impl.get_request_id
        (PortableInterceptor.RequestInfo.Impl.Object_Ptr
           (Entity_Of
              (Self)));
   end get_request_id;

   -------------------
   -- get_operation --
   -------------------

   function get_operation
     (Self : Local_Ref)
     return CORBA.String
   is
   begin
      if CORBA.Object.Is_Nil
        (CORBA.Object.Ref
           (Self))
      then
         CORBA.Raise_Inv_Objref
           (CORBA.Default_Sys_Member);
      end if;
      return PortableInterceptor.RequestInfo.Impl.get_operation
        (PortableInterceptor.RequestInfo.Impl.Object_Ptr
           (Entity_Of
              (Self)));
   end get_operation;

   -------------------
   -- get_arguments --
   -------------------

   function get_arguments
     (Self : Local_Ref)
     return Dynamic.ParameterList
   is
   begin
      if CORBA.Object.Is_Nil
        (CORBA.Object.Ref
           (Self))
      then
         CORBA.Raise_Inv_Objref
           (CORBA.Default_Sys_Member);
      end if;
      return PortableInterceptor.RequestInfo.Impl.get_arguments
        (PortableInterceptor.RequestInfo.Impl.Object_Ptr
           (Entity_Of
              (Self)));
   end get_arguments;

   --------------------
   -- get_exceptions --
   --------------------

   function get_exceptions
     (Self : Local_Ref)
     return Dynamic.ExceptionList
   is
   begin
      if CORBA.Object.Is_Nil
        (CORBA.Object.Ref
           (Self))
      then
         CORBA.Raise_Inv_Objref
           (CORBA.Default_Sys_Member);
      end if;
      return PortableInterceptor.RequestInfo.Impl.get_exceptions
        (PortableInterceptor.RequestInfo.Impl.Object_Ptr
           (Entity_Of
              (Self)));
   end get_exceptions;

   ------------------
   -- get_contexts --
   ------------------

   function get_contexts
     (Self : Local_Ref)
     return Dynamic.ContextList
   is
   begin
      if CORBA.Object.Is_Nil
        (CORBA.Object.Ref
           (Self))
      then
         CORBA.Raise_Inv_Objref
           (CORBA.Default_Sys_Member);
      end if;
      return PortableInterceptor.RequestInfo.Impl.get_contexts
        (PortableInterceptor.RequestInfo.Impl.Object_Ptr
           (Entity_Of
              (Self)));
   end get_contexts;

   ---------------------------
   -- get_operation_context --
   ---------------------------

   function get_operation_context
     (Self : Local_Ref)
     return Dynamic.RequestContext
   is
   begin
      if CORBA.Object.Is_Nil
        (CORBA.Object.Ref
           (Self))
      then
         CORBA.Raise_Inv_Objref
           (CORBA.Default_Sys_Member);
      end if;
      return PortableInterceptor.RequestInfo.Impl.get_operation_context
        (PortableInterceptor.RequestInfo.Impl.Object_Ptr
           (Entity_Of
              (Self)));
   end get_operation_context;

   ----------------
   -- get_result --
   ----------------

   function get_result
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
      return PortableInterceptor.RequestInfo.Impl.get_result
        (PortableInterceptor.RequestInfo.Impl.Object_Ptr
           (Entity_Of
              (Self)));
   end get_result;

   ---------------------------
   -- get_response_expected --
   ---------------------------

   function get_response_expected
     (Self : Local_Ref)
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
      return PortableInterceptor.RequestInfo.Impl.get_response_expected
        (PortableInterceptor.RequestInfo.Impl.Object_Ptr
           (Entity_Of
              (Self)));
   end get_response_expected;

   --------------------
   -- get_sync_scope --
   --------------------

   function get_sync_scope
     (Self : Local_Ref)
     return Messaging.SyncScope
   is
   begin
      if CORBA.Object.Is_Nil
        (CORBA.Object.Ref
           (Self))
      then
         CORBA.Raise_Inv_Objref
           (CORBA.Default_Sys_Member);
      end if;
      return PortableInterceptor.RequestInfo.Impl.get_sync_scope
        (PortableInterceptor.RequestInfo.Impl.Object_Ptr
           (Entity_Of
              (Self)));
   end get_sync_scope;

   ----------------------
   -- get_reply_status --
   ----------------------

   function get_reply_status
     (Self : Local_Ref)
     return PortableInterceptor.ReplyStatus
   is
   begin
      if CORBA.Object.Is_Nil
        (CORBA.Object.Ref
           (Self))
      then
         CORBA.Raise_Inv_Objref
           (CORBA.Default_Sys_Member);
      end if;
      return PortableInterceptor.RequestInfo.Impl.get_reply_status
        (PortableInterceptor.RequestInfo.Impl.Object_Ptr
           (Entity_Of
              (Self)));
   end get_reply_status;

   ---------------------------
   -- get_forward_reference --
   ---------------------------

   function get_forward_reference
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
      return PortableInterceptor.RequestInfo.Impl.get_forward_reference
        (PortableInterceptor.RequestInfo.Impl.Object_Ptr
           (Entity_Of
              (Self)));
   end get_forward_reference;

   --------------
   -- get_slot --
   --------------

   function get_slot
     (Self : Local_Ref;
      id : PortableInterceptor.SlotId)
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
      return PortableInterceptor.RequestInfo.Impl.get_slot
        (PortableInterceptor.RequestInfo.Impl.Object_Ptr
           (Entity_Of
              (Self)),
         id);
   end get_slot;

   ---------------------------------
   -- get_request_service_context --
   ---------------------------------

   function get_request_service_context
     (Self : Local_Ref;
      id : IOP.ServiceId)
     return IOP.ServiceContext
   is
   begin
      if CORBA.Object.Is_Nil
        (CORBA.Object.Ref
           (Self))
      then
         CORBA.Raise_Inv_Objref
           (CORBA.Default_Sys_Member);
      end if;
      return PortableInterceptor.RequestInfo.Impl.get_request_service_context
        (PortableInterceptor.RequestInfo.Impl.Object_Ptr
           (Entity_Of
              (Self)),
         id);
   end get_request_service_context;

   -------------------------------
   -- get_reply_service_context --
   -------------------------------

   function get_reply_service_context
     (Self : Local_Ref;
      id : IOP.ServiceId)
     return IOP.ServiceContext
   is
   begin
      if CORBA.Object.Is_Nil
        (CORBA.Object.Ref
           (Self))
      then
         CORBA.Raise_Inv_Objref
           (CORBA.Default_Sys_Member);
      end if;
      return PortableInterceptor.RequestInfo.Impl.get_reply_service_context
        (PortableInterceptor.RequestInfo.Impl.Object_Ptr
           (Entity_Of
              (Self)),
         id);
   end get_reply_service_context;

end PortableInterceptor.RequestInfo;

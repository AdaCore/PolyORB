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

with PolyORB.Exceptions;
with PortableInterceptor.ORBInitInfo.Impl;

package body PortableInterceptor.ORBInitInfo is

   -----------------
   -- Get_Members --
   -----------------

   procedure Get_Members
     (From : Ada.Exceptions.Exception_Occurrence;
      To : out PortableInterceptor.ORBInitInfo.DuplicateName_Members)
   is
   begin
      PolyORB.Exceptions.User_Get_Members
        (From,
         To);
   end Get_Members;

   -----------------
   -- Get_Members --
   -----------------

   procedure Get_Members
     (From : Ada.Exceptions.Exception_Occurrence;
      To : out PortableInterceptor.ORBInitInfo.InvalidName_Members)
   is
   begin
      PolyORB.Exceptions.User_Get_Members
        (From,
         To);
   end Get_Members;

   -------------------
   -- get_arguments --
   -------------------

   function get_arguments
     (Self : Local_Ref)
     return CORBA.IDL_Sequences.StringSeq
   is
   begin
      if CORBA.Object.Is_Nil
        (CORBA.Object.Ref
           (Self))
      then
         CORBA.Raise_Inv_Objref
           (CORBA.Default_Sys_Member);
      end if;
      return PortableInterceptor.ORBInitInfo.Impl.get_arguments
        (PortableInterceptor.ORBInitInfo.Impl.Object_Ptr
           (Entity_Of
              (Self)));
   end get_arguments;

   ----------------
   -- get_orb_id --
   ----------------

   function get_orb_id
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
      return PortableInterceptor.ORBInitInfo.Impl.get_orb_id
        (PortableInterceptor.ORBInitInfo.Impl.Object_Ptr
           (Entity_Of
              (Self)));
   end get_orb_id;

   -----------------------
   -- get_codec_factory --
   -----------------------

   function get_codec_factory
     (Self : Local_Ref)
     return IOP.CodecFactory.Local_Ref
   is
   begin
      if CORBA.Object.Is_Nil
        (CORBA.Object.Ref
           (Self))
      then
         CORBA.Raise_Inv_Objref
           (CORBA.Default_Sys_Member);
      end if;
      return PortableInterceptor.ORBInitInfo.Impl.get_codec_factory
        (PortableInterceptor.ORBInitInfo.Impl.Object_Ptr
           (Entity_Of
              (Self)));
   end get_codec_factory;

   --------------------------------
   -- register_initial_reference --
   --------------------------------

   procedure register_initial_reference
     (Self : Local_Ref;
      id : PortableInterceptor.ORBInitInfo.ObjectId;
      obj : CORBA.Object.Ref)
   is
   begin
      if CORBA.Object.Is_Nil
        (CORBA.Object.Ref
           (Self))
      then
         CORBA.Raise_Inv_Objref
           (CORBA.Default_Sys_Member);
      end if;
      PortableInterceptor.ORBInitInfo.Impl.register_initial_reference
        (PortableInterceptor.ORBInitInfo.Impl.Object_Ptr
           (Entity_Of
              (Self)),
         id,
         obj);
   end register_initial_reference;

   --------------------------------
   -- resolve_initial_references --
   --------------------------------

   function resolve_initial_references
     (Self : Local_Ref;
      id : PortableInterceptor.ORBInitInfo.ObjectId)
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
      return PortableInterceptor.ORBInitInfo.Impl.resolve_initial_references
        (PortableInterceptor.ORBInitInfo.Impl.Object_Ptr
           (Entity_Of
              (Self)),
         id);
   end resolve_initial_references;

   ------------------------------------
   -- add_client_request_interceptor --
   ------------------------------------

   procedure add_client_request_interceptor
     (Self : Local_Ref;
      interceptor : PortableInterceptor.ClientRequestInterceptor.Local_Ref)
   is
   begin
      if CORBA.Object.Is_Nil
        (CORBA.Object.Ref
           (Self))
      then
         CORBA.Raise_Inv_Objref
           (CORBA.Default_Sys_Member);
      end if;
      PortableInterceptor.ORBInitInfo.Impl.add_client_request_interceptor
        (PortableInterceptor.ORBInitInfo.Impl.Object_Ptr
           (Entity_Of
              (Self)),
         interceptor);
   end add_client_request_interceptor;

   ------------------------------------
   -- add_server_request_interceptor --
   ------------------------------------

   procedure add_server_request_interceptor
     (Self : Local_Ref;
      interceptor : PortableInterceptor.ServerRequestInterceptor.Local_Ref)
   is
   begin
      if CORBA.Object.Is_Nil
        (CORBA.Object.Ref
           (Self))
      then
         CORBA.Raise_Inv_Objref
           (CORBA.Default_Sys_Member);
      end if;
      PortableInterceptor.ORBInitInfo.Impl.add_server_request_interceptor
        (PortableInterceptor.ORBInitInfo.Impl.Object_Ptr
           (Entity_Of
              (Self)),
         interceptor);
   end add_server_request_interceptor;

   -------------------------
   -- add_ior_interceptor --
   -------------------------

   procedure add_ior_interceptor
     (Self : Local_Ref;
      interceptor : PortableInterceptor.IORInterceptor.Local_Ref)
   is
   begin
      if CORBA.Object.Is_Nil
        (CORBA.Object.Ref
           (Self))
      then
         CORBA.Raise_Inv_Objref
           (CORBA.Default_Sys_Member);
      end if;
      PortableInterceptor.ORBInitInfo.Impl.add_ior_interceptor
        (PortableInterceptor.ORBInitInfo.Impl.Object_Ptr
           (Entity_Of
              (Self)),
         interceptor);
   end add_ior_interceptor;

   ----------------------
   -- allocate_slot_id --
   ----------------------

   function allocate_slot_id
     (Self : Local_Ref)
     return PortableInterceptor.SlotId
   is
   begin
      if CORBA.Object.Is_Nil
        (CORBA.Object.Ref
           (Self))
      then
         CORBA.Raise_Inv_Objref
           (CORBA.Default_Sys_Member);
      end if;
      return PortableInterceptor.ORBInitInfo.Impl.allocate_slot_id
        (PortableInterceptor.ORBInitInfo.Impl.Object_Ptr
           (Entity_Of
              (Self)));
   end allocate_slot_id;

   -----------------------------
   -- register_policy_factory --
   -----------------------------

   procedure register_policy_factory
     (Self : Local_Ref;
      IDL_type : CORBA.PolicyType;
      policy_factory : PortableInterceptor.PolicyFactory.Local_Ref)
   is
   begin
      if CORBA.Object.Is_Nil
        (CORBA.Object.Ref
           (Self))
      then
         CORBA.Raise_Inv_Objref
           (CORBA.Default_Sys_Member);
      end if;
      PortableInterceptor.ORBInitInfo.Impl.register_policy_factory
        (PortableInterceptor.ORBInitInfo.Impl.Object_Ptr
           (Entity_Of
              (Self)),
         IDL_type,
         policy_factory);
   end register_policy_factory;

end PortableInterceptor.ORBInitInfo;

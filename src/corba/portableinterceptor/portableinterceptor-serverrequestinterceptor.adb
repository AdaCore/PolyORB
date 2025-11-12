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
with CORBA;
pragma Elaborate_All (CORBA);
with PortableInterceptor.ServerRequestInterceptor.Impl;

package body PortableInterceptor.ServerRequestInterceptor is

   --------------------------------------
   -- receive_request_service_contexts --
   --------------------------------------

   procedure receive_request_service_contexts
     (Self : Local_Ref;
      ri : PortableInterceptor.ServerRequestInfo.Local_Ref)
   is
   begin
      if CORBA.Object.Is_Nil
        (CORBA.Object.Ref
           (Self))
      then
         CORBA.Raise_Inv_Objref
           (CORBA.Default_Sys_Member);
      end if;
      PortableInterceptor.ServerRequestInterceptor.Impl.receive_request_service_contexts
        (PortableInterceptor.ServerRequestInterceptor.Impl.Object_Ptr
           (Entity_Of
              (Self)),
         ri);
   end receive_request_service_contexts;

   ---------------------
   -- receive_request --
   ---------------------

   procedure receive_request
     (Self : Local_Ref;
      ri : PortableInterceptor.ServerRequestInfo.Local_Ref)
   is
   begin
      if CORBA.Object.Is_Nil
        (CORBA.Object.Ref
           (Self))
      then
         CORBA.Raise_Inv_Objref
           (CORBA.Default_Sys_Member);
      end if;
      PortableInterceptor.ServerRequestInterceptor.Impl.receive_request
        (PortableInterceptor.ServerRequestInterceptor.Impl.Object_Ptr
           (Entity_Of
              (Self)),
         ri);
   end receive_request;

   ----------------
   -- send_reply --
   ----------------

   procedure send_reply
     (Self : Local_Ref;
      ri : PortableInterceptor.ServerRequestInfo.Local_Ref)
   is
   begin
      if CORBA.Object.Is_Nil
        (CORBA.Object.Ref
           (Self))
      then
         CORBA.Raise_Inv_Objref
           (CORBA.Default_Sys_Member);
      end if;
      PortableInterceptor.ServerRequestInterceptor.Impl.send_reply
        (PortableInterceptor.ServerRequestInterceptor.Impl.Object_Ptr
           (Entity_Of
              (Self)),
         ri);
   end send_reply;

   --------------------
   -- send_exception --
   --------------------

   procedure send_exception
     (Self : Local_Ref;
      ri : PortableInterceptor.ServerRequestInfo.Local_Ref)
   is
   begin
      if CORBA.Object.Is_Nil
        (CORBA.Object.Ref
           (Self))
      then
         CORBA.Raise_Inv_Objref
           (CORBA.Default_Sys_Member);
      end if;
      PortableInterceptor.ServerRequestInterceptor.Impl.send_exception
        (PortableInterceptor.ServerRequestInterceptor.Impl.Object_Ptr
           (Entity_Of
              (Self)),
         ri);
   end send_exception;

   ----------------
   -- send_other --
   ----------------

   procedure send_other
     (Self : Local_Ref;
      ri : PortableInterceptor.ServerRequestInfo.Local_Ref)
   is
   begin
      if CORBA.Object.Is_Nil
        (CORBA.Object.Ref
           (Self))
      then
         CORBA.Raise_Inv_Objref
           (CORBA.Default_Sys_Member);
      end if;
      PortableInterceptor.ServerRequestInterceptor.Impl.send_other
        (PortableInterceptor.ServerRequestInterceptor.Impl.Object_Ptr
           (Entity_Of
              (Self)),
         ri);
   end send_other;

end PortableInterceptor.ServerRequestInterceptor;

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
with PortableInterceptor.ClientRequestInterceptor.Impl;

package body PortableInterceptor.ClientRequestInterceptor is

   ------------------
   -- send_request --
   ------------------

   procedure send_request
     (Self : Local_Ref;
      ri : PortableInterceptor.ClientRequestInfo.Local_Ref)
   is
   begin
      if CORBA.Object.Is_Nil
        (CORBA.Object.Ref
           (Self))
      then
         CORBA.Raise_Inv_Objref
           (CORBA.Default_Sys_Member);
      end if;
      PortableInterceptor.ClientRequestInterceptor.Impl.send_request
        (PortableInterceptor.ClientRequestInterceptor.Impl.Object_Ptr
           (Entity_Of
              (Self)),
         ri);
   end send_request;

   ---------------
   -- send_poll --
   ---------------

   procedure send_poll
     (Self : Local_Ref;
      ri : PortableInterceptor.ClientRequestInfo.Local_Ref)
   is
   begin
      if CORBA.Object.Is_Nil
        (CORBA.Object.Ref
           (Self))
      then
         CORBA.Raise_Inv_Objref
           (CORBA.Default_Sys_Member);
      end if;
      PortableInterceptor.ClientRequestInterceptor.Impl.send_poll
        (PortableInterceptor.ClientRequestInterceptor.Impl.Object_Ptr
           (Entity_Of
              (Self)),
         ri);
   end send_poll;

   -------------------
   -- receive_reply --
   -------------------

   procedure receive_reply
     (Self : Local_Ref;
      ri : PortableInterceptor.ClientRequestInfo.Local_Ref)
   is
   begin
      if CORBA.Object.Is_Nil
        (CORBA.Object.Ref
           (Self))
      then
         CORBA.Raise_Inv_Objref
           (CORBA.Default_Sys_Member);
      end if;
      PortableInterceptor.ClientRequestInterceptor.Impl.receive_reply
        (PortableInterceptor.ClientRequestInterceptor.Impl.Object_Ptr
           (Entity_Of
              (Self)),
         ri);
   end receive_reply;

   -----------------------
   -- receive_exception --
   -----------------------

   procedure receive_exception
     (Self : Local_Ref;
      ri : PortableInterceptor.ClientRequestInfo.Local_Ref)
   is
   begin
      if CORBA.Object.Is_Nil
        (CORBA.Object.Ref
           (Self))
      then
         CORBA.Raise_Inv_Objref
           (CORBA.Default_Sys_Member);
      end if;
      PortableInterceptor.ClientRequestInterceptor.Impl.receive_exception
        (PortableInterceptor.ClientRequestInterceptor.Impl.Object_Ptr
           (Entity_Of
              (Self)),
         ri);
   end receive_exception;

   -------------------
   -- receive_other --
   -------------------

   procedure receive_other
     (Self : Local_Ref;
      ri : PortableInterceptor.ClientRequestInfo.Local_Ref)
   is
   begin
      if CORBA.Object.Is_Nil
        (CORBA.Object.Ref
           (Self))
      then
         CORBA.Raise_Inv_Objref
           (CORBA.Default_Sys_Member);
      end if;
      PortableInterceptor.ClientRequestInterceptor.Impl.receive_other
        (PortableInterceptor.ClientRequestInterceptor.Impl.Object_Ptr
           (Entity_Of
              (Self)),
         ri);
   end receive_other;

end PortableInterceptor.ClientRequestInterceptor;

pragma Style_Checks ("NM32766");
pragma Wide_Character_Encoding (Brackets);
pragma Warnings (Off, "use of an anonymous access type allocator");

---------------------------------------------------
--  This file has been generated automatically from
--  /Users/heathdorn/Documents/Playground/Agents/RefactorTeam/code_refactor/PolyORB/idls/Misc/PortableInterceptor.idl
--  by IAC (IDL to Ada Compiler) 20.0w (rev. 41a9b833).
---------------------------------------------------
--  NOTE: If you modify this file by hand, your
--  changes will be lost when you re-run the
--  IDL to Ada compiler.
---------------------------------------------------

with PortableInterceptor.Interceptor;
with PolyORB.Std;
with PortableInterceptor.ServerRequestInfo;

package PortableInterceptor.ServerRequestInterceptor is

   type Local_Ref is
     new PortableInterceptor.Interceptor.Local_Ref with null record;

   Repository_Id : constant PolyORB.Std.String :=
     "IDL:omg.org/PortableInterceptor/ServerRequestInterceptor:1.0";

   procedure receive_request_service_contexts
     (Self : Local_Ref;
      ri : PortableInterceptor.ServerRequestInfo.Local_Ref);

   receive_request_service_contexts_Repository_Id : constant PolyORB.Std.String :=
     "IDL:omg.org/PortableInterceptor/ServerRequestInterceptor/receive_request_service_contexts:1.0";

   procedure receive_request
     (Self : Local_Ref;
      ri : PortableInterceptor.ServerRequestInfo.Local_Ref);

   receive_request_Repository_Id : constant PolyORB.Std.String :=
     "IDL:omg.org/PortableInterceptor/ServerRequestInterceptor/receive_request:1.0";

   procedure send_reply
     (Self : Local_Ref;
      ri : PortableInterceptor.ServerRequestInfo.Local_Ref);

   send_reply_Repository_Id : constant PolyORB.Std.String :=
     "IDL:omg.org/PortableInterceptor/ServerRequestInterceptor/send_reply:1.0";

   procedure send_exception
     (Self : Local_Ref;
      ri : PortableInterceptor.ServerRequestInfo.Local_Ref);

   send_exception_Repository_Id : constant PolyORB.Std.String :=
     "IDL:omg.org/PortableInterceptor/ServerRequestInterceptor/send_exception:1.0";

   procedure send_other
     (Self : Local_Ref;
      ri : PortableInterceptor.ServerRequestInfo.Local_Ref);

   send_other_Repository_Id : constant PolyORB.Std.String :=
     "IDL:omg.org/PortableInterceptor/ServerRequestInterceptor/send_other:1.0";

end PortableInterceptor.ServerRequestInterceptor;

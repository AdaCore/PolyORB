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
with PortableInterceptor.ClientRequestInfo;

package PortableInterceptor.ClientRequestInterceptor is

   type Local_Ref is
     new PortableInterceptor.Interceptor.Local_Ref with null record;

   Repository_Id : constant PolyORB.Std.String :=
     "IDL:omg.org/PortableInterceptor/ClientRequestInterceptor:1.0";

   procedure send_request
     (Self : Local_Ref;
      ri : PortableInterceptor.ClientRequestInfo.Local_Ref);

   send_request_Repository_Id : constant PolyORB.Std.String :=
     "IDL:omg.org/PortableInterceptor/ClientRequestInterceptor/send_request:1.0";

   procedure send_poll
     (Self : Local_Ref;
      ri : PortableInterceptor.ClientRequestInfo.Local_Ref);

   send_poll_Repository_Id : constant PolyORB.Std.String :=
     "IDL:omg.org/PortableInterceptor/ClientRequestInterceptor/send_poll:1.0";

   procedure receive_reply
     (Self : Local_Ref;
      ri : PortableInterceptor.ClientRequestInfo.Local_Ref);

   receive_reply_Repository_Id : constant PolyORB.Std.String :=
     "IDL:omg.org/PortableInterceptor/ClientRequestInterceptor/receive_reply:1.0";

   procedure receive_exception
     (Self : Local_Ref;
      ri : PortableInterceptor.ClientRequestInfo.Local_Ref);

   receive_exception_Repository_Id : constant PolyORB.Std.String :=
     "IDL:omg.org/PortableInterceptor/ClientRequestInterceptor/receive_exception:1.0";

   procedure receive_other
     (Self : Local_Ref;
      ri : PortableInterceptor.ClientRequestInfo.Local_Ref);

   receive_other_Repository_Id : constant PolyORB.Std.String :=
     "IDL:omg.org/PortableInterceptor/ClientRequestInterceptor/receive_other:1.0";

end PortableInterceptor.ClientRequestInterceptor;

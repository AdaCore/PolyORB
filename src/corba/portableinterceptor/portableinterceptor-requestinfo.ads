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

with CORBA.Object;
with PolyORB.Std;
with CORBA;
pragma Elaborate_All (CORBA);
with Dynamic;
with Messaging;
with IOP;

package PortableInterceptor.RequestInfo is

   type Local_Ref is
     new CORBA.Object.Ref with null record;

   Repository_Id : constant PolyORB.Std.String :=
     "IDL:omg.org/PortableInterceptor/RequestInfo:1.0";

   request_id_Repository_Id : constant PolyORB.Std.String :=
     "IDL:omg.org/PortableInterceptor/RequestInfo/request_id:1.0";

   function get_request_id
     (Self : Local_Ref)
     return CORBA.Unsigned_Long;

   operation_Repository_Id : constant PolyORB.Std.String :=
     "IDL:omg.org/PortableInterceptor/RequestInfo/operation:1.0";

   function get_operation
     (Self : Local_Ref)
     return CORBA.String;

   arguments_Repository_Id : constant PolyORB.Std.String :=
     "IDL:omg.org/PortableInterceptor/RequestInfo/arguments:1.0";

   function get_arguments
     (Self : Local_Ref)
     return Dynamic.ParameterList;

   exceptions_Repository_Id : constant PolyORB.Std.String :=
     "IDL:omg.org/PortableInterceptor/RequestInfo/exceptions:1.0";

   function get_exceptions
     (Self : Local_Ref)
     return Dynamic.ExceptionList;

   contexts_Repository_Id : constant PolyORB.Std.String :=
     "IDL:omg.org/PortableInterceptor/RequestInfo/contexts:1.0";

   function get_contexts
     (Self : Local_Ref)
     return Dynamic.ContextList;

   operation_context_Repository_Id : constant PolyORB.Std.String :=
     "IDL:omg.org/PortableInterceptor/RequestInfo/operation_context:1.0";

   function get_operation_context
     (Self : Local_Ref)
     return Dynamic.RequestContext;

   result_Repository_Id : constant PolyORB.Std.String :=
     "IDL:omg.org/PortableInterceptor/RequestInfo/result:1.0";

   function get_result
     (Self : Local_Ref)
     return CORBA.Any;

   response_expected_Repository_Id : constant PolyORB.Std.String :=
     "IDL:omg.org/PortableInterceptor/RequestInfo/response_expected:1.0";

   function get_response_expected
     (Self : Local_Ref)
     return CORBA.Boolean;

   sync_scope_Repository_Id : constant PolyORB.Std.String :=
     "IDL:omg.org/PortableInterceptor/RequestInfo/sync_scope:1.0";

   function get_sync_scope
     (Self : Local_Ref)
     return Messaging.SyncScope;

   reply_status_Repository_Id : constant PolyORB.Std.String :=
     "IDL:omg.org/PortableInterceptor/RequestInfo/reply_status:1.0";

   function get_reply_status
     (Self : Local_Ref)
     return PortableInterceptor.ReplyStatus;

   forward_reference_Repository_Id : constant PolyORB.Std.String :=
     "IDL:omg.org/PortableInterceptor/RequestInfo/forward_reference:1.0";

   function get_forward_reference
     (Self : Local_Ref)
     return CORBA.Object.Ref;

   function get_slot
     (Self : Local_Ref;
      id : PortableInterceptor.SlotId)
     return CORBA.Any;

   get_slot_Repository_Id : constant PolyORB.Std.String :=
     "IDL:omg.org/PortableInterceptor/RequestInfo/get_slot:1.0";

   function get_request_service_context
     (Self : Local_Ref;
      id : IOP.ServiceId)
     return IOP.ServiceContext;

   get_request_service_context_Repository_Id : constant PolyORB.Std.String :=
     "IDL:omg.org/PortableInterceptor/RequestInfo/get_request_service_context:1.0";

   function get_reply_service_context
     (Self : Local_Ref;
      id : IOP.ServiceId)
     return IOP.ServiceContext;

   get_reply_service_context_Repository_Id : constant PolyORB.Std.String :=
     "IDL:omg.org/PortableInterceptor/RequestInfo/get_reply_service_context:1.0";

end PortableInterceptor.RequestInfo;

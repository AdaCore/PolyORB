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

with PortableInterceptor.RequestInfo;
with PolyORB.Std;
with CORBA;
pragma Elaborate_All (CORBA);
with CORBA.IDL_Sequences;
with CORBA.Policy;
with IOP;

package PortableInterceptor.ServerRequestInfo is

   type Local_Ref is
     new PortableInterceptor.RequestInfo.Local_Ref with null record;

   Repository_Id : constant PolyORB.Std.String :=
     "IDL:omg.org/PortableInterceptor/ServerRequestInfo:1.0";

   sending_exception_Repository_Id : constant PolyORB.Std.String :=
     "IDL:omg.org/PortableInterceptor/ServerRequestInfo/sending_exception:1.0";

   function get_sending_exception
     (Self : Local_Ref)
     return CORBA.Any;

   server_id_Repository_Id : constant PolyORB.Std.String :=
     "IDL:omg.org/PortableInterceptor/ServerRequestInfo/server_id:1.0";

   function get_server_id
     (Self : Local_Ref)
     return PortableInterceptor.ServerId;

   orb_id_Repository_Id : constant PolyORB.Std.String :=
     "IDL:omg.org/PortableInterceptor/ServerRequestInfo/orb_id:1.0";

   function get_orb_id
     (Self : Local_Ref)
     return PortableInterceptor.ORBId;

   adapter_name_Repository_Id : constant PolyORB.Std.String :=
     "IDL:omg.org/PortableInterceptor/ServerRequestInfo/adapter_name:1.0";

   function get_adapter_name
     (Self : Local_Ref)
     return PortableInterceptor.AdapterName;

   object_id_Repository_Id : constant PolyORB.Std.String :=
     "IDL:omg.org/PortableInterceptor/ServerRequestInfo/object_id:1.0";

   function get_object_id
     (Self : Local_Ref)
     return PortableInterceptor.ObjectId;

   adapter_id_Repository_Id : constant PolyORB.Std.String :=
     "IDL:omg.org/PortableInterceptor/ServerRequestInfo/adapter_id:1.0";

   function get_adapter_id
     (Self : Local_Ref)
     return CORBA.IDL_Sequences.OctetSeq;

   target_most_derived_interface_Repository_Id : constant PolyORB.Std.String :=
     "IDL:omg.org/PortableInterceptor/ServerRequestInfo/target_most_derived_interface:1.0";

   function get_target_most_derived_interface
     (Self : Local_Ref)
     return CORBA.RepositoryId;

   function get_server_policy
     (Self : Local_Ref;
      IDL_type : CORBA.PolicyType)
     return CORBA.Policy.Ref;

   get_server_policy_Repository_Id : constant PolyORB.Std.String :=
     "IDL:omg.org/PortableInterceptor/ServerRequestInfo/get_server_policy:1.0";

   procedure set_slot
     (Self : Local_Ref;
      id : PortableInterceptor.SlotId;
      data : CORBA.Any);

   set_slot_Repository_Id : constant PolyORB.Std.String :=
     "IDL:omg.org/PortableInterceptor/ServerRequestInfo/set_slot:1.0";

   function target_is_a
     (Self : Local_Ref;
      id : CORBA.RepositoryId)
     return CORBA.Boolean;

   target_is_a_Repository_Id : constant PolyORB.Std.String :=
     "IDL:omg.org/PortableInterceptor/ServerRequestInfo/target_is_a:1.0";

   procedure add_reply_service_context
     (Self : Local_Ref;
      service_context : IOP.ServiceContext;
      replace : CORBA.Boolean);

   add_reply_service_context_Repository_Id : constant PolyORB.Std.String :=
     "IDL:omg.org/PortableInterceptor/ServerRequestInfo/add_reply_service_context:1.0";

end PortableInterceptor.ServerRequestInfo;

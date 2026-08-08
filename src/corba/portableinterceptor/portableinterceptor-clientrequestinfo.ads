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
with CORBA.Object;
with IOP;
with CORBA;
pragma Elaborate_All (CORBA);
with CORBA.Policy;

package PortableInterceptor.ClientRequestInfo is

   type Local_Ref is
     new PortableInterceptor.RequestInfo.Local_Ref with null record;

   Repository_Id : constant PolyORB.Std.String :=
     "IDL:omg.org/PortableInterceptor/ClientRequestInfo:1.0";

   target_Repository_Id : constant PolyORB.Std.String :=
     "IDL:omg.org/PortableInterceptor/ClientRequestInfo/target:1.0";

   function get_target
     (Self : Local_Ref)
     return CORBA.Object.Ref;

   effective_target_Repository_Id : constant PolyORB.Std.String :=
     "IDL:omg.org/PortableInterceptor/ClientRequestInfo/effective_target:1.0";

   function get_effective_target
     (Self : Local_Ref)
     return CORBA.Object.Ref;

   effective_profile_Repository_Id : constant PolyORB.Std.String :=
     "IDL:omg.org/PortableInterceptor/ClientRequestInfo/effective_profile:1.0";

   function get_effective_profile
     (Self : Local_Ref)
     return IOP.TaggedProfile;

   received_exception_Repository_Id : constant PolyORB.Std.String :=
     "IDL:omg.org/PortableInterceptor/ClientRequestInfo/received_exception:1.0";

   function get_received_exception
     (Self : Local_Ref)
     return CORBA.Any;

   received_exception_id_Repository_Id : constant PolyORB.Std.String :=
     "IDL:omg.org/PortableInterceptor/ClientRequestInfo/received_exception_id:1.0";

   function get_received_exception_id
     (Self : Local_Ref)
     return CORBA.RepositoryId;

   function get_effective_component
     (Self : Local_Ref;
      id : IOP.ComponentId)
     return IOP.TaggedComponent;

   get_effective_component_Repository_Id : constant PolyORB.Std.String :=
     "IDL:omg.org/PortableInterceptor/ClientRequestInfo/get_effective_component:1.0";

   function get_effective_components
     (Self : Local_Ref;
      id : IOP.ComponentId)
     return IOP.TaggedComponentSeq;

   get_effective_components_Repository_Id : constant PolyORB.Std.String :=
     "IDL:omg.org/PortableInterceptor/ClientRequestInfo/get_effective_components:1.0";

   function get_request_policy
     (Self : Local_Ref;
      IDL_type : CORBA.PolicyType)
     return CORBA.Policy.Ref;

   get_request_policy_Repository_Id : constant PolyORB.Std.String :=
     "IDL:omg.org/PortableInterceptor/ClientRequestInfo/get_request_policy:1.0";

   procedure add_request_service_context
     (Self : Local_Ref;
      service_context : IOP.ServiceContext;
      replace : CORBA.Boolean);

   add_request_service_context_Repository_Id : constant PolyORB.Std.String :=
     "IDL:omg.org/PortableInterceptor/ClientRequestInfo/add_request_service_context:1.0";

end PortableInterceptor.ClientRequestInfo;

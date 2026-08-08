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
with Ada.Exceptions;
with CORBA.IDL_Sequences;
with IOP;
with IOP.CodecFactory;
with PortableInterceptor.ClientRequestInterceptor;
with PortableInterceptor.ServerRequestInterceptor;
with PortableInterceptor.IORInterceptor;
with PortableInterceptor.PolicyFactory;

package PortableInterceptor.ORBInitInfo is

   type Local_Ref is
     new CORBA.Object.Ref with null record;

   Repository_Id : constant PolyORB.Std.String :=
     "IDL:omg.org/PortableInterceptor/ORBInitInfo:1.0";

   type ObjectId is
     new CORBA.String;

   ObjectId_Repository_Id : constant PolyORB.Std.String :=
     "IDL:omg.org/PortableInterceptor/ORBInitInfo/ObjectId:1.0";

   DuplicateName : exception;

   type DuplicateName_Members is
     new CORBA.Idl_Exception_Members with record
         name : CORBA.String;
      end record;

   DuplicateName_Repository_Id : constant PolyORB.Std.String :=
     "IDL:omg.org/PortableInterceptor/ORBInitInfo/DuplicateName:1.0";

   procedure Get_Members
     (From : Ada.Exceptions.Exception_Occurrence;
      To : out PortableInterceptor.ORBInitInfo.DuplicateName_Members);

   InvalidName : exception;

   type InvalidName_Members is
     new CORBA.Idl_Exception_Members with null record;

   InvalidName_Repository_Id : constant PolyORB.Std.String :=
     "IDL:omg.org/PortableInterceptor/ORBInitInfo/InvalidName:1.0";

   procedure Get_Members
     (From : Ada.Exceptions.Exception_Occurrence;
      To : out PortableInterceptor.ORBInitInfo.InvalidName_Members);

   arguments_Repository_Id : constant PolyORB.Std.String :=
     "IDL:omg.org/PortableInterceptor/ORBInitInfo/arguments:1.0";

   function get_arguments
     (Self : Local_Ref)
     return CORBA.IDL_Sequences.StringSeq;

   orb_id_Repository_Id : constant PolyORB.Std.String :=
     "IDL:omg.org/PortableInterceptor/ORBInitInfo/orb_id:1.0";

   function get_orb_id
     (Self : Local_Ref)
     return CORBA.String;

   codec_factory_Repository_Id : constant PolyORB.Std.String :=
     "IDL:omg.org/PortableInterceptor/ORBInitInfo/codec_factory:1.0";

   function get_codec_factory
     (Self : Local_Ref)
     return IOP.CodecFactory.Local_Ref;

   procedure register_initial_reference
     (Self : Local_Ref;
      id : PortableInterceptor.ORBInitInfo.ObjectId;
      obj : CORBA.Object.Ref);

   register_initial_reference_Repository_Id : constant PolyORB.Std.String :=
     "IDL:omg.org/PortableInterceptor/ORBInitInfo/register_initial_reference:1.0";

   function resolve_initial_references
     (Self : Local_Ref;
      id : PortableInterceptor.ORBInitInfo.ObjectId)
     return CORBA.Object.Ref;

   resolve_initial_references_Repository_Id : constant PolyORB.Std.String :=
     "IDL:omg.org/PortableInterceptor/ORBInitInfo/resolve_initial_references:1.0";

   procedure add_client_request_interceptor
     (Self : Local_Ref;
      interceptor : PortableInterceptor.ClientRequestInterceptor.Local_Ref);

   add_client_request_interceptor_Repository_Id : constant PolyORB.Std.String :=
     "IDL:omg.org/PortableInterceptor/ORBInitInfo/add_client_request_interceptor:1.0";

   procedure add_server_request_interceptor
     (Self : Local_Ref;
      interceptor : PortableInterceptor.ServerRequestInterceptor.Local_Ref);

   add_server_request_interceptor_Repository_Id : constant PolyORB.Std.String :=
     "IDL:omg.org/PortableInterceptor/ORBInitInfo/add_server_request_interceptor:1.0";

   procedure add_ior_interceptor
     (Self : Local_Ref;
      interceptor : PortableInterceptor.IORInterceptor.Local_Ref);

   add_ior_interceptor_Repository_Id : constant PolyORB.Std.String :=
     "IDL:omg.org/PortableInterceptor/ORBInitInfo/add_ior_interceptor:1.0";

   function allocate_slot_id
     (Self : Local_Ref)
     return PortableInterceptor.SlotId;

   allocate_slot_id_Repository_Id : constant PolyORB.Std.String :=
     "IDL:omg.org/PortableInterceptor/ORBInitInfo/allocate_slot_id:1.0";

   procedure register_policy_factory
     (Self : Local_Ref;
      IDL_type : CORBA.PolicyType;
      policy_factory : PortableInterceptor.PolicyFactory.Local_Ref);

   register_policy_factory_Repository_Id : constant PolyORB.Std.String :=
     "IDL:omg.org/PortableInterceptor/ORBInitInfo/register_policy_factory:1.0";

end PortableInterceptor.ORBInitInfo;

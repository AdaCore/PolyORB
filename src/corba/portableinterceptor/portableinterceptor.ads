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

--  Source: 
--  /Users/heathdorn/Documents/Playground/Agents/RefactorTeam/code_refactor/PolyORB/idls/Misc/PortableInterceptor.idl

--  // File: PortableInterceptor.idl
--  // CORBA 3.0, Chapter 21

--  #ifndef _PORTABLE_INTERCEPTOR_IDL_
--  #define _PORTABLE_INTERCEPTOR_IDL_

--  #ifdef _PRE_3_0_COMPILER_ 
--  #pragma prefix "omg.org"
--  #include <orb.idl>
--  #include <IOP.idl>
--  #include <Messaging.idl>
--  #include <Dynamic.idl>
--  #else
--  import ::CORBA;
--  import ::IOP;
--  import ::Messaging;
--  import ::Dynamic;
--  #endif // _PRE_3_0_COMPILER_

--  module PortableInterceptor {

--  #ifndef _PRE_3_0_COMPILER_ 
--      typeprefix PortableInterceptor "omg.org";
--  #endif // _PRE_3_0_COMPILER_

--      local interface Interceptor {
--          readonly attribute string name;
--      };

--      exception ForwardRequest {
--          Object forward;
--      };

--      typedef short ReplyStatus;

--      // Valid reply_status values:
--      const ReplyStatus SUCCESSFUL = 0;
--      const ReplyStatus SYSTEM_EXCEPTION = 1;
--      const ReplyStatus USER_EXCEPTION = 2;
--      const ReplyStatus LOCATION_FORWARD = 3;
--      const ReplyStatus TRANSPORT_RETRY = 4;
--      const ReplyStatus UNKNOWN = 5;

--      typedef unsigned long SlotId;

--      exception InvalidSlot {};

--      local interface Current : CORBA::Current {
--          any get_slot (in SlotId id) raises (InvalidSlot);
--          void set_slot (in SlotId id, in any data) raises (InvalidSlot);
--      };

--      local interface RequestInfo {

--          readonly attribute unsigned long request_id;
--          readonly attribute string operation;
--          readonly attribute Dynamic::ParameterList arguments;
--          readonly attribute Dynamic::ExceptionList exceptions;
--          readonly attribute Dynamic::ContextList contexts;
--          readonly attribute Dynamic::RequestContext operation_context;
--          readonly attribute any result;
--          readonly attribute boolean response_expected;
--          readonly attribute Messaging::SyncScope sync_scope;
--          readonly attribute ReplyStatus reply_status;
--          readonly attribute Object forward_reference;

--          any get_slot (in SlotId id) raises (InvalidSlot);
--          IOP::ServiceContext get_request_service_context (
--              in IOP::ServiceId id);
--              IOP::ServiceContext get_reply_service_context (
--              in IOP::ServiceId id);
--      };

--      local interface ClientRequestInfo : RequestInfo {

--          readonly attribute Object target;
--          readonly attribute Object effective_target;
--          readonly attribute IOP::TaggedProfile effective_profile;
--          readonly attribute any received_exception;
--          readonly attribute CORBA::RepositoryId received_exception_id;

--          IOP::TaggedComponent get_effective_component (
--              in IOP::ComponentId id);
--          IOP::TaggedComponentSeq get_effective_components (
--              in IOP::ComponentId id);
--          CORBA::Policy get_request_policy (in CORBA::PolicyType type);
--          void add_request_service_context (
--              in IOP::ServiceContext service_context,
--              in boolean replace);
--      };

--      typedef string ServerId ;
--      typedef string ORBId ;
--      typedef CORBA::StringSeq AdapterName ;
--      typedef CORBA::OctetSeq ObjectId ;

--      local interface ServerRequestInfo : RequestInfo {
--          readonly attribute any sending_exception;
--  	readonly attribute ServerId server_id;
--  	readonly attribute ORBId orb_id;
--  	readonly attribute AdapterName adapter_name;
--          readonly attribute ObjectId object_id;
--          readonly attribute CORBA::OctetSeq adapter_id;
--          readonly attribute CORBA::RepositoryId 
--  target_most_derived_interface;

--          CORBA::Policy get_server_policy (in CORBA::PolicyType type);
--          void set_slot (in SlotId id, in any data) raises (InvalidSlot);
--          boolean target_is_a (in CORBA::RepositoryId id);
--          void add_reply_service_context (
--              in IOP::ServiceContext service_context,
--              in boolean replace);
--      };

--      local interface ClientRequestInterceptor : Interceptor {

--          void send_request (in ClientRequestInfo ri)
--              raises (ForwardRequest);
--          void send_poll (in ClientRequestInfo ri);
--          void receive_reply (in ClientRequestInfo ri);
--          void receive_exception (in ClientRequestInfo ri)
--              raises (ForwardRequest);
--          void receive_other (in ClientRequestInfo ri)
--              raises (ForwardRequest);
--      };

--      local interface ServerRequestInterceptor : Interceptor {

--          void receive_request_service_contexts (in ServerRequestInfo ri)
--              raises (ForwardRequest);
--          void receive_request (in ServerRequestInfo ri)
--              raises (ForwardRequest);
--          void send_reply (in ServerRequestInfo ri);
--          void send_exception (in ServerRequestInfo ri)
--              raises (ForwardRequest);
--          void send_other (in ServerRequestInfo ri)
--              raises (ForwardRequest);
--      };

--  //PolyORB:NI:    abstract valuetype ObjectReferenceFactory {
--  //PolyORB:NI:	Object make_object( in string repository_id, 
--  //PolyORB:NI:            in ObjectId id ) ;
--  //PolyORB:NI:    };
--  //PolyORB:NI:
--  //PolyORB:NI:    abstract valuetype ObjectReferenceTemplate :
--  //PolyORB:NI:	ObjectReferenceFactory {
--  //PolyORB:NI:	readonly attribute ServerId server_id ;
--  //PolyORB:NI:	readonly attribute ORBId orb_id ;
--  //PolyORB:NI:        readonly attribute AdapterName adapter_name ;
--  //PolyORB:NI:    } ;
--  //PolyORB:NI:
--  //PolyORB:NI:    typedef sequence<ObjectReferenceTemplate> 
--  ObjectReferenceTemplateSeq;
--      typedef string AdapterManagerId;
--      typedef short AdapterState ;

--      const AdapterState  HOLDING      = 0 ;
--      const AdapterState  ACTIVE       = 1 ;
--      const AdapterState  DISCARDING   = 2 ;
--      const AdapterState  INACTIVE     = 3 ;
--      const AdapterState  NON_EXISTENT = 4 ;

--      local interface IORInfo {
--          CORBA::Policy get_effective_policy (in CORBA::PolicyType type);
--          void add_ior_component (
--              in IOP::TaggedComponent a_component);
--          void add_ior_component_to_profile (
--              in IOP::TaggedComponent a_component,
--              in IOP::ProfileId profile_id);
--      };

--      local interface IORInterceptor : Interceptor {
--          void establish_components (in IORInfo info);
--      };

--      local interface IORInterceptor_3_0 : IORInterceptor {
--  	void components_established( in IORInfo info );
--  	void adapter_manager_state_changed(
--  	    in AdapterManagerId id, 
--              in AdapterState state );
--  //PolyORB:NI:	void adapter_state_changed( 
--  //PolyORB:NI:	    in ObjectReferenceTemplateSeq templates,
--  //PolyORB:NI:	    in AdapterState state);
--      };

--      local interface PolicyFactory {
--          CORBA::Policy create_policy (
--              in CORBA::PolicyType type,
--              in any value)
--              raises (CORBA::PolicyError);
--      };

--      local interface ORBInitInfo {

--          typedef string ObjectId;

--          exception DuplicateName {
--              string name;
--          };

--          exception InvalidName {};

--          readonly attribute CORBA::StringSeq arguments;
--          readonly attribute string orb_id;
--          readonly attribute IOP::CodecFactory codec_factory;
--          void register_initial_reference (in ObjectId id, in Object obj)
--              raises (InvalidName);
--          Object resolve_initial_references(
--  	    in ObjectId id) raises (InvalidName);
--          void add_client_request_interceptor (
--              in ClientRequestInterceptor interceptor)
--              raises (DuplicateName);
--          void add_server_request_interceptor (
--              in ServerRequestInterceptor interceptor)
--              raises (DuplicateName);
--          void add_ior_interceptor (in IORInterceptor interceptor)
--              raises (DuplicateName);
--          SlotId allocate_slot_id ();
--          void register_policy_factory (
--              in CORBA::PolicyType type,
--              in PolicyFactory policy_factory);
--      };

--      local interface ORBInitializer {
--          void pre_init (in ORBInitInfo info);
--          void post_init (in ORBInitInfo info);
--      };

--  }; // module PortableInterceptor
--  #endif // _PORTABLE_INTERCEPTOR_IDL_

--  End source: 
--  /Users/heathdorn/Documents/Playground/Agents/RefactorTeam/code_refactor/PolyORB/idls/Misc/PortableInterceptor.idl
--   -- 229 lines

--  Source: 
--  /Users/heathdorn/Documents/Playground/Agents/RefactorTeam/code_refactor/PolyORB/idls/CORBA_IDL/orb.idl

--  // File: orb.idl
--  // From CORBA 3.0

--  // PolyORB Notes:
--  //   NI - Not Implemented
--  //   IL - Implementation Limitation

--  #ifndef _ORB_IDL_
--  #define _ORB_IDL_

--  //PolyORB:WAidlac: For now, idlac supports typeprefix statement only
--  //inside a scoped_name. This definition has been moved inside the
--  //CORBA module.
--  //#ifdef _PRE_3_0_COMPILER_
--  //#pragma prefix "omg.org"
--  //#else
--  //typeprefix CORBA "omg.org"
--  //#endif
--  //PolyORB:WAidlac:end

--  #ifdef _PRE_3_0_COMPILER_ 
--  #ifdef _NO_LOCAL_
--  #define local
--  #endif
--  #endif

--  // This module brings together many files defining the CORBA module
--  //   (It really ought to be called CORBA.idl, but that's history.)
--  // This file includes only the "real" interfaces that are included
--  //   in the "orb.idl" interface supplied by every ORB and that can be
--  //   brought into an IDL compilation by "import ::CORBA"
--  //   or in pre-3.0 IDL compilers by the include directive
--  //   "#include <orb.idl>".

--  module CORBA {

--  //PolyORB:WAidlac: For now, idlac supports typeprefix statement only
--  //inside a scoped_name. This definition has been moved inside the
--  //CORBA module.
--  #ifdef _PRE_3_0_COMPILER_
--  #pragma prefix "omg.org"
--  #else
--  typeprefix CORBA "omg.org"; // ";" suppresses iac warning about missing 
--  ";".
--  #endif
--  //PolyORB:WAidlac:end


--  // The following forward references list *all* the interfaces and 
--  valuetypes
--  //   defined in the CORBA module. This serves two purposes: documentation
--  //   and compilability. Documentation is nice: since some of the 
--  interfaces
--  //   must be declared as forward references, it is more consistent to
--  //   declare them all.
--  //
--  //   As far as compilability, it might be possible to avoid having to 
--  declare
--  //   many of the forward reference by rearranging the order of the 
--  interface 
--  //   declarations, but there's no reason to do bother doing that. After 
--  all,
--  //   that's the reason for the design of forward references. Doing a 
--  forward
--  //   reference allows the definition order to be relatively logical.In 
--  //   particular, it allows the "include"s to be done in chapter order 
--  //   (almost), the only exception being the InterfaceRepository (Chapter 
--  10). 
--  //   It contains some data definitions needed by Chapter 4 interfaces.
--  //   The other reason not to try to rearrange the order is that it's 
--  hard.

--  // Forward references, alphabetically
--  //PolyORB:NI:    interface ConstructionPolicy;        // Chapter  4, 
--  CORBA_DomainManager.idl
--      local interface Current;             // Chapter  4, CORBA_Current.idl
--      interface DomainManager;             // Chapter  4, 
--  CORBA_DomainManager.idl
--      interface Policy;                    // Chapter  4, CORBA_Policy.idl
--  //PolyORB:NI:    local interface PollableSet;         // Chapter  7, 
--  CORBA_Pollable.idl
--  //PolyORB:NI:    abstract valuetype CustomMarshal;    // Chapter  5, 
--  CORBA_valuetype.idl
--  //PolyORB:NI:    abstract valuetype DataInputStream;  // Chapter  5, 
--  CORBA_Stream.idl
--  //PolyORB:NI:    abstract valuetype DataOutputStream; // Chapter  5, 
--  CORBA_Stream.idl

--  // Forward references to Chapter 10, CORBA_InterfaceRepository.idl
--  //PolyORB:IL:    interface AbstractInterfaceDef;
--  //PolyORB:IL:    interface AliasDef;
--      interface ArrayDef;
--      interface AttributeDef;
--  //PolyORB:IL:    interface ConstantDef;
--      interface Contained;
--      interface Container;
--  //PolyORB:IL:    interface EnumDef;
--  //PolyORB:IL:    interface ExceptionDef;
--  //PolyORB:IL:    interface ExtInterfaceDef;
--  //PolyORB:NI:    interface ExtValueDef;
--  //PolyORB:IL:    interface ExtAbstractInterfaceDef;
--  //PolyORB:IL:    interface ExtLocalInterfaceDef;
--      interface FixedDef;
--  //PolyORB:IL:    interface IDLType;
--  //PolyORB:IL:    interface InterfaceDef;
--      interface IRObject;
--  //PolyORB:IL:    interface LocalInterfaceDef;
--  //PolyORB:IL:    interface ModuleDef;
--  //PolyORB:IL:    interface NativeDef;
--      interface OperationDef;
--      interface PrimitiveDef; 
--      interface Repository;
--      interface SequenceDef;
--      interface StringDef;
--  //PolyORB:IL:    interface StructDef;
--      interface TypeCode;
--      interface TypedefDef;
--  //PolyORB:IL:    interface UnionDef;
--  //PolyORB:IL:    interface ValueDef;
--  //PolyORB:IL:    interface ValueBoxDef;
--      interface ValueMemberDef;
--      interface WstringDef;

--      typedef string Identifier;

--  // Chapter 3: IDL Syntax and Semantics
--  #include <CORBA_StandardExceptions.idl>

--  // Chapter 4: ORB Interface
--  #include <CORBA_Current.idl>
--  #include <CORBA_Policy.idl>
--  #include <CORBA_DomainManager.idl>

--  // Chapter 7: Pollable
--  //PolyORB:NI:#include <CORBA_Pollable.idl>

--  // Chapter 10: The Interface Repository
--  #include <CORBA_InterfaceRepository.idl>

--  // more Chapter 4: ORB Interface
--  // CORBA_TypeCode.idl depends on CORBA_InterfaceRepository.idl
--  #include <CORBA_TypeCode.idl>

--  // Chapter 5: Value Type Semantics
--  //PolyORB:NI:#include <CORBA_CustomMarshal.idl>
--  #include <CORBA_Stream.idl>

--  //----------------------------------------------------------------------------
--  //PolyORB:AB: This code is copied from CORBA Pseudo IDL specification,
--  //primary because it define some entities, required for CORBA Services;
--  //and for completeness.

--  // The "define" fakes out the compiler to let it compile the "Context" 
--  //    interface and references to it even though "context" is a keyword
--  #define Context CContext

--  // The "define" fakes out the compiler to let it compile the "Object" 
--  //    interface and references to it even though "Object" is a keyword
--  #define Object OObject

--  // The "define" fakes out the compiler to let it compile the "ValueBase" 
--  //    valuetype and references to it even though "ValueBase" is a keyword
--  #define ValueBase VValueBase


--  // Forward references, alphabetically 
--      interface Context;                   // Chapter  7, CORBA_Context.idl
--      interface NVList;                    // Chapter  7, CORBA_NVList.idl
--      interface Object;                    // Chapter  4, CORBA_Object.idl
--      interface ORB;                       // Chapter  4, CORBA_ORB.idl
--      interface Request;                   // Chapter  7, CORBA_Request.idl
--      interface ServerRequest;             // Chapter  8, 
--  CORBA_ServerRequest.idl
--  //PolyORB:NI:    valuetype ValueBase;                 // Chapter  4, 
--  CORBA_ValueBase.idl

--      typedef unsigned long Flags;

--  // Chapter 4: ORB Interface
--  #include <CORBA_Object.idl>
--  #include <CORBA_ORB.idl>

--  //PolyORB:NI:// Chapter 5: Value Type Semantics
--  //PolyORB:NI:#include <CORBA_ValueBase.idl>

--  // Chapter 7: Dynamic Invocation Interface
--  #include <CORBA_Request.idl>
--  #include <CORBA_Context.idl>
--  #include <CORBA_NVList.idl>

--  //PolyORB:NI:// Chapter 8: Dynamic Skeleton Interface
--  #include <CORBA_ServerRequest.idl>

--  //PolyORB:AE:
--  //----------------------------------------------------------------------------

--  };

--  #undef Context
--  #undef Object
--  #undef ValueBase

--  #endif // _ORB_IDL_


--  End source: 
--  /Users/heathdorn/Documents/Playground/Agents/RefactorTeam/code_refactor/PolyORB/idls/CORBA_IDL/orb.idl
--   -- 188 lines

--  Source: 
--  /Users/heathdorn/Documents/Playground/Agents/RefactorTeam/code_refactor/PolyORB/idls/Interop/IOP.idl

--  // File: IOP.idl
--  // From CORBA 3.0: Chapter 13, ORB Interoperability Achitecture

--  // PolyORB:WAGCORBA This file has been updated to take into acocunt OMG
--  // Issue 5232 (anonymous sequence types are deprecated).

--  #ifndef _IOP_IDL_
--  #define _IOP_IDL_

--  #ifdef _PRE_3_0_COMPILER_ 
--  #pragma prefix "omg.org"

--  #include <orb.idl>
--  #else
--  import ::CORBA;
--  #endif // _PRE_3_0_COMPILER_

--  module IOP {

--  #ifndef _PRE_3_0_COMPILER_ 
--      typeprefix IOP "omg.org";
--  #endif // _PRE_3_0_COMPILER_

--      // IOR Profiles

--      // Standard Protocol Profile tag values 
--      typedef unsigned long           ProfileId;
--      const ProfileId                 TAG_INTERNET_IOP = 0;
--      const ProfileId                 TAG_MULTIPLE_COMPONENTS = 1;
--      const ProfileId                 TAG_SCCP_IOP = 2;

--      typedef CORBA::OctetSeq ProfileData;

--      struct TaggedProfile {
--          ProfileId                   tag;
--          ProfileData profile_data;
--      };
--      typedef sequence <TaggedProfile> TaggedProfileSeq ;
--     
--      // The IOR

--      // an Interoperable Object Reference is a sequence of
--      // object-specific protocol profiles, plus a type ID.
--      struct IOR {
--          string                      type_id;
--          TaggedProfileSeq            profiles;
--      };
--      

--      // IOR Components


--      // Standard way of representing multicomponent profiles.
--      // This would be encapsulated in a TaggedProfile.

--      typedef unsigned long ComponentId;
--      typedef CORBA::OctetSeq ComponentData;
--      struct TaggedComponent {
--          ComponentId                 tag;
--          ComponentData              component_data;
--      };

--      typedef sequence <TaggedComponent> TaggedComponentSeq;
--      typedef CORBA::OctetSeq ObjectKey;

--      typedef sequence <TaggedComponent> MultipleComponentProfile;

--      const ComponentId           TAG_ORB_TYPE                = 0;
--      const ComponentId           TAG_CODE_SETS               = 1;
--      const ComponentId           TAG_POLICIES                = 2;   
--      const ComponentId           TAG_ALTERNATE_IIOP_ADDRESS  = 3;
--      const ComponentId           TAG_ASSOCIATION_OPTIONS     = 13;
--      const ComponentId           TAG_SEC_NAME                = 14;
--      const ComponentId           TAG_SPKM_1_SEC_MECH         = 15;
--      const ComponentId           TAG_SPKM_2_SEC_MECH         = 16;
--      const ComponentId           TAG_KerberosV5_SEC_MECH     = 17;
--      const ComponentId           TAG_CSI_ECMA_Secret_SEC_MECH= 18;
--      const ComponentId           TAG_CSI_ECMA_Hybrid_SEC_MECH= 19;
--      const ComponentId           TAG_SSL_SEC_TRANS           = 20;
--      const ComponentId           TAG_CSI_ECMA_Public_SEC_MECH= 21;
--      const ComponentId           TAG_GENERIC_SEC_MECH        = 22;
--      const ComponentId           TAG_FIREWALL_TRANS          = 23; 
--      const ComponentId           TAG_SCCP_CONTACT_INFO       = 24; 
--      const ComponentId           TAG_JAVA_CODEBASE           = 25;

--      const ComponentId           TAG_TRANSACTION_POLICY      = 26;
--      const ComponentId           TAG_MESSAGE_ROUTER          = 30;
--      const ComponentId           TAG_OTS_POLICY              = 31;
--      const ComponentId           TAG_INV_POLICY              = 32;

--      const ComponentId           TAG_CSI_SEC_MECH_LIST       = 33;
--      const ComponentId           TAG_NULL_TAG                = 34;
--      const ComponentId           TAG_SECIOP_SEC_TRANS        = 35;

--      const ComponentId           TAG_TLS_SEC_TRANS           = 36;

--      const ComponentId           TAG_ACTIVITY_POLICY         = 37;
--   

--      const ComponentId           TAG_COMPLETE_OBJECT_KEY     = 5;
--      const ComponentId           TAG_ENDPOINT_ID_POSITION    = 6;
--      const ComponentId           TAG_LOCATION_POLICY         = 12;
--      const ComponentId           TAG_DCE_STRING_BINDING      = 100;
--      const ComponentId           TAG_DCE_BINDING_NAME        = 101;
--      const ComponentId           TAG_DCE_NO_PIPES            = 102;

--      const ComponentId           TAG_DCE_SEC_MECH            = 103;

--      const ComponentId           TAG_INET_SEC_TRANS          = 123;

--      // Service Contexts

--      typedef CORBA::OctetSeq ContextData;

--      typedef unsigned long       ServiceId;
--      struct ServiceContext {
--          ServiceId               context_id;
--          ContextData             context_data;
--      };
--      typedef sequence <ServiceContext> ServiceContextList;
--      const ServiceId             TransactionService          = 0;
--      const ServiceId             CodeSets                    = 1;
--      const ServiceId             ChainBypassCheck            = 2;
--      const ServiceId             ChainBypassInfo             = 3;
--      const ServiceId             LogicalThreadId             = 4;
--      const ServiceId             BI_DIR_IIOP                 = 5;
--      const ServiceId             SendingContextRunTime       = 6;
--      const ServiceId             INVOCATION_POLICIES         = 7;
--      const ServiceId             FORWARDED_IDENTITY          = 8;
--      const ServiceId             UnknownExceptionInfo        = 9;
--      const ServiceId             RTCorbaPriority             = 10;
--      const ServiceId             RTCorbaPriorityRange        = 11;
--      const ServiceId             FT_GROUP_VERSION            = 12;
--      const ServiceId             FT_REQUEST                  = 13;
--      const ServiceId             ExceptionDetailMessage      = 14;
--      const ServiceId             SecurityAttributeService    = 15;
--      const ServiceId             ActivityService             = 16;

--      // Coder Decoder from Portable Interceptor

--      local interface Codec {
--          exception InvalidTypeForEncoding {};
--          exception FormatMismatch {};
--          exception TypeMismatch {};

--          CORBA::OctetSeq encode (in any data)
--              raises (InvalidTypeForEncoding);
--          any decode (in CORBA::OctetSeq data)
--              raises (FormatMismatch);
--          CORBA::OctetSeq encode_value (in any data)
--              raises (InvalidTypeForEncoding);
--          any decode_value (
--              in CORBA::OctetSeq data,
--              in CORBA::TypeCode tc)
--              raises (FormatMismatch, TypeMismatch);
--      };

--      // Codec Factory

--      typedef short EncodingFormat;
--      const EncodingFormat ENCODING_CDR_ENCAPS = 0;

--      struct Encoding {
--          EncodingFormat format;
--          octet major_version;
--          octet minor_version;
--      };

--      local interface CodecFactory {
--          exception UnknownEncoding {};
--          Codec create_codec (in Encoding enc)
--              raises (UnknownEncoding);
--      };
--  };

--  // #include <IOP_DCE.idl>

--  #endif  // _IOP_IDL_


--  End source: 
--  /Users/heathdorn/Documents/Playground/Agents/RefactorTeam/code_refactor/PolyORB/idls/Interop/IOP.idl
--   -- 180 lines

--  Source: 
--  /Users/heathdorn/Documents/Playground/Agents/RefactorTeam/code_refactor/PolyORB/idls/Misc/Messaging.idl

--  // File: Messaging.idl
--  // CORBA 3.0, Chapter 22

--  #ifndef _MESSAGING_IDL_
--  #define _MESSAGING_IDL_

--  #ifdef _PRE_3_0_COMPILER_ 
--  #pragma prefix "omg.org"
--  #include <orb.idl>
--  #include <Dynamic.idl>
--  #include <IOP.idl>
--  #include <TimeBase.idl>
--  #else
--  import ::CORBA;
--  //PolyORB:NI:import ::Dynamic;
--  //PolyORB:NI:import ::IOP;
--  //PolyORB:NI:import ::TimeBase;
--  #endif // _PRE_3_0_COMPILER_

--  // App developers should never have to use this IDL file. The ORB vendor
--  // should supply an implementation language version of this file, and
--  // that should be used by app developers if necessary.

--  // Most IDL compilers don't accept the "native" keyword in application 
--  IDL 
--  //    files. In order to compile an IDL (really PIDL) file that has it, 
--  the 
--  //    following trick can be used: change what the compiler sees. Instead
--  //    of letting the compiler see the keyword "native", use a 
--  preprocessor
--  //    definition that results in valid IDL, even if it doesn't yield
--  //    useful stubs and skeletons. Of course, PIDL never results in
--  //    the standard stubs so that's not a problem.
--  //
--  // Set the variable _MASK_NATIVE_ in the IDL compiler to enable it to
--  // parse this file.

--  #ifdef _MASK_NATIVE_
--  #define native typedef long
--  #endif // _MASK_NATIVE_

--  module Messaging {

--  #ifndef _PRE_3_0_COMPILER_ 
--      typeprefix Messaging "omg.org";
--  #endif // _PRE_3_0_COMPILER_

--      typedef short RebindMode;
--      const RebindMode TRANSPARENT = 0;
--      const RebindMode NO_REBIND = 1;
--      const RebindMode NO_RECONNECT = 2;

--      typedef short SyncScope;
--      const SyncScope SYNC_NONE = 0;
--      const SyncScope SYNC_WITH_TRANSPORT = 1;
--      const SyncScope SYNC_WITH_SERVER = 2;
--      const SyncScope SYNC_WITH_TARGET = 3;

--      typedef short RoutingType;
--      const RoutingType ROUTE_NONE = 0;
--      const RoutingType ROUTE_FORWARD = 1;
--      const RoutingType ROUTE_STORE_AND_FORWARD =2;

--      typedef short Priority;

--      typedef unsigned short Ordering;
--      const Ordering ORDER_ANY = 0x01;
--      const Ordering ORDER_TEMPORAL = 0x02;
--      const Ordering ORDER_PRIORITY = 0x04;
--      const Ordering ORDER_DEADLINE = 0x08;

--      // Rebind Policy (default = TRANSPARENT)
--      const CORBA::PolicyType REBIND_POLICY_TYPE = 23;

--  //PolyORB:NI:    local interface RebindPolicy : CORBA::Policy {
--  //PolyORB:NI:        readonly attribute RebindMode rebind_mode;
--  //PolyORB:NI:    };

--      // Synchronization Policy (default = SYNC_WITH_TRANSPORT)
--      const CORBA::PolicyType SYNC_SCOPE_POLICY_TYPE = 24;

--  //PolyORB:NI:    local interface SyncScopePolicy : CORBA::Policy {
--  //PolyORB:NI:        readonly attribute SyncScope synchronization;
--  //PolyORB:NI:    };

--      // Priority Policies
--      const CORBA::PolicyType REQUEST_PRIORITY_POLICY_TYPE = 25;

--      struct PriorityRange {
--          Priority min;
--          Priority max;
--      };

--  //PolyORB:NI:    local interface RequestPriorityPolicy : CORBA::Policy {
--  //PolyORB:NI:        readonly attribute PriorityRange priority_range;
--  //PolyORB:NI:    };

--      const CORBA::PolicyType REPLY_PRIORITY_POLICY_TYPE = 26;

--  //PolyORB:NI:    interface ReplyPriorityPolicy : CORBA::Policy {
--  //PolyORB:NI:        readonly attribute PriorityRange priority_range;
--  //PolyORB:NI:    };

--      // Timeout Policies
--      const CORBA::PolicyType REQUEST_START_TIME_POLICY_TYPE = 27;

--  //PolyORB:NI:    local interface RequestStartTimePolicy : CORBA::Policy {
--  //PolyORB:NI:        readonly attribute TimeBase::UtcT start_time;
--  //PolyORB:NI:    };

--      const CORBA::PolicyType REQUEST_END_TIME_POLICY_TYPE = 28;

--  //PolyORB:NI:    local interface RequestEndTimePolicy : CORBA::Policy {
--  //PolyORB:NI:        readonly attribute TimeBase::UtcT end_time;
--  //PolyORB:NI:    };

--      const CORBA::PolicyType REPLY_START_TIME_POLICY_TYPE = 29;

--  //PolyORB:NI:    local interface ReplyStartTimePolicy : CORBA::Policy {
--  //PolyORB:NI:        readonly attribute TimeBase::UtcT start_time;
--  //PolyORB:NI:    };

--      const CORBA::PolicyType REPLY_END_TIME_POLICY_TYPE = 30;

--  //PolyORB:NI:    local interface ReplyEndTimePolicy : CORBA::Policy {
--  //PolyORB:NI:        readonly attribute TimeBase::UtcT end_time;
--  //PolyORB:NI:    };

--      const CORBA::PolicyType RELATIVE_REQ_TIMEOUT_POLICY_TYPE = 31;

--  //PolyORB:NI:    local interface RelativeRequestTimeoutPolicy : 
--  CORBA::Policy {
--  //PolyORB:NI:        readonly attribute TimeBase::TimeT relative_expiry;
--  //PolyORB:NI:    };

--      const CORBA::PolicyType RELATIVE_RT_TIMEOUT_POLICY_TYPE = 32;

--  //PolyORB:NI:    local interface RelativeRoundtripTimeoutPolicy : 
--  CORBA::Policy {
--  //PolyORB:NI:        readonly attribute TimeBase::TimeT relative_expiry;
--  //PolyORB:NI:    };

--      const CORBA::PolicyType ROUTING_POLICY_TYPE = 33;

--      struct RoutingTypeRange {
--          RoutingType min;
--          RoutingType max;
--      };

--  //PolyORB:NI:    local interface RoutingPolicy : CORBA::Policy {
--  //PolyORB:NI:        readonly attribute RoutingTypeRange routing_range;
--  //PolyORB:NI:    };

--      const CORBA::PolicyType MAX_HOPS_POLICY_TYPE = 34;

--  //PolyORB:NI:    local interface MaxHopsPolicy : CORBA::Policy {
--  //PolyORB:NI:        readonly attribute unsigned short max_hops;
--  //PolyORB:NI:    };

--      // Router Delivery-ordering Policy (default = ORDER_TEMPORAL)
--      const CORBA::PolicyType QUEUE_ORDER_POLICY_TYPE = 35;

--  //PolyORB:NI:    local interface QueueOrderPolicy : CORBA::Policy {
--  //PolyORB:NI:        readonly attribute Ordering allowed_orders;
--  //PolyORB:NI:    };

--      // Profile components through which policy values are expressed in 
--  IORs

--      struct PolicyValue {
--          CORBA::PolicyType ptype;
--          sequence<octet> pvalue;
--      };

--      typedef sequence<PolicyValue> PolicyValueSeq;

--  //PolyORB:NI:    native UserExceptionBase;
--  //PolyORB:NI:    valuetype ExceptionHolder {
--  //PolyORB:NI:      void raise_exception() raises (UserExceptionBase);
--  //PolyORB:NI:      void raise_exception_with_list(
--  //PolyORB:NI:	  in Dynamic::ExceptionList exc_list)
--  //PolyORB:NI:	raises (UserExceptionBase);
--  //PolyORB:NI:      private boolean is_system_exception;
--  //PolyORB:NI:      private boolean byte_order;
--  //PolyORB:NI:      private sequence<octet> marshaled_exception;
--  //PolyORB:NI:    };
--  //PolyORB:NI:
--  //PolyORB:NI:    // For handling Routing
--  //PolyORB:NI:    interface ReplyHandler { };
--  //PolyORB:NI:
--  //PolyORB:NI:    // Generic Poller Valuetype
--  //PolyORB:NI:
--  //PolyORB:NI:    valuetype Poller : CORBA::Pollable {
--  //PolyORB:NI:        readonly attribute Object operation_target;
--  //PolyORB:NI:        readonly attribute string operation_name;
--  //PolyORB:NI:        attribute ReplyHandler associated_handler;
--  //PolyORB:NI:        readonly attribute boolean is_from_poller;
--  //PolyORB:NI:        private Object target;
--  //PolyORB:NI:        private string op_name;
--  //PolyORB:NI:    };

--  }; // module Messaging
--  #endif // _MESSAGING_IDL_

--  End source: 
--  /Users/heathdorn/Documents/Playground/Agents/RefactorTeam/code_refactor/PolyORB/idls/Misc/Messaging.idl
--   -- 198 lines

--  Source: 
--  /Users/heathdorn/Documents/Playground/Agents/RefactorTeam/code_refactor/PolyORB/idls/Misc/Dynamic.idl

--  // File: Dynamic.idl
--  // CORBA 3.0, Chapter 21

--  #ifndef _DYNAMIC_IDL_
--  #define _DYNAMIC_IDL_

--  #ifdef _PRE_3_0_COMPILER_ 
--  #pragma prefix "omg.org"
--  #include <orb.idl>
--  #else
--  import ::CORBA;
--  #endif // _PRE_3_0_COMPILER_

--  module Dynamic {
--  #ifndef _PRE_3_0_COMPILER_ 
--      typeprefix Dynamic "omg.org";
--  #endif // _PRE_3_0_COMPILER_

--      struct Parameter {
--          any argument;
--          CORBA::ParameterMode mode;
--      };

--      typedef sequence<Parameter> ParameterList;
--      typedef CORBA::StringSeq ContextList;
--      typedef sequence<CORBA::TypeCode> ExceptionList;
--      typedef CORBA::StringSeq RequestContext;

--  }; // module Dynamic
--  #endif // _DYNAMIC_IDL_

--  End source: 
--  /Users/heathdorn/Documents/Playground/Agents/RefactorTeam/code_refactor/PolyORB/idls/Misc/Dynamic.idl
--   -- 31 lines

---------------------------------------------------

with PolyORB.Std;
with CORBA;
pragma Elaborate_All (CORBA);
with CORBA.Object;
with Ada.Exceptions;
with CORBA.IDL_Sequences;

package PortableInterceptor is

   Repository_Id : constant PolyORB.Std.String :=
     "IDL:omg.org/PortableInterceptor:1.0";

   ForwardRequest : exception;

   type ForwardRequest_Members is
     new CORBA.Idl_Exception_Members with record
         forward : CORBA.Object.Ref;
      end record;

   ForwardRequest_Repository_Id : constant PolyORB.Std.String :=
     "IDL:omg.org/PortableInterceptor/ForwardRequest:1.0";

   procedure Get_Members
     (From : Ada.Exceptions.Exception_Occurrence;
      To : out PortableInterceptor.ForwardRequest_Members);

   type ReplyStatus is
     new CORBA.Short;

   ReplyStatus_Repository_Id : constant PolyORB.Std.String :=
     "IDL:omg.org/PortableInterceptor/ReplyStatus:1.0";

   SUCCESSFUL : constant PortableInterceptor.ReplyStatus :=
     0;

   SYSTEM_EXCEPTION : constant PortableInterceptor.ReplyStatus :=
     1;

   USER_EXCEPTION : constant PortableInterceptor.ReplyStatus :=
     2;

   LOCATION_FORWARD : constant PortableInterceptor.ReplyStatus :=
     3;

   TRANSPORT_RETRY : constant PortableInterceptor.ReplyStatus :=
     4;

   UNKNOWN : constant PortableInterceptor.ReplyStatus :=
     5;

   type SlotId is
     new CORBA.Unsigned_Long;

   SlotId_Repository_Id : constant PolyORB.Std.String :=
     "IDL:omg.org/PortableInterceptor/SlotId:1.0";

   InvalidSlot : exception;

   type InvalidSlot_Members is
     new CORBA.Idl_Exception_Members with null record;

   InvalidSlot_Repository_Id : constant PolyORB.Std.String :=
     "IDL:omg.org/PortableInterceptor/InvalidSlot:1.0";

   procedure Get_Members
     (From : Ada.Exceptions.Exception_Occurrence;
      To : out PortableInterceptor.InvalidSlot_Members);

   type ServerId is
     new CORBA.String;

   ServerId_Repository_Id : constant PolyORB.Std.String :=
     "IDL:omg.org/PortableInterceptor/ServerId:1.0";

   type ORBId is
     new CORBA.String;

   ORBId_Repository_Id : constant PolyORB.Std.String :=
     "IDL:omg.org/PortableInterceptor/ORBId:1.0";

   type AdapterName is
     new CORBA.IDL_Sequences.StringSeq;

   AdapterName_Repository_Id : constant PolyORB.Std.String :=
     "IDL:omg.org/PortableInterceptor/AdapterName:1.0";

   type ObjectId is
     new CORBA.IDL_Sequences.OctetSeq;

   ObjectId_Repository_Id : constant PolyORB.Std.String :=
     "IDL:omg.org/PortableInterceptor/ObjectId:1.0";

   type AdapterManagerId is
     new CORBA.String;

   AdapterManagerId_Repository_Id : constant PolyORB.Std.String :=
     "IDL:omg.org/PortableInterceptor/AdapterManagerId:1.0";

   type AdapterState is
     new CORBA.Short;

   AdapterState_Repository_Id : constant PolyORB.Std.String :=
     "IDL:omg.org/PortableInterceptor/AdapterState:1.0";

   HOLDING : constant PortableInterceptor.AdapterState :=
     0;

   ACTIVE : constant PortableInterceptor.AdapterState :=
     1;

   DISCARDING : constant PortableInterceptor.AdapterState :=
     2;

   INACTIVE : constant PortableInterceptor.AdapterState :=
     3;

   NON_EXISTENT : constant PortableInterceptor.AdapterState :=
     4;

end PortableInterceptor;

pragma Style_Checks ("NM32766");
pragma Wide_Character_Encoding (Brackets);
pragma Warnings (Off, "use of an anonymous access type allocator");

---------------------------------------------------
--  This file has been generated automatically from
--  /Users/heathdorn/Documents/Playground/Agents/RefactorTeam/code_refactor/PolyORB/idls/RTCORBA/RTCosScheduling.idl
--  by IAC (IDL to Ada Compiler) 20.0w (rev. 41a9b833).
---------------------------------------------------
--  NOTE: If you modify this file by hand, your
--  changes will be lost when you re-run the
--  IDL to Ada compiler.
---------------------------------------------------

--  Source: 
--  /Users/heathdorn/Documents/Playground/Agents/RefactorTeam/code_refactor/PolyORB/idls/RTCORBA/RTCosScheduling.idl

--  //File: RTCosScheduling.idl
--  #ifndef _RT_COS_SCHEDULING_
--  #define _RT_COS_SCHEDULING_

--  #ifdef _PRE_3_0_COMPILER_
--  #pragma prefix "omg.org"

--  #include <orb.idl>
--  #include <PortableServer.idl>
--  #else
--  import ::CORBA;
--  import ::PortableServer;
--  #endif

--  // IDL
--  module RTCosScheduling {
--    exception UnknownName {};

--    // locality constrained interface
--    local interface ClientScheduler {

--      void schedule_activity(in string activity_name )
--        raises (UnknownName);
--      };

--    // locality constrained interface
--    local interface ServerScheduler {

--      PortableServer::POA create_POA (
--        in PortableServer::POA parent,
--        in string adapter_name,
--        in PortableServer::POAManager a_POAManager,
--        in CORBA::PolicyList policies)
--          raises (PortableServer::POA::AdapterAlreadyExists,
--                  PortableServer::POA::InvalidPolicy);

--      void schedule_object(in Object obj, in string name)
--        raises (UnknownName);

--    };
--  };
--  #endif // _RT_COS_SCHEDULING_IDL_

--  End source: 
--  /Users/heathdorn/Documents/Playground/Agents/RefactorTeam/code_refactor/PolyORB/idls/RTCORBA/RTCosScheduling.idl
--   -- 43 lines

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
--  /Users/heathdorn/Documents/Playground/Agents/RefactorTeam/code_refactor/PolyORB/idls/Misc/PortableServer.idl

--  // File: PortableServer.idl
--  // CORBA 3.0, Chapter 11

--  #ifndef _PORTABLE_SERVER_IDL_
--  #define _PORTABLE_SERVER_IDL_

--  #ifdef _PRE_3_0_COMPILER_ 
--  #pragma prefix "omg.org"

--  #include <orb.idl>
--  #else
--  import ::CORBA;
--  #endif // _PRE_3_0_COMPILER_

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

--  module PortableServer {

--  #ifndef _PRE_3_0_COMPILER_ 
--      typeprefix PortableServer "omg.org";
--  #endif // _PRE_3_0_COMPILER_

--      local interface POA;                          // forward declaration
--      typedef sequence<POA> POAList; 
--      native Servant;
--      typedef CORBA::OctetSeq ObjectId;
--      exception ForwardRequest {
--          Object forward_reference;
--      };
--      
--      // Policy interfaces
--      const CORBA::PolicyType THREAD_POLICY_ID                = 16;
--      const CORBA::PolicyType LIFESPAN_POLICY_ID              = 17;
--      const CORBA::PolicyType ID_UNIQUENESS_POLICY_ID         = 18;
--      const CORBA::PolicyType ID_ASSIGNMENT_POLICY_ID         = 19;
--      const CORBA::PolicyType IMPLICIT_ACTIVATION_POLICY_ID   = 20;
--      const CORBA::PolicyType SERVANT_RETENTION_POLICY_ID     = 21;
--      const CORBA::PolicyType REQUEST_PROCESSING_POLICY_ID    = 22;
--      
--      enum ThreadPolicyValue {
--          ORB_CTRL_MODEL,
--          SINGLE_THREAD_MODEL,
--  	MAIN_THREAD_MODEL
--      };
--      local interface ThreadPolicy : CORBA::Policy {
--          readonly attribute ThreadPolicyValue value;
--      };
--      
--      enum LifespanPolicyValue {
--          TRANSIENT,
--          PERSISTENT
--      };
--      local interface LifespanPolicy : CORBA::Policy {
--          readonly attribute LifespanPolicyValue value;
--      };
--      
--      enum IdUniquenessPolicyValue {
--          UNIQUE_ID,
--          MULTIPLE_ID
--      };
--      local interface IdUniquenessPolicy : CORBA::Policy {
--          readonly attribute IdUniquenessPolicyValue value;
--      };
--      
--      enum IdAssignmentPolicyValue {
--          USER_ID,
--          SYSTEM_ID
--      };
--      local interface IdAssignmentPolicy : CORBA::Policy {
--          readonly attribute IdAssignmentPolicyValue value;
--      };
--      
--      enum ImplicitActivationPolicyValue {
--          IMPLICIT_ACTIVATION,
--          NO_IMPLICIT_ACTIVATION
--      };
--      local interface ImplicitActivationPolicy : CORBA::Policy {
--          readonly attribute ImplicitActivationPolicyValue value;
--      };
--      
--      enum ServantRetentionPolicyValue {
--          RETAIN,
--          NON_RETAIN
--      };
--      local interface ServantRetentionPolicy : CORBA::Policy {
--          readonly attribute ServantRetentionPolicyValue value;
--      };
--      
--      enum RequestProcessingPolicyValue {
--          USE_ACTIVE_OBJECT_MAP_ONLY,
--          USE_DEFAULT_SERVANT,
--          USE_SERVANT_MANAGER
--      };
--      local interface RequestProcessingPolicy : CORBA::Policy {
--          readonly attribute RequestProcessingPolicyValue value;
--      };
--      
--      // POAManager interface
--      local interface POAManager {
--          exception AdapterInactive{};
--          enum State {HOLDING, ACTIVE, DISCARDING, INACTIVE};
--          void activate()             raises(AdapterInactive);
--          void hold_requests(     in boolean wait_for_completion)
--                                      raises(AdapterInactive);
--          void discard_requests(  in boolean wait_for_completion)
--                                      raises(AdapterInactive);
--          void deactivate(        in boolean etherealize_objects, 
--                                  in boolean wait_for_completion)
--                                      raises(AdapterInactive);
--          State get_state();
--  //PolyORB:NI:        string get_id();
--      };
--      
--  //PolyORB:NI:    // PoaManagerFactory
--  //PolyORB:NI:    local interface POAManagerFactory {
--  //PolyORB:NI:        typedef sequence<POAManager> POAManagerSeq;
--  //PolyORB:NI:        exception ManagerAlreadyExists {};
--  //PolyORB:NI:        POAManager create_POAManager(
--  //PolyORB:NI:	    in string id,
--  //PolyORB:NI:	    in CORBA::PolicyList policies
--  //PolyORB:NI:	) raises(ManagerAlreadyExists, CORBA::PolicyError);
--  //PolyORB:NI:        POAManagerSeq list();
--  //PolyORB:NI:        POAManager find( in string id);
--  //PolyORB:NI:    };

--      // AdapterActivator interface
--      local interface AdapterActivator {
--          boolean unknown_adapter(in POA parent, 
--                                  in string name);
--      };
--      
--      // ServantManager interface
--      local interface ServantManager{ };
--      
--      local interface ServantActivator : ServantManager {
--          Servant incarnate (     in ObjectId oid,
--                                  in POA      adapter)
--                                      raises (ForwardRequest);
--          void etherealize (      in ObjectId oid, 
--                                  in POA      adapter,
--                                  in Servant  serv,
--                                  in boolean  cleanup_in_progress,
--                                  in boolean  remaining_activations);
--      };
--      
--      local interface ServantLocator : ServantManager {
--          native  Cookie;
--  //PolyORB:IL:        Servant preinvoke(      in ObjectId oid,
--  //PolyORB:IL:                                in POA      adapter,
--  //PolyORB:IL:                                in CORBA::Identifier 
--  //PolyORB:IL:                                            operation,
--  //PolyORB:IL:                                out Cookie  the_cookie)
--  //PolyORB:IL:                                    raises (ForwardRequest);
--  //PolyORB:IL:        void postinvoke(        in ObjectId oid,
--  //PolyORB:IL:                                in POA      adapter,
--  //PolyORB:IL:                                in CORBA::Identifier
--  //PolyORB:IL:                                            operation,
--  //PolyORB:IL:                                in Cookie   the_cookie,
--  //PolyORB:IL:                                in Servant  the_servant );
--      };
--      
--      local interface POA {
--          exception AdapterAlreadyExists {};
--          exception AdapterNonExistent {};
--          exception InvalidPolicy {unsigned short index;};
--          exception NoServant {};
--          exception ObjectAlreadyActive {};
--          exception ObjectNotActive {};
--          exception ServantAlreadyActive {};
--          exception ServantNotActive {};
--          exception WrongAdapter {};
--          exception WrongPolicy {};

--          // POA creation and destruction
--          POA create_POA(         in string       adapter_name,
--                                  in POAManager   a_POAManager,
--                                  in CORBA::PolicyList policies)
--                                      raises (AdapterAlreadyExists, 
--  InvalidPolicy);
--          POA find_POA(           in string       adapter_name, 
--                                  in boolean      activate_it)
--                                      raises (AdapterNonExistent);
--          void destroy(           in boolean      etherealize_objects, 
--                                  in boolean      wait_for_completion);

--          // Factories for Policy objects
--          ThreadPolicy        create_thread_policy(in ThreadPolicyValue 
--  value);
--          LifespanPolicy      
--              create_lifespan_policy(in LifespanPolicyValue value);
--          IdUniquenessPolicy  create_id_uniqueness_policy(
--                                  in IdUniquenessPolicyValue value);
--          IdAssignmentPolicy  create_id_assignment_policy(
--                                  in IdAssignmentPolicyValue value);
--          ImplicitActivationPolicy create_implicit_activation_policy(
--                                  in ImplicitActivationPolicyValue value);
--          ServantRetentionPolicy create_servant_retention_policy(
--                                  in ServantRetentionPolicyValue value);
--          RequestProcessingPolicy create_request_processing_policy(
--                                  in RequestProcessingPolicyValue value);
--          
--          // POA attributes
--          readonly attribute string       the_name;
--          readonly attribute POA          the_parent;
--          readonly attribute POAList      the_children; 
--          readonly attribute POAManager   the_POAManager;
--          attribute AdapterActivator      the_activator;
--          
--          // Servant Manager registration:
--          ServantManager  get_servant_manager()    
--  	  raises (WrongPolicy);
--          void            set_servant_manager(in      ServantManager imgr)
--  	  raises (WrongPolicy);
--          
--          // operations for the USE_DEFAULT_SERVANT policy
--          Servant         get_servant() raises (NoServant, WrongPolicy);
--          void            set_servant( in Servant      p_servant) 
--  	  raises (WrongPolicy);
--          
--          // object activation and deactivation
--          ObjectId        activate_object(in Servant p_servant)
--  	  raises (ServantAlreadyActive, WrongPolicy);
--          void activate_object_with_id(in ObjectId    id,
--                                       in Servant     p_servant)
--  	  raises (ServantAlreadyActive, 
--  		  ObjectAlreadyActive,  
--  		  WrongPolicy);
--          void deactivate_object(      in ObjectId oid) 
--  	  raises (ObjectNotActive, WrongPolicy);
--          
--          // reference creation operations
--          Object create_reference (    in CORBA::RepositoryId intf)
--  	  raises (WrongPolicy);
--          Object create_reference_with_id (
--                                      in ObjectId             oid,
--                                      in CORBA::RepositoryId  intf);
--          
--          // Identity mapping operations:
--          ObjectId servant_to_id(     in Servant              p_servant) 
--  	  raises (ServantNotActive, WrongPolicy);
--          Object servant_to_reference(in Servant              p_servant) 
--  	  raises (ServantNotActive, WrongPolicy);
--          Servant reference_to_servant(in Object              reference)
--  	  raises(ObjectNotActive, WrongAdapter, WrongPolicy);
--          ObjectId reference_to_id(   in Object               reference)
--  	  raises (WrongAdapter, WrongPolicy);
--          Servant id_to_servant(      in ObjectId             oid)
--  	  raises (ObjectNotActive, WrongPolicy);
--          Object id_to_reference(     in ObjectId             oid)
--  	  raises (ObjectNotActive, WrongPolicy);

--  //PolyORB:NI:        readonly attribute CORBA::OctetSeq id;
--  //PolyORB:NI:        readonly attribute POAManagerFactory 
--  the_POAManagerFactory;
--      };
--      
--      // Current interface
--      local interface Current : CORBA::Current {
--          exception   NoContext { };
--          POA         get_POA()       raises (NoContext);
--          ObjectId    get_object_id() raises (NoContext);
--          Object      get_reference() raises (NoContext);
--          Servant     get_servant()   raises (NoContext);
--      };
--  };
--  #endif // _PORTABLE_SERVER_IDL_

--  End source: 
--  /Users/heathdorn/Documents/Playground/Agents/RefactorTeam/code_refactor/PolyORB/idls/Misc/PortableServer.idl
--   -- 277 lines

---------------------------------------------------

with PolyORB.Std;
with CORBA;
pragma Elaborate_All (CORBA);
with Ada.Exceptions;

package RTCosScheduling is

   Repository_Id : constant PolyORB.Std.String :=
     "IDL:RTCosScheduling:1.0";

   UnknownName : exception;

   type UnknownName_Members is
     new CORBA.Idl_Exception_Members with null record;

   UnknownName_Repository_Id : constant PolyORB.Std.String :=
     "IDL:RTCosScheduling/UnknownName:1.0";

   procedure Get_Members
     (From : Ada.Exceptions.Exception_Occurrence;
      To : out RTCosScheduling.UnknownName_Members);

end RTCosScheduling;

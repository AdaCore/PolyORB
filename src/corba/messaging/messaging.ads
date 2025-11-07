pragma Style_Checks ("NM32766");
pragma Wide_Character_Encoding (Brackets);
pragma Warnings (Off, "use of an anonymous access type allocator");

---------------------------------------------------
--  This file has been generated automatically from
--  /Users/heathdorn/Documents/Playground/Agents/RefactorTeam/code_refactor/PolyORB/idls/Misc/Messaging.idl
--  by IAC (IDL to Ada Compiler) 20.0w (rev. 41a9b833).
---------------------------------------------------
--  NOTE: If you modify this file by hand, your
--  changes will be lost when you re-run the
--  IDL to Ada compiler.
---------------------------------------------------

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

---------------------------------------------------

with PolyORB.Std;
with CORBA;
pragma Elaborate_All (CORBA);
with CORBA.Sequences.Unbounded;

package Messaging is

   Repository_Id : constant PolyORB.Std.String :=
     "IDL:omg.org/Messaging:1.0";

   type RebindMode is
     new CORBA.Short;

   RebindMode_Repository_Id : constant PolyORB.Std.String :=
     "IDL:omg.org/Messaging/RebindMode:1.0";

   TRANSPARENT : constant Messaging.RebindMode :=
     0;

   NO_REBIND : constant Messaging.RebindMode :=
     1;

   NO_RECONNECT : constant Messaging.RebindMode :=
     2;

   type SyncScope is
     new CORBA.Short;

   SyncScope_Repository_Id : constant PolyORB.Std.String :=
     "IDL:omg.org/Messaging/SyncScope:1.0";

   SYNC_NONE : constant Messaging.SyncScope :=
     0;

   SYNC_WITH_TRANSPORT : constant Messaging.SyncScope :=
     1;

   SYNC_WITH_SERVER : constant Messaging.SyncScope :=
     2;

   SYNC_WITH_TARGET : constant Messaging.SyncScope :=
     3;

   type RoutingType is
     new CORBA.Short;

   RoutingType_Repository_Id : constant PolyORB.Std.String :=
     "IDL:omg.org/Messaging/RoutingType:1.0";

   ROUTE_NONE : constant Messaging.RoutingType :=
     0;

   ROUTE_FORWARD : constant Messaging.RoutingType :=
     1;

   ROUTE_STORE_AND_FORWARD : constant Messaging.RoutingType :=
     2;

   type Priority is
     new CORBA.Short;

   Priority_Repository_Id : constant PolyORB.Std.String :=
     "IDL:omg.org/Messaging/Priority:1.0";

   type Ordering is
     new CORBA.Unsigned_Short;

   Ordering_Repository_Id : constant PolyORB.Std.String :=
     "IDL:omg.org/Messaging/Ordering:1.0";

   ORDER_ANY : constant Messaging.Ordering :=
     16#1#;

   ORDER_TEMPORAL : constant Messaging.Ordering :=
     16#2#;

   ORDER_PRIORITY : constant Messaging.Ordering :=
     16#4#;

   ORDER_DEADLINE : constant Messaging.Ordering :=
     16#8#;

   REBIND_POLICY_TYPE : constant CORBA.PolicyType :=
     23;

   SYNC_SCOPE_POLICY_TYPE : constant CORBA.PolicyType :=
     24;

   REQUEST_PRIORITY_POLICY_TYPE : constant CORBA.PolicyType :=
     25;

   type PriorityRange is
     record
         min : Messaging.Priority;
         max : Messaging.Priority;
      end record;

   PriorityRange_Repository_Id : constant PolyORB.Std.String :=
     "IDL:omg.org/Messaging/PriorityRange:1.0";

   REPLY_PRIORITY_POLICY_TYPE : constant CORBA.PolicyType :=
     26;

   REQUEST_START_TIME_POLICY_TYPE : constant CORBA.PolicyType :=
     27;

   REQUEST_END_TIME_POLICY_TYPE : constant CORBA.PolicyType :=
     28;

   REPLY_START_TIME_POLICY_TYPE : constant CORBA.PolicyType :=
     29;

   REPLY_END_TIME_POLICY_TYPE : constant CORBA.PolicyType :=
     30;

   RELATIVE_REQ_TIMEOUT_POLICY_TYPE : constant CORBA.PolicyType :=
     31;

   RELATIVE_RT_TIMEOUT_POLICY_TYPE : constant CORBA.PolicyType :=
     32;

   ROUTING_POLICY_TYPE : constant CORBA.PolicyType :=
     33;

   type RoutingTypeRange is
     record
         min : Messaging.RoutingType;
         max : Messaging.RoutingType;
      end record;

   RoutingTypeRange_Repository_Id : constant PolyORB.Std.String :=
     "IDL:omg.org/Messaging/RoutingTypeRange:1.0";

   MAX_HOPS_POLICY_TYPE : constant CORBA.PolicyType :=
     34;

   QUEUE_ORDER_POLICY_TYPE : constant CORBA.PolicyType :=
     35;

   package IDL_SEQUENCE_octet is
     new CORBA.Sequences.Unbounded
        (CORBA.Octet);

   type IDL_AT_Sequence_octet is
     new Messaging.IDL_SEQUENCE_octet.Sequence;

   IDL_AT_Sequence_octet_Repository_Id : constant PolyORB.Std.String :=
     "IDL:omg.org/Messaging/IDL_AT_Sequence_octet:1.0";

   type PolicyValue is
     record
         ptype : CORBA.PolicyType;
         pvalue : Messaging.IDL_AT_Sequence_octet;
      end record;

   PolicyValue_Repository_Id : constant PolyORB.Std.String :=
     "IDL:omg.org/Messaging/PolicyValue:1.0";

   package IDL_SEQUENCE_Messaging_PolicyValue is
     new CORBA.Sequences.Unbounded
        (Messaging.PolicyValue);

   type PolicyValueSeq is
     new Messaging.IDL_SEQUENCE_Messaging_PolicyValue.Sequence;

   PolicyValueSeq_Repository_Id : constant PolyORB.Std.String :=
     "IDL:omg.org/Messaging/PolicyValueSeq:1.0";

end Messaging;

pragma Style_Checks ("NM32766");
pragma Wide_Character_Encoding (Brackets);
pragma Warnings (Off, "use of an anonymous access type allocator");

---------------------------------------------------
--  This file has been generated automatically from
--  /Users/heathdorn/Documents/Playground/Agents/RefactorTeam/code_refactor/PolyORB/idls/Interop/IOP.idl
--  by IAC (IDL to Ada Compiler) 20.0w (rev. 41a9b833).
---------------------------------------------------
--  NOTE: If you modify this file by hand, your
--  changes will be lost when you re-run the
--  IDL to Ada compiler.
---------------------------------------------------

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
with CORBA.IDL_Sequences;
with CORBA.Sequences.Unbounded;

package IOP is

   Repository_Id : constant PolyORB.Std.String :=
     "IDL:omg.org/IOP:1.0";

   type ProfileId is
     new CORBA.Unsigned_Long;

   ProfileId_Repository_Id : constant PolyORB.Std.String :=
     "IDL:omg.org/IOP/ProfileId:1.0";

   TAG_INTERNET_IOP : constant IOP.ProfileId :=
     0;

   TAG_MULTIPLE_COMPONENTS : constant IOP.ProfileId :=
     1;

   TAG_SCCP_IOP : constant IOP.ProfileId :=
     2;

   type ProfileData is
     new CORBA.IDL_Sequences.OctetSeq;

   ProfileData_Repository_Id : constant PolyORB.Std.String :=
     "IDL:omg.org/IOP/ProfileData:1.0";

   type TaggedProfile is
     record
         tag : IOP.ProfileId;
         profile_data : IOP.ProfileData;
      end record;

   TaggedProfile_Repository_Id : constant PolyORB.Std.String :=
     "IDL:omg.org/IOP/TaggedProfile:1.0";

   package IDL_SEQUENCE_IOP_TaggedProfile is
     new CORBA.Sequences.Unbounded
        (IOP.TaggedProfile);

   type TaggedProfileSeq is
     new IOP.IDL_SEQUENCE_IOP_TaggedProfile.Sequence;

   TaggedProfileSeq_Repository_Id : constant PolyORB.Std.String :=
     "IDL:omg.org/IOP/TaggedProfileSeq:1.0";

   type IOR is
     record
         type_id : CORBA.String;
         profiles : IOP.TaggedProfileSeq;
      end record;

   IOR_Repository_Id : constant PolyORB.Std.String :=
     "IDL:omg.org/IOP/IOR:1.0";

   type ComponentId is
     new CORBA.Unsigned_Long;

   ComponentId_Repository_Id : constant PolyORB.Std.String :=
     "IDL:omg.org/IOP/ComponentId:1.0";

   type ComponentData is
     new CORBA.IDL_Sequences.OctetSeq;

   ComponentData_Repository_Id : constant PolyORB.Std.String :=
     "IDL:omg.org/IOP/ComponentData:1.0";

   type TaggedComponent is
     record
         tag : IOP.ComponentId;
         component_data : IOP.ComponentData;
      end record;

   TaggedComponent_Repository_Id : constant PolyORB.Std.String :=
     "IDL:omg.org/IOP/TaggedComponent:1.0";

   package IDL_SEQUENCE_IOP_TaggedComponent is
     new CORBA.Sequences.Unbounded
        (IOP.TaggedComponent);

   type TaggedComponentSeq is
     new IOP.IDL_SEQUENCE_IOP_TaggedComponent.Sequence;

   TaggedComponentSeq_Repository_Id : constant PolyORB.Std.String :=
     "IDL:omg.org/IOP/TaggedComponentSeq:1.0";

   type ObjectKey is
     new CORBA.IDL_Sequences.OctetSeq;

   ObjectKey_Repository_Id : constant PolyORB.Std.String :=
     "IDL:omg.org/IOP/ObjectKey:1.0";

   package IDL_SEQUENCE_IOP_TaggedComponent_1 is
     new CORBA.Sequences.Unbounded
        (IOP.TaggedComponent);

   type MultipleComponentProfile is
     new IOP.IDL_SEQUENCE_IOP_TaggedComponent_1.Sequence;

   MultipleComponentProfile_Repository_Id : constant PolyORB.Std.String :=
     "IDL:omg.org/IOP/MultipleComponentProfile:1.0";

   TAG_ORB_TYPE : constant IOP.ComponentId :=
     0;

   TAG_CODE_SETS : constant IOP.ComponentId :=
     1;

   TAG_POLICIES : constant IOP.ComponentId :=
     2;

   TAG_ALTERNATE_IIOP_ADDRESS : constant IOP.ComponentId :=
     3;

   TAG_ASSOCIATION_OPTIONS : constant IOP.ComponentId :=
     13;

   TAG_SEC_NAME : constant IOP.ComponentId :=
     14;

   TAG_SPKM_1_SEC_MECH : constant IOP.ComponentId :=
     15;

   TAG_SPKM_2_SEC_MECH : constant IOP.ComponentId :=
     16;

   TAG_KerberosV5_SEC_MECH : constant IOP.ComponentId :=
     17;

   TAG_CSI_ECMA_Secret_SEC_MECH : constant IOP.ComponentId :=
     18;

   TAG_CSI_ECMA_Hybrid_SEC_MECH : constant IOP.ComponentId :=
     19;

   TAG_SSL_SEC_TRANS : constant IOP.ComponentId :=
     20;

   TAG_CSI_ECMA_Public_SEC_MECH : constant IOP.ComponentId :=
     21;

   TAG_GENERIC_SEC_MECH : constant IOP.ComponentId :=
     22;

   TAG_FIREWALL_TRANS : constant IOP.ComponentId :=
     23;

   TAG_SCCP_CONTACT_INFO : constant IOP.ComponentId :=
     24;

   TAG_JAVA_CODEBASE : constant IOP.ComponentId :=
     25;

   TAG_TRANSACTION_POLICY : constant IOP.ComponentId :=
     26;

   TAG_MESSAGE_ROUTER : constant IOP.ComponentId :=
     30;

   TAG_OTS_POLICY : constant IOP.ComponentId :=
     31;

   TAG_INV_POLICY : constant IOP.ComponentId :=
     32;

   TAG_CSI_SEC_MECH_LIST : constant IOP.ComponentId :=
     33;

   TAG_NULL_TAG : constant IOP.ComponentId :=
     34;

   TAG_SECIOP_SEC_TRANS : constant IOP.ComponentId :=
     35;

   TAG_TLS_SEC_TRANS : constant IOP.ComponentId :=
     36;

   TAG_ACTIVITY_POLICY : constant IOP.ComponentId :=
     37;

   TAG_COMPLETE_OBJECT_KEY : constant IOP.ComponentId :=
     5;

   TAG_ENDPOINT_ID_POSITION : constant IOP.ComponentId :=
     6;

   TAG_LOCATION_POLICY : constant IOP.ComponentId :=
     12;

   TAG_DCE_STRING_BINDING : constant IOP.ComponentId :=
     100;

   TAG_DCE_BINDING_NAME : constant IOP.ComponentId :=
     101;

   TAG_DCE_NO_PIPES : constant IOP.ComponentId :=
     102;

   TAG_DCE_SEC_MECH : constant IOP.ComponentId :=
     103;

   TAG_INET_SEC_TRANS : constant IOP.ComponentId :=
     123;

   type ContextData is
     new CORBA.IDL_Sequences.OctetSeq;

   ContextData_Repository_Id : constant PolyORB.Std.String :=
     "IDL:omg.org/IOP/ContextData:1.0";

   type ServiceId is
     new CORBA.Unsigned_Long;

   ServiceId_Repository_Id : constant PolyORB.Std.String :=
     "IDL:omg.org/IOP/ServiceId:1.0";

   type ServiceContext is
     record
         context_id : IOP.ServiceId;
         context_data : IOP.ContextData;
      end record;

   ServiceContext_Repository_Id : constant PolyORB.Std.String :=
     "IDL:omg.org/IOP/ServiceContext:1.0";

   package IDL_SEQUENCE_IOP_ServiceContext is
     new CORBA.Sequences.Unbounded
        (IOP.ServiceContext);

   type ServiceContextList is
     new IOP.IDL_SEQUENCE_IOP_ServiceContext.Sequence;

   ServiceContextList_Repository_Id : constant PolyORB.Std.String :=
     "IDL:omg.org/IOP/ServiceContextList:1.0";

   TransactionService : constant IOP.ServiceId :=
     0;

   CodeSets : constant IOP.ServiceId :=
     1;

   ChainBypassCheck : constant IOP.ServiceId :=
     2;

   ChainBypassInfo : constant IOP.ServiceId :=
     3;

   LogicalThreadId : constant IOP.ServiceId :=
     4;

   BI_DIR_IIOP : constant IOP.ServiceId :=
     5;

   SendingContextRunTime : constant IOP.ServiceId :=
     6;

   INVOCATION_POLICIES : constant IOP.ServiceId :=
     7;

   FORWARDED_IDENTITY : constant IOP.ServiceId :=
     8;

   UnknownExceptionInfo : constant IOP.ServiceId :=
     9;

   RTCorbaPriority : constant IOP.ServiceId :=
     10;

   RTCorbaPriorityRange : constant IOP.ServiceId :=
     11;

   FT_GROUP_VERSION : constant IOP.ServiceId :=
     12;

   FT_REQUEST : constant IOP.ServiceId :=
     13;

   ExceptionDetailMessage : constant IOP.ServiceId :=
     14;

   SecurityAttributeService : constant IOP.ServiceId :=
     15;

   ActivityService : constant IOP.ServiceId :=
     16;

   type EncodingFormat is
     new CORBA.Short;

   EncodingFormat_Repository_Id : constant PolyORB.Std.String :=
     "IDL:omg.org/IOP/EncodingFormat:1.0";

   ENCODING_CDR_ENCAPS : constant IOP.EncodingFormat :=
     0;

   type Encoding is
     record
         format : IOP.EncodingFormat;
         major_version : CORBA.Octet;
         minor_version : CORBA.Octet;
      end record;

   Encoding_Repository_Id : constant PolyORB.Std.String :=
     "IDL:omg.org/IOP/Encoding:1.0";

end IOP;

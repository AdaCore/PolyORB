pragma Style_Checks ("NM32766");
pragma Wide_Character_Encoding (Brackets);
pragma Warnings (Off, "use of an anonymous access type allocator");

---------------------------------------------------
--  This file has been generated automatically from
--  /Users/heathdorn/Documents/Playground/Agents/RefactorTeam/code_refactor/PolyORB/idls/Interop/CSIIOP.idl
--  by IAC (IDL to Ada Compiler) 20.0w (rev. 41a9b833).
---------------------------------------------------
--  NOTE: If you modify this file by hand, your
--  changes will be lost when you re-run the
--  IDL to Ada compiler.
---------------------------------------------------

--  Source: 
--  /Users/heathdorn/Documents/Playground/Agents/RefactorTeam/code_refactor/PolyORB/idls/Interop/CSIIOP.idl

--  //
--  // File: CSIIOP.idl
--  // CORBA 3.0, Chapter 22

--  #ifndef _CSIIOP_IDL_
--  #define _CSIIOP_IDL_

--  #ifdef _PRE_3_0_COMPILER_
--  #pragma prefix "omg.org"
--  #include <IOP.idl>
--  #include <CSI.idl>
--  #else
--  import ::IOP;
--  import ::CSI;
--  #endif // _PRE_3_0_COMPILER_
--      
--  module CSIIOP {

--  #ifndef _PRE_3_0_COMPILER_
--      typeprefix CSIIOP "omg.org";
--  #endif // _PRE_3_0_COMPILER_

--      // Association options
--      
--      typedef unsigned short AssociationOptions;
--     
--      const AssociationOptions NoProtection = 1;
--      const AssociationOptions Integrity = 2;     
--      const AssociationOptions Confidentiality = 4; 
--      const AssociationOptions DetectReplay = 8;     
--      const AssociationOptions DetectMisordering = 16;
--      const AssociationOptions EstablishTrustInTarget = 32; 
--      const AssociationOptions EstablishTrustInClient = 64;
--      const AssociationOptions NoDelegation = 128;
--      const AssociationOptions SimpleDelegation = 256; 
--      const AssociationOptions CompositeDelegation = 512;
--      const AssociationOptions IdentityAssertion = 1024;
--      const AssociationOptions DelegationByClient = 2048;
--      
--      // The high order 20-bits of each ServiceConfigurationSyntax constant
--      // shall contain the Vendor Minor Codeset ID (VMCID) of the
--      // organization that defined the syntax. The low order 12 bits shall
--      // contain the organization-scoped syntax identifier. The high-order 
--  20
--      // bits of all syntaxes defined by the OMG shall contain the VMCID
--      // allocated to the OMG (that is, 0x4F4D0).

--      typedef unsigned long ServiceConfigurationSyntax;

--      const ServiceConfigurationSyntax SCS_GeneralNames = CSI::OMGVMCID | 
--  0;
--      const ServiceConfigurationSyntax SCS_GSSExportedName = CSI::OMGVMCID 
--  | 1;
--        
--      typedef sequence <octet> ServiceSpecificName;

--      // The name field of the ServiceConfiguration structure identifies a
--      // privilege authority in the format identified in the syntax field. 
--  If the
--      // syntax is SCS_GeneralNames, the name field contains an ASN.1 (BER)
--      // SEQUENCE [1..MAX] OF GeneralName, as defined by the type 
--  GeneralNames in
--      // [IETF RFC 2459]. If the syntax is SCS_GSSExportedName, the name 
--  field 
--      // contains a GSS exported name encoded according to the rules in 
--      // [IETF RFC 2743] Section 3.2, "Mechanism-Independent Exported Name
--      // Object Format," p. 84.

--      struct ServiceConfiguration {
--  	ServiceConfigurationSyntax syntax;
--          ServiceSpecificName name;
--      };

--      typedef sequence <ServiceConfiguration> ServiceConfigurationList;

--      // The body of the TAG_NULL_TAG component is a sequence of octets of
--      // length 0.

--      // type used to define AS layer functionality within a compound 
--  mechanism
--      // definition
--        
--      struct AS_ContextSec {
--          AssociationOptions target_supports;
--          AssociationOptions target_requires;
--          CSI::OID client_authentication_mech;
--  	CSI::GSS_NT_ExportedName target_name;
--      };

--      // type used to define SAS layer functionality within a compound 
--  mechanism
--      // definition
--   
--      struct SAS_ContextSec {
--  	AssociationOptions target_supports;
--  	AssociationOptions target_requires;
--  	ServiceConfigurationList  privilege_authorities;
--  	CSI::OIDList supported_naming_mechanisms;
--  	CSI::IdentityTokenType supported_identity_types;
--      };

--      // type used in the body of a TAG_CSI_SEC_MECH_LIST component to
--      // describe a compound mechanism
--        
--      struct CompoundSecMech {
--  	AssociationOptions target_requires;
--  	IOP::TaggedComponent transport_mech;
--  	AS_ContextSec as_context_mech;
--  	SAS_ContextSec sas_context_mech;
--      };

--      typedef sequence <CompoundSecMech> CompoundSecMechanisms;

--      // type corresponding to the body of a TAG_CSI_SEC_MECH_LIST 
--      // component
--        
--      struct CompoundSecMechList {
--  	boolean stateful;
--  	CompoundSecMechanisms  mechanism_list;
--      };

--      struct TransportAddress {
--  	string host_name;
--  	unsigned short port;
--      };

--      typedef sequence <TransportAddress> TransportAddressList;

--      // Tagged component for configuring SECIOP as a CSIv2 transport 
--  mechanism

--      const IOP::ComponentId TAG_SECIOP_SEC_TRANS = 35;

--      struct SECIOP_SEC_TRANS {
--  	AssociationOptions target_supports;
--  	AssociationOptions target_requires;
--  	CSI::OID mech_oid;
--  	CSI::GSS_NT_ExportedName target_name;
--  	TransportAddressList addresses;
--      };

--      // tagged component for configuring TLS/SSL as a CSIv2 transport 
--  mechanism

--      const IOP::ComponentId TAG_TLS_SEC_TRANS = 36;

--      struct TLS_SEC_TRANS {
--  	AssociationOptions target_supports;
--  	AssociationOptions target_requires;
--  	TransportAddressList addresses;
--      };

--  }; //CSIIOP

--  #endif // _CSIIOP_IDL_

--  End source: 
--  /Users/heathdorn/Documents/Playground/Agents/RefactorTeam/code_refactor/PolyORB/idls/Interop/CSIIOP.idl
--   -- 146 lines

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

--  Source: 
--  /Users/heathdorn/Documents/Playground/Agents/RefactorTeam/code_refactor/PolyORB/idls/Interop/CSI.idl

--  //
--  // CSI.idl
--  // CORBA Core 3.0 Chapter 24

--  #ifndef _CSI_IDL_
--  #define _CSI_IDL_

--  #ifdef _PRE_3_0_COMPILER_
--  #pragma prefix "omg.org"
--  #else
--  #endif // _PRE_3_0_COMPILER_

--  module CSI {

--  #ifndef _PRE_3_0_COMPILER_ 
--      typeprefix CSI "omg.org";
--  #endif // _PRE_3_0_COMPILER_

--      // The OMG VMCID; same value as CORBA::OMGVMCID. Do not change ever.

--      const unsigned long OMGVMCID = 0x4F4D0;

--      // An X509CertificateChain contains an ASN.1 BER encoded SEQUENCE 
--      // [1..MAX] OF X.509 certificates encapsulated in a sequence of 
--  octets. The
--      // subject's certificate shall come first in the list. Each following
--   
--      // certificate shall directly certify the one preceding it. The ASN.1
--      // representation of Certificate is as defined in [IETF RFC 2459].

--      typedef sequence <octet> X509CertificateChain; 

--      // an X.501 type name or Distinguished Name encapsulated in a 
--  sequence of
--      // octets containing the ASN.1 encoding.

--      typedef sequence <octet> X501DistinguishedName;

--      // UTF-8 Encoding of String

--      typedef sequence <octet> UTF8String;

--      // ASN.1 Encoding of an OBJECT IDENTIFIER

--      typedef sequence <octet> OID;

--      typedef sequence <OID> OIDList;

--      // A sequence of octets containing a GSStoken. Initial context tokens
--   are
--      // ASN.1 encoded as defined in [IETF RFC 2743] Section 3.1, 
--      // "Mechanism-Independent token Format", pp. 81-82. Initial context 
--  tokens
--      // contain an ASN.1 tag followed by a token length, a mechanism 
--  identifier,
--      // and a mechanism-specific token (i.e. a 
--  GSSUP::InitialContextToken). The
--      // encoding of all other GSS tokens (e.g. error tokens and final 
--  context
--      // tokens) is mechanism dependent.

--      typedef sequence <octet> GSSToken;

--      // An encoding of a GSS Mechanism-Independent Exported Name Object as
--      // defined in [IETF RFC 2743] Section 3.2, "GSS Mechanism-Independent
--      // Exported Name Object Format," p. 84.

--      typedef sequence <octet> GSS_NT_ExportedName;

--      typedef sequence <GSS_NT_ExportedName> GSS_NT_ExportedNameList;

--      // The MsgType enumeration defines the complete set of service 
--  context
--      // message types used by the CSI context management protocols, 
--  including
--      // those message types pertaining only to the stateful application of
--   the 
--      // protocols (to insure proper alignment of the identifiers between
--      // stateless and stateful implementations). Specifically, the 
--      // MTMessageInContext is not sent by stateless clients (although it 
--  may
--      // be received by stateless targets).

--      typedef short MsgType;
--        
--      const MsgType MTEstablishContext = 0;
--      const MsgType MTCompleteEstablishContext = 1;      
--      const MsgType MTContextError = 4; 
--      const MsgType MTMessageInContext = 5;

--      // The ContextId type is used carry session identifiers. A stateless 
--      // application of the service context protocol is indicated by a 
--  session
--      // identifier value of 0.

--      typedef unsigned long long ContextId;

--      // The AuthorizationElementType defines the contents and encoding of
--      // the_element field of the AuthorizationElement.

--      // The high order 20-bits of each AuthorizationElementType constant
--      // shall contain the Vendor Minor Codeset ID (VMCID) of the
--      // organization that defined the element type. The low order 12 bits
--      // shall contain the organization-scoped element type identifier. The
--      // high-order 20 bits of all element types defined by the OMG shall
--      // contain the VMCID allocated to the OMG (that is, 0x4F4D0).
--        
--      typedef unsigned long AuthorizationElementType;

--      // An AuthorizationElementType of X509AttributeCertChain indicates 
--  that 
--      // the_element field of the AuthorizationElement contains an ASN.1 
--  BER
--      // SEQUENCE composed of an (X.509) AttributeCertificate followed by a
--      // SEQUENCE OF (X.509) Certificate. The two-part SEQUENCE is 
--  encapsulated
--      // in an octet stream. The chain of identity certificates is provided
--      // to certify the attribute certificate. Each certificate in the 
--  chain 
--      // shall directly certify the one preceding it. The first certificate
--      // in the chain shall certify the attribute certificate. The ASN.1
--      // representation of (X.509) Certificate is as defined in [IETF RFC 
--  2459].
--      // The ASN.1 representation of (X.509) AtributeCertificate is as 
--  defined
--      // in [IETF ID PKIXAC].  

--      const AuthorizationElementType X509AttributeCertChain = OMGVMCID | 1;

--      typedef sequence <octet> AuthorizationElementContents;

--      // The AuthorizationElement contains one element of an authorization 
--  token.
--      // Each element of an authorization token is logically a PAC.

--      struct AuthorizationElement {
--  	AuthorizationElementType   the_type;
--  	AuthorizationElementContents   the_element;
--      };

--      // The AuthorizationToken is made up of a sequence of 
--      // AuthorizationElements

--      typedef sequence <AuthorizationElement> AuthorizationToken;
--        
--      typedef unsigned long IdentityTokenType;

--      // Additional standard identity token types shall only be defined by 
--  the
--      // OMG. All IdentityTokenType constants shall be a power of 2.

--      const IdentityTokenType ITTAbsent = 0;      
--      const IdentityTokenType ITTAnonymous = 1;
--      const IdentityTokenType ITTPrincipalName = 2;
--      const IdentityTokenType ITTX509CertChain = 4;
--      const IdentityTokenType ITTDistinguishedName = 8;

--      typedef sequence <octet> IdentityExtension;
--        
--      union IdentityToken switch ( IdentityTokenType ) {
--  	case ITTAbsent: boolean absent;
--  	case ITTAnonymous: boolean anonymous;
--          case ITTPrincipalName: GSS_NT_ExportedName principal_name;
--  	case ITTX509CertChain: X509CertificateChain certificate_chain;
--  	case ITTDistinguishedName: X501DistinguishedName dn;
--  	default: IdentityExtension id;
--      };

--      struct EstablishContext {
--  	ContextId client_context_id;
--  	AuthorizationToken authorization_token;
--  	IdentityToken identity_token;
--  	GSSToken client_authentication_token;
--      };
--        
--      struct CompleteEstablishContext {
--  	ContextId client_context_id;
--  	boolean context_stateful;
--  	GSSToken final_context_token;
--      };

--      struct ContextError {
--  	ContextId client_context_id;
--  	long major_status;
--  	long minor_status;
--  	GSSToken error_token;
--      };

--      // Not sent by stateless clients. If received by a stateless server, 
--  a
--      // ContextError message should be returned, indicating the session 
--  does
--      // not exist.
--        
--      struct MessageInContext {
--  	ContextId client_context_id;
--  	boolean discard_context;
--      };
--        
--      union SASContextBody switch ( MsgType ) {
--  	case MTEstablishContext: EstablishContext establish_msg;
--  	case MTCompleteEstablishContext: CompleteEstablishContext complete_msg;
--  	case MTContextError: ContextError error_msg;
--  	case MTMessageInContext: MessageInContext in_context_msg;
--      };

--      // The following type represents the string representation of an 
--  ASN.1
--      // OBJECT IDENTIFIER (OID). OIDs are represented by the string "oid:"
--      // followed by the integer base 10 representation of the OID 
--  separated
--      // by dots. For example, the OID corresponding to the OMG is 
--  represented
--      // as: "oid:2.23.130"     

--      typedef string StringOID;

--      // The GSS Object Identifier for the KRB5 mechanism is:
--      // { iso(1) member-body(2) United States(840) mit(113554) infosys(1)
--      // gssapi(2) krb5(2) }

--      const StringOID KRB5MechOID = "oid:1.2.840.113554.1.2.2";

--      // The GSS Object Identifier for name objects of the 
--  Mechanism-idependent
--      // Exported Name Object type is:
--      // { iso(1) org(3) dod(6) internet(1) security(5) nametypes(6)
--      // gss-api-exported-name(4) }

--      const StringOID GSS_NT_Export_Name_OID = "oid:1.3.6.1.5.6.4";

--      // The GSS Object Identifier for the scoped-username name form is:
--      // { iso-itu-t (2) international-organization (23) omg (130) security
--   (1)
--      // naming (2) scoped-username(1) }

--      const StringOID GSS_NT_Scoped_Username_OID = "oid:2.23.130.1.2.1";

--  }; // CSI

--  #endif

--  End source: 
--  /Users/heathdorn/Documents/Playground/Agents/RefactorTeam/code_refactor/PolyORB/idls/Interop/CSI.idl
--   -- 214 lines

---------------------------------------------------

with PolyORB.Std;
with CORBA;
pragma Elaborate_All (CORBA);
with CORBA.Sequences.Unbounded;
with CSI;
with IOP;

package CSIIOP is

   Repository_Id : constant PolyORB.Std.String :=
     "IDL:omg.org/CSIIOP:1.0";

   type AssociationOptions is
     new CORBA.Unsigned_Short;

   AssociationOptions_Repository_Id : constant PolyORB.Std.String :=
     "IDL:omg.org/CSIIOP/AssociationOptions:1.0";

   NoProtection : constant CSIIOP.AssociationOptions :=
     1;

   Integrity : constant CSIIOP.AssociationOptions :=
     2;

   Confidentiality : constant CSIIOP.AssociationOptions :=
     4;

   DetectReplay : constant CSIIOP.AssociationOptions :=
     8;

   DetectMisordering : constant CSIIOP.AssociationOptions :=
     16;

   EstablishTrustInTarget : constant CSIIOP.AssociationOptions :=
     32;

   EstablishTrustInClient : constant CSIIOP.AssociationOptions :=
     64;

   NoDelegation : constant CSIIOP.AssociationOptions :=
     128;

   SimpleDelegation : constant CSIIOP.AssociationOptions :=
     256;

   CompositeDelegation : constant CSIIOP.AssociationOptions :=
     512;

   IdentityAssertion : constant CSIIOP.AssociationOptions :=
     1024;

   DelegationByClient : constant CSIIOP.AssociationOptions :=
     2048;

   type ServiceConfigurationSyntax is
     new CORBA.Unsigned_Long;

   ServiceConfigurationSyntax_Repository_Id : constant PolyORB.Std.String :=
     "IDL:omg.org/CSIIOP/ServiceConfigurationSyntax:1.0";

   SCS_GeneralNames : constant CSIIOP.ServiceConfigurationSyntax :=
     324816;

   SCS_GSSExportedName : constant CSIIOP.ServiceConfigurationSyntax :=
     324817;

   package IDL_SEQUENCE_octet is
     new CORBA.Sequences.Unbounded
        (CORBA.Octet);

   type ServiceSpecificName is
     new CSIIOP.IDL_SEQUENCE_octet.Sequence;

   ServiceSpecificName_Repository_Id : constant PolyORB.Std.String :=
     "IDL:omg.org/CSIIOP/ServiceSpecificName:1.0";

   type ServiceConfiguration is
     record
         syntax : CSIIOP.ServiceConfigurationSyntax;
         name : CSIIOP.ServiceSpecificName;
      end record;

   ServiceConfiguration_Repository_Id : constant PolyORB.Std.String :=
     "IDL:omg.org/CSIIOP/ServiceConfiguration:1.0";

   package IDL_SEQUENCE_CSIIOP_ServiceConfiguration is
     new CORBA.Sequences.Unbounded
        (CSIIOP.ServiceConfiguration);

   type ServiceConfigurationList is
     new CSIIOP.IDL_SEQUENCE_CSIIOP_ServiceConfiguration.Sequence;

   ServiceConfigurationList_Repository_Id : constant PolyORB.Std.String :=
     "IDL:omg.org/CSIIOP/ServiceConfigurationList:1.0";

   type AS_ContextSec is
     record
         target_supports : CSIIOP.AssociationOptions;
         target_requires : CSIIOP.AssociationOptions;
         client_authentication_mech : CSI.OID;
         target_name : CSI.GSS_NT_ExportedName;
      end record;

   AS_ContextSec_Repository_Id : constant PolyORB.Std.String :=
     "IDL:omg.org/CSIIOP/AS_ContextSec:1.0";

   type SAS_ContextSec is
     record
         target_supports : CSIIOP.AssociationOptions;
         target_requires : CSIIOP.AssociationOptions;
         privilege_authorities : CSIIOP.ServiceConfigurationList;
         supported_naming_mechanisms : CSI.OIDList;
         supported_identity_types : CSI.IdentityTokenType;
      end record;

   SAS_ContextSec_Repository_Id : constant PolyORB.Std.String :=
     "IDL:omg.org/CSIIOP/SAS_ContextSec:1.0";

   type CompoundSecMech is
     record
         target_requires : CSIIOP.AssociationOptions;
         transport_mech : IOP.TaggedComponent;
         as_context_mech : CSIIOP.AS_ContextSec;
         sas_context_mech : CSIIOP.SAS_ContextSec;
      end record;

   CompoundSecMech_Repository_Id : constant PolyORB.Std.String :=
     "IDL:omg.org/CSIIOP/CompoundSecMech:1.0";

   package IDL_SEQUENCE_CSIIOP_CompoundSecMech is
     new CORBA.Sequences.Unbounded
        (CSIIOP.CompoundSecMech);

   type CompoundSecMechanisms is
     new CSIIOP.IDL_SEQUENCE_CSIIOP_CompoundSecMech.Sequence;

   CompoundSecMechanisms_Repository_Id : constant PolyORB.Std.String :=
     "IDL:omg.org/CSIIOP/CompoundSecMechanisms:1.0";

   type CompoundSecMechList is
     record
         stateful : CORBA.Boolean;
         mechanism_list : CSIIOP.CompoundSecMechanisms;
      end record;

   CompoundSecMechList_Repository_Id : constant PolyORB.Std.String :=
     "IDL:omg.org/CSIIOP/CompoundSecMechList:1.0";

   type TransportAddress is
     record
         host_name : CORBA.String;
         port : CORBA.Unsigned_Short;
      end record;

   TransportAddress_Repository_Id : constant PolyORB.Std.String :=
     "IDL:omg.org/CSIIOP/TransportAddress:1.0";

   package IDL_SEQUENCE_CSIIOP_TransportAddress is
     new CORBA.Sequences.Unbounded
        (CSIIOP.TransportAddress);

   type TransportAddressList is
     new CSIIOP.IDL_SEQUENCE_CSIIOP_TransportAddress.Sequence;

   TransportAddressList_Repository_Id : constant PolyORB.Std.String :=
     "IDL:omg.org/CSIIOP/TransportAddressList:1.0";

   TAG_SECIOP_SEC_TRANS : constant IOP.ComponentId :=
     35;

   type SECIOP_SEC_TRANS is
     record
         target_supports : CSIIOP.AssociationOptions;
         target_requires : CSIIOP.AssociationOptions;
         mech_oid : CSI.OID;
         target_name : CSI.GSS_NT_ExportedName;
         addresses : CSIIOP.TransportAddressList;
      end record;

   SECIOP_SEC_TRANS_Repository_Id : constant PolyORB.Std.String :=
     "IDL:omg.org/CSIIOP/SECIOP_SEC_TRANS:1.0";

   TAG_TLS_SEC_TRANS : constant IOP.ComponentId :=
     36;

   type TLS_SEC_TRANS is
     record
         target_supports : CSIIOP.AssociationOptions;
         target_requires : CSIIOP.AssociationOptions;
         addresses : CSIIOP.TransportAddressList;
      end record;

   TLS_SEC_TRANS_Repository_Id : constant PolyORB.Std.String :=
     "IDL:omg.org/CSIIOP/TLS_SEC_TRANS:1.0";

end CSIIOP;

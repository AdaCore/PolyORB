pragma Style_Checks ("NM32766");
pragma Wide_Character_Encoding (Brackets);
pragma Warnings (Off, "use of an anonymous access type allocator");

---------------------------------------------------
--  This file has been generated automatically from
--  /Users/heathdorn/Documents/Playground/Agents/RefactorTeam/code_refactor/PolyORB/idls/Interop/GSSUP.idl
--  by IAC (IDL to Ada Compiler) 20.0w (rev. 41a9b833).
---------------------------------------------------
--  NOTE: If you modify this file by hand, your
--  changes will be lost when you re-run the
--  IDL to Ada compiler.
---------------------------------------------------

--  Source: 
--  /Users/heathdorn/Documents/Playground/Agents/RefactorTeam/code_refactor/PolyORB/idls/Interop/GSSUP.idl

--  //
--  // File: GSSUP.idl
--  // CORBA 3.0 Chapter 24

--  #ifndef _GSSUP_IDL_
--  #define _GSSUP_IDL_

--  #ifdef _PRE_3_0_COMPILER_
--  #include <CSI.idl>

--  #pragma prefix "omg.org"
--  #else
--  import ::CSI;
--  #endif // _PRE_3_0_COMPILER_

--  module GSSUP {

--  #ifndef _PRE_3_0_COMPILER_ 
--      typeprefix GSSUP "omg.org";
--  #endif // _PRE_3_0_COMPILER_

--      // The GSS Object Identifier allocated for the 
--      // username/password mechanism is defined below.
--      //
--      // { iso-itu-t (2) international-organization (23) omg (130)
--      //	security (1) authentication (1) gssup-mechanism (1) }

--      const CSI::StringOID GSSUPMechOID = "oid:2.23.130.1.1.1";

--      // The following structure defines the inner contents of the
--      // username password initial context token. This structure is
--      // CDR encapsulated and appended at the end of the
--      // username/password GSS (initial context) Token.

--      struct InitialContextToken {
--  	CSI::UTF8String username;
--  	CSI::UTF8String password;
--  	CSI::GSS_NT_ExportedName target_name;
--      };

--      typedef unsigned long ErrorCode;

--      // GSSUP Mechanism-Specific Error Token
--      struct ErrorToken {
--  	ErrorCode error_code;
--      };

--      // The context validator has chosen not to reveal the GSSUP
--      // specific cause of the failure.
--      const ErrorCode GSS_UP_S_G_UNSPECIFIED = 1;

--      // The user identified in the username field of the
--      // GSSUP::InitialContextToken is unknown to the target.
--      const ErrorCode GSS_UP_S_G_NOUSER = 2;

--      // The password supplied in the GSSUP::InitialContextToken was
--      // incorrect.
--      const ErrorCode GSS_UP_S_G_BAD_PASSWORD = 3;

--      // The target_name supplied in the GSSUP::InitialContextToken does
--      // not match a target_name in a mechanism definition of the target.
--      const ErrorCode GSS_UP_S_G_BAD_TARGET = 4;

--  }; // GSSUP

--  #endif

--  End source: 
--  /Users/heathdorn/Documents/Playground/Agents/RefactorTeam/code_refactor/PolyORB/idls/Interop/GSSUP.idl
--   -- 67 lines

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
with CSI;
with CORBA;
pragma Elaborate_All (CORBA);

package GSSUP is

   Repository_Id : constant PolyORB.Std.String :=
     "IDL:omg.org/GSSUP:1.0";

   GSSUPMechOID : constant CSI.StringOID :=
     CSI.To_CORBA_String
        ("oid:2.23.130.1.1.1");

   type InitialContextToken is
     record
         username : CSI.UTF8String;
         password : CSI.UTF8String;
         target_name : CSI.GSS_NT_ExportedName;
      end record;

   InitialContextToken_Repository_Id : constant PolyORB.Std.String :=
     "IDL:omg.org/GSSUP/InitialContextToken:1.0";

   type ErrorCode is
     new CORBA.Unsigned_Long;

   ErrorCode_Repository_Id : constant PolyORB.Std.String :=
     "IDL:omg.org/GSSUP/ErrorCode:1.0";

   type ErrorToken is
     record
         error_code : GSSUP.ErrorCode;
      end record;

   ErrorToken_Repository_Id : constant PolyORB.Std.String :=
     "IDL:omg.org/GSSUP/ErrorToken:1.0";

   GSS_UP_S_G_UNSPECIFIED : constant GSSUP.ErrorCode :=
     1;

   GSS_UP_S_G_NOUSER : constant GSSUP.ErrorCode :=
     2;

   GSS_UP_S_G_BAD_PASSWORD : constant GSSUP.ErrorCode :=
     3;

   GSS_UP_S_G_BAD_TARGET : constant GSSUP.ErrorCode :=
     4;

end GSSUP;

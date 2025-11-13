pragma Style_Checks ("NM32766");
pragma Wide_Character_Encoding (Brackets);
pragma Warnings (Off, "use of an anonymous access type allocator");

---------------------------------------------------
--  This file has been generated automatically from
--  /Users/heathdorn/Documents/Playground/Agents/RefactorTeam/code_refactor/PolyORB/idls/Misc/DynamicAny.idl
--  by IAC (IDL to Ada Compiler) 20.0w (rev. 41a9b833).
---------------------------------------------------
--  NOTE: If you modify this file by hand, your
--  changes will be lost when you re-run the
--  IDL to Ada compiler.
---------------------------------------------------

--  Source: 
--  /Users/heathdorn/Documents/Playground/Agents/RefactorTeam/code_refactor/PolyORB/idls/Misc/DynamicAny.idl

--  // File: DynamicAny.idl
--  // CORBA 3.0, Chapter 9

--  #ifndef _DYNAMIC_ANY_IDL_
--  #define _DYNAMIC_ANY_IDL_

--  #ifdef _PRE_3_0_COMPILER_ 
--  #pragma prefix "omg.org"
--  #include <orb.idl>
--  #else
--  import ::CORBA;
--  #endif // _PRE_3_0_COMPILER_

--  module DynamicAny {

--  #ifndef _PRE_3_0_COMPILER_ 
--      typeprefix DynamicAny "omg.org";
--  #endif // _PRE_3_0_COMPILER_

--      local interface DynAny {
--          exception InvalidValue {};
--          exception TypeMismatch {};
--                  
--          CORBA::TypeCode type();

--          void assign(        in DynAny   dyn_any)   
--  	  raises(TypeMismatch);
--          void from_any(      in any      value)      
--  	  raises(TypeMismatch, InvalidValue);
--          any to_any();

--          boolean equal(      in DynAny   dyn_any);

--          void destroy();
--          DynAny copy();

--          void insert_boolean(in boolean  value)      
--  	  raises(TypeMismatch, InvalidValue);
--          void insert_octet(  in octet    value)     
--  	  raises(TypeMismatch, InvalidValue);
--          void insert_char(   in char     value)     
--  	  raises(TypeMismatch, InvalidValue);
--          void insert_short(  in short    value)     
--  	  raises(TypeMismatch, InvalidValue);
--          void insert_ushort( in unsigned short 
--                                          value)     
--  	  raises(TypeMismatch, InvalidValue);
--          void insert_long(   in long     value)     
--  	  raises(TypeMismatch, InvalidValue);
--          void insert_ulong(  in unsigned long 
--                                          value)     
--  	  raises(TypeMismatch, InvalidValue);
--          void insert_float(  in float    value)     
--  	  raises(TypeMismatch, InvalidValue);
--          void insert_double( in double   value)      
--  	  raises(TypeMismatch, InvalidValue);
--          void insert_string( in string   value)     
--  	  raises(TypeMismatch, InvalidValue);
--  //PolyORB:NI:        void insert_reference(in Object value)     
--  //PolyORB:NI:	  raises(TypeMismatch, InvalidValue);
--          void insert_typecode(in CORBA::TypeCode 
--                                          value)     
--  	  raises(TypeMismatch, InvalidValue);
--          void insert_longlong(in long long value)   
--  	  raises(TypeMismatch, InvalidValue);
--          void insert_ulonglong(in unsigned long long 
--                                          value)     
--  	  raises(TypeMismatch, InvalidValue);
--          void insert_longdouble(in long double 
--                                          value)     
--  	  raises(TypeMismatch, InvalidValue);
--          void insert_wchar(  in wchar    value)      
--  	  raises(TypeMismatch, InvalidValue);
--          void insert_wstring(in wstring  value)     
--  	  raises(TypeMismatch, InvalidValue);
--          void insert_any(    in any      value)     
--  	  raises(TypeMismatch, InvalidValue);
--          void insert_dyn_any(in DynAny   value)     
--  	  raises(TypeMismatch, InvalidValue);
--  //PolyORB:NI:        void insert_val(    in ValueBase value)     
--  //PolyORB:NI:	  raises(TypeMismatch, InvalidValue);

--          boolean         get_boolean()               
--  	  raises(TypeMismatch, InvalidValue);
--          octet           get_octet()                 
--  	  raises(TypeMismatch, InvalidValue);
--          char            get_char()                 
--  	  raises(TypeMismatch, InvalidValue);
--          short           get_short()                 
--  	  raises(TypeMismatch, InvalidValue);
--          unsigned short  get_ushort()                
--  	  raises(TypeMismatch, InvalidValue);
--          long            get_long()                  
--  	  raises(TypeMismatch, InvalidValue);
--          unsigned long   get_ulong()                
--  	  raises(TypeMismatch, InvalidValue);
--          float           get_float()                 
--  	  raises(TypeMismatch, InvalidValue);
--          double          get_double()                
--  	  raises(TypeMismatch, InvalidValue);
--          string          get_string()               
--  	  raises(TypeMismatch, InvalidValue);
--  //PolyORB:NI:        Object          get_reference()            
--  //PolyORB:NI:	  raises(TypeMismatch, InvalidValue);
--          CORBA::TypeCode get_typecode()             
--  	  raises(TypeMismatch, InvalidValue);
--          long long       get_longlong()             
--  	  raises(TypeMismatch, InvalidValue);
--          unsigned long long get_ulonglong()          
--  	  raises(TypeMismatch, InvalidValue);
--          long double     get_longdouble()            
--  	  raises(TypeMismatch, InvalidValue);
--          wchar           get_wchar()                
--  	  raises(TypeMismatch, InvalidValue);
--          wstring         get_wstring()              
--  	  raises(TypeMismatch, InvalidValue);
--          any             get_any()                   
--  	  raises(TypeMismatch, InvalidValue);
--          DynAny          get_dyn_any()               
--  	  raises(TypeMismatch, InvalidValue);
--  //PolyORB:NI:        ValueBase       get_val()                   
--  //PolyORB:NI:	  raises(TypeMismatch, InvalidValue);

--          boolean         seek(in long index);
--          void            rewind();
--          boolean         next();
--          unsigned long   component_count();
--          DynAny          current_component()        
--  	  raises(TypeMismatch);

--  //PolyORB:NI:        void insert_abstract(in CORBA::AbstractBase value)
--  //PolyORB:NI:	  raises(TypeMismatch, InvalidValue);
--  //PolyORB:NI:        CORBA::AbstractBase get_abstract()
--  //PolyORB:NI:	  raises(TypeMismatch, InvalidValue);
--  //PolyORB:NI:
--  //PolyORB:NI:        void insert_boolean_seq(in CORBA::BooleanSeq value)
--  //PolyORB:NI:	  raises(TypeMismatch, InvalidValue);
--  //PolyORB:NI:        void insert_octet_seq(in CORBA::OctetSeq value)
--  //PolyORB:NI:	  raises(TypeMismatch, InvalidValue);
--  //PolyORB:NI:        void insert_char_seq(in CORBA::CharSeq value)
--  //PolyORB:NI:	  raises(TypeMismatch, InvalidValue);
--  //PolyORB:NI:        void insert_short_seq(in CORBA::ShortSeq value)
--  //PolyORB:NI:	  raises(TypeMismatch, InvalidValue);
--  //PolyORB:NI:        void insert_ushort_seq(in CORBA::UShortSeq value)
--  //PolyORB:NI:	  raises(TypeMismatch, InvalidValue);
--  //PolyORB:NI:        void insert_long_seq(in CORBA::LongSeq value)
--  //PolyORB:NI:	  raises(TypeMismatch, InvalidValue);
--  //PolyORB:NI:        void insert_ulong_seq(in CORBA::ULongSeq value)
--  //PolyORB:NI:	  raises(TypeMismatch, InvalidValue);
--  //PolyORB:NI:        void insert_float_seq(in CORBA::FloatSeq value)
--  //PolyORB:NI:	  raises(TypeMismatch, InvalidValue);
--  //PolyORB:NI:        void insert_double_seq(in CORBA::DoubleSeq value)
--  //PolyORB:NI:	  raises(TypeMismatch, InvalidValue);
--  //PolyORB:NI:        void insert_longlong_seq(in CORBA::LongLongSeq 
--  value)
--  //PolyORB:NI:	  raises(TypeMismatch, InvalidValue);
--  //PolyORB:NI:        void insert_ulonglong_seq(in CORBA::ULongLongSeq 
--  value)
--  //PolyORB:NI:	  raises(TypeMismatch, InvalidValue);
--  //PolyORB:NI:        void insert_longdouble_seq(in CORBA::LongDoubleSeq 
--  value)
--  //PolyORB:NI:	  raises(TypeMismatch, InvalidValue);
--  //PolyORB:NI:        void insert_wchar_seq(in CORBA::WCharSeq value)
--  //PolyORB:NI:	  raises(TypeMismatch, InvalidValue);
--  //PolyORB:NI:        CORBA::BooleanSeq get_boolean_seq()
--  //PolyORB:NI:	  raises(TypeMismatch, InvalidValue);
--  //PolyORB:NI:        CORBA::OctetSeq get_octet_seq()
--  //PolyORB:NI:	  raises(TypeMismatch, InvalidValue);
--  //PolyORB:NI:        CORBA::CharSeq get_char_seq()
--  //PolyORB:NI:	  raises(TypeMismatch, InvalidValue);
--  //PolyORB:NI:        CORBA::ShortSeq get_short_seq()
--  //PolyORB:NI:	  raises(TypeMismatch, InvalidValue);
--  //PolyORB:NI:        CORBA::UShortSeq get_ushort_seq()
--  //PolyORB:NI:	  raises(TypeMismatch, InvalidValue);
--  //PolyORB:NI:        CORBA::LongSeq get_long_seq()
--  //PolyORB:NI:	  raises(TypeMismatch, InvalidValue);
--  //PolyORB:NI:        CORBA::ULongSeq get_ulong_seq()
--  //PolyORB:NI:	  raises(TypeMismatch, InvalidValue);
--  //PolyORB:NI:        CORBA::FloatSeq get_float_seq()
--  //PolyORB:NI:	  raises(TypeMismatch, InvalidValue);
--  //PolyORB:NI:        CORBA::DoubleSeq get_double_seq()
--  //PolyORB:NI:	  raises(TypeMismatch, InvalidValue);
--  //PolyORB:NI:        CORBA::LongLongSeq get_longlong_seq()
--  //PolyORB:NI:	  raises(TypeMismatch, InvalidValue);
--  //PolyORB:NI:        CORBA::ULongLongSeq get_ulonglong_seq()
--  //PolyORB:NI:	  raises(TypeMismatch, InvalidValue);
--  //PolyORB:NI:        CORBA::LongDoubleSeq get_longdouble_seq()
--  //PolyORB:NI:	  raises(TypeMismatch, InvalidValue);
--  //PolyORB:NI:        CORBA::WCharSeq get_wchar_seq()
--  //PolyORB:NI:	  raises(TypeMismatch, InvalidValue);
--      };
--      
--      local interface DynFixed : DynAny {
--          string          get_value();
--          boolean         set_value(in string val)    
--  	  raises(TypeMismatch, InvalidValue);
--      };
--      
--      local interface DynEnum : DynAny {
--          string          get_as_string();
--          void            set_as_string(in string value) 
--  	  raises(InvalidValue);
--          unsigned long   get_as_ulong();
--          void            set_as_ulong( in unsigned long value) 
--  	  raises(InvalidValue);
--      };
--      typedef string FieldName;
--      struct NameValuePair {
--          FieldName   id;
--          any         value;
--      };
--      
--      typedef sequence<NameValuePair> NameValuePairSeq;
--      struct NameDynAnyPair {
--          FieldName   id;
--          DynAny      value;
--      };

--      typedef sequence<NameDynAnyPair> NameDynAnyPairSeq;
--      local interface DynStruct : DynAny {
--          FieldName           current_member_name()  
--  	  raises(TypeMismatch, InvalidValue);
--          CORBA::TCKind       current_member_kind()   
--  	  raises(TypeMismatch, InvalidValue);
--          NameValuePairSeq    get_members();
--          void                set_members(in NameValuePairSeq value)    
--  	  raises(TypeMismatch, InvalidValue);
--          NameDynAnyPairSeq   get_members_as_dyn_any();
--          void  set_members_as_dyn_any(in NameDynAnyPairSeq value)
--  	  raises(TypeMismatch, InvalidValue);
--      };

--      local interface DynUnion : DynAny {
--          DynAny              get_discriminator();
--          void                set_discriminator(in DynAny d) 
--  	  raises(TypeMismatch);
--          void                set_to_default_member() 
--  	  raises(TypeMismatch);
--          void                set_to_no_active_member() 
--  	  raises(TypeMismatch);
--          boolean             has_no_active_member();
--          CORBA::TCKind       discriminator_kind();
--          DynAny              member()               
--  	  raises(InvalidValue);
--          FieldName           member_name()          
--  	  raises(InvalidValue);
--          CORBA::TCKind       member_kind()           
--  	  raises(InvalidValue);
--      };
--      
--      typedef sequence<any>    AnySeq;
--      typedef sequence<DynAny> DynAnySeq;
--      local interface DynSequence : DynAny {
--          unsigned long       get_length();
--          void                set_length(in unsigned long len) 
--  	  raises(InvalidValue);
--          AnySeq              get_elements();
--          void                set_elements(in AnySeq value)
--  	  raises(TypeMismatch, InvalidValue);
--          DynAnySeq           get_elements_as_dyn_any();
--          void                set_elements_as_dyn_any(in DynAnySeq value)
--  	  raises(TypeMismatch, InvalidValue);
--      };

--      local interface DynArray : DynAny {
--          AnySeq              get_elements();
--          void                set_elements(in AnySeq value)
--  	  raises(TypeMismatch, InvalidValue);
--          DynAnySeq           get_elements_as_dyn_any();
--          void                set_elements_as_dyn_any(in DynAnySeq value)
--  	  raises(TypeMismatch, InvalidValue);
--      };
--      
--      local interface DynValueCommon : DynAny {
--          boolean             is_null();
--          void                set_to_null();
--          void                set_to_value();
--      };

--      local interface DynValue : DynValueCommon {
--          FieldName           current_member_name()
--  	  raises(TypeMismatch, InvalidValue);
--          CORBA::TCKind       current_member_kind()
--  	  raises(TypeMismatch, InvalidValue);
--          NameValuePairSeq    get_members()
--  	  raises(InvalidValue);
--          void                set_members(in NameValuePairSeq value)
--  	  raises(TypeMismatch, InvalidValue);
--          NameDynAnyPairSeq   get_members_as_dyn_any()
--  	  raises(InvalidValue);
--          void                set_members_as_dyn_any(in NameDynAnyPairSeq 
--  value)
--  	  raises(TypeMismatch, InvalidValue);
--      };

--      local interface DynValueBox : DynValueCommon {
--          any                 get_boxed_value()
--  	  raises(InvalidValue);
--          void                set_boxed_value(in any boxed) 
--  	  raises(TypeMismatch, InvalidValue);
--          DynAny              get_boxed_value_as_dyn_any()
--  	  raises(InvalidValue);
--          void                set_boxed_value_as_dyn_any(in DynAny boxed)
--  	  raises(TypeMismatch);
--      };

--      exception MustTruncate { };

--      local interface DynAnyFactory {
--          exception InconsistentTypeCode {};
--          DynAny create_dyn_any(in any value)
--  	  raises(InconsistentTypeCode);
--  //PolyORB:NI:        DynAny create_dyn_any_from_type_code(in 
--  CORBA::TypeCode type)
--  //PolyORB:NI:	  raises(InconsistentTypeCode);
--          DynAny create_dyn_any_without_truncation(in any value)
--  	  raises(InconsistentTypeCode, MustTruncate);
--  //PolyORB:NI:        DynAnySeq create_multiple_dyn_anys(
--  //PolyORB:NI:	  in AnySeq values,
--  //PolyORB:NI:	  in boolean allow_truncate)
--  //PolyORB:NI:	raises(InconsistentTypeCode, MustTruncate);
--  //PolyORB:NI:        AnySeq create_multiple_anys(in DynAnySeq values);
--      };



--  }; // module DynamicAny
--  #endif // _DYNAMIC_ANY_IDL_

--  End source: 
--  /Users/heathdorn/Documents/Playground/Agents/RefactorTeam/code_refactor/PolyORB/idls/Misc/DynamicAny.idl
--   -- 324 lines

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
with CORBA.Forward;
pragma Elaborate_All (CORBA.Forward);
with CORBA;
pragma Elaborate_All (CORBA);
with CORBA.Sequences.Unbounded;
with Ada.Exceptions;

package DynamicAny is

   Repository_Id : constant PolyORB.Std.String :=
     "IDL:omg.org/DynamicAny:1.0";

   package DynAny_Forward is
     new CORBA.Forward;

   type FieldName is
     new CORBA.String;

   FieldName_Repository_Id : constant PolyORB.Std.String :=
     "IDL:omg.org/DynamicAny/FieldName:1.0";

   type NameValuePair is
     record
         id : DynamicAny.FieldName;
         value : CORBA.Any;
      end record;

   NameValuePair_Repository_Id : constant PolyORB.Std.String :=
     "IDL:omg.org/DynamicAny/NameValuePair:1.0";

   package IDL_SEQUENCE_DynamicAny_NameValuePair is
     new CORBA.Sequences.Unbounded
        (DynamicAny.NameValuePair);

   type NameValuePairSeq is
     new DynamicAny.IDL_SEQUENCE_DynamicAny_NameValuePair.Sequence;

   NameValuePairSeq_Repository_Id : constant PolyORB.Std.String :=
     "IDL:omg.org/DynamicAny/NameValuePairSeq:1.0";

   type NameDynAnyPair is
     record
         id : DynamicAny.FieldName;
         value : DynamicAny.DynAny_Forward.Ref;
      end record;

   NameDynAnyPair_Repository_Id : constant PolyORB.Std.String :=
     "IDL:omg.org/DynamicAny/NameDynAnyPair:1.0";

   package IDL_SEQUENCE_DynamicAny_NameDynAnyPair is
     new CORBA.Sequences.Unbounded
        (DynamicAny.NameDynAnyPair);

   type NameDynAnyPairSeq is
     new DynamicAny.IDL_SEQUENCE_DynamicAny_NameDynAnyPair.Sequence;

   NameDynAnyPairSeq_Repository_Id : constant PolyORB.Std.String :=
     "IDL:omg.org/DynamicAny/NameDynAnyPairSeq:1.0";

   package IDL_SEQUENCE_any is
     new CORBA.Sequences.Unbounded
        (CORBA.Any);

   type AnySeq is
     new DynamicAny.IDL_SEQUENCE_any.Sequence;

   AnySeq_Repository_Id : constant PolyORB.Std.String :=
     "IDL:omg.org/DynamicAny/AnySeq:1.0";

   package IDL_SEQUENCE_DynamicAny_DynAny_Forward is
     new CORBA.Sequences.Unbounded
        (DynamicAny.DynAny_Forward.Ref);

   type DynAnySeq is
     new DynamicAny.IDL_SEQUENCE_DynamicAny_DynAny_Forward.Sequence;

   DynAnySeq_Repository_Id : constant PolyORB.Std.String :=
     "IDL:omg.org/DynamicAny/DynAnySeq:1.0";

   MustTruncate : exception;

   type MustTruncate_Members is
     new CORBA.Idl_Exception_Members with null record;

   MustTruncate_Repository_Id : constant PolyORB.Std.String :=
     "IDL:omg.org/DynamicAny/MustTruncate:1.0";

   procedure Get_Members
     (From : Ada.Exceptions.Exception_Occurrence;
      To : out DynamicAny.MustTruncate_Members);

end DynamicAny;

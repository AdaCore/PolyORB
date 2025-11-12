pragma Style_Checks ("NM32766");
pragma Wide_Character_Encoding (Brackets);
pragma Warnings (Off, "use of an anonymous access type allocator");

---------------------------------------------------
--  This file has been generated automatically from
--  /Users/heathdorn/Documents/Playground/Agents/RefactorTeam/code_refactor/PolyORB/idls/Interop/CSI.idl
--  by IAC (IDL to Ada Compiler) 20.0w (rev. 41a9b833).
---------------------------------------------------
--  NOTE: If you modify this file by hand, your
--  changes will be lost when you re-run the
--  IDL to Ada compiler.
---------------------------------------------------

with CORBA;
pragma Elaborate_All (CORBA);
with PolyORB.Any;
with PolyORB.Sequences.Unbounded.CORBA_Helper;
pragma Elaborate_All (PolyORB.Sequences.Unbounded.CORBA_Helper);
with PolyORB.Types;

package CSI.Helper is

   TC_IDL_SEQUENCE_octet : CORBA.TypeCode.Object;

   function From_Any
     (Item : CORBA.Any)
     return CSI.IDL_SEQUENCE_octet.Sequence;

   function To_Any
     (Item : CSI.IDL_SEQUENCE_octet.Sequence)
     return CORBA.Any;

   TC_X509CertificateChain : CORBA.TypeCode.Object;

   function From_Any
     (Item : CORBA.Any)
     return CSI.X509CertificateChain;

   function To_Any
     (Item : CSI.X509CertificateChain)
     return CORBA.Any;

   TC_IDL_SEQUENCE_octet_1 : CORBA.TypeCode.Object;

   function From_Any
     (Item : CORBA.Any)
     return CSI.IDL_SEQUENCE_octet_1.Sequence;

   function To_Any
     (Item : CSI.IDL_SEQUENCE_octet_1.Sequence)
     return CORBA.Any;

   TC_X501DistinguishedName : CORBA.TypeCode.Object;

   function From_Any
     (Item : CORBA.Any)
     return CSI.X501DistinguishedName;

   function To_Any
     (Item : CSI.X501DistinguishedName)
     return CORBA.Any;

   TC_IDL_SEQUENCE_octet_2 : CORBA.TypeCode.Object;

   function From_Any
     (Item : CORBA.Any)
     return CSI.IDL_SEQUENCE_octet_2.Sequence;

   function To_Any
     (Item : CSI.IDL_SEQUENCE_octet_2.Sequence)
     return CORBA.Any;

   TC_UTF8String : CORBA.TypeCode.Object;

   function From_Any
     (Item : CORBA.Any)
     return CSI.UTF8String;

   function To_Any
     (Item : CSI.UTF8String)
     return CORBA.Any;

   TC_IDL_SEQUENCE_octet_3 : CORBA.TypeCode.Object;

   function From_Any
     (Item : CORBA.Any)
     return CSI.IDL_SEQUENCE_octet_3.Sequence;

   function To_Any
     (Item : CSI.IDL_SEQUENCE_octet_3.Sequence)
     return CORBA.Any;

   TC_OID : CORBA.TypeCode.Object;

   function From_Any
     (Item : CORBA.Any)
     return CSI.OID;

   function To_Any
     (Item : CSI.OID)
     return CORBA.Any;

   TC_IDL_SEQUENCE_CSI_OID : CORBA.TypeCode.Object;

   function From_Any
     (Item : CORBA.Any)
     return CSI.IDL_SEQUENCE_CSI_OID.Sequence;

   function To_Any
     (Item : CSI.IDL_SEQUENCE_CSI_OID.Sequence)
     return CORBA.Any;

   TC_OIDList : CORBA.TypeCode.Object;

   function From_Any
     (Item : CORBA.Any)
     return CSI.OIDList;

   function To_Any
     (Item : CSI.OIDList)
     return CORBA.Any;

   TC_IDL_SEQUENCE_octet_4 : CORBA.TypeCode.Object;

   function From_Any
     (Item : CORBA.Any)
     return CSI.IDL_SEQUENCE_octet_4.Sequence;

   function To_Any
     (Item : CSI.IDL_SEQUENCE_octet_4.Sequence)
     return CORBA.Any;

   TC_GSSToken : CORBA.TypeCode.Object;

   function From_Any
     (Item : CORBA.Any)
     return CSI.GSSToken;

   function To_Any
     (Item : CSI.GSSToken)
     return CORBA.Any;

   TC_IDL_SEQUENCE_octet_5 : CORBA.TypeCode.Object;

   function From_Any
     (Item : CORBA.Any)
     return CSI.IDL_SEQUENCE_octet_5.Sequence;

   function To_Any
     (Item : CSI.IDL_SEQUENCE_octet_5.Sequence)
     return CORBA.Any;

   TC_GSS_NT_ExportedName : CORBA.TypeCode.Object;

   function From_Any
     (Item : CORBA.Any)
     return CSI.GSS_NT_ExportedName;

   function To_Any
     (Item : CSI.GSS_NT_ExportedName)
     return CORBA.Any;

   TC_IDL_SEQUENCE_CSI_GSS_NT_ExportedName : CORBA.TypeCode.Object;

   function From_Any
     (Item : CORBA.Any)
     return CSI.IDL_SEQUENCE_CSI_GSS_NT_ExportedName.Sequence;

   function To_Any
     (Item : CSI.IDL_SEQUENCE_CSI_GSS_NT_ExportedName.Sequence)
     return CORBA.Any;

   TC_GSS_NT_ExportedNameList : CORBA.TypeCode.Object;

   function From_Any
     (Item : CORBA.Any)
     return CSI.GSS_NT_ExportedNameList;

   function To_Any
     (Item : CSI.GSS_NT_ExportedNameList)
     return CORBA.Any;

   TC_MsgType : CORBA.TypeCode.Object;

   function From_Any
     (Item : CORBA.Any)
     return CSI.MsgType;

   function To_Any
     (Item : CSI.MsgType)
     return CORBA.Any;

   TC_ContextId : CORBA.TypeCode.Object;

   function From_Any
     (Item : CORBA.Any)
     return CSI.ContextId;

   function To_Any
     (Item : CSI.ContextId)
     return CORBA.Any;

   TC_AuthorizationElementType : CORBA.TypeCode.Object;

   function From_Any
     (Item : CORBA.Any)
     return CSI.AuthorizationElementType;

   function To_Any
     (Item : CSI.AuthorizationElementType)
     return CORBA.Any;

   TC_IDL_SEQUENCE_octet_6 : CORBA.TypeCode.Object;

   function From_Any
     (Item : CORBA.Any)
     return CSI.IDL_SEQUENCE_octet_6.Sequence;

   function To_Any
     (Item : CSI.IDL_SEQUENCE_octet_6.Sequence)
     return CORBA.Any;

   TC_AuthorizationElementContents : CORBA.TypeCode.Object;

   function From_Any
     (Item : CORBA.Any)
     return CSI.AuthorizationElementContents;

   function To_Any
     (Item : CSI.AuthorizationElementContents)
     return CORBA.Any;

   TC_AuthorizationElement : CORBA.TypeCode.Object;

   function From_Any
     (Item : CORBA.Any)
     return CSI.AuthorizationElement;

   function To_Any
     (Item : CSI.AuthorizationElement)
     return CORBA.Any;

   TC_IDL_SEQUENCE_CSI_AuthorizationElement : CORBA.TypeCode.Object;

   function From_Any
     (Item : CORBA.Any)
     return CSI.IDL_SEQUENCE_CSI_AuthorizationElement.Sequence;

   function To_Any
     (Item : CSI.IDL_SEQUENCE_CSI_AuthorizationElement.Sequence)
     return CORBA.Any;

   TC_AuthorizationToken : CORBA.TypeCode.Object;

   function From_Any
     (Item : CORBA.Any)
     return CSI.AuthorizationToken;

   function To_Any
     (Item : CSI.AuthorizationToken)
     return CORBA.Any;

   TC_IdentityTokenType : CORBA.TypeCode.Object;

   function From_Any
     (Item : CORBA.Any)
     return CSI.IdentityTokenType;

   function To_Any
     (Item : CSI.IdentityTokenType)
     return CORBA.Any;

   TC_IDL_SEQUENCE_octet_7 : CORBA.TypeCode.Object;

   function From_Any
     (Item : CORBA.Any)
     return CSI.IDL_SEQUENCE_octet_7.Sequence;

   function To_Any
     (Item : CSI.IDL_SEQUENCE_octet_7.Sequence)
     return CORBA.Any;

   TC_IdentityExtension : CORBA.TypeCode.Object;

   function From_Any
     (Item : CORBA.Any)
     return CSI.IdentityExtension;

   function To_Any
     (Item : CSI.IdentityExtension)
     return CORBA.Any;

   TC_IdentityToken : CORBA.TypeCode.Object;

   function From_Any
     (Item : CORBA.Any)
     return CSI.IdentityToken;

   function To_Any
     (Item : CSI.IdentityToken)
     return CORBA.Any;

   TC_EstablishContext : CORBA.TypeCode.Object;

   function From_Any
     (Item : CORBA.Any)
     return CSI.EstablishContext;

   function To_Any
     (Item : CSI.EstablishContext)
     return CORBA.Any;

   TC_CompleteEstablishContext : CORBA.TypeCode.Object;

   function From_Any
     (Item : CORBA.Any)
     return CSI.CompleteEstablishContext;

   function To_Any
     (Item : CSI.CompleteEstablishContext)
     return CORBA.Any;

   TC_ContextError : CORBA.TypeCode.Object;

   function From_Any
     (Item : CORBA.Any)
     return CSI.ContextError;

   function To_Any
     (Item : CSI.ContextError)
     return CORBA.Any;

   TC_MessageInContext : CORBA.TypeCode.Object;

   function From_Any
     (Item : CORBA.Any)
     return CSI.MessageInContext;

   function To_Any
     (Item : CSI.MessageInContext)
     return CORBA.Any;

   TC_SASContextBody : CORBA.TypeCode.Object;

   function From_Any
     (Item : CORBA.Any)
     return CSI.SASContextBody;

   function To_Any
     (Item : CSI.SASContextBody)
     return CORBA.Any;

   TC_StringOID : CORBA.TypeCode.Object;

   function From_Any
     (Item : CORBA.Any)
     return CSI.StringOID;

   function To_Any
     (Item : CSI.StringOID)
     return CORBA.Any;

   
   package Internals is

      function IDL_SEQUENCE_octet_Element_Wrap
        (X : access CORBA.Octet)
        return PolyORB.Any.Content'Class;

      function Wrap
        (X : access CSI.IDL_SEQUENCE_octet.Sequence)
        return PolyORB.Any.Content'Class;

      package IDL_SEQUENCE_octet_Helper is
        new CSI.IDL_SEQUENCE_octet.CORBA_Helper
           (Element_From_Any => CORBA.From_Any,
            Element_To_Any => CORBA.To_Any,
            Element_Wrap => CSI.Helper.Internals.IDL_SEQUENCE_octet_Element_Wrap);

      procedure Initialize_IDL_SEQUENCE_octet;

      procedure Initialize_X509CertificateChain;

      function IDL_SEQUENCE_octet_1_Element_Wrap
        (X : access CORBA.Octet)
        return PolyORB.Any.Content'Class;

      function Wrap
        (X : access CSI.IDL_SEQUENCE_octet_1.Sequence)
        return PolyORB.Any.Content'Class;

      package IDL_SEQUENCE_octet_1_Helper is
        new CSI.IDL_SEQUENCE_octet_1.CORBA_Helper
           (Element_From_Any => CORBA.From_Any,
            Element_To_Any => CORBA.To_Any,
            Element_Wrap => CSI.Helper.Internals.IDL_SEQUENCE_octet_1_Element_Wrap);

      procedure Initialize_IDL_SEQUENCE_octet_1;

      procedure Initialize_X501DistinguishedName;

      function IDL_SEQUENCE_octet_2_Element_Wrap
        (X : access CORBA.Octet)
        return PolyORB.Any.Content'Class;

      function Wrap
        (X : access CSI.IDL_SEQUENCE_octet_2.Sequence)
        return PolyORB.Any.Content'Class;

      package IDL_SEQUENCE_octet_2_Helper is
        new CSI.IDL_SEQUENCE_octet_2.CORBA_Helper
           (Element_From_Any => CORBA.From_Any,
            Element_To_Any => CORBA.To_Any,
            Element_Wrap => CSI.Helper.Internals.IDL_SEQUENCE_octet_2_Element_Wrap);

      procedure Initialize_IDL_SEQUENCE_octet_2;

      procedure Initialize_UTF8String;

      function IDL_SEQUENCE_octet_3_Element_Wrap
        (X : access CORBA.Octet)
        return PolyORB.Any.Content'Class;

      function Wrap
        (X : access CSI.IDL_SEQUENCE_octet_3.Sequence)
        return PolyORB.Any.Content'Class;

      package IDL_SEQUENCE_octet_3_Helper is
        new CSI.IDL_SEQUENCE_octet_3.CORBA_Helper
           (Element_From_Any => CORBA.From_Any,
            Element_To_Any => CORBA.To_Any,
            Element_Wrap => CSI.Helper.Internals.IDL_SEQUENCE_octet_3_Element_Wrap);

      procedure Initialize_IDL_SEQUENCE_octet_3;

      procedure Initialize_OID;

      function IDL_SEQUENCE_CSI_OID_Element_Wrap
        (X : access CSI.OID)
        return PolyORB.Any.Content'Class;

      function Wrap
        (X : access CSI.IDL_SEQUENCE_CSI_OID.Sequence)
        return PolyORB.Any.Content'Class;

      package IDL_SEQUENCE_CSI_OID_Helper is
        new CSI.IDL_SEQUENCE_CSI_OID.CORBA_Helper
           (Element_From_Any => CSI.Helper.From_Any,
            Element_To_Any => CSI.Helper.To_Any,
            Element_Wrap => CSI.Helper.Internals.IDL_SEQUENCE_CSI_OID_Element_Wrap);

      procedure Initialize_IDL_SEQUENCE_CSI_OID;

      procedure Initialize_OIDList;

      function IDL_SEQUENCE_octet_4_Element_Wrap
        (X : access CORBA.Octet)
        return PolyORB.Any.Content'Class;

      function Wrap
        (X : access CSI.IDL_SEQUENCE_octet_4.Sequence)
        return PolyORB.Any.Content'Class;

      package IDL_SEQUENCE_octet_4_Helper is
        new CSI.IDL_SEQUENCE_octet_4.CORBA_Helper
           (Element_From_Any => CORBA.From_Any,
            Element_To_Any => CORBA.To_Any,
            Element_Wrap => CSI.Helper.Internals.IDL_SEQUENCE_octet_4_Element_Wrap);

      procedure Initialize_IDL_SEQUENCE_octet_4;

      procedure Initialize_GSSToken;

      function IDL_SEQUENCE_octet_5_Element_Wrap
        (X : access CORBA.Octet)
        return PolyORB.Any.Content'Class;

      function Wrap
        (X : access CSI.IDL_SEQUENCE_octet_5.Sequence)
        return PolyORB.Any.Content'Class;

      package IDL_SEQUENCE_octet_5_Helper is
        new CSI.IDL_SEQUENCE_octet_5.CORBA_Helper
           (Element_From_Any => CORBA.From_Any,
            Element_To_Any => CORBA.To_Any,
            Element_Wrap => CSI.Helper.Internals.IDL_SEQUENCE_octet_5_Element_Wrap);

      procedure Initialize_IDL_SEQUENCE_octet_5;

      procedure Initialize_GSS_NT_ExportedName;

      function IDL_SEQUENCE_CSI_GSS_NT_ExportedName_Element_Wrap
        (X : access CSI.GSS_NT_ExportedName)
        return PolyORB.Any.Content'Class;

      function Wrap
        (X : access CSI.IDL_SEQUENCE_CSI_GSS_NT_ExportedName.Sequence)
        return PolyORB.Any.Content'Class;

      package IDL_SEQUENCE_CSI_GSS_NT_ExportedName_Helper is
        new CSI.IDL_SEQUENCE_CSI_GSS_NT_ExportedName.CORBA_Helper
           (Element_From_Any => CSI.Helper.From_Any,
            Element_To_Any => CSI.Helper.To_Any,
            Element_Wrap => CSI.Helper.Internals.IDL_SEQUENCE_CSI_GSS_NT_ExportedName_Element_Wrap);

      procedure Initialize_IDL_SEQUENCE_CSI_GSS_NT_ExportedName;

      procedure Initialize_GSS_NT_ExportedNameList;

      function Wrap
        (X : access CSI.MsgType)
        return PolyORB.Any.Content'Class;

      procedure Initialize_MsgType;

      function Wrap
        (X : access CSI.ContextId)
        return PolyORB.Any.Content'Class;

      procedure Initialize_ContextId;

      function Wrap
        (X : access CSI.AuthorizationElementType)
        return PolyORB.Any.Content'Class;

      procedure Initialize_AuthorizationElementType;

      function IDL_SEQUENCE_octet_6_Element_Wrap
        (X : access CORBA.Octet)
        return PolyORB.Any.Content'Class;

      function Wrap
        (X : access CSI.IDL_SEQUENCE_octet_6.Sequence)
        return PolyORB.Any.Content'Class;

      package IDL_SEQUENCE_octet_6_Helper is
        new CSI.IDL_SEQUENCE_octet_6.CORBA_Helper
           (Element_From_Any => CORBA.From_Any,
            Element_To_Any => CORBA.To_Any,
            Element_Wrap => CSI.Helper.Internals.IDL_SEQUENCE_octet_6_Element_Wrap);

      procedure Initialize_IDL_SEQUENCE_octet_6;

      procedure Initialize_AuthorizationElementContents;

      type Ptr_Ü_AuthorizationElement is
        access all CSI.AuthorizationElement;

      type Content_Ü_AuthorizationElement is
        new PolyORB.Any.Aggregate_Content with record
            V : Ptr_Ü_AuthorizationElement;
         end record;

      function Get_Aggregate_Element
        (Acc : not null access Content_Ü_AuthorizationElement;
         Tc : PolyORB.Any.TypeCode.Object_Ptr;
         Index : PolyORB.Types.Unsigned_Long;
         Mech : not null access PolyORB.Any.Mechanism)
        return PolyORB.Any.Content'Class;

      function Get_Aggregate_Count
        (Acc : Content_Ü_AuthorizationElement)
        return PolyORB.Types.Unsigned_Long;

      procedure Set_Aggregate_Count
        (Acc : in out Content_Ü_AuthorizationElement;
         Count : PolyORB.Types.Unsigned_Long);

      function Unchecked_Get_V
        (Acc : not null access Content_Ü_AuthorizationElement)
        return PolyORB.Types.Address;

      function Clone
        (Acc : Content_Ü_AuthorizationElement;
         Into : PolyORB.Any.Content_Ptr := null)
        return PolyORB.Any.Content_Ptr;

      procedure Finalize_Value
        (Acc : in out Content_Ü_AuthorizationElement);

      function Wrap
        (X : access CSI.AuthorizationElement)
        return PolyORB.Any.Content'Class;

      procedure Initialize_AuthorizationElement;

      function IDL_SEQUENCE_CSI_AuthorizationElement_Element_Wrap
        (X : access CSI.AuthorizationElement)
        return PolyORB.Any.Content'Class;

      function Wrap
        (X : access CSI.IDL_SEQUENCE_CSI_AuthorizationElement.Sequence)
        return PolyORB.Any.Content'Class;

      package IDL_SEQUENCE_CSI_AuthorizationElement_Helper is
        new CSI.IDL_SEQUENCE_CSI_AuthorizationElement.CORBA_Helper
           (Element_From_Any => CSI.Helper.From_Any,
            Element_To_Any => CSI.Helper.To_Any,
            Element_Wrap => CSI.Helper.Internals.IDL_SEQUENCE_CSI_AuthorizationElement_Element_Wrap);

      procedure Initialize_IDL_SEQUENCE_CSI_AuthorizationElement;

      procedure Initialize_AuthorizationToken;

      function Wrap
        (X : access CSI.IdentityTokenType)
        return PolyORB.Any.Content'Class;

      procedure Initialize_IdentityTokenType;

      function IDL_SEQUENCE_octet_7_Element_Wrap
        (X : access CORBA.Octet)
        return PolyORB.Any.Content'Class;

      function Wrap
        (X : access CSI.IDL_SEQUENCE_octet_7.Sequence)
        return PolyORB.Any.Content'Class;

      package IDL_SEQUENCE_octet_7_Helper is
        new CSI.IDL_SEQUENCE_octet_7.CORBA_Helper
           (Element_From_Any => CORBA.From_Any,
            Element_To_Any => CORBA.To_Any,
            Element_Wrap => CSI.Helper.Internals.IDL_SEQUENCE_octet_7_Element_Wrap);

      procedure Initialize_IDL_SEQUENCE_octet_7;

      procedure Initialize_IdentityExtension;

      type Ptr_Ü_IdentityToken is
        access all CSI.IdentityToken;

      type Content_Ü_IdentityToken is
        new PolyORB.Any.Aggregate_Content with record
            V : Ptr_Ü_IdentityToken;
            Switch_Cache : aliased CSI.IdentityTokenType;
         end record;

      function Get_Aggregate_Element
        (Acc : not null access Content_Ü_IdentityToken;
         Tc : PolyORB.Any.TypeCode.Object_Ptr;
         Index : PolyORB.Types.Unsigned_Long;
         Mech : not null access PolyORB.Any.Mechanism)
        return PolyORB.Any.Content'Class;

      procedure Set_Aggregate_Element
        (Acc : in out Content_Ü_IdentityToken;
         Tc : PolyORB.Any.TypeCode.Object_Ptr;
         Index : PolyORB.Types.Unsigned_Long;
         From_C : in out PolyORB.Any.Any_Container'Class);

      function Get_Aggregate_Count
        (Acc : Content_Ü_IdentityToken)
        return PolyORB.Types.Unsigned_Long;

      procedure Set_Aggregate_Count
        (Acc : in out Content_Ü_IdentityToken;
         Count : PolyORB.Types.Unsigned_Long);

      function Unchecked_Get_V
        (Acc : not null access Content_Ü_IdentityToken)
        return PolyORB.Types.Address;

      function Clone
        (Acc : Content_Ü_IdentityToken;
         Into : PolyORB.Any.Content_Ptr := null)
        return PolyORB.Any.Content_Ptr;

      procedure Finalize_Value
        (Acc : in out Content_Ü_IdentityToken);

      function Wrap
        (X : access CSI.IdentityToken)
        return PolyORB.Any.Content'Class;

      procedure Initialize_IdentityToken;

      type Ptr_Ü_EstablishContext is
        access all CSI.EstablishContext;

      type Content_Ü_EstablishContext is
        new PolyORB.Any.Aggregate_Content with record
            V : Ptr_Ü_EstablishContext;
         end record;

      function Get_Aggregate_Element
        (Acc : not null access Content_Ü_EstablishContext;
         Tc : PolyORB.Any.TypeCode.Object_Ptr;
         Index : PolyORB.Types.Unsigned_Long;
         Mech : not null access PolyORB.Any.Mechanism)
        return PolyORB.Any.Content'Class;

      function Get_Aggregate_Count
        (Acc : Content_Ü_EstablishContext)
        return PolyORB.Types.Unsigned_Long;

      procedure Set_Aggregate_Count
        (Acc : in out Content_Ü_EstablishContext;
         Count : PolyORB.Types.Unsigned_Long);

      function Unchecked_Get_V
        (Acc : not null access Content_Ü_EstablishContext)
        return PolyORB.Types.Address;

      function Clone
        (Acc : Content_Ü_EstablishContext;
         Into : PolyORB.Any.Content_Ptr := null)
        return PolyORB.Any.Content_Ptr;

      procedure Finalize_Value
        (Acc : in out Content_Ü_EstablishContext);

      function Wrap
        (X : access CSI.EstablishContext)
        return PolyORB.Any.Content'Class;

      procedure Initialize_EstablishContext;

      type Ptr_Ü_CompleteEstablishContext is
        access all CSI.CompleteEstablishContext;

      type Content_Ü_CompleteEstablishContext is
        new PolyORB.Any.Aggregate_Content with record
            V : Ptr_Ü_CompleteEstablishContext;
         end record;

      function Get_Aggregate_Element
        (Acc : not null access Content_Ü_CompleteEstablishContext;
         Tc : PolyORB.Any.TypeCode.Object_Ptr;
         Index : PolyORB.Types.Unsigned_Long;
         Mech : not null access PolyORB.Any.Mechanism)
        return PolyORB.Any.Content'Class;

      function Get_Aggregate_Count
        (Acc : Content_Ü_CompleteEstablishContext)
        return PolyORB.Types.Unsigned_Long;

      procedure Set_Aggregate_Count
        (Acc : in out Content_Ü_CompleteEstablishContext;
         Count : PolyORB.Types.Unsigned_Long);

      function Unchecked_Get_V
        (Acc : not null access Content_Ü_CompleteEstablishContext)
        return PolyORB.Types.Address;

      function Clone
        (Acc : Content_Ü_CompleteEstablishContext;
         Into : PolyORB.Any.Content_Ptr := null)
        return PolyORB.Any.Content_Ptr;

      procedure Finalize_Value
        (Acc : in out Content_Ü_CompleteEstablishContext);

      function Wrap
        (X : access CSI.CompleteEstablishContext)
        return PolyORB.Any.Content'Class;

      procedure Initialize_CompleteEstablishContext;

      type Ptr_Ü_ContextError is
        access all CSI.ContextError;

      type Content_Ü_ContextError is
        new PolyORB.Any.Aggregate_Content with record
            V : Ptr_Ü_ContextError;
         end record;

      function Get_Aggregate_Element
        (Acc : not null access Content_Ü_ContextError;
         Tc : PolyORB.Any.TypeCode.Object_Ptr;
         Index : PolyORB.Types.Unsigned_Long;
         Mech : not null access PolyORB.Any.Mechanism)
        return PolyORB.Any.Content'Class;

      function Get_Aggregate_Count
        (Acc : Content_Ü_ContextError)
        return PolyORB.Types.Unsigned_Long;

      procedure Set_Aggregate_Count
        (Acc : in out Content_Ü_ContextError;
         Count : PolyORB.Types.Unsigned_Long);

      function Unchecked_Get_V
        (Acc : not null access Content_Ü_ContextError)
        return PolyORB.Types.Address;

      function Clone
        (Acc : Content_Ü_ContextError;
         Into : PolyORB.Any.Content_Ptr := null)
        return PolyORB.Any.Content_Ptr;

      procedure Finalize_Value
        (Acc : in out Content_Ü_ContextError);

      function Wrap
        (X : access CSI.ContextError)
        return PolyORB.Any.Content'Class;

      procedure Initialize_ContextError;

      type Ptr_Ü_MessageInContext is
        access all CSI.MessageInContext;

      type Content_Ü_MessageInContext is
        new PolyORB.Any.Aggregate_Content with record
            V : Ptr_Ü_MessageInContext;
         end record;

      function Get_Aggregate_Element
        (Acc : not null access Content_Ü_MessageInContext;
         Tc : PolyORB.Any.TypeCode.Object_Ptr;
         Index : PolyORB.Types.Unsigned_Long;
         Mech : not null access PolyORB.Any.Mechanism)
        return PolyORB.Any.Content'Class;

      function Get_Aggregate_Count
        (Acc : Content_Ü_MessageInContext)
        return PolyORB.Types.Unsigned_Long;

      procedure Set_Aggregate_Count
        (Acc : in out Content_Ü_MessageInContext;
         Count : PolyORB.Types.Unsigned_Long);

      function Unchecked_Get_V
        (Acc : not null access Content_Ü_MessageInContext)
        return PolyORB.Types.Address;

      function Clone
        (Acc : Content_Ü_MessageInContext;
         Into : PolyORB.Any.Content_Ptr := null)
        return PolyORB.Any.Content_Ptr;

      procedure Finalize_Value
        (Acc : in out Content_Ü_MessageInContext);

      function Wrap
        (X : access CSI.MessageInContext)
        return PolyORB.Any.Content'Class;

      procedure Initialize_MessageInContext;

      type Ptr_Ü_SASContextBody is
        access all CSI.SASContextBody;

      type Content_Ü_SASContextBody is
        new PolyORB.Any.Aggregate_Content with record
            V : Ptr_Ü_SASContextBody;
            Switch_Cache : aliased CSI.MsgType;
         end record;

      function Get_Aggregate_Element
        (Acc : not null access Content_Ü_SASContextBody;
         Tc : PolyORB.Any.TypeCode.Object_Ptr;
         Index : PolyORB.Types.Unsigned_Long;
         Mech : not null access PolyORB.Any.Mechanism)
        return PolyORB.Any.Content'Class;

      procedure Set_Aggregate_Element
        (Acc : in out Content_Ü_SASContextBody;
         Tc : PolyORB.Any.TypeCode.Object_Ptr;
         Index : PolyORB.Types.Unsigned_Long;
         From_C : in out PolyORB.Any.Any_Container'Class);

      function Get_Aggregate_Count
        (Acc : Content_Ü_SASContextBody)
        return PolyORB.Types.Unsigned_Long;

      procedure Set_Aggregate_Count
        (Acc : in out Content_Ü_SASContextBody;
         Count : PolyORB.Types.Unsigned_Long);

      function Unchecked_Get_V
        (Acc : not null access Content_Ü_SASContextBody)
        return PolyORB.Types.Address;

      function Clone
        (Acc : Content_Ü_SASContextBody;
         Into : PolyORB.Any.Content_Ptr := null)
        return PolyORB.Any.Content_Ptr;

      procedure Finalize_Value
        (Acc : in out Content_Ü_SASContextBody);

      function Wrap
        (X : access CSI.SASContextBody)
        return PolyORB.Any.Content'Class;

      procedure Initialize_SASContextBody;

      function Wrap
        (X : access CSI.StringOID)
        return PolyORB.Any.Content'Class;

      procedure Initialize_StringOID;

   end Internals;

end CSI.Helper;

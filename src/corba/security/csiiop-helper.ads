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

with CORBA;
pragma Elaborate_All (CORBA);
with PolyORB.Any;
with PolyORB.Sequences.Unbounded.CORBA_Helper;
pragma Elaborate_All (PolyORB.Sequences.Unbounded.CORBA_Helper);
with PolyORB.Types;

package CSIIOP.Helper is

   TC_AssociationOptions : CORBA.TypeCode.Object;

   function From_Any
     (Item : CORBA.Any)
     return CSIIOP.AssociationOptions;

   function To_Any
     (Item : CSIIOP.AssociationOptions)
     return CORBA.Any;

   TC_ServiceConfigurationSyntax : CORBA.TypeCode.Object;

   function From_Any
     (Item : CORBA.Any)
     return CSIIOP.ServiceConfigurationSyntax;

   function To_Any
     (Item : CSIIOP.ServiceConfigurationSyntax)
     return CORBA.Any;

   TC_IDL_SEQUENCE_octet : CORBA.TypeCode.Object;

   function From_Any
     (Item : CORBA.Any)
     return CSIIOP.IDL_SEQUENCE_octet.Sequence;

   function To_Any
     (Item : CSIIOP.IDL_SEQUENCE_octet.Sequence)
     return CORBA.Any;

   TC_ServiceSpecificName : CORBA.TypeCode.Object;

   function From_Any
     (Item : CORBA.Any)
     return CSIIOP.ServiceSpecificName;

   function To_Any
     (Item : CSIIOP.ServiceSpecificName)
     return CORBA.Any;

   TC_ServiceConfiguration : CORBA.TypeCode.Object;

   function From_Any
     (Item : CORBA.Any)
     return CSIIOP.ServiceConfiguration;

   function To_Any
     (Item : CSIIOP.ServiceConfiguration)
     return CORBA.Any;

   TC_IDL_SEQUENCE_CSIIOP_ServiceConfiguration : CORBA.TypeCode.Object;

   function From_Any
     (Item : CORBA.Any)
     return CSIIOP.IDL_SEQUENCE_CSIIOP_ServiceConfiguration.Sequence;

   function To_Any
     (Item : CSIIOP.IDL_SEQUENCE_CSIIOP_ServiceConfiguration.Sequence)
     return CORBA.Any;

   TC_ServiceConfigurationList : CORBA.TypeCode.Object;

   function From_Any
     (Item : CORBA.Any)
     return CSIIOP.ServiceConfigurationList;

   function To_Any
     (Item : CSIIOP.ServiceConfigurationList)
     return CORBA.Any;

   TC_AS_ContextSec : CORBA.TypeCode.Object;

   function From_Any
     (Item : CORBA.Any)
     return CSIIOP.AS_ContextSec;

   function To_Any
     (Item : CSIIOP.AS_ContextSec)
     return CORBA.Any;

   TC_SAS_ContextSec : CORBA.TypeCode.Object;

   function From_Any
     (Item : CORBA.Any)
     return CSIIOP.SAS_ContextSec;

   function To_Any
     (Item : CSIIOP.SAS_ContextSec)
     return CORBA.Any;

   TC_CompoundSecMech : CORBA.TypeCode.Object;

   function From_Any
     (Item : CORBA.Any)
     return CSIIOP.CompoundSecMech;

   function To_Any
     (Item : CSIIOP.CompoundSecMech)
     return CORBA.Any;

   TC_IDL_SEQUENCE_CSIIOP_CompoundSecMech : CORBA.TypeCode.Object;

   function From_Any
     (Item : CORBA.Any)
     return CSIIOP.IDL_SEQUENCE_CSIIOP_CompoundSecMech.Sequence;

   function To_Any
     (Item : CSIIOP.IDL_SEQUENCE_CSIIOP_CompoundSecMech.Sequence)
     return CORBA.Any;

   TC_CompoundSecMechanisms : CORBA.TypeCode.Object;

   function From_Any
     (Item : CORBA.Any)
     return CSIIOP.CompoundSecMechanisms;

   function To_Any
     (Item : CSIIOP.CompoundSecMechanisms)
     return CORBA.Any;

   TC_CompoundSecMechList : CORBA.TypeCode.Object;

   function From_Any
     (Item : CORBA.Any)
     return CSIIOP.CompoundSecMechList;

   function To_Any
     (Item : CSIIOP.CompoundSecMechList)
     return CORBA.Any;

   TC_TransportAddress : CORBA.TypeCode.Object;

   function From_Any
     (Item : CORBA.Any)
     return CSIIOP.TransportAddress;

   function To_Any
     (Item : CSIIOP.TransportAddress)
     return CORBA.Any;

   TC_IDL_SEQUENCE_CSIIOP_TransportAddress : CORBA.TypeCode.Object;

   function From_Any
     (Item : CORBA.Any)
     return CSIIOP.IDL_SEQUENCE_CSIIOP_TransportAddress.Sequence;

   function To_Any
     (Item : CSIIOP.IDL_SEQUENCE_CSIIOP_TransportAddress.Sequence)
     return CORBA.Any;

   TC_TransportAddressList : CORBA.TypeCode.Object;

   function From_Any
     (Item : CORBA.Any)
     return CSIIOP.TransportAddressList;

   function To_Any
     (Item : CSIIOP.TransportAddressList)
     return CORBA.Any;

   TC_SECIOP_SEC_TRANS : CORBA.TypeCode.Object;

   function From_Any
     (Item : CORBA.Any)
     return CSIIOP.SECIOP_SEC_TRANS;

   function To_Any
     (Item : CSIIOP.SECIOP_SEC_TRANS)
     return CORBA.Any;

   TC_TLS_SEC_TRANS : CORBA.TypeCode.Object;

   function From_Any
     (Item : CORBA.Any)
     return CSIIOP.TLS_SEC_TRANS;

   function To_Any
     (Item : CSIIOP.TLS_SEC_TRANS)
     return CORBA.Any;

   
   package Internals is

      function Wrap
        (X : access CSIIOP.AssociationOptions)
        return PolyORB.Any.Content'Class;

      procedure Initialize_AssociationOptions;

      function Wrap
        (X : access CSIIOP.ServiceConfigurationSyntax)
        return PolyORB.Any.Content'Class;

      procedure Initialize_ServiceConfigurationSyntax;

      function IDL_SEQUENCE_octet_Element_Wrap
        (X : access CORBA.Octet)
        return PolyORB.Any.Content'Class;

      function Wrap
        (X : access CSIIOP.IDL_SEQUENCE_octet.Sequence)
        return PolyORB.Any.Content'Class;

      package IDL_SEQUENCE_octet_Helper is
        new CSIIOP.IDL_SEQUENCE_octet.CORBA_Helper
           (Element_From_Any => CORBA.From_Any,
            Element_To_Any => CORBA.To_Any,
            Element_Wrap => CSIIOP.Helper.Internals.IDL_SEQUENCE_octet_Element_Wrap);

      procedure Initialize_IDL_SEQUENCE_octet;

      procedure Initialize_ServiceSpecificName;

      type Ptr_Ü_ServiceConfiguration is
        access all CSIIOP.ServiceConfiguration;

      type Content_Ü_ServiceConfiguration is
        new PolyORB.Any.Aggregate_Content with record
            V : Ptr_Ü_ServiceConfiguration;
         end record;

      function Get_Aggregate_Element
        (Acc : not null access Content_Ü_ServiceConfiguration;
         Tc : PolyORB.Any.TypeCode.Object_Ptr;
         Index : PolyORB.Types.Unsigned_Long;
         Mech : not null access PolyORB.Any.Mechanism)
        return PolyORB.Any.Content'Class;

      function Get_Aggregate_Count
        (Acc : Content_Ü_ServiceConfiguration)
        return PolyORB.Types.Unsigned_Long;

      procedure Set_Aggregate_Count
        (Acc : in out Content_Ü_ServiceConfiguration;
         Count : PolyORB.Types.Unsigned_Long);

      function Unchecked_Get_V
        (Acc : not null access Content_Ü_ServiceConfiguration)
        return PolyORB.Types.Address;

      function Clone
        (Acc : Content_Ü_ServiceConfiguration;
         Into : PolyORB.Any.Content_Ptr := null)
        return PolyORB.Any.Content_Ptr;

      procedure Finalize_Value
        (Acc : in out Content_Ü_ServiceConfiguration);

      function Wrap
        (X : access CSIIOP.ServiceConfiguration)
        return PolyORB.Any.Content'Class;

      procedure Initialize_ServiceConfiguration;

      function IDL_SEQUENCE_CSIIOP_ServiceConfiguration_Element_Wrap
        (X : access CSIIOP.ServiceConfiguration)
        return PolyORB.Any.Content'Class;

      function Wrap
        (X : access CSIIOP.IDL_SEQUENCE_CSIIOP_ServiceConfiguration.Sequence)
        return PolyORB.Any.Content'Class;

      package IDL_SEQUENCE_CSIIOP_ServiceConfiguration_Helper is
        new CSIIOP.IDL_SEQUENCE_CSIIOP_ServiceConfiguration.CORBA_Helper
           (Element_From_Any => CSIIOP.Helper.From_Any,
            Element_To_Any => CSIIOP.Helper.To_Any,
            Element_Wrap => CSIIOP.Helper.Internals.IDL_SEQUENCE_CSIIOP_ServiceConfiguration_Element_Wrap);

      procedure Initialize_IDL_SEQUENCE_CSIIOP_ServiceConfiguration;

      procedure Initialize_ServiceConfigurationList;

      type Ptr_Ü_AS_ContextSec is
        access all CSIIOP.AS_ContextSec;

      type Content_Ü_AS_ContextSec is
        new PolyORB.Any.Aggregate_Content with record
            V : Ptr_Ü_AS_ContextSec;
         end record;

      function Get_Aggregate_Element
        (Acc : not null access Content_Ü_AS_ContextSec;
         Tc : PolyORB.Any.TypeCode.Object_Ptr;
         Index : PolyORB.Types.Unsigned_Long;
         Mech : not null access PolyORB.Any.Mechanism)
        return PolyORB.Any.Content'Class;

      function Get_Aggregate_Count
        (Acc : Content_Ü_AS_ContextSec)
        return PolyORB.Types.Unsigned_Long;

      procedure Set_Aggregate_Count
        (Acc : in out Content_Ü_AS_ContextSec;
         Count : PolyORB.Types.Unsigned_Long);

      function Unchecked_Get_V
        (Acc : not null access Content_Ü_AS_ContextSec)
        return PolyORB.Types.Address;

      function Clone
        (Acc : Content_Ü_AS_ContextSec;
         Into : PolyORB.Any.Content_Ptr := null)
        return PolyORB.Any.Content_Ptr;

      procedure Finalize_Value
        (Acc : in out Content_Ü_AS_ContextSec);

      function Wrap
        (X : access CSIIOP.AS_ContextSec)
        return PolyORB.Any.Content'Class;

      procedure Initialize_AS_ContextSec;

      type Ptr_Ü_SAS_ContextSec is
        access all CSIIOP.SAS_ContextSec;

      type Content_Ü_SAS_ContextSec is
        new PolyORB.Any.Aggregate_Content with record
            V : Ptr_Ü_SAS_ContextSec;
         end record;

      function Get_Aggregate_Element
        (Acc : not null access Content_Ü_SAS_ContextSec;
         Tc : PolyORB.Any.TypeCode.Object_Ptr;
         Index : PolyORB.Types.Unsigned_Long;
         Mech : not null access PolyORB.Any.Mechanism)
        return PolyORB.Any.Content'Class;

      function Get_Aggregate_Count
        (Acc : Content_Ü_SAS_ContextSec)
        return PolyORB.Types.Unsigned_Long;

      procedure Set_Aggregate_Count
        (Acc : in out Content_Ü_SAS_ContextSec;
         Count : PolyORB.Types.Unsigned_Long);

      function Unchecked_Get_V
        (Acc : not null access Content_Ü_SAS_ContextSec)
        return PolyORB.Types.Address;

      function Clone
        (Acc : Content_Ü_SAS_ContextSec;
         Into : PolyORB.Any.Content_Ptr := null)
        return PolyORB.Any.Content_Ptr;

      procedure Finalize_Value
        (Acc : in out Content_Ü_SAS_ContextSec);

      function Wrap
        (X : access CSIIOP.SAS_ContextSec)
        return PolyORB.Any.Content'Class;

      procedure Initialize_SAS_ContextSec;

      type Ptr_Ü_CompoundSecMech is
        access all CSIIOP.CompoundSecMech;

      type Content_Ü_CompoundSecMech is
        new PolyORB.Any.Aggregate_Content with record
            V : Ptr_Ü_CompoundSecMech;
         end record;

      function Get_Aggregate_Element
        (Acc : not null access Content_Ü_CompoundSecMech;
         Tc : PolyORB.Any.TypeCode.Object_Ptr;
         Index : PolyORB.Types.Unsigned_Long;
         Mech : not null access PolyORB.Any.Mechanism)
        return PolyORB.Any.Content'Class;

      function Get_Aggregate_Count
        (Acc : Content_Ü_CompoundSecMech)
        return PolyORB.Types.Unsigned_Long;

      procedure Set_Aggregate_Count
        (Acc : in out Content_Ü_CompoundSecMech;
         Count : PolyORB.Types.Unsigned_Long);

      function Unchecked_Get_V
        (Acc : not null access Content_Ü_CompoundSecMech)
        return PolyORB.Types.Address;

      function Clone
        (Acc : Content_Ü_CompoundSecMech;
         Into : PolyORB.Any.Content_Ptr := null)
        return PolyORB.Any.Content_Ptr;

      procedure Finalize_Value
        (Acc : in out Content_Ü_CompoundSecMech);

      function Wrap
        (X : access CSIIOP.CompoundSecMech)
        return PolyORB.Any.Content'Class;

      procedure Initialize_CompoundSecMech;

      function IDL_SEQUENCE_CSIIOP_CompoundSecMech_Element_Wrap
        (X : access CSIIOP.CompoundSecMech)
        return PolyORB.Any.Content'Class;

      function Wrap
        (X : access CSIIOP.IDL_SEQUENCE_CSIIOP_CompoundSecMech.Sequence)
        return PolyORB.Any.Content'Class;

      package IDL_SEQUENCE_CSIIOP_CompoundSecMech_Helper is
        new CSIIOP.IDL_SEQUENCE_CSIIOP_CompoundSecMech.CORBA_Helper
           (Element_From_Any => CSIIOP.Helper.From_Any,
            Element_To_Any => CSIIOP.Helper.To_Any,
            Element_Wrap => CSIIOP.Helper.Internals.IDL_SEQUENCE_CSIIOP_CompoundSecMech_Element_Wrap);

      procedure Initialize_IDL_SEQUENCE_CSIIOP_CompoundSecMech;

      procedure Initialize_CompoundSecMechanisms;

      type Ptr_Ü_CompoundSecMechList is
        access all CSIIOP.CompoundSecMechList;

      type Content_Ü_CompoundSecMechList is
        new PolyORB.Any.Aggregate_Content with record
            V : Ptr_Ü_CompoundSecMechList;
         end record;

      function Get_Aggregate_Element
        (Acc : not null access Content_Ü_CompoundSecMechList;
         Tc : PolyORB.Any.TypeCode.Object_Ptr;
         Index : PolyORB.Types.Unsigned_Long;
         Mech : not null access PolyORB.Any.Mechanism)
        return PolyORB.Any.Content'Class;

      function Get_Aggregate_Count
        (Acc : Content_Ü_CompoundSecMechList)
        return PolyORB.Types.Unsigned_Long;

      procedure Set_Aggregate_Count
        (Acc : in out Content_Ü_CompoundSecMechList;
         Count : PolyORB.Types.Unsigned_Long);

      function Unchecked_Get_V
        (Acc : not null access Content_Ü_CompoundSecMechList)
        return PolyORB.Types.Address;

      function Clone
        (Acc : Content_Ü_CompoundSecMechList;
         Into : PolyORB.Any.Content_Ptr := null)
        return PolyORB.Any.Content_Ptr;

      procedure Finalize_Value
        (Acc : in out Content_Ü_CompoundSecMechList);

      function Wrap
        (X : access CSIIOP.CompoundSecMechList)
        return PolyORB.Any.Content'Class;

      procedure Initialize_CompoundSecMechList;

      type Ptr_Ü_TransportAddress is
        access all CSIIOP.TransportAddress;

      type Content_Ü_TransportAddress is
        new PolyORB.Any.Aggregate_Content with record
            V : Ptr_Ü_TransportAddress;
         end record;

      function Get_Aggregate_Element
        (Acc : not null access Content_Ü_TransportAddress;
         Tc : PolyORB.Any.TypeCode.Object_Ptr;
         Index : PolyORB.Types.Unsigned_Long;
         Mech : not null access PolyORB.Any.Mechanism)
        return PolyORB.Any.Content'Class;

      function Get_Aggregate_Count
        (Acc : Content_Ü_TransportAddress)
        return PolyORB.Types.Unsigned_Long;

      procedure Set_Aggregate_Count
        (Acc : in out Content_Ü_TransportAddress;
         Count : PolyORB.Types.Unsigned_Long);

      function Unchecked_Get_V
        (Acc : not null access Content_Ü_TransportAddress)
        return PolyORB.Types.Address;

      function Clone
        (Acc : Content_Ü_TransportAddress;
         Into : PolyORB.Any.Content_Ptr := null)
        return PolyORB.Any.Content_Ptr;

      procedure Finalize_Value
        (Acc : in out Content_Ü_TransportAddress);

      function Wrap
        (X : access CSIIOP.TransportAddress)
        return PolyORB.Any.Content'Class;

      procedure Initialize_TransportAddress;

      function IDL_SEQUENCE_CSIIOP_TransportAddress_Element_Wrap
        (X : access CSIIOP.TransportAddress)
        return PolyORB.Any.Content'Class;

      function Wrap
        (X : access CSIIOP.IDL_SEQUENCE_CSIIOP_TransportAddress.Sequence)
        return PolyORB.Any.Content'Class;

      package IDL_SEQUENCE_CSIIOP_TransportAddress_Helper is
        new CSIIOP.IDL_SEQUENCE_CSIIOP_TransportAddress.CORBA_Helper
           (Element_From_Any => CSIIOP.Helper.From_Any,
            Element_To_Any => CSIIOP.Helper.To_Any,
            Element_Wrap => CSIIOP.Helper.Internals.IDL_SEQUENCE_CSIIOP_TransportAddress_Element_Wrap);

      procedure Initialize_IDL_SEQUENCE_CSIIOP_TransportAddress;

      procedure Initialize_TransportAddressList;

      type Ptr_Ü_SECIOP_SEC_TRANS is
        access all CSIIOP.SECIOP_SEC_TRANS;

      type Content_Ü_SECIOP_SEC_TRANS is
        new PolyORB.Any.Aggregate_Content with record
            V : Ptr_Ü_SECIOP_SEC_TRANS;
         end record;

      function Get_Aggregate_Element
        (Acc : not null access Content_Ü_SECIOP_SEC_TRANS;
         Tc : PolyORB.Any.TypeCode.Object_Ptr;
         Index : PolyORB.Types.Unsigned_Long;
         Mech : not null access PolyORB.Any.Mechanism)
        return PolyORB.Any.Content'Class;

      function Get_Aggregate_Count
        (Acc : Content_Ü_SECIOP_SEC_TRANS)
        return PolyORB.Types.Unsigned_Long;

      procedure Set_Aggregate_Count
        (Acc : in out Content_Ü_SECIOP_SEC_TRANS;
         Count : PolyORB.Types.Unsigned_Long);

      function Unchecked_Get_V
        (Acc : not null access Content_Ü_SECIOP_SEC_TRANS)
        return PolyORB.Types.Address;

      function Clone
        (Acc : Content_Ü_SECIOP_SEC_TRANS;
         Into : PolyORB.Any.Content_Ptr := null)
        return PolyORB.Any.Content_Ptr;

      procedure Finalize_Value
        (Acc : in out Content_Ü_SECIOP_SEC_TRANS);

      function Wrap
        (X : access CSIIOP.SECIOP_SEC_TRANS)
        return PolyORB.Any.Content'Class;

      procedure Initialize_SECIOP_SEC_TRANS;

      type Ptr_Ü_TLS_SEC_TRANS is
        access all CSIIOP.TLS_SEC_TRANS;

      type Content_Ü_TLS_SEC_TRANS is
        new PolyORB.Any.Aggregate_Content with record
            V : Ptr_Ü_TLS_SEC_TRANS;
         end record;

      function Get_Aggregate_Element
        (Acc : not null access Content_Ü_TLS_SEC_TRANS;
         Tc : PolyORB.Any.TypeCode.Object_Ptr;
         Index : PolyORB.Types.Unsigned_Long;
         Mech : not null access PolyORB.Any.Mechanism)
        return PolyORB.Any.Content'Class;

      function Get_Aggregate_Count
        (Acc : Content_Ü_TLS_SEC_TRANS)
        return PolyORB.Types.Unsigned_Long;

      procedure Set_Aggregate_Count
        (Acc : in out Content_Ü_TLS_SEC_TRANS;
         Count : PolyORB.Types.Unsigned_Long);

      function Unchecked_Get_V
        (Acc : not null access Content_Ü_TLS_SEC_TRANS)
        return PolyORB.Types.Address;

      function Clone
        (Acc : Content_Ü_TLS_SEC_TRANS;
         Into : PolyORB.Any.Content_Ptr := null)
        return PolyORB.Any.Content_Ptr;

      procedure Finalize_Value
        (Acc : in out Content_Ü_TLS_SEC_TRANS);

      function Wrap
        (X : access CSIIOP.TLS_SEC_TRANS)
        return PolyORB.Any.Content'Class;

      procedure Initialize_TLS_SEC_TRANS;

   end Internals;

end CSIIOP.Helper;

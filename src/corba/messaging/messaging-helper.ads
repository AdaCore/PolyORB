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

with CORBA;
pragma Elaborate_All (CORBA);
with PolyORB.Any;
with PolyORB.Types;
with PolyORB.Sequences.Unbounded.CORBA_Helper;
pragma Elaborate_All (PolyORB.Sequences.Unbounded.CORBA_Helper);

package Messaging.Helper is

   TC_RebindMode : CORBA.TypeCode.Object;

   function From_Any
     (Item : CORBA.Any)
     return Messaging.RebindMode;

   function To_Any
     (Item : Messaging.RebindMode)
     return CORBA.Any;

   TC_SyncScope : CORBA.TypeCode.Object;

   function From_Any
     (Item : CORBA.Any)
     return Messaging.SyncScope;

   function To_Any
     (Item : Messaging.SyncScope)
     return CORBA.Any;

   TC_RoutingType : CORBA.TypeCode.Object;

   function From_Any
     (Item : CORBA.Any)
     return Messaging.RoutingType;

   function To_Any
     (Item : Messaging.RoutingType)
     return CORBA.Any;

   TC_Priority : CORBA.TypeCode.Object;

   function From_Any
     (Item : CORBA.Any)
     return Messaging.Priority;

   function To_Any
     (Item : Messaging.Priority)
     return CORBA.Any;

   TC_Ordering : CORBA.TypeCode.Object;

   function From_Any
     (Item : CORBA.Any)
     return Messaging.Ordering;

   function To_Any
     (Item : Messaging.Ordering)
     return CORBA.Any;

   TC_PriorityRange : CORBA.TypeCode.Object;

   function From_Any
     (Item : CORBA.Any)
     return Messaging.PriorityRange;

   function To_Any
     (Item : Messaging.PriorityRange)
     return CORBA.Any;

   TC_RoutingTypeRange : CORBA.TypeCode.Object;

   function From_Any
     (Item : CORBA.Any)
     return Messaging.RoutingTypeRange;

   function To_Any
     (Item : Messaging.RoutingTypeRange)
     return CORBA.Any;

   TC_IDL_SEQUENCE_octet : CORBA.TypeCode.Object;

   function From_Any
     (Item : CORBA.Any)
     return Messaging.IDL_SEQUENCE_octet.Sequence;

   function To_Any
     (Item : Messaging.IDL_SEQUENCE_octet.Sequence)
     return CORBA.Any;

   TC_IDL_AT_Sequence_octet : CORBA.TypeCode.Object;

   function From_Any
     (Item : CORBA.Any)
     return Messaging.IDL_AT_Sequence_octet;

   function To_Any
     (Item : Messaging.IDL_AT_Sequence_octet)
     return CORBA.Any;

   TC_PolicyValue : CORBA.TypeCode.Object;

   function From_Any
     (Item : CORBA.Any)
     return Messaging.PolicyValue;

   function To_Any
     (Item : Messaging.PolicyValue)
     return CORBA.Any;

   TC_IDL_SEQUENCE_Messaging_PolicyValue : CORBA.TypeCode.Object;

   function From_Any
     (Item : CORBA.Any)
     return Messaging.IDL_SEQUENCE_Messaging_PolicyValue.Sequence;

   function To_Any
     (Item : Messaging.IDL_SEQUENCE_Messaging_PolicyValue.Sequence)
     return CORBA.Any;

   TC_PolicyValueSeq : CORBA.TypeCode.Object;

   function From_Any
     (Item : CORBA.Any)
     return Messaging.PolicyValueSeq;

   function To_Any
     (Item : Messaging.PolicyValueSeq)
     return CORBA.Any;

   
   package Internals is

      function Wrap
        (X : access Messaging.RebindMode)
        return PolyORB.Any.Content'Class;

      procedure Initialize_RebindMode;

      function Wrap
        (X : access Messaging.SyncScope)
        return PolyORB.Any.Content'Class;

      procedure Initialize_SyncScope;

      function Wrap
        (X : access Messaging.RoutingType)
        return PolyORB.Any.Content'Class;

      procedure Initialize_RoutingType;

      function Wrap
        (X : access Messaging.Priority)
        return PolyORB.Any.Content'Class;

      procedure Initialize_Priority;

      function Wrap
        (X : access Messaging.Ordering)
        return PolyORB.Any.Content'Class;

      procedure Initialize_Ordering;

      type Ptr_Ü_PriorityRange is
        access all Messaging.PriorityRange;

      type Content_Ü_PriorityRange is
        new PolyORB.Any.Aggregate_Content with record
            V : Ptr_Ü_PriorityRange;
         end record;

      function Get_Aggregate_Element
        (Acc : not null access Content_Ü_PriorityRange;
         Tc : PolyORB.Any.TypeCode.Object_Ptr;
         Index : PolyORB.Types.Unsigned_Long;
         Mech : not null access PolyORB.Any.Mechanism)
        return PolyORB.Any.Content'Class;

      function Get_Aggregate_Count
        (Acc : Content_Ü_PriorityRange)
        return PolyORB.Types.Unsigned_Long;

      procedure Set_Aggregate_Count
        (Acc : in out Content_Ü_PriorityRange;
         Count : PolyORB.Types.Unsigned_Long);

      function Unchecked_Get_V
        (Acc : not null access Content_Ü_PriorityRange)
        return PolyORB.Types.Address;

      function Clone
        (Acc : Content_Ü_PriorityRange;
         Into : PolyORB.Any.Content_Ptr := null)
        return PolyORB.Any.Content_Ptr;

      procedure Finalize_Value
        (Acc : in out Content_Ü_PriorityRange);

      function Wrap
        (X : access Messaging.PriorityRange)
        return PolyORB.Any.Content'Class;

      procedure Initialize_PriorityRange;

      type Ptr_Ü_RoutingTypeRange is
        access all Messaging.RoutingTypeRange;

      type Content_Ü_RoutingTypeRange is
        new PolyORB.Any.Aggregate_Content with record
            V : Ptr_Ü_RoutingTypeRange;
         end record;

      function Get_Aggregate_Element
        (Acc : not null access Content_Ü_RoutingTypeRange;
         Tc : PolyORB.Any.TypeCode.Object_Ptr;
         Index : PolyORB.Types.Unsigned_Long;
         Mech : not null access PolyORB.Any.Mechanism)
        return PolyORB.Any.Content'Class;

      function Get_Aggregate_Count
        (Acc : Content_Ü_RoutingTypeRange)
        return PolyORB.Types.Unsigned_Long;

      procedure Set_Aggregate_Count
        (Acc : in out Content_Ü_RoutingTypeRange;
         Count : PolyORB.Types.Unsigned_Long);

      function Unchecked_Get_V
        (Acc : not null access Content_Ü_RoutingTypeRange)
        return PolyORB.Types.Address;

      function Clone
        (Acc : Content_Ü_RoutingTypeRange;
         Into : PolyORB.Any.Content_Ptr := null)
        return PolyORB.Any.Content_Ptr;

      procedure Finalize_Value
        (Acc : in out Content_Ü_RoutingTypeRange);

      function Wrap
        (X : access Messaging.RoutingTypeRange)
        return PolyORB.Any.Content'Class;

      procedure Initialize_RoutingTypeRange;

      function IDL_SEQUENCE_octet_Element_Wrap
        (X : access CORBA.Octet)
        return PolyORB.Any.Content'Class;

      function Wrap
        (X : access Messaging.IDL_SEQUENCE_octet.Sequence)
        return PolyORB.Any.Content'Class;

      package IDL_SEQUENCE_octet_Helper is
        new Messaging.IDL_SEQUENCE_octet.CORBA_Helper
           (Element_From_Any => CORBA.From_Any,
            Element_To_Any => CORBA.To_Any,
            Element_Wrap => Messaging.Helper.Internals.IDL_SEQUENCE_octet_Element_Wrap);

      procedure Initialize_IDL_SEQUENCE_octet;

      procedure Initialize_IDL_AT_Sequence_octet;

      type Ptr_Ü_PolicyValue is
        access all Messaging.PolicyValue;

      type Content_Ü_PolicyValue is
        new PolyORB.Any.Aggregate_Content with record
            V : Ptr_Ü_PolicyValue;
         end record;

      function Get_Aggregate_Element
        (Acc : not null access Content_Ü_PolicyValue;
         Tc : PolyORB.Any.TypeCode.Object_Ptr;
         Index : PolyORB.Types.Unsigned_Long;
         Mech : not null access PolyORB.Any.Mechanism)
        return PolyORB.Any.Content'Class;

      function Get_Aggregate_Count
        (Acc : Content_Ü_PolicyValue)
        return PolyORB.Types.Unsigned_Long;

      procedure Set_Aggregate_Count
        (Acc : in out Content_Ü_PolicyValue;
         Count : PolyORB.Types.Unsigned_Long);

      function Unchecked_Get_V
        (Acc : not null access Content_Ü_PolicyValue)
        return PolyORB.Types.Address;

      function Clone
        (Acc : Content_Ü_PolicyValue;
         Into : PolyORB.Any.Content_Ptr := null)
        return PolyORB.Any.Content_Ptr;

      procedure Finalize_Value
        (Acc : in out Content_Ü_PolicyValue);

      function Wrap
        (X : access Messaging.PolicyValue)
        return PolyORB.Any.Content'Class;

      procedure Initialize_PolicyValue;

      function IDL_SEQUENCE_Messaging_PolicyValue_Element_Wrap
        (X : access Messaging.PolicyValue)
        return PolyORB.Any.Content'Class;

      function Wrap
        (X : access Messaging.IDL_SEQUENCE_Messaging_PolicyValue.Sequence)
        return PolyORB.Any.Content'Class;

      package IDL_SEQUENCE_Messaging_PolicyValue_Helper is
        new Messaging.IDL_SEQUENCE_Messaging_PolicyValue.CORBA_Helper
           (Element_From_Any => Messaging.Helper.From_Any,
            Element_To_Any => Messaging.Helper.To_Any,
            Element_Wrap => Messaging.Helper.Internals.IDL_SEQUENCE_Messaging_PolicyValue_Element_Wrap);

      procedure Initialize_IDL_SEQUENCE_Messaging_PolicyValue;

      procedure Initialize_PolicyValueSeq;

   end Internals;

end Messaging.Helper;

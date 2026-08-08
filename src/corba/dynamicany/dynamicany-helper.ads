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

with CORBA;
pragma Elaborate_All (CORBA);
with CORBA.Object;
with PolyORB.Any;
with PolyORB.Types;
with PolyORB.Sequences.Unbounded.CORBA_Helper;
pragma Elaborate_All (PolyORB.Sequences.Unbounded.CORBA_Helper);

package DynamicAny.Helper is

   TC_DynAny : CORBA.TypeCode.Object;

   function Unchecked_To_Local_Ref
     (The_Ref : CORBA.Object.Ref'Class)
     return DynamicAny.DynAny_Forward.Ref;

   function To_Local_Ref
     (The_Ref : CORBA.Object.Ref'Class)
     return DynamicAny.DynAny_Forward.Ref;

   TC_FieldName : CORBA.TypeCode.Object;

   function From_Any
     (Item : CORBA.Any)
     return DynamicAny.FieldName;

   function To_Any
     (Item : DynamicAny.FieldName)
     return CORBA.Any;

   TC_NameValuePair : CORBA.TypeCode.Object;

   function From_Any
     (Item : CORBA.Any)
     return DynamicAny.NameValuePair;

   function To_Any
     (Item : DynamicAny.NameValuePair)
     return CORBA.Any;

   TC_IDL_SEQUENCE_DynamicAny_NameValuePair : CORBA.TypeCode.Object;

   function From_Any
     (Item : CORBA.Any)
     return DynamicAny.IDL_SEQUENCE_DynamicAny_NameValuePair.Sequence;

   function To_Any
     (Item : DynamicAny.IDL_SEQUENCE_DynamicAny_NameValuePair.Sequence)
     return CORBA.Any;

   TC_NameValuePairSeq : CORBA.TypeCode.Object;

   function From_Any
     (Item : CORBA.Any)
     return DynamicAny.NameValuePairSeq;

   function To_Any
     (Item : DynamicAny.NameValuePairSeq)
     return CORBA.Any;

   TC_NameDynAnyPair : CORBA.TypeCode.Object;

   TC_IDL_SEQUENCE_DynamicAny_NameDynAnyPair : CORBA.TypeCode.Object;

   TC_NameDynAnyPairSeq : CORBA.TypeCode.Object;

   TC_IDL_SEQUENCE_any : CORBA.TypeCode.Object;

   function From_Any
     (Item : CORBA.Any)
     return DynamicAny.IDL_SEQUENCE_any.Sequence;

   function To_Any
     (Item : DynamicAny.IDL_SEQUENCE_any.Sequence)
     return CORBA.Any;

   TC_AnySeq : CORBA.TypeCode.Object;

   function From_Any
     (Item : CORBA.Any)
     return DynamicAny.AnySeq;

   function To_Any
     (Item : DynamicAny.AnySeq)
     return CORBA.Any;

   TC_IDL_SEQUENCE_DynamicAny_DynAny_Forward : CORBA.TypeCode.Object;

   TC_DynAnySeq : CORBA.TypeCode.Object;

   TC_MustTruncate : CORBA.TypeCode.Object;

   function From_Any
     (Item : CORBA.Any)
     return DynamicAny.MustTruncate_Members;

   function To_Any
     (Item : DynamicAny.MustTruncate_Members)
     return CORBA.Any;

   procedure Raise_MustTruncate
     (Members : DynamicAny.MustTruncate_Members);

   pragma No_Return (Raise_MustTruncate);

   
   package Internals is

      procedure Initialize_DynAny;

      function Wrap
        (X : access DynamicAny.FieldName)
        return PolyORB.Any.Content'Class;

      procedure Initialize_FieldName;

      type Ptr_Ü_NameValuePair is
        access all DynamicAny.NameValuePair;

      type Content_Ü_NameValuePair is
        new PolyORB.Any.Aggregate_Content with record
            V : Ptr_Ü_NameValuePair;
         end record;

      function Get_Aggregate_Element
        (Acc : not null access Content_Ü_NameValuePair;
         Tc : PolyORB.Any.TypeCode.Object_Ptr;
         Index : PolyORB.Types.Unsigned_Long;
         Mech : not null access PolyORB.Any.Mechanism)
        return PolyORB.Any.Content'Class;

      function Get_Aggregate_Count
        (Acc : Content_Ü_NameValuePair)
        return PolyORB.Types.Unsigned_Long;

      procedure Set_Aggregate_Count
        (Acc : in out Content_Ü_NameValuePair;
         Count : PolyORB.Types.Unsigned_Long);

      function Unchecked_Get_V
        (Acc : not null access Content_Ü_NameValuePair)
        return PolyORB.Types.Address;

      function Clone
        (Acc : Content_Ü_NameValuePair;
         Into : PolyORB.Any.Content_Ptr := null)
        return PolyORB.Any.Content_Ptr;

      procedure Finalize_Value
        (Acc : in out Content_Ü_NameValuePair);

      function Wrap
        (X : access DynamicAny.NameValuePair)
        return PolyORB.Any.Content'Class;

      procedure Initialize_NameValuePair;

      function IDL_SEQUENCE_DynamicAny_NameValuePair_Element_Wrap
        (X : access DynamicAny.NameValuePair)
        return PolyORB.Any.Content'Class;

      function Wrap
        (X : access DynamicAny.IDL_SEQUENCE_DynamicAny_NameValuePair.Sequence)
        return PolyORB.Any.Content'Class;

      package IDL_SEQUENCE_DynamicAny_NameValuePair_Helper is
        new DynamicAny.IDL_SEQUENCE_DynamicAny_NameValuePair.CORBA_Helper
           (Element_From_Any => DynamicAny.Helper.From_Any,
            Element_To_Any => DynamicAny.Helper.To_Any,
            Element_Wrap => DynamicAny.Helper.Internals.IDL_SEQUENCE_DynamicAny_NameValuePair_Element_Wrap);

      procedure Initialize_IDL_SEQUENCE_DynamicAny_NameValuePair;

      procedure Initialize_NameValuePairSeq;

      type Ptr_Ü_NameDynAnyPair is
        access all DynamicAny.NameDynAnyPair;

      type Content_Ü_NameDynAnyPair is
        new PolyORB.Any.Aggregate_Content with record
            V : Ptr_Ü_NameDynAnyPair;
         end record;

      function Get_Aggregate_Element
        (Acc : not null access Content_Ü_NameDynAnyPair;
         Tc : PolyORB.Any.TypeCode.Object_Ptr;
         Index : PolyORB.Types.Unsigned_Long;
         Mech : not null access PolyORB.Any.Mechanism)
        return PolyORB.Any.Content'Class;

      function Get_Aggregate_Count
        (Acc : Content_Ü_NameDynAnyPair)
        return PolyORB.Types.Unsigned_Long;

      procedure Set_Aggregate_Count
        (Acc : in out Content_Ü_NameDynAnyPair;
         Count : PolyORB.Types.Unsigned_Long);

      function Unchecked_Get_V
        (Acc : not null access Content_Ü_NameDynAnyPair)
        return PolyORB.Types.Address;

      function Clone
        (Acc : Content_Ü_NameDynAnyPair;
         Into : PolyORB.Any.Content_Ptr := null)
        return PolyORB.Any.Content_Ptr;

      procedure Finalize_Value
        (Acc : in out Content_Ü_NameDynAnyPair);

      function Wrap
        (X : access DynamicAny.NameDynAnyPair)
        return PolyORB.Any.Content'Class;

      procedure Initialize_NameDynAnyPair;

      procedure Initialize_IDL_SEQUENCE_DynamicAny_NameDynAnyPair;

      procedure Initialize_NameDynAnyPairSeq;

      function IDL_SEQUENCE_any_Element_Wrap
        (X : access CORBA.Any)
        return PolyORB.Any.Content'Class;

      function Wrap
        (X : access DynamicAny.IDL_SEQUENCE_any.Sequence)
        return PolyORB.Any.Content'Class;

      package IDL_SEQUENCE_any_Helper is
        new DynamicAny.IDL_SEQUENCE_any.CORBA_Helper
           (Element_From_Any => CORBA.From_Any,
            Element_To_Any => CORBA.To_Any,
            Element_Wrap => DynamicAny.Helper.Internals.IDL_SEQUENCE_any_Element_Wrap);

      procedure Initialize_IDL_SEQUENCE_any;

      procedure Initialize_AnySeq;

      procedure Initialize_IDL_SEQUENCE_DynamicAny_DynAny_Forward;

      procedure Initialize_DynAnySeq;

      procedure Initialize_MustTruncate;

   end Internals;

end DynamicAny.Helper;

pragma Style_Checks ("NM32766");
pragma Wide_Character_Encoding (Brackets);
pragma Warnings (Off, "use of an anonymous access type allocator");

---------------------------------------------------
--  This file has been generated automatically from
--  /Users/heathdorn/Documents/Playground/Agents/RefactorTeam/code_refactor/PolyORB/idls/Interop/CONV_FRAME.idl
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

package CONV_FRAME.Helper is

   TC_CodeSetId : CORBA.TypeCode.Object;

   function From_Any
     (Item : CORBA.Any)
     return CONV_FRAME.CodeSetId;

   function To_Any
     (Item : CONV_FRAME.CodeSetId)
     return CORBA.Any;

   TC_IDL_SEQUENCE_CONV_FRAME_CodeSetId : CORBA.TypeCode.Object;

   function From_Any
     (Item : CORBA.Any)
     return CONV_FRAME.IDL_SEQUENCE_CONV_FRAME_CodeSetId.Sequence;

   function To_Any
     (Item : CONV_FRAME.IDL_SEQUENCE_CONV_FRAME_CodeSetId.Sequence)
     return CORBA.Any;

   TC_CodeSetIdSeq : CORBA.TypeCode.Object;

   function From_Any
     (Item : CORBA.Any)
     return CONV_FRAME.CodeSetIdSeq;

   function To_Any
     (Item : CONV_FRAME.CodeSetIdSeq)
     return CORBA.Any;

   TC_CodeSetComponent : CORBA.TypeCode.Object;

   function From_Any
     (Item : CORBA.Any)
     return CONV_FRAME.CodeSetComponent;

   function To_Any
     (Item : CONV_FRAME.CodeSetComponent)
     return CORBA.Any;

   TC_CodeSetComponentInfo : CORBA.TypeCode.Object;

   function From_Any
     (Item : CORBA.Any)
     return CONV_FRAME.CodeSetComponentInfo;

   function To_Any
     (Item : CONV_FRAME.CodeSetComponentInfo)
     return CORBA.Any;

   TC_CodeSetContext : CORBA.TypeCode.Object;

   function From_Any
     (Item : CORBA.Any)
     return CONV_FRAME.CodeSetContext;

   function To_Any
     (Item : CONV_FRAME.CodeSetContext)
     return CORBA.Any;

   
   package Internals is

      function Wrap
        (X : access CONV_FRAME.CodeSetId)
        return PolyORB.Any.Content'Class;

      procedure Initialize_CodeSetId;

      function IDL_SEQUENCE_CONV_FRAME_CodeSetId_Element_Wrap
        (X : access CONV_FRAME.CodeSetId)
        return PolyORB.Any.Content'Class;

      function Wrap
        (X : access CONV_FRAME.IDL_SEQUENCE_CONV_FRAME_CodeSetId.Sequence)
        return PolyORB.Any.Content'Class;

      package IDL_SEQUENCE_CONV_FRAME_CodeSetId_Helper is
        new CONV_FRAME.IDL_SEQUENCE_CONV_FRAME_CodeSetId.CORBA_Helper
           (Element_From_Any => CONV_FRAME.Helper.From_Any,
            Element_To_Any => CONV_FRAME.Helper.To_Any,
            Element_Wrap => CONV_FRAME.Helper.Internals.IDL_SEQUENCE_CONV_FRAME_CodeSetId_Element_Wrap);

      procedure Initialize_IDL_SEQUENCE_CONV_FRAME_CodeSetId;

      procedure Initialize_CodeSetIdSeq;

      type Ptr_Ü_CodeSetComponent is
        access all CONV_FRAME.CodeSetComponent;

      type Content_Ü_CodeSetComponent is
        new PolyORB.Any.Aggregate_Content with record
            V : Ptr_Ü_CodeSetComponent;
         end record;

      function Get_Aggregate_Element
        (Acc : not null access Content_Ü_CodeSetComponent;
         Tc : PolyORB.Any.TypeCode.Object_Ptr;
         Index : PolyORB.Types.Unsigned_Long;
         Mech : not null access PolyORB.Any.Mechanism)
        return PolyORB.Any.Content'Class;

      function Get_Aggregate_Count
        (Acc : Content_Ü_CodeSetComponent)
        return PolyORB.Types.Unsigned_Long;

      procedure Set_Aggregate_Count
        (Acc : in out Content_Ü_CodeSetComponent;
         Count : PolyORB.Types.Unsigned_Long);

      function Unchecked_Get_V
        (Acc : not null access Content_Ü_CodeSetComponent)
        return PolyORB.Types.Address;

      function Clone
        (Acc : Content_Ü_CodeSetComponent;
         Into : PolyORB.Any.Content_Ptr := null)
        return PolyORB.Any.Content_Ptr;

      procedure Finalize_Value
        (Acc : in out Content_Ü_CodeSetComponent);

      function Wrap
        (X : access CONV_FRAME.CodeSetComponent)
        return PolyORB.Any.Content'Class;

      procedure Initialize_CodeSetComponent;

      type Ptr_Ü_CodeSetComponentInfo is
        access all CONV_FRAME.CodeSetComponentInfo;

      type Content_Ü_CodeSetComponentInfo is
        new PolyORB.Any.Aggregate_Content with record
            V : Ptr_Ü_CodeSetComponentInfo;
         end record;

      function Get_Aggregate_Element
        (Acc : not null access Content_Ü_CodeSetComponentInfo;
         Tc : PolyORB.Any.TypeCode.Object_Ptr;
         Index : PolyORB.Types.Unsigned_Long;
         Mech : not null access PolyORB.Any.Mechanism)
        return PolyORB.Any.Content'Class;

      function Get_Aggregate_Count
        (Acc : Content_Ü_CodeSetComponentInfo)
        return PolyORB.Types.Unsigned_Long;

      procedure Set_Aggregate_Count
        (Acc : in out Content_Ü_CodeSetComponentInfo;
         Count : PolyORB.Types.Unsigned_Long);

      function Unchecked_Get_V
        (Acc : not null access Content_Ü_CodeSetComponentInfo)
        return PolyORB.Types.Address;

      function Clone
        (Acc : Content_Ü_CodeSetComponentInfo;
         Into : PolyORB.Any.Content_Ptr := null)
        return PolyORB.Any.Content_Ptr;

      procedure Finalize_Value
        (Acc : in out Content_Ü_CodeSetComponentInfo);

      function Wrap
        (X : access CONV_FRAME.CodeSetComponentInfo)
        return PolyORB.Any.Content'Class;

      procedure Initialize_CodeSetComponentInfo;

      type Ptr_Ü_CodeSetContext is
        access all CONV_FRAME.CodeSetContext;

      type Content_Ü_CodeSetContext is
        new PolyORB.Any.Aggregate_Content with record
            V : Ptr_Ü_CodeSetContext;
         end record;

      function Get_Aggregate_Element
        (Acc : not null access Content_Ü_CodeSetContext;
         Tc : PolyORB.Any.TypeCode.Object_Ptr;
         Index : PolyORB.Types.Unsigned_Long;
         Mech : not null access PolyORB.Any.Mechanism)
        return PolyORB.Any.Content'Class;

      function Get_Aggregate_Count
        (Acc : Content_Ü_CodeSetContext)
        return PolyORB.Types.Unsigned_Long;

      procedure Set_Aggregate_Count
        (Acc : in out Content_Ü_CodeSetContext;
         Count : PolyORB.Types.Unsigned_Long);

      function Unchecked_Get_V
        (Acc : not null access Content_Ü_CodeSetContext)
        return PolyORB.Types.Address;

      function Clone
        (Acc : Content_Ü_CodeSetContext;
         Into : PolyORB.Any.Content_Ptr := null)
        return PolyORB.Any.Content_Ptr;

      procedure Finalize_Value
        (Acc : in out Content_Ü_CodeSetContext);

      function Wrap
        (X : access CONV_FRAME.CodeSetContext)
        return PolyORB.Any.Content'Class;

      procedure Initialize_CodeSetContext;

   end Internals;

end CONV_FRAME.Helper;

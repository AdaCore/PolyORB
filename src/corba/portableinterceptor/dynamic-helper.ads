pragma Style_Checks ("NM32766");
pragma Wide_Character_Encoding (Brackets);
pragma Warnings (Off, "use of an anonymous access type allocator");

---------------------------------------------------
--  This file has been generated automatically from
--  /Users/heathdorn/Documents/Playground/Agents/RefactorTeam/code_refactor/PolyORB/idls/Misc/Dynamic.idl
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

package Dynamic.Helper is

   TC_Parameter : CORBA.TypeCode.Object;

   function From_Any
     (Item : CORBA.Any)
     return Dynamic.Parameter;

   function To_Any
     (Item : Dynamic.Parameter)
     return CORBA.Any;

   TC_IDL_SEQUENCE_Dynamic_Parameter : CORBA.TypeCode.Object;

   function From_Any
     (Item : CORBA.Any)
     return Dynamic.IDL_SEQUENCE_Dynamic_Parameter.Sequence;

   function To_Any
     (Item : Dynamic.IDL_SEQUENCE_Dynamic_Parameter.Sequence)
     return CORBA.Any;

   TC_ParameterList : CORBA.TypeCode.Object;

   function From_Any
     (Item : CORBA.Any)
     return Dynamic.ParameterList;

   function To_Any
     (Item : Dynamic.ParameterList)
     return CORBA.Any;

   TC_ContextList : CORBA.TypeCode.Object;

   function From_Any
     (Item : CORBA.Any)
     return Dynamic.ContextList;

   function To_Any
     (Item : Dynamic.ContextList)
     return CORBA.Any;

   TC_IDL_SEQUENCE_CORBA_TypeCode : CORBA.TypeCode.Object;

   function From_Any
     (Item : CORBA.Any)
     return Dynamic.IDL_SEQUENCE_CORBA_TypeCode.Sequence;

   function To_Any
     (Item : Dynamic.IDL_SEQUENCE_CORBA_TypeCode.Sequence)
     return CORBA.Any;

   TC_ExceptionList : CORBA.TypeCode.Object;

   function From_Any
     (Item : CORBA.Any)
     return Dynamic.ExceptionList;

   function To_Any
     (Item : Dynamic.ExceptionList)
     return CORBA.Any;

   TC_RequestContext : CORBA.TypeCode.Object;

   function From_Any
     (Item : CORBA.Any)
     return Dynamic.RequestContext;

   function To_Any
     (Item : Dynamic.RequestContext)
     return CORBA.Any;

   
   package Internals is

      type Ptr_Ü_Parameter is
        access all Dynamic.Parameter;

      type Content_Ü_Parameter is
        new PolyORB.Any.Aggregate_Content with record
            V : Ptr_Ü_Parameter;
         end record;

      function Get_Aggregate_Element
        (Acc : not null access Content_Ü_Parameter;
         Tc : PolyORB.Any.TypeCode.Object_Ptr;
         Index : PolyORB.Types.Unsigned_Long;
         Mech : not null access PolyORB.Any.Mechanism)
        return PolyORB.Any.Content'Class;

      function Get_Aggregate_Count
        (Acc : Content_Ü_Parameter)
        return PolyORB.Types.Unsigned_Long;

      procedure Set_Aggregate_Count
        (Acc : in out Content_Ü_Parameter;
         Count : PolyORB.Types.Unsigned_Long);

      function Unchecked_Get_V
        (Acc : not null access Content_Ü_Parameter)
        return PolyORB.Types.Address;

      function Clone
        (Acc : Content_Ü_Parameter;
         Into : PolyORB.Any.Content_Ptr := null)
        return PolyORB.Any.Content_Ptr;

      procedure Finalize_Value
        (Acc : in out Content_Ü_Parameter);

      function Wrap
        (X : access Dynamic.Parameter)
        return PolyORB.Any.Content'Class;

      procedure Initialize_Parameter;

      function IDL_SEQUENCE_Dynamic_Parameter_Element_Wrap
        (X : access Dynamic.Parameter)
        return PolyORB.Any.Content'Class;

      function Wrap
        (X : access Dynamic.IDL_SEQUENCE_Dynamic_Parameter.Sequence)
        return PolyORB.Any.Content'Class;

      package IDL_SEQUENCE_Dynamic_Parameter_Helper is
        new Dynamic.IDL_SEQUENCE_Dynamic_Parameter.CORBA_Helper
           (Element_From_Any => Dynamic.Helper.From_Any,
            Element_To_Any => Dynamic.Helper.To_Any,
            Element_Wrap => Dynamic.Helper.Internals.IDL_SEQUENCE_Dynamic_Parameter_Element_Wrap);

      procedure Initialize_IDL_SEQUENCE_Dynamic_Parameter;

      procedure Initialize_ParameterList;

      procedure Initialize_ContextList;

      function IDL_SEQUENCE_CORBA_TypeCode_Element_Wrap
        (X : access CORBA.TypeCode.Object)
        return PolyORB.Any.Content'Class;

      function Wrap
        (X : access Dynamic.IDL_SEQUENCE_CORBA_TypeCode.Sequence)
        return PolyORB.Any.Content'Class;

      package IDL_SEQUENCE_CORBA_TypeCode_Helper is
        new Dynamic.IDL_SEQUENCE_CORBA_TypeCode.CORBA_Helper
           (Element_From_Any => CORBA.From_Any,
            Element_To_Any => CORBA.To_Any,
            Element_Wrap => Dynamic.Helper.Internals.IDL_SEQUENCE_CORBA_TypeCode_Element_Wrap);

      procedure Initialize_IDL_SEQUENCE_CORBA_TypeCode;

      procedure Initialize_ExceptionList;

      procedure Initialize_RequestContext;

   end Internals;

end Dynamic.Helper;

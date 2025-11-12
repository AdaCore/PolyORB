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

with CORBA;
pragma Elaborate_All (CORBA);
with PolyORB.Any;
with PolyORB.Types;

package GSSUP.Helper is

   TC_InitialContextToken : CORBA.TypeCode.Object;

   function From_Any
     (Item : CORBA.Any)
     return GSSUP.InitialContextToken;

   function To_Any
     (Item : GSSUP.InitialContextToken)
     return CORBA.Any;

   TC_ErrorCode : CORBA.TypeCode.Object;

   function From_Any
     (Item : CORBA.Any)
     return GSSUP.ErrorCode;

   function To_Any
     (Item : GSSUP.ErrorCode)
     return CORBA.Any;

   TC_ErrorToken : CORBA.TypeCode.Object;

   function From_Any
     (Item : CORBA.Any)
     return GSSUP.ErrorToken;

   function To_Any
     (Item : GSSUP.ErrorToken)
     return CORBA.Any;

   
   package Internals is

      type Ptr_Ü_InitialContextToken is
        access all GSSUP.InitialContextToken;

      type Content_Ü_InitialContextToken is
        new PolyORB.Any.Aggregate_Content with record
            V : Ptr_Ü_InitialContextToken;
         end record;

      function Get_Aggregate_Element
        (Acc : not null access Content_Ü_InitialContextToken;
         Tc : PolyORB.Any.TypeCode.Object_Ptr;
         Index : PolyORB.Types.Unsigned_Long;
         Mech : not null access PolyORB.Any.Mechanism)
        return PolyORB.Any.Content'Class;

      function Get_Aggregate_Count
        (Acc : Content_Ü_InitialContextToken)
        return PolyORB.Types.Unsigned_Long;

      procedure Set_Aggregate_Count
        (Acc : in out Content_Ü_InitialContextToken;
         Count : PolyORB.Types.Unsigned_Long);

      function Unchecked_Get_V
        (Acc : not null access Content_Ü_InitialContextToken)
        return PolyORB.Types.Address;

      function Clone
        (Acc : Content_Ü_InitialContextToken;
         Into : PolyORB.Any.Content_Ptr := null)
        return PolyORB.Any.Content_Ptr;

      procedure Finalize_Value
        (Acc : in out Content_Ü_InitialContextToken);

      function Wrap
        (X : access GSSUP.InitialContextToken)
        return PolyORB.Any.Content'Class;

      procedure Initialize_InitialContextToken;

      function Wrap
        (X : access GSSUP.ErrorCode)
        return PolyORB.Any.Content'Class;

      procedure Initialize_ErrorCode;

      type Ptr_Ü_ErrorToken is
        access all GSSUP.ErrorToken;

      type Content_Ü_ErrorToken is
        new PolyORB.Any.Aggregate_Content with record
            V : Ptr_Ü_ErrorToken;
         end record;

      function Get_Aggregate_Element
        (Acc : not null access Content_Ü_ErrorToken;
         Tc : PolyORB.Any.TypeCode.Object_Ptr;
         Index : PolyORB.Types.Unsigned_Long;
         Mech : not null access PolyORB.Any.Mechanism)
        return PolyORB.Any.Content'Class;

      function Get_Aggregate_Count
        (Acc : Content_Ü_ErrorToken)
        return PolyORB.Types.Unsigned_Long;

      procedure Set_Aggregate_Count
        (Acc : in out Content_Ü_ErrorToken;
         Count : PolyORB.Types.Unsigned_Long);

      function Unchecked_Get_V
        (Acc : not null access Content_Ü_ErrorToken)
        return PolyORB.Types.Address;

      function Clone
        (Acc : Content_Ü_ErrorToken;
         Into : PolyORB.Any.Content_Ptr := null)
        return PolyORB.Any.Content_Ptr;

      procedure Finalize_Value
        (Acc : in out Content_Ü_ErrorToken);

      function Wrap
        (X : access GSSUP.ErrorToken)
        return PolyORB.Any.Content'Class;

      procedure Initialize_ErrorToken;

   end Internals;

end GSSUP.Helper;

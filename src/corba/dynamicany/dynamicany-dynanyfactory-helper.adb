pragma Style_Checks ("NM32766");
pragma Warnings (Off, "use of an anonymous access type allocator");
pragma Warnings (Off, "unnecessary with of ancestor");

---------------------------------------------------
--  This file has been generated automatically from
--  /Users/heathdorn/Documents/Playground/Agents/RefactorTeam/code_refactor/PolyORB/idls/Misc/DynamicAny.idl
--  by IAC (IDL to Ada Compiler) 20.0w (rev. 41a9b833).
---------------------------------------------------
--  NOTE: If you modify this file by hand, your
--  changes will be lost when you re-run the
--  IDL to Ada compiler.
---------------------------------------------------

with PolyORB.Std;
with PolyORB.Any;
with PolyORB.Exceptions;
with PolyORB.Utils.Strings;
with PolyORB.Utils.Strings.Lists;
with PolyORB.Initialization;

package body DynamicAny.DynAnyFactory.Helper is

   
   package body Internals is

      DynAnyFactory_Initialized : PolyORB.Std.Boolean :=
        False;

      ------------------------------
      -- Initialize_DynAnyFactory --
      ------------------------------

      procedure Initialize_DynAnyFactory is
         Name_Ü : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("DynAnyFactory");
         Id_Ü : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("IDL:omg.org/DynamicAny/DynAnyFactory:1.0");
      begin
         if not DynAnyFactory_Initialized
         then
            DynAnyFactory_Initialized :=
              True;
            DynamicAny.DynAnyFactory.Helper.TC_DynAnyFactory :=
              CORBA.TypeCode.Internals.To_CORBA_Object
                 (PolyORB.Any.TypeCode.Tcf_Object);
            CORBA.Internals.Add_Parameter
              (TC_DynAnyFactory,
               CORBA.To_Any
                 (Name_Ü));
            CORBA.Internals.Add_Parameter
              (TC_DynAnyFactory,
               CORBA.To_Any
                 (Id_Ü));
            CORBA.TypeCode.Internals.Disable_Ref_Counting
              (TC_DynAnyFactory);
            CORBA.TypeCode.Internals.Freeze
              (TC_DynAnyFactory);
         end if;
      end Initialize_DynAnyFactory;

      procedure Raise_InconsistentTypeCode_From_Any
        (Item : PolyORB.Any.Any;
         Message : PolyORB.Std.String);

      pragma No_Return (Raise_InconsistentTypeCode_From_Any);

      -----------------------------------------
      -- Raise_InconsistentTypeCode_From_Any --
      -----------------------------------------

      procedure Raise_InconsistentTypeCode_From_Any
        (Item : PolyORB.Any.Any;
         Message : PolyORB.Std.String)
      is
         Members : constant DynamicAny.DynAnyFactory.InconsistentTypeCode_Members :=
           DynamicAny.DynAnyFactory.Helper.From_Any
              (CORBA.Any
                 (Item));
      begin
         PolyORB.Exceptions.User_Raise_Exception
           (InconsistentTypeCode'Identity,
            Members,
            Message);
      end Raise_InconsistentTypeCode_From_Any;

      InconsistentTypeCode_Initialized : PolyORB.Std.Boolean :=
        False;

      -------------------------------------
      -- Initialize_InconsistentTypeCode --
      -------------------------------------

      procedure Initialize_InconsistentTypeCode is
         Name_Ü : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("InconsistentTypeCode_Members");
         Id_Ü : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("IDL:omg.org/DynamicAny/DynAnyFactory/InconsistentTypeCode:1.0");
      begin
         if not InconsistentTypeCode_Initialized
         then
            InconsistentTypeCode_Initialized :=
              True;
            DynamicAny.DynAnyFactory.Helper.TC_InconsistentTypeCode :=
              CORBA.TypeCode.Internals.To_CORBA_Object
                 (PolyORB.Any.TypeCode.Tcf_Except);
            CORBA.Internals.Add_Parameter
              (TC_InconsistentTypeCode,
               CORBA.To_Any
                 (Name_Ü));
            CORBA.Internals.Add_Parameter
              (TC_InconsistentTypeCode,
               CORBA.To_Any
                 (Id_Ü));
            PolyORB.Exceptions.Register_Exception
              (CORBA.TypeCode.Internals.To_PolyORB_Object
                 (TC_InconsistentTypeCode),
               Raise_InconsistentTypeCode_From_Any'Access);
            CORBA.TypeCode.Internals.Disable_Ref_Counting
              (TC_InconsistentTypeCode);
            CORBA.TypeCode.Internals.Freeze
              (TC_InconsistentTypeCode);
         end if;
      end Initialize_InconsistentTypeCode;

   end Internals;

   ----------------------------
   -- Unchecked_To_Local_Ref --
   ----------------------------

   function Unchecked_To_Local_Ref
     (The_Ref : CORBA.Object.Ref'Class)
     return DynamicAny.DynAnyFactory.Local_Ref
   is
      Result : DynamicAny.DynAnyFactory.Local_Ref;
   begin
      Set
        (Result,
         CORBA.Object.Object_Of
           (The_Ref));
      return Result;
   end Unchecked_To_Local_Ref;

   ------------------
   -- To_Local_Ref --
   ------------------

   function To_Local_Ref
     (The_Ref : CORBA.Object.Ref'Class)
     return DynamicAny.DynAnyFactory.Local_Ref
   is
   begin
      if (CORBA.Object.Is_Nil
        (The_Ref)
         or else CORBA.Object.Is_A
           (The_Ref,
            Repository_Id))
      then
         return Unchecked_To_Local_Ref
           (The_Ref);
      end if;
      CORBA.Raise_Bad_Param
        (CORBA.Default_Sys_Member);
   end To_Local_Ref;

   --------------
   -- From_Any --
   --------------

   function From_Any
     (Item : CORBA.Any)
     return DynamicAny.DynAnyFactory.InconsistentTypeCode_Members
   is
      Result_Ü : DynamicAny.DynAnyFactory.InconsistentTypeCode_Members;
      pragma Warnings (Off);
      pragma Unreferenced (Item);
      pragma Warnings (On);
   begin
      return Result_Ü;
   end From_Any;

   ------------
   -- To_Any --
   ------------

   function To_Any
     (Item : DynamicAny.DynAnyFactory.InconsistentTypeCode_Members)
     return CORBA.Any
   is
      Result_Ü : constant CORBA.Any :=
        CORBA.Internals.Get_Empty_Any_Aggregate
           (DynamicAny.DynAnyFactory.Helper.TC_InconsistentTypeCode);
      pragma Warnings (Off);
      pragma Unreferenced (Item);
      pragma Warnings (On);
   begin
      return Result_Ü;
   end To_Any;

   --------------------------------
   -- Raise_InconsistentTypeCode --
   --------------------------------

   procedure Raise_InconsistentTypeCode
     (Members : DynamicAny.DynAnyFactory.InconsistentTypeCode_Members)
   is
   begin
      PolyORB.Exceptions.User_Raise_Exception
        (InconsistentTypeCode'Identity,
         Members);
   end Raise_InconsistentTypeCode;

   -----------------------------
   -- Deferred_Initialization --
   -----------------------------

   procedure Deferred_Initialization is
   begin
      DynamicAny.DynAnyFactory.Helper.Internals.Initialize_DynAnyFactory;
      DynamicAny.DynAnyFactory.Helper.Internals.Initialize_InconsistentTypeCode;
   end Deferred_Initialization;

begin
   declare
      use PolyORB.Utils.Strings;
      use PolyORB.Utils.Strings.Lists;
   begin
      PolyORB.Initialization.Register_Module
        (PolyORB.Initialization.Module_Info'
           (Name => +"DynamicAny.DynAnyFactory.Helper",
            Conflicts => PolyORB.Utils.Strings.Lists.Empty,
            Depends => +"any"
               & "exceptions",
            Provides => PolyORB.Utils.Strings.Lists.Empty,
            Implicit => False,
            Init => Deferred_Initialization'Access,
            Shutdown => null));
   end;
end DynamicAny.DynAnyFactory.Helper;

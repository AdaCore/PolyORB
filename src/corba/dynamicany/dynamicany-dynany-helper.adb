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

package body DynamicAny.DynAny.Helper is

   
   package body Internals is

      DynAny_Initialized : PolyORB.Std.Boolean :=
        False;

      -----------------------
      -- Initialize_DynAny --
      -----------------------

      procedure Initialize_DynAny is
         Name_Ü : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("DynAny");
         Id_Ü : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("IDL:omg.org/DynamicAny/DynAny:1.0");
      begin
         if not DynAny_Initialized
         then
            DynAny_Initialized :=
              True;
            DynamicAny.DynAny.Helper.TC_DynAny :=
              CORBA.TypeCode.Internals.To_CORBA_Object
                 (PolyORB.Any.TypeCode.Tcf_Object);
            CORBA.Internals.Add_Parameter
              (TC_DynAny,
               CORBA.To_Any
                 (Name_Ü));
            CORBA.Internals.Add_Parameter
              (TC_DynAny,
               CORBA.To_Any
                 (Id_Ü));
            CORBA.TypeCode.Internals.Disable_Ref_Counting
              (TC_DynAny);
            CORBA.TypeCode.Internals.Freeze
              (TC_DynAny);
         end if;
      end Initialize_DynAny;

      procedure Raise_InvalidValue_From_Any
        (Item : PolyORB.Any.Any;
         Message : PolyORB.Std.String);

      pragma No_Return (Raise_InvalidValue_From_Any);

      ---------------------------------
      -- Raise_InvalidValue_From_Any --
      ---------------------------------

      procedure Raise_InvalidValue_From_Any
        (Item : PolyORB.Any.Any;
         Message : PolyORB.Std.String)
      is
         Members : constant DynamicAny.DynAny.InvalidValue_Members :=
           DynamicAny.DynAny.Helper.From_Any
              (CORBA.Any
                 (Item));
      begin
         PolyORB.Exceptions.User_Raise_Exception
           (InvalidValue'Identity,
            Members,
            Message);
      end Raise_InvalidValue_From_Any;

      InvalidValue_Initialized : PolyORB.Std.Boolean :=
        False;

      -----------------------------
      -- Initialize_InvalidValue --
      -----------------------------

      procedure Initialize_InvalidValue is
         Name_Ü : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("InvalidValue_Members");
         Id_Ü : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("IDL:omg.org/DynamicAny/DynAny/InvalidValue:1.0");
      begin
         if not InvalidValue_Initialized
         then
            InvalidValue_Initialized :=
              True;
            DynamicAny.DynAny.Helper.TC_InvalidValue :=
              CORBA.TypeCode.Internals.To_CORBA_Object
                 (PolyORB.Any.TypeCode.Tcf_Except);
            CORBA.Internals.Add_Parameter
              (TC_InvalidValue,
               CORBA.To_Any
                 (Name_Ü));
            CORBA.Internals.Add_Parameter
              (TC_InvalidValue,
               CORBA.To_Any
                 (Id_Ü));
            PolyORB.Exceptions.Register_Exception
              (CORBA.TypeCode.Internals.To_PolyORB_Object
                 (TC_InvalidValue),
               Raise_InvalidValue_From_Any'Access);
            CORBA.TypeCode.Internals.Disable_Ref_Counting
              (TC_InvalidValue);
            CORBA.TypeCode.Internals.Freeze
              (TC_InvalidValue);
         end if;
      end Initialize_InvalidValue;

      procedure Raise_TypeMismatch_From_Any
        (Item : PolyORB.Any.Any;
         Message : PolyORB.Std.String);

      pragma No_Return (Raise_TypeMismatch_From_Any);

      ---------------------------------
      -- Raise_TypeMismatch_From_Any --
      ---------------------------------

      procedure Raise_TypeMismatch_From_Any
        (Item : PolyORB.Any.Any;
         Message : PolyORB.Std.String)
      is
         Members : constant DynamicAny.DynAny.TypeMismatch_Members :=
           DynamicAny.DynAny.Helper.From_Any
              (CORBA.Any
                 (Item));
      begin
         PolyORB.Exceptions.User_Raise_Exception
           (TypeMismatch'Identity,
            Members,
            Message);
      end Raise_TypeMismatch_From_Any;

      TypeMismatch_Initialized : PolyORB.Std.Boolean :=
        False;

      -----------------------------
      -- Initialize_TypeMismatch --
      -----------------------------

      procedure Initialize_TypeMismatch is
         Name_Ü : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("TypeMismatch_Members");
         Id_Ü : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("IDL:omg.org/DynamicAny/DynAny/TypeMismatch:1.0");
      begin
         if not TypeMismatch_Initialized
         then
            TypeMismatch_Initialized :=
              True;
            DynamicAny.DynAny.Helper.TC_TypeMismatch :=
              CORBA.TypeCode.Internals.To_CORBA_Object
                 (PolyORB.Any.TypeCode.Tcf_Except);
            CORBA.Internals.Add_Parameter
              (TC_TypeMismatch,
               CORBA.To_Any
                 (Name_Ü));
            CORBA.Internals.Add_Parameter
              (TC_TypeMismatch,
               CORBA.To_Any
                 (Id_Ü));
            PolyORB.Exceptions.Register_Exception
              (CORBA.TypeCode.Internals.To_PolyORB_Object
                 (TC_TypeMismatch),
               Raise_TypeMismatch_From_Any'Access);
            CORBA.TypeCode.Internals.Disable_Ref_Counting
              (TC_TypeMismatch);
            CORBA.TypeCode.Internals.Freeze
              (TC_TypeMismatch);
         end if;
      end Initialize_TypeMismatch;

   end Internals;

   ----------------------------
   -- Unchecked_To_Local_Ref --
   ----------------------------

   function Unchecked_To_Local_Ref
     (The_Ref : CORBA.Object.Ref'Class)
     return DynamicAny.DynAny.Local_Ref
   is
      Result : DynamicAny.DynAny.Local_Ref;
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
     return DynamicAny.DynAny.Local_Ref
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
     return DynamicAny.DynAny.InvalidValue_Members
   is
      Result_Ü : DynamicAny.DynAny.InvalidValue_Members;
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
     (Item : DynamicAny.DynAny.InvalidValue_Members)
     return CORBA.Any
   is
      Result_Ü : constant CORBA.Any :=
        CORBA.Internals.Get_Empty_Any_Aggregate
           (DynamicAny.DynAny.Helper.TC_InvalidValue);
      pragma Warnings (Off);
      pragma Unreferenced (Item);
      pragma Warnings (On);
   begin
      return Result_Ü;
   end To_Any;

   ------------------------
   -- Raise_InvalidValue --
   ------------------------

   procedure Raise_InvalidValue
     (Members : DynamicAny.DynAny.InvalidValue_Members)
   is
   begin
      PolyORB.Exceptions.User_Raise_Exception
        (InvalidValue'Identity,
         Members);
   end Raise_InvalidValue;

   --------------
   -- From_Any --
   --------------

   function From_Any
     (Item : CORBA.Any)
     return DynamicAny.DynAny.TypeMismatch_Members
   is
      Result_Ü : DynamicAny.DynAny.TypeMismatch_Members;
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
     (Item : DynamicAny.DynAny.TypeMismatch_Members)
     return CORBA.Any
   is
      Result_Ü : constant CORBA.Any :=
        CORBA.Internals.Get_Empty_Any_Aggregate
           (DynamicAny.DynAny.Helper.TC_TypeMismatch);
      pragma Warnings (Off);
      pragma Unreferenced (Item);
      pragma Warnings (On);
   begin
      return Result_Ü;
   end To_Any;

   ------------------------
   -- Raise_TypeMismatch --
   ------------------------

   procedure Raise_TypeMismatch
     (Members : DynamicAny.DynAny.TypeMismatch_Members)
   is
   begin
      PolyORB.Exceptions.User_Raise_Exception
        (TypeMismatch'Identity,
         Members);
   end Raise_TypeMismatch;

   -----------------------------
   -- Deferred_Initialization --
   -----------------------------

   procedure Deferred_Initialization is
   begin
      DynamicAny.DynAny.Helper.Internals.Initialize_DynAny;
      DynamicAny.DynAny.Helper.Internals.Initialize_InvalidValue;
      DynamicAny.DynAny.Helper.Internals.Initialize_TypeMismatch;
   end Deferred_Initialization;

begin
   declare
      use PolyORB.Utils.Strings;
      use PolyORB.Utils.Strings.Lists;
   begin
      PolyORB.Initialization.Register_Module
        (PolyORB.Initialization.Module_Info'
           (Name => +"DynamicAny.DynAny.Helper",
            Conflicts => PolyORB.Utils.Strings.Lists.Empty,
            Depends => +"any"
               & "exceptions",
            Provides => PolyORB.Utils.Strings.Lists.Empty,
            Implicit => False,
            Init => Deferred_Initialization'Access,
            Shutdown => null));
   end;
end DynamicAny.DynAny.Helper;

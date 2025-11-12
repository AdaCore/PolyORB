pragma Style_Checks ("NM32766");
pragma Warnings (Off, "use of an anonymous access type allocator");
pragma Warnings (Off, "unnecessary with of ancestor");

---------------------------------------------------
--  This file has been generated automatically from
--  /Users/heathdorn/Documents/Playground/Agents/RefactorTeam/code_refactor/PolyORB/idls/Interop/IOP.idl
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

package body IOP.Codec.Helper is

   
   package body Internals is

      Codec_Initialized : PolyORB.Std.Boolean :=
        False;

      ----------------------
      -- Initialize_Codec --
      ----------------------

      procedure Initialize_Codec is
         Name_Ü : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("Codec");
         Id_Ü : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("IDL:omg.org/IOP/Codec:1.0");
      begin
         if not Codec_Initialized
         then
            Codec_Initialized :=
              True;
            IOP.Codec.Helper.TC_Codec :=
              CORBA.TypeCode.Internals.To_CORBA_Object
                 (PolyORB.Any.TypeCode.Tcf_Object);
            CORBA.Internals.Add_Parameter
              (TC_Codec,
               CORBA.To_Any
                 (Name_Ü));
            CORBA.Internals.Add_Parameter
              (TC_Codec,
               CORBA.To_Any
                 (Id_Ü));
            CORBA.TypeCode.Internals.Disable_Ref_Counting
              (TC_Codec);
            CORBA.TypeCode.Internals.Freeze
              (TC_Codec);
         end if;
      end Initialize_Codec;

      procedure Raise_InvalidTypeForEncoding_From_Any
        (Item : PolyORB.Any.Any;
         Message : PolyORB.Std.String);

      pragma No_Return (Raise_InvalidTypeForEncoding_From_Any);

      -------------------------------------------
      -- Raise_InvalidTypeForEncoding_From_Any --
      -------------------------------------------

      procedure Raise_InvalidTypeForEncoding_From_Any
        (Item : PolyORB.Any.Any;
         Message : PolyORB.Std.String)
      is
         Members : constant IOP.Codec.InvalidTypeForEncoding_Members :=
           IOP.Codec.Helper.From_Any
              (CORBA.Any
                 (Item));
      begin
         PolyORB.Exceptions.User_Raise_Exception
           (InvalidTypeForEncoding'Identity,
            Members,
            Message);
      end Raise_InvalidTypeForEncoding_From_Any;

      InvalidTypeForEncoding_Initialized : PolyORB.Std.Boolean :=
        False;

      ---------------------------------------
      -- Initialize_InvalidTypeForEncoding --
      ---------------------------------------

      procedure Initialize_InvalidTypeForEncoding is
         Name_Ü : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("InvalidTypeForEncoding_Members");
         Id_Ü : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("IDL:omg.org/IOP/Codec/InvalidTypeForEncoding:1.0");
      begin
         if not InvalidTypeForEncoding_Initialized
         then
            InvalidTypeForEncoding_Initialized :=
              True;
            IOP.Codec.Helper.TC_InvalidTypeForEncoding :=
              CORBA.TypeCode.Internals.To_CORBA_Object
                 (PolyORB.Any.TypeCode.Tcf_Except);
            CORBA.Internals.Add_Parameter
              (TC_InvalidTypeForEncoding,
               CORBA.To_Any
                 (Name_Ü));
            CORBA.Internals.Add_Parameter
              (TC_InvalidTypeForEncoding,
               CORBA.To_Any
                 (Id_Ü));
            PolyORB.Exceptions.Register_Exception
              (CORBA.TypeCode.Internals.To_PolyORB_Object
                 (TC_InvalidTypeForEncoding),
               Raise_InvalidTypeForEncoding_From_Any'Access);
            CORBA.TypeCode.Internals.Disable_Ref_Counting
              (TC_InvalidTypeForEncoding);
            CORBA.TypeCode.Internals.Freeze
              (TC_InvalidTypeForEncoding);
         end if;
      end Initialize_InvalidTypeForEncoding;

      procedure Raise_FormatMismatch_From_Any
        (Item : PolyORB.Any.Any;
         Message : PolyORB.Std.String);

      pragma No_Return (Raise_FormatMismatch_From_Any);

      -----------------------------------
      -- Raise_FormatMismatch_From_Any --
      -----------------------------------

      procedure Raise_FormatMismatch_From_Any
        (Item : PolyORB.Any.Any;
         Message : PolyORB.Std.String)
      is
         Members : constant IOP.Codec.FormatMismatch_Members :=
           IOP.Codec.Helper.From_Any
              (CORBA.Any
                 (Item));
      begin
         PolyORB.Exceptions.User_Raise_Exception
           (FormatMismatch'Identity,
            Members,
            Message);
      end Raise_FormatMismatch_From_Any;

      FormatMismatch_Initialized : PolyORB.Std.Boolean :=
        False;

      -------------------------------
      -- Initialize_FormatMismatch --
      -------------------------------

      procedure Initialize_FormatMismatch is
         Name_Ü : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("FormatMismatch_Members");
         Id_Ü : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("IDL:omg.org/IOP/Codec/FormatMismatch:1.0");
      begin
         if not FormatMismatch_Initialized
         then
            FormatMismatch_Initialized :=
              True;
            IOP.Codec.Helper.TC_FormatMismatch :=
              CORBA.TypeCode.Internals.To_CORBA_Object
                 (PolyORB.Any.TypeCode.Tcf_Except);
            CORBA.Internals.Add_Parameter
              (TC_FormatMismatch,
               CORBA.To_Any
                 (Name_Ü));
            CORBA.Internals.Add_Parameter
              (TC_FormatMismatch,
               CORBA.To_Any
                 (Id_Ü));
            PolyORB.Exceptions.Register_Exception
              (CORBA.TypeCode.Internals.To_PolyORB_Object
                 (TC_FormatMismatch),
               Raise_FormatMismatch_From_Any'Access);
            CORBA.TypeCode.Internals.Disable_Ref_Counting
              (TC_FormatMismatch);
            CORBA.TypeCode.Internals.Freeze
              (TC_FormatMismatch);
         end if;
      end Initialize_FormatMismatch;

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
         Members : constant IOP.Codec.TypeMismatch_Members :=
           IOP.Codec.Helper.From_Any
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
              ("IDL:omg.org/IOP/Codec/TypeMismatch:1.0");
      begin
         if not TypeMismatch_Initialized
         then
            TypeMismatch_Initialized :=
              True;
            IOP.Codec.Helper.TC_TypeMismatch :=
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
     return IOP.Codec.Local_Ref
   is
      Result : IOP.Codec.Local_Ref;
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
     return IOP.Codec.Local_Ref
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
     return IOP.Codec.InvalidTypeForEncoding_Members
   is
      Result_Ü : IOP.Codec.InvalidTypeForEncoding_Members;
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
     (Item : IOP.Codec.InvalidTypeForEncoding_Members)
     return CORBA.Any
   is
      Result_Ü : constant CORBA.Any :=
        CORBA.Internals.Get_Empty_Any_Aggregate
           (IOP.Codec.Helper.TC_InvalidTypeForEncoding);
      pragma Warnings (Off);
      pragma Unreferenced (Item);
      pragma Warnings (On);
   begin
      return Result_Ü;
   end To_Any;

   ----------------------------------
   -- Raise_InvalidTypeForEncoding --
   ----------------------------------

   procedure Raise_InvalidTypeForEncoding
     (Members : IOP.Codec.InvalidTypeForEncoding_Members)
   is
   begin
      PolyORB.Exceptions.User_Raise_Exception
        (InvalidTypeForEncoding'Identity,
         Members);
   end Raise_InvalidTypeForEncoding;

   --------------
   -- From_Any --
   --------------

   function From_Any
     (Item : CORBA.Any)
     return IOP.Codec.FormatMismatch_Members
   is
      Result_Ü : IOP.Codec.FormatMismatch_Members;
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
     (Item : IOP.Codec.FormatMismatch_Members)
     return CORBA.Any
   is
      Result_Ü : constant CORBA.Any :=
        CORBA.Internals.Get_Empty_Any_Aggregate
           (IOP.Codec.Helper.TC_FormatMismatch);
      pragma Warnings (Off);
      pragma Unreferenced (Item);
      pragma Warnings (On);
   begin
      return Result_Ü;
   end To_Any;

   --------------------------
   -- Raise_FormatMismatch --
   --------------------------

   procedure Raise_FormatMismatch
     (Members : IOP.Codec.FormatMismatch_Members)
   is
   begin
      PolyORB.Exceptions.User_Raise_Exception
        (FormatMismatch'Identity,
         Members);
   end Raise_FormatMismatch;

   --------------
   -- From_Any --
   --------------

   function From_Any
     (Item : CORBA.Any)
     return IOP.Codec.TypeMismatch_Members
   is
      Result_Ü : IOP.Codec.TypeMismatch_Members;
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
     (Item : IOP.Codec.TypeMismatch_Members)
     return CORBA.Any
   is
      Result_Ü : constant CORBA.Any :=
        CORBA.Internals.Get_Empty_Any_Aggregate
           (IOP.Codec.Helper.TC_TypeMismatch);
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
     (Members : IOP.Codec.TypeMismatch_Members)
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
      IOP.Codec.Helper.Internals.Initialize_Codec;
      IOP.Codec.Helper.Internals.Initialize_InvalidTypeForEncoding;
      IOP.Codec.Helper.Internals.Initialize_FormatMismatch;
      IOP.Codec.Helper.Internals.Initialize_TypeMismatch;
   end Deferred_Initialization;

begin
   declare
      use PolyORB.Utils.Strings;
      use PolyORB.Utils.Strings.Lists;
   begin
      PolyORB.Initialization.Register_Module
        (PolyORB.Initialization.Module_Info'
           (Name => +"IOP.Codec.Helper",
            Conflicts => PolyORB.Utils.Strings.Lists.Empty,
            Depends => +"any"
               & "exceptions",
            Provides => PolyORB.Utils.Strings.Lists.Empty,
            Implicit => False,
            Init => Deferred_Initialization'Access,
            Shutdown => null));
   end;
end IOP.Codec.Helper;

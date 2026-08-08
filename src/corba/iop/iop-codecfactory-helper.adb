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

package body IOP.CodecFactory.Helper is

   
   package body Internals is

      CodecFactory_Initialized : PolyORB.Std.Boolean :=
        False;

      -----------------------------
      -- Initialize_CodecFactory --
      -----------------------------

      procedure Initialize_CodecFactory is
         Name_Ü : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("CodecFactory");
         Id_Ü : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("IDL:omg.org/IOP/CodecFactory:1.0");
      begin
         if not CodecFactory_Initialized
         then
            CodecFactory_Initialized :=
              True;
            IOP.CodecFactory.Helper.TC_CodecFactory :=
              CORBA.TypeCode.Internals.To_CORBA_Object
                 (PolyORB.Any.TypeCode.Tcf_Object);
            CORBA.Internals.Add_Parameter
              (TC_CodecFactory,
               CORBA.To_Any
                 (Name_Ü));
            CORBA.Internals.Add_Parameter
              (TC_CodecFactory,
               CORBA.To_Any
                 (Id_Ü));
            CORBA.TypeCode.Internals.Disable_Ref_Counting
              (TC_CodecFactory);
            CORBA.TypeCode.Internals.Freeze
              (TC_CodecFactory);
         end if;
      end Initialize_CodecFactory;

      procedure Raise_UnknownEncoding_From_Any
        (Item : PolyORB.Any.Any;
         Message : PolyORB.Std.String);

      pragma No_Return (Raise_UnknownEncoding_From_Any);

      ------------------------------------
      -- Raise_UnknownEncoding_From_Any --
      ------------------------------------

      procedure Raise_UnknownEncoding_From_Any
        (Item : PolyORB.Any.Any;
         Message : PolyORB.Std.String)
      is
         Members : constant IOP.CodecFactory.UnknownEncoding_Members :=
           IOP.CodecFactory.Helper.From_Any
              (CORBA.Any
                 (Item));
      begin
         PolyORB.Exceptions.User_Raise_Exception
           (UnknownEncoding'Identity,
            Members,
            Message);
      end Raise_UnknownEncoding_From_Any;

      UnknownEncoding_Initialized : PolyORB.Std.Boolean :=
        False;

      --------------------------------
      -- Initialize_UnknownEncoding --
      --------------------------------

      procedure Initialize_UnknownEncoding is
         Name_Ü : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("UnknownEncoding_Members");
         Id_Ü : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("IDL:omg.org/IOP/CodecFactory/UnknownEncoding:1.0");
      begin
         if not UnknownEncoding_Initialized
         then
            UnknownEncoding_Initialized :=
              True;
            IOP.CodecFactory.Helper.TC_UnknownEncoding :=
              CORBA.TypeCode.Internals.To_CORBA_Object
                 (PolyORB.Any.TypeCode.Tcf_Except);
            CORBA.Internals.Add_Parameter
              (TC_UnknownEncoding,
               CORBA.To_Any
                 (Name_Ü));
            CORBA.Internals.Add_Parameter
              (TC_UnknownEncoding,
               CORBA.To_Any
                 (Id_Ü));
            PolyORB.Exceptions.Register_Exception
              (CORBA.TypeCode.Internals.To_PolyORB_Object
                 (TC_UnknownEncoding),
               Raise_UnknownEncoding_From_Any'Access);
            CORBA.TypeCode.Internals.Disable_Ref_Counting
              (TC_UnknownEncoding);
            CORBA.TypeCode.Internals.Freeze
              (TC_UnknownEncoding);
         end if;
      end Initialize_UnknownEncoding;

   end Internals;

   ----------------------------
   -- Unchecked_To_Local_Ref --
   ----------------------------

   function Unchecked_To_Local_Ref
     (The_Ref : CORBA.Object.Ref'Class)
     return IOP.CodecFactory.Local_Ref
   is
      Result : IOP.CodecFactory.Local_Ref;
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
     return IOP.CodecFactory.Local_Ref
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
     return IOP.CodecFactory.UnknownEncoding_Members
   is
      Result_Ü : IOP.CodecFactory.UnknownEncoding_Members;
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
     (Item : IOP.CodecFactory.UnknownEncoding_Members)
     return CORBA.Any
   is
      Result_Ü : constant CORBA.Any :=
        CORBA.Internals.Get_Empty_Any_Aggregate
           (IOP.CodecFactory.Helper.TC_UnknownEncoding);
      pragma Warnings (Off);
      pragma Unreferenced (Item);
      pragma Warnings (On);
   begin
      return Result_Ü;
   end To_Any;

   ---------------------------
   -- Raise_UnknownEncoding --
   ---------------------------

   procedure Raise_UnknownEncoding
     (Members : IOP.CodecFactory.UnknownEncoding_Members)
   is
   begin
      PolyORB.Exceptions.User_Raise_Exception
        (UnknownEncoding'Identity,
         Members);
   end Raise_UnknownEncoding;

   -----------------------------
   -- Deferred_Initialization --
   -----------------------------

   procedure Deferred_Initialization is
   begin
      IOP.CodecFactory.Helper.Internals.Initialize_CodecFactory;
      IOP.CodecFactory.Helper.Internals.Initialize_UnknownEncoding;
   end Deferred_Initialization;

begin
   declare
      use PolyORB.Utils.Strings;
      use PolyORB.Utils.Strings.Lists;
   begin
      PolyORB.Initialization.Register_Module
        (PolyORB.Initialization.Module_Info'
           (Name => +"IOP.CodecFactory.Helper",
            Conflicts => PolyORB.Utils.Strings.Lists.Empty,
            Depends => +"any"
               & "exceptions",
            Provides => PolyORB.Utils.Strings.Lists.Empty,
            Implicit => False,
            Init => Deferred_Initialization'Access,
            Shutdown => null));
   end;
end IOP.CodecFactory.Helper;

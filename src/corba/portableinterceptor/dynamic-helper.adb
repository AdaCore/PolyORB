pragma Style_Checks ("NM32766");
pragma Warnings (Off, "use of an anonymous access type allocator");
pragma Warnings (Off, "unnecessary with of ancestor");

---------------------------------------------------
--  This file has been generated automatically from
--  /Users/heathdorn/Documents/Playground/Agents/RefactorTeam/code_refactor/PolyORB/idls/Misc/Dynamic.idl
--  by IAC (IDL to Ada Compiler) 20.0w (rev. 41a9b833).
---------------------------------------------------
--  NOTE: If you modify this file by hand, your
--  changes will be lost when you re-run the
--  IDL to Ada compiler.
---------------------------------------------------

with CORBA.Repository_Root.Helper;
with Ada.Unchecked_Conversion;
with PolyORB.Utils.Unchecked_Deallocation;
with PolyORB.Std;
with CORBA.IDL_Sequences.Helper;
with CORBA.IDL_Sequences;
with PolyORB.Utils.Strings;
with PolyORB.Utils.Strings.Lists;
with PolyORB.Initialization;

package body Dynamic.Helper is

   
   package body Internals is

      ---------------------------
      -- Get_Aggregate_Element --
      ---------------------------

      function Get_Aggregate_Element
        (Acc : not null access Content_Ü_Parameter;
         Tc : PolyORB.Any.TypeCode.Object_Ptr;
         Index : PolyORB.Types.Unsigned_Long;
         Mech : not null access PolyORB.Any.Mechanism)
        return PolyORB.Any.Content'Class
      is
         use type PolyORB.Any.Mechanism;
         pragma Suppress (Validity_Check);
         pragma Unreferenced (Tc);
      begin
         Mech.all :=
           PolyORB.Any.By_Reference;
         case Index is
            when 0 =>
               return CORBA.Wrap
                 (Acc.V.argument'Unrestricted_Access);
            when 1 =>
               return CORBA.Repository_Root.Helper.Internals.Wrap
                 (Acc.V.mode'Unrestricted_Access);
            pragma Warnings (Off);
            when others =>
               raise Constraint_Error;
            pragma Warnings (On);

         end case;
      end Get_Aggregate_Element;

      -------------------------
      -- Get_Aggregate_Count --
      -------------------------

      function Get_Aggregate_Count
        (Acc : Content_Ü_Parameter)
        return PolyORB.Types.Unsigned_Long
      is
         pragma Unreferenced (Acc);
      begin
         return 2;
      end Get_Aggregate_Count;

      -------------------------
      -- Set_Aggregate_Count --
      -------------------------

      procedure Set_Aggregate_Count
        (Acc : in out Content_Ü_Parameter;
         Count : PolyORB.Types.Unsigned_Long)
      is
      begin
         null;
      end Set_Aggregate_Count;

      ---------------------
      -- Unchecked_Get_V --
      ---------------------

      function Unchecked_Get_V
        (Acc : not null access Content_Ü_Parameter)
        return PolyORB.Types.Address
      is
         function To_Address
           is new Ada.Unchecked_Conversion
              (Ptr_Ü_Parameter,
               PolyORB.Types.Address);

      begin
         return To_Address
           (Acc.V);
      end Unchecked_Get_V;

      -----------
      -- Clone --
      -----------

      function Clone
        (Acc : Content_Ü_Parameter;
         Into : PolyORB.Any.Content_Ptr := null)
        return PolyORB.Any.Content_Ptr
      is
         use type PolyORB.Any.Content_Ptr;
         Target : PolyORB.Any.Content_Ptr;
      begin
         if (Into
            /= null)
         then
            if (Into.all
               not in Content_Ü_Parameter)
            then
               return null;
            end if;
            Target :=
              Into;
            Content_Ü_Parameter
              (Target.all).V.all :=
              Acc.V.all;
         else
            Target :=
              new Content_Ü_Parameter;
            Content_Ü_Parameter
              (Target.all).V :=
              new Dynamic.Parameter'
                 (Acc.V.all);
         end if;
         return Target;
      end Clone;

      --------------------
      -- Finalize_Value --
      --------------------

      procedure Finalize_Value
        (Acc : in out Content_Ü_Parameter)
      is
         procedure Free
           is new PolyORB.Utils.Unchecked_Deallocation.Free

           (Object => Dynamic.Parameter,

            Name   => Ptr_Ü_Parameter);

      begin
         Free
           (Acc.V);
      end Finalize_Value;

      ----------
      -- Wrap --
      ----------

      function Wrap
        (X : access Dynamic.Parameter)
        return PolyORB.Any.Content'Class
      is
      begin
         return Content_Ü_Parameter'
           (PolyORB.Any.Aggregate_Content with
            V => Ptr_Ü_Parameter'
              (X.all'Unchecked_Access));
      end Wrap;

      Parameter_Initialized : PolyORB.Std.Boolean :=
        False;

      --------------------------
      -- Initialize_Parameter --
      --------------------------

      procedure Initialize_Parameter is
         Name_Ü : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("Parameter");
         Id_Ü : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("IDL:omg.org/Dynamic/Parameter:1.0");
         Argument_Name_Ü_argument : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("argument");
         Argument_Name_Ü_mode : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("mode");
      begin
         if not Parameter_Initialized
         then
            Parameter_Initialized :=
              True;
            Dynamic.Helper.TC_Parameter :=
              CORBA.TypeCode.Internals.To_CORBA_Object
                 (PolyORB.Any.TypeCode.Tcf_Struct);
            CORBA.Internals.Add_Parameter
              (TC_Parameter,
               CORBA.To_Any
                 (Name_Ü));
            CORBA.Internals.Add_Parameter
              (TC_Parameter,
               CORBA.To_Any
                 (Id_Ü));
            CORBA.Internals.Add_Parameter
              (TC_Parameter,
               CORBA.To_Any
                 (CORBA.TC_Any));
            CORBA.Internals.Add_Parameter
              (TC_Parameter,
               CORBA.To_Any
                 (Argument_Name_Ü_argument));
            CORBA.Repository_Root.Helper.Internals.Initialize_ParameterMode;
            CORBA.Internals.Add_Parameter
              (TC_Parameter,
               CORBA.To_Any
                 (CORBA.Repository_Root.Helper.TC_ParameterMode));
            CORBA.Internals.Add_Parameter
              (TC_Parameter,
               CORBA.To_Any
                 (Argument_Name_Ü_mode));
            CORBA.TypeCode.Internals.Disable_Ref_Counting
              (TC_Parameter);
            CORBA.TypeCode.Internals.Freeze
              (TC_Parameter);
         end if;
      end Initialize_Parameter;

      -------------------------------------------------
      -- IDL_SEQUENCE_Dynamic_Parameter_Element_Wrap --
      -------------------------------------------------

      function IDL_SEQUENCE_Dynamic_Parameter_Element_Wrap
        (X : access Dynamic.Parameter)
        return PolyORB.Any.Content'Class
      is
      begin
         return Dynamic.Helper.Internals.Wrap
           (X.all'Unrestricted_Access);
      end IDL_SEQUENCE_Dynamic_Parameter_Element_Wrap;

      function Wrap
        (X : access Dynamic.IDL_SEQUENCE_Dynamic_Parameter.Sequence)
        return PolyORB.Any.Content'Class
        renames IDL_SEQUENCE_Dynamic_Parameter_Helper.Wrap;

      IDL_SEQUENCE_Dynamic_Parameter_Initialized : PolyORB.Std.Boolean :=
        False;

      -----------------------------------------------
      -- Initialize_IDL_SEQUENCE_Dynamic_Parameter --
      -----------------------------------------------

      procedure Initialize_IDL_SEQUENCE_Dynamic_Parameter is
      begin
         if not IDL_SEQUENCE_Dynamic_Parameter_Initialized
         then
            IDL_SEQUENCE_Dynamic_Parameter_Initialized :=
              True;
            Dynamic.Helper.Internals.Initialize_Parameter;
            Dynamic.Helper.TC_IDL_SEQUENCE_Dynamic_Parameter :=
              CORBA.TypeCode.Internals.Build_Sequence_TC
                 (Dynamic.Helper.TC_Parameter,
                  0);
            CORBA.TypeCode.Internals.Disable_Ref_Counting
              (Dynamic.Helper.TC_IDL_SEQUENCE_Dynamic_Parameter);
            IDL_SEQUENCE_Dynamic_Parameter_Helper.Initialize
              (Element_TC => Dynamic.Helper.TC_Parameter,
               Sequence_TC => Dynamic.Helper.TC_IDL_SEQUENCE_Dynamic_Parameter);
            CORBA.TypeCode.Internals.Freeze
              (TC_IDL_SEQUENCE_Dynamic_Parameter);
         end if;
      end Initialize_IDL_SEQUENCE_Dynamic_Parameter;

      ParameterList_Initialized : PolyORB.Std.Boolean :=
        False;

      ------------------------------
      -- Initialize_ParameterList --
      ------------------------------

      procedure Initialize_ParameterList is
         Name_Ü : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("ParameterList");
         Id_Ü : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("IDL:omg.org/Dynamic/ParameterList:1.0");
      begin
         if not ParameterList_Initialized
         then
            ParameterList_Initialized :=
              True;
            Dynamic.Helper.Internals.Initialize_IDL_SEQUENCE_Dynamic_Parameter;
            Dynamic.Helper.TC_ParameterList :=
              CORBA.TypeCode.Internals.Build_Alias_TC
                 (Name => Name_Ü,
                  Id => Id_Ü,
                  Parent => Dynamic.Helper.TC_IDL_SEQUENCE_Dynamic_Parameter);
            CORBA.TypeCode.Internals.Disable_Ref_Counting
              (TC_ParameterList);
            CORBA.TypeCode.Internals.Freeze
              (TC_ParameterList);
         end if;
      end Initialize_ParameterList;

      ContextList_Initialized : PolyORB.Std.Boolean :=
        False;

      ----------------------------
      -- Initialize_ContextList --
      ----------------------------

      procedure Initialize_ContextList is
         Name_Ü : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("ContextList");
         Id_Ü : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("IDL:omg.org/Dynamic/ContextList:1.0");
      begin
         if not ContextList_Initialized
         then
            ContextList_Initialized :=
              True;
            Dynamic.Helper.TC_ContextList :=
              CORBA.TypeCode.Internals.Build_Alias_TC
                 (Name => Name_Ü,
                  Id => Id_Ü,
                  Parent => CORBA.IDL_Sequences.Helper.TC_StringSeq);
            CORBA.TypeCode.Internals.Disable_Ref_Counting
              (TC_ContextList);
            CORBA.TypeCode.Internals.Freeze
              (TC_ContextList);
         end if;
      end Initialize_ContextList;

      ----------------------------------------------
      -- IDL_SEQUENCE_CORBA_TypeCode_Element_Wrap --
      ----------------------------------------------

      function IDL_SEQUENCE_CORBA_TypeCode_Element_Wrap
        (X : access CORBA.TypeCode.Object)
        return PolyORB.Any.Content'Class
      is
      begin
         return CORBA.Wrap
           (X.all'Unrestricted_Access);
      end IDL_SEQUENCE_CORBA_TypeCode_Element_Wrap;

      function Wrap
        (X : access Dynamic.IDL_SEQUENCE_CORBA_TypeCode.Sequence)
        return PolyORB.Any.Content'Class
        renames IDL_SEQUENCE_CORBA_TypeCode_Helper.Wrap;

      IDL_SEQUENCE_CORBA_TypeCode_Initialized : PolyORB.Std.Boolean :=
        False;

      --------------------------------------------
      -- Initialize_IDL_SEQUENCE_CORBA_TypeCode --
      --------------------------------------------

      procedure Initialize_IDL_SEQUENCE_CORBA_TypeCode is
      begin
         if not IDL_SEQUENCE_CORBA_TypeCode_Initialized
         then
            IDL_SEQUENCE_CORBA_TypeCode_Initialized :=
              True;
            Dynamic.Helper.TC_IDL_SEQUENCE_CORBA_TypeCode :=
              CORBA.TypeCode.Internals.Build_Sequence_TC
                 (CORBA.TC_TypeCode,
                  0);
            CORBA.TypeCode.Internals.Disable_Ref_Counting
              (Dynamic.Helper.TC_IDL_SEQUENCE_CORBA_TypeCode);
            IDL_SEQUENCE_CORBA_TypeCode_Helper.Initialize
              (Element_TC => CORBA.TC_TypeCode,
               Sequence_TC => Dynamic.Helper.TC_IDL_SEQUENCE_CORBA_TypeCode);
            CORBA.TypeCode.Internals.Freeze
              (TC_IDL_SEQUENCE_CORBA_TypeCode);
         end if;
      end Initialize_IDL_SEQUENCE_CORBA_TypeCode;

      ExceptionList_Initialized : PolyORB.Std.Boolean :=
        False;

      ------------------------------
      -- Initialize_ExceptionList --
      ------------------------------

      procedure Initialize_ExceptionList is
         Name_Ü : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("ExceptionList");
         Id_Ü : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("IDL:omg.org/Dynamic/ExceptionList:1.0");
      begin
         if not ExceptionList_Initialized
         then
            ExceptionList_Initialized :=
              True;
            Dynamic.Helper.Internals.Initialize_IDL_SEQUENCE_CORBA_TypeCode;
            Dynamic.Helper.TC_ExceptionList :=
              CORBA.TypeCode.Internals.Build_Alias_TC
                 (Name => Name_Ü,
                  Id => Id_Ü,
                  Parent => Dynamic.Helper.TC_IDL_SEQUENCE_CORBA_TypeCode);
            CORBA.TypeCode.Internals.Disable_Ref_Counting
              (TC_ExceptionList);
            CORBA.TypeCode.Internals.Freeze
              (TC_ExceptionList);
         end if;
      end Initialize_ExceptionList;

      RequestContext_Initialized : PolyORB.Std.Boolean :=
        False;

      -------------------------------
      -- Initialize_RequestContext --
      -------------------------------

      procedure Initialize_RequestContext is
         Name_Ü : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("RequestContext");
         Id_Ü : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("IDL:omg.org/Dynamic/RequestContext:1.0");
      begin
         if not RequestContext_Initialized
         then
            RequestContext_Initialized :=
              True;
            Dynamic.Helper.TC_RequestContext :=
              CORBA.TypeCode.Internals.Build_Alias_TC
                 (Name => Name_Ü,
                  Id => Id_Ü,
                  Parent => CORBA.IDL_Sequences.Helper.TC_StringSeq);
            CORBA.TypeCode.Internals.Disable_Ref_Counting
              (TC_RequestContext);
            CORBA.TypeCode.Internals.Freeze
              (TC_RequestContext);
         end if;
      end Initialize_RequestContext;

   end Internals;

   --------------
   -- From_Any --
   --------------

   function From_Any
     (Item : CORBA.Any)
     return Dynamic.Parameter
   is
   begin
      return (argument => CORBA.From_Any
        (CORBA.Internals.Get_Aggregate_Element
           (Item,
            CORBA.TC_Any,
            0)),
      mode => CORBA.Repository_Root.Helper.From_Any
        (CORBA.Internals.Get_Aggregate_Element
           (Item,
            CORBA.Repository_Root.Helper.TC_ParameterMode,
            1)));
   end From_Any;

   ------------
   -- To_Any --
   ------------

   function To_Any
     (Item : Dynamic.Parameter)
     return CORBA.Any
   is
      Result : constant CORBA.Any :=
        CORBA.Internals.Get_Empty_Any
           (TC_Parameter);
   begin
      PolyORB.Any.Set_Value
        (CORBA.Get_Container
           (Result).all,
         new PolyORB.Any.Content'Class'
           (Dynamic.Helper.Internals.Wrap
              (new Dynamic.Parameter'
                 (Item))),
         False);
      return Result;
   end To_Any;

   function From_Any
     (Item : CORBA.Any)
     return Dynamic.IDL_SEQUENCE_Dynamic_Parameter.Sequence
     renames Dynamic.Helper.Internals.IDL_SEQUENCE_Dynamic_Parameter_Helper.From_Any;

   function To_Any
     (Item : Dynamic.IDL_SEQUENCE_Dynamic_Parameter.Sequence)
     return CORBA.Any
     renames Dynamic.Helper.Internals.IDL_SEQUENCE_Dynamic_Parameter_Helper.To_Any;

   --------------
   -- From_Any --
   --------------

   function From_Any
     (Item : CORBA.Any)
     return Dynamic.ParameterList
   is
      Result : constant Dynamic.IDL_SEQUENCE_Dynamic_Parameter.Sequence :=
        Dynamic.Helper.From_Any
           (Item);
   begin
      return Dynamic.ParameterList
        (Result);
   end From_Any;

   ------------
   -- To_Any --
   ------------

   function To_Any
     (Item : Dynamic.ParameterList)
     return CORBA.Any
   is
      Result : CORBA.Any :=
        Dynamic.Helper.To_Any
           (Dynamic.IDL_SEQUENCE_Dynamic_Parameter.Sequence
              (Item));
   begin
      CORBA.Internals.Set_Type
        (Result,
         TC_ParameterList);
      return Result;
   end To_Any;

   --------------
   -- From_Any --
   --------------

   function From_Any
     (Item : CORBA.Any)
     return Dynamic.ContextList
   is
      Result : constant CORBA.IDL_Sequences.StringSeq :=
        CORBA.IDL_Sequences.Helper.From_Any
           (Item);
   begin
      return Dynamic.ContextList
        (Result);
   end From_Any;

   ------------
   -- To_Any --
   ------------

   function To_Any
     (Item : Dynamic.ContextList)
     return CORBA.Any
   is
      Result : CORBA.Any :=
        CORBA.IDL_Sequences.Helper.To_Any
           (CORBA.IDL_Sequences.StringSeq
              (Item));
   begin
      CORBA.Internals.Set_Type
        (Result,
         TC_ContextList);
      return Result;
   end To_Any;

   function From_Any
     (Item : CORBA.Any)
     return Dynamic.IDL_SEQUENCE_CORBA_TypeCode.Sequence
     renames Dynamic.Helper.Internals.IDL_SEQUENCE_CORBA_TypeCode_Helper.From_Any;

   function To_Any
     (Item : Dynamic.IDL_SEQUENCE_CORBA_TypeCode.Sequence)
     return CORBA.Any
     renames Dynamic.Helper.Internals.IDL_SEQUENCE_CORBA_TypeCode_Helper.To_Any;

   --------------
   -- From_Any --
   --------------

   function From_Any
     (Item : CORBA.Any)
     return Dynamic.ExceptionList
   is
      Result : constant Dynamic.IDL_SEQUENCE_CORBA_TypeCode.Sequence :=
        Dynamic.Helper.From_Any
           (Item);
   begin
      return Dynamic.ExceptionList
        (Result);
   end From_Any;

   ------------
   -- To_Any --
   ------------

   function To_Any
     (Item : Dynamic.ExceptionList)
     return CORBA.Any
   is
      Result : CORBA.Any :=
        Dynamic.Helper.To_Any
           (Dynamic.IDL_SEQUENCE_CORBA_TypeCode.Sequence
              (Item));
   begin
      CORBA.Internals.Set_Type
        (Result,
         TC_ExceptionList);
      return Result;
   end To_Any;

   --------------
   -- From_Any --
   --------------

   function From_Any
     (Item : CORBA.Any)
     return Dynamic.RequestContext
   is
      Result : constant CORBA.IDL_Sequences.StringSeq :=
        CORBA.IDL_Sequences.Helper.From_Any
           (Item);
   begin
      return Dynamic.RequestContext
        (Result);
   end From_Any;

   ------------
   -- To_Any --
   ------------

   function To_Any
     (Item : Dynamic.RequestContext)
     return CORBA.Any
   is
      Result : CORBA.Any :=
        CORBA.IDL_Sequences.Helper.To_Any
           (CORBA.IDL_Sequences.StringSeq
              (Item));
   begin
      CORBA.Internals.Set_Type
        (Result,
         TC_RequestContext);
      return Result;
   end To_Any;

   -----------------------------
   -- Deferred_Initialization --
   -----------------------------

   procedure Deferred_Initialization is
   begin
      Dynamic.Helper.Internals.Initialize_Parameter;
      Dynamic.Helper.Internals.Initialize_IDL_SEQUENCE_Dynamic_Parameter;
      Dynamic.Helper.Internals.Initialize_ParameterList;
      Dynamic.Helper.Internals.Initialize_ContextList;
      Dynamic.Helper.Internals.Initialize_IDL_SEQUENCE_CORBA_TypeCode;
      Dynamic.Helper.Internals.Initialize_ExceptionList;
      Dynamic.Helper.Internals.Initialize_RequestContext;
   end Deferred_Initialization;

begin
   declare
      use PolyORB.Utils.Strings;
      use PolyORB.Utils.Strings.Lists;
   begin
      PolyORB.Initialization.Register_Module
        (PolyORB.Initialization.Module_Info'
           (Name => +"Dynamic.Helper",
            Conflicts => PolyORB.Utils.Strings.Lists.Empty,
            Depends => +"any"
               & "corba",
            Provides => PolyORB.Utils.Strings.Lists.Empty,
            Implicit => False,
            Init => Deferred_Initialization'Access,
            Shutdown => null));
   end;
end Dynamic.Helper;

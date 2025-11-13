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
with Ada.Unchecked_Conversion;
with PolyORB.Utils.Unchecked_Deallocation;
with CORBA.Object.Helper;
with DynamicAny.DynAny.Helper;
with PolyORB.Exceptions;
with PolyORB.Utils.Strings;
with PolyORB.Utils.Strings.Lists;
with PolyORB.Initialization;

package body DynamicAny.Helper is

   
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
            DynamicAny.Helper.TC_DynAny :=
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

      ----------
      -- Wrap --
      ----------

      function Wrap
        (X : access DynamicAny.FieldName)
        return PolyORB.Any.Content'Class
      is
      begin
         return CORBA.Wrap
           (CORBA.String
              (X.all)'Unrestricted_Access);
      end Wrap;

      FieldName_Initialized : PolyORB.Std.Boolean :=
        False;

      --------------------------
      -- Initialize_FieldName --
      --------------------------

      procedure Initialize_FieldName is
         Name_Ü : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("FieldName");
         Id_Ü : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("IDL:omg.org/DynamicAny/FieldName:1.0");
      begin
         if not FieldName_Initialized
         then
            FieldName_Initialized :=
              True;
            DynamicAny.Helper.TC_FieldName :=
              CORBA.TypeCode.Internals.Build_Alias_TC
                 (Name => Name_Ü,
                  Id => Id_Ü,
                  Parent => CORBA.TC_String);
            CORBA.TypeCode.Internals.Disable_Ref_Counting
              (TC_FieldName);
            CORBA.TypeCode.Internals.Freeze
              (TC_FieldName);
         end if;
      end Initialize_FieldName;

      ---------------------------
      -- Get_Aggregate_Element --
      ---------------------------

      function Get_Aggregate_Element
        (Acc : not null access Content_Ü_NameValuePair;
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
                 (CORBA.String
                    (Acc.V.id)'Unrestricted_Access);
            when 1 =>
               return CORBA.Wrap
                 (Acc.V.value'Unrestricted_Access);
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
        (Acc : Content_Ü_NameValuePair)
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
        (Acc : in out Content_Ü_NameValuePair;
         Count : PolyORB.Types.Unsigned_Long)
      is
      begin
         null;
      end Set_Aggregate_Count;

      ---------------------
      -- Unchecked_Get_V --
      ---------------------

      function Unchecked_Get_V
        (Acc : not null access Content_Ü_NameValuePair)
        return PolyORB.Types.Address
      is
         function To_Address
           is new Ada.Unchecked_Conversion
              (Ptr_Ü_NameValuePair,
               PolyORB.Types.Address);

      begin
         return To_Address
           (Acc.V);
      end Unchecked_Get_V;

      -----------
      -- Clone --
      -----------

      function Clone
        (Acc : Content_Ü_NameValuePair;
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
               not in Content_Ü_NameValuePair)
            then
               return null;
            end if;
            Target :=
              Into;
            Content_Ü_NameValuePair
              (Target.all).V.all :=
              Acc.V.all;
         else
            Target :=
              new Content_Ü_NameValuePair;
            Content_Ü_NameValuePair
              (Target.all).V :=
              new DynamicAny.NameValuePair'
                 (Acc.V.all);
         end if;
         return Target;
      end Clone;

      --------------------
      -- Finalize_Value --
      --------------------

      procedure Finalize_Value
        (Acc : in out Content_Ü_NameValuePair)
      is
         procedure Free
           is new PolyORB.Utils.Unchecked_Deallocation.Free

           (Object => DynamicAny.NameValuePair,

            Name   => Ptr_Ü_NameValuePair);

      begin
         Free
           (Acc.V);
      end Finalize_Value;

      ----------
      -- Wrap --
      ----------

      function Wrap
        (X : access DynamicAny.NameValuePair)
        return PolyORB.Any.Content'Class
      is
      begin
         return Content_Ü_NameValuePair'
           (PolyORB.Any.Aggregate_Content with
            V => Ptr_Ü_NameValuePair'
              (X.all'Unchecked_Access));
      end Wrap;

      NameValuePair_Initialized : PolyORB.Std.Boolean :=
        False;

      ------------------------------
      -- Initialize_NameValuePair --
      ------------------------------

      procedure Initialize_NameValuePair is
         Name_Ü : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("NameValuePair");
         Id_Ü : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("IDL:omg.org/DynamicAny/NameValuePair:1.0");
         Argument_Name_Ü_id : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("id");
         Argument_Name_Ü_value : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("value");
      begin
         if not NameValuePair_Initialized
         then
            NameValuePair_Initialized :=
              True;
            DynamicAny.Helper.TC_NameValuePair :=
              CORBA.TypeCode.Internals.To_CORBA_Object
                 (PolyORB.Any.TypeCode.Tcf_Struct);
            CORBA.Internals.Add_Parameter
              (TC_NameValuePair,
               CORBA.To_Any
                 (Name_Ü));
            CORBA.Internals.Add_Parameter
              (TC_NameValuePair,
               CORBA.To_Any
                 (Id_Ü));
            DynamicAny.Helper.Internals.Initialize_FieldName;
            CORBA.Internals.Add_Parameter
              (TC_NameValuePair,
               CORBA.To_Any
                 (DynamicAny.Helper.TC_FieldName));
            CORBA.Internals.Add_Parameter
              (TC_NameValuePair,
               CORBA.To_Any
                 (Argument_Name_Ü_id));
            CORBA.Internals.Add_Parameter
              (TC_NameValuePair,
               CORBA.To_Any
                 (CORBA.TC_Any));
            CORBA.Internals.Add_Parameter
              (TC_NameValuePair,
               CORBA.To_Any
                 (Argument_Name_Ü_value));
            CORBA.TypeCode.Internals.Disable_Ref_Counting
              (TC_NameValuePair);
            CORBA.TypeCode.Internals.Freeze
              (TC_NameValuePair);
         end if;
      end Initialize_NameValuePair;

      --------------------------------------------------------
      -- IDL_SEQUENCE_DynamicAny_NameValuePair_Element_Wrap --
      --------------------------------------------------------

      function IDL_SEQUENCE_DynamicAny_NameValuePair_Element_Wrap
        (X : access DynamicAny.NameValuePair)
        return PolyORB.Any.Content'Class
      is
      begin
         return DynamicAny.Helper.Internals.Wrap
           (X.all'Unrestricted_Access);
      end IDL_SEQUENCE_DynamicAny_NameValuePair_Element_Wrap;

      function Wrap
        (X : access DynamicAny.IDL_SEQUENCE_DynamicAny_NameValuePair.Sequence)
        return PolyORB.Any.Content'Class
        renames IDL_SEQUENCE_DynamicAny_NameValuePair_Helper.Wrap;

      IDL_SEQUENCE_DynamicAny_NameValuePair_Initialized : PolyORB.Std.Boolean :=
        False;

      ------------------------------------------------------
      -- Initialize_IDL_SEQUENCE_DynamicAny_NameValuePair --
      ------------------------------------------------------

      procedure Initialize_IDL_SEQUENCE_DynamicAny_NameValuePair is
      begin
         if not IDL_SEQUENCE_DynamicAny_NameValuePair_Initialized
         then
            IDL_SEQUENCE_DynamicAny_NameValuePair_Initialized :=
              True;
            DynamicAny.Helper.Internals.Initialize_NameValuePair;
            DynamicAny.Helper.TC_IDL_SEQUENCE_DynamicAny_NameValuePair :=
              CORBA.TypeCode.Internals.Build_Sequence_TC
                 (DynamicAny.Helper.TC_NameValuePair,
                  0);
            CORBA.TypeCode.Internals.Disable_Ref_Counting
              (DynamicAny.Helper.TC_IDL_SEQUENCE_DynamicAny_NameValuePair);
            IDL_SEQUENCE_DynamicAny_NameValuePair_Helper.Initialize
              (Element_TC => DynamicAny.Helper.TC_NameValuePair,
               Sequence_TC => DynamicAny.Helper.TC_IDL_SEQUENCE_DynamicAny_NameValuePair);
            CORBA.TypeCode.Internals.Freeze
              (TC_IDL_SEQUENCE_DynamicAny_NameValuePair);
         end if;
      end Initialize_IDL_SEQUENCE_DynamicAny_NameValuePair;

      NameValuePairSeq_Initialized : PolyORB.Std.Boolean :=
        False;

      ---------------------------------
      -- Initialize_NameValuePairSeq --
      ---------------------------------

      procedure Initialize_NameValuePairSeq is
         Name_Ü : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("NameValuePairSeq");
         Id_Ü : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("IDL:omg.org/DynamicAny/NameValuePairSeq:1.0");
      begin
         if not NameValuePairSeq_Initialized
         then
            NameValuePairSeq_Initialized :=
              True;
            DynamicAny.Helper.Internals.Initialize_IDL_SEQUENCE_DynamicAny_NameValuePair;
            DynamicAny.Helper.TC_NameValuePairSeq :=
              CORBA.TypeCode.Internals.Build_Alias_TC
                 (Name => Name_Ü,
                  Id => Id_Ü,
                  Parent => DynamicAny.Helper.TC_IDL_SEQUENCE_DynamicAny_NameValuePair);
            CORBA.TypeCode.Internals.Disable_Ref_Counting
              (TC_NameValuePairSeq);
            CORBA.TypeCode.Internals.Freeze
              (TC_NameValuePairSeq);
         end if;
      end Initialize_NameValuePairSeq;

      ---------------------------
      -- Get_Aggregate_Element --
      ---------------------------

      function Get_Aggregate_Element
        (Acc : not null access Content_Ü_NameDynAnyPair;
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
                 (CORBA.String
                    (Acc.V.id)'Unrestricted_Access);
            when 1 =>
               return CORBA.Object.Helper.Wrap
                 (CORBA.Object.Ref
                    (Acc.V.value)'Unrestricted_Access);
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
        (Acc : Content_Ü_NameDynAnyPair)
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
        (Acc : in out Content_Ü_NameDynAnyPair;
         Count : PolyORB.Types.Unsigned_Long)
      is
      begin
         null;
      end Set_Aggregate_Count;

      ---------------------
      -- Unchecked_Get_V --
      ---------------------

      function Unchecked_Get_V
        (Acc : not null access Content_Ü_NameDynAnyPair)
        return PolyORB.Types.Address
      is
         function To_Address
           is new Ada.Unchecked_Conversion
              (Ptr_Ü_NameDynAnyPair,
               PolyORB.Types.Address);

      begin
         return To_Address
           (Acc.V);
      end Unchecked_Get_V;

      -----------
      -- Clone --
      -----------

      function Clone
        (Acc : Content_Ü_NameDynAnyPair;
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
               not in Content_Ü_NameDynAnyPair)
            then
               return null;
            end if;
            Target :=
              Into;
            Content_Ü_NameDynAnyPair
              (Target.all).V.all :=
              Acc.V.all;
         else
            Target :=
              new Content_Ü_NameDynAnyPair;
            Content_Ü_NameDynAnyPair
              (Target.all).V :=
              new DynamicAny.NameDynAnyPair'
                 (Acc.V.all);
         end if;
         return Target;
      end Clone;

      --------------------
      -- Finalize_Value --
      --------------------

      procedure Finalize_Value
        (Acc : in out Content_Ü_NameDynAnyPair)
      is
         procedure Free
           is new PolyORB.Utils.Unchecked_Deallocation.Free

           (Object => DynamicAny.NameDynAnyPair,

            Name   => Ptr_Ü_NameDynAnyPair);

      begin
         Free
           (Acc.V);
      end Finalize_Value;

      ----------
      -- Wrap --
      ----------

      function Wrap
        (X : access DynamicAny.NameDynAnyPair)
        return PolyORB.Any.Content'Class
      is
      begin
         return Content_Ü_NameDynAnyPair'
           (PolyORB.Any.Aggregate_Content with
            V => Ptr_Ü_NameDynAnyPair'
              (X.all'Unchecked_Access));
      end Wrap;

      NameDynAnyPair_Initialized : PolyORB.Std.Boolean :=
        False;

      -------------------------------
      -- Initialize_NameDynAnyPair --
      -------------------------------

      procedure Initialize_NameDynAnyPair is
         Name_Ü : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("NameDynAnyPair");
         Id_Ü : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("IDL:omg.org/DynamicAny/NameDynAnyPair:1.0");
         Argument_Name_Ü_id : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("id");
         Argument_Name_Ü_value : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("value");
      begin
         if not NameDynAnyPair_Initialized
         then
            NameDynAnyPair_Initialized :=
              True;
            DynamicAny.Helper.TC_NameDynAnyPair :=
              CORBA.TypeCode.Internals.To_CORBA_Object
                 (PolyORB.Any.TypeCode.Tcf_Struct);
            CORBA.Internals.Add_Parameter
              (TC_NameDynAnyPair,
               CORBA.To_Any
                 (Name_Ü));
            CORBA.Internals.Add_Parameter
              (TC_NameDynAnyPair,
               CORBA.To_Any
                 (Id_Ü));
            DynamicAny.Helper.Internals.Initialize_FieldName;
            CORBA.Internals.Add_Parameter
              (TC_NameDynAnyPair,
               CORBA.To_Any
                 (DynamicAny.Helper.TC_FieldName));
            CORBA.Internals.Add_Parameter
              (TC_NameDynAnyPair,
               CORBA.To_Any
                 (Argument_Name_Ü_id));
            DynamicAny.DynAny.Helper.Internals.Initialize_DynAny;
            CORBA.Internals.Add_Parameter
              (TC_NameDynAnyPair,
               CORBA.To_Any
                 (DynamicAny.DynAny.Helper.TC_DynAny));
            CORBA.Internals.Add_Parameter
              (TC_NameDynAnyPair,
               CORBA.To_Any
                 (Argument_Name_Ü_value));
            CORBA.TypeCode.Internals.Disable_Ref_Counting
              (TC_NameDynAnyPair);
            CORBA.TypeCode.Internals.Freeze
              (TC_NameDynAnyPair);
         end if;
      end Initialize_NameDynAnyPair;

      IDL_SEQUENCE_DynamicAny_NameDynAnyPair_Initialized : PolyORB.Std.Boolean :=
        False;

      -------------------------------------------------------
      -- Initialize_IDL_SEQUENCE_DynamicAny_NameDynAnyPair --
      -------------------------------------------------------

      procedure Initialize_IDL_SEQUENCE_DynamicAny_NameDynAnyPair is
      begin
         if not IDL_SEQUENCE_DynamicAny_NameDynAnyPair_Initialized
         then
            IDL_SEQUENCE_DynamicAny_NameDynAnyPair_Initialized :=
              True;
            DynamicAny.Helper.Internals.Initialize_NameDynAnyPair;
            DynamicAny.Helper.TC_IDL_SEQUENCE_DynamicAny_NameDynAnyPair :=
              CORBA.TypeCode.Internals.Build_Sequence_TC
                 (DynamicAny.Helper.TC_NameDynAnyPair,
                  0);
            CORBA.TypeCode.Internals.Disable_Ref_Counting
              (DynamicAny.Helper.TC_IDL_SEQUENCE_DynamicAny_NameDynAnyPair);
            CORBA.TypeCode.Internals.Freeze
              (TC_IDL_SEQUENCE_DynamicAny_NameDynAnyPair);
         end if;
      end Initialize_IDL_SEQUENCE_DynamicAny_NameDynAnyPair;

      NameDynAnyPairSeq_Initialized : PolyORB.Std.Boolean :=
        False;

      ----------------------------------
      -- Initialize_NameDynAnyPairSeq --
      ----------------------------------

      procedure Initialize_NameDynAnyPairSeq is
         Name_Ü : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("NameDynAnyPairSeq");
         Id_Ü : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("IDL:omg.org/DynamicAny/NameDynAnyPairSeq:1.0");
      begin
         if not NameDynAnyPairSeq_Initialized
         then
            NameDynAnyPairSeq_Initialized :=
              True;
            DynamicAny.Helper.Internals.Initialize_IDL_SEQUENCE_DynamicAny_NameDynAnyPair;
            DynamicAny.Helper.TC_NameDynAnyPairSeq :=
              CORBA.TypeCode.Internals.Build_Alias_TC
                 (Name => Name_Ü,
                  Id => Id_Ü,
                  Parent => DynamicAny.Helper.TC_IDL_SEQUENCE_DynamicAny_NameDynAnyPair);
            CORBA.TypeCode.Internals.Disable_Ref_Counting
              (TC_NameDynAnyPairSeq);
            CORBA.TypeCode.Internals.Freeze
              (TC_NameDynAnyPairSeq);
         end if;
      end Initialize_NameDynAnyPairSeq;

      -----------------------------------
      -- IDL_SEQUENCE_any_Element_Wrap --
      -----------------------------------

      function IDL_SEQUENCE_any_Element_Wrap
        (X : access CORBA.Any)
        return PolyORB.Any.Content'Class
      is
      begin
         return CORBA.Wrap
           (X.all'Unrestricted_Access);
      end IDL_SEQUENCE_any_Element_Wrap;

      function Wrap
        (X : access DynamicAny.IDL_SEQUENCE_any.Sequence)
        return PolyORB.Any.Content'Class
        renames IDL_SEQUENCE_any_Helper.Wrap;

      IDL_SEQUENCE_any_Initialized : PolyORB.Std.Boolean :=
        False;

      ---------------------------------
      -- Initialize_IDL_SEQUENCE_any --
      ---------------------------------

      procedure Initialize_IDL_SEQUENCE_any is
      begin
         if not IDL_SEQUENCE_any_Initialized
         then
            IDL_SEQUENCE_any_Initialized :=
              True;
            DynamicAny.Helper.TC_IDL_SEQUENCE_any :=
              CORBA.TypeCode.Internals.Build_Sequence_TC
                 (CORBA.TC_Any,
                  0);
            CORBA.TypeCode.Internals.Disable_Ref_Counting
              (DynamicAny.Helper.TC_IDL_SEQUENCE_any);
            IDL_SEQUENCE_any_Helper.Initialize
              (Element_TC => CORBA.TC_Any,
               Sequence_TC => DynamicAny.Helper.TC_IDL_SEQUENCE_any);
            CORBA.TypeCode.Internals.Freeze
              (TC_IDL_SEQUENCE_any);
         end if;
      end Initialize_IDL_SEQUENCE_any;

      AnySeq_Initialized : PolyORB.Std.Boolean :=
        False;

      -----------------------
      -- Initialize_AnySeq --
      -----------------------

      procedure Initialize_AnySeq is
         Name_Ü : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("AnySeq");
         Id_Ü : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("IDL:omg.org/DynamicAny/AnySeq:1.0");
      begin
         if not AnySeq_Initialized
         then
            AnySeq_Initialized :=
              True;
            DynamicAny.Helper.Internals.Initialize_IDL_SEQUENCE_any;
            DynamicAny.Helper.TC_AnySeq :=
              CORBA.TypeCode.Internals.Build_Alias_TC
                 (Name => Name_Ü,
                  Id => Id_Ü,
                  Parent => DynamicAny.Helper.TC_IDL_SEQUENCE_any);
            CORBA.TypeCode.Internals.Disable_Ref_Counting
              (TC_AnySeq);
            CORBA.TypeCode.Internals.Freeze
              (TC_AnySeq);
         end if;
      end Initialize_AnySeq;

      IDL_SEQUENCE_DynamicAny_DynAny_Forward_Initialized : PolyORB.Std.Boolean :=
        False;

      -------------------------------------------------------
      -- Initialize_IDL_SEQUENCE_DynamicAny_DynAny_Forward --
      -------------------------------------------------------

      procedure Initialize_IDL_SEQUENCE_DynamicAny_DynAny_Forward is
      begin
         if not IDL_SEQUENCE_DynamicAny_DynAny_Forward_Initialized
         then
            IDL_SEQUENCE_DynamicAny_DynAny_Forward_Initialized :=
              True;
            DynamicAny.DynAny.Helper.Internals.Initialize_DynAny;
            DynamicAny.Helper.TC_IDL_SEQUENCE_DynamicAny_DynAny_Forward :=
              CORBA.TypeCode.Internals.Build_Sequence_TC
                 (DynamicAny.DynAny.Helper.TC_DynAny,
                  0);
            CORBA.TypeCode.Internals.Disable_Ref_Counting
              (DynamicAny.Helper.TC_IDL_SEQUENCE_DynamicAny_DynAny_Forward);
            CORBA.TypeCode.Internals.Freeze
              (TC_IDL_SEQUENCE_DynamicAny_DynAny_Forward);
         end if;
      end Initialize_IDL_SEQUENCE_DynamicAny_DynAny_Forward;

      DynAnySeq_Initialized : PolyORB.Std.Boolean :=
        False;

      --------------------------
      -- Initialize_DynAnySeq --
      --------------------------

      procedure Initialize_DynAnySeq is
         Name_Ü : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("DynAnySeq");
         Id_Ü : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("IDL:omg.org/DynamicAny/DynAnySeq:1.0");
      begin
         if not DynAnySeq_Initialized
         then
            DynAnySeq_Initialized :=
              True;
            DynamicAny.Helper.Internals.Initialize_IDL_SEQUENCE_DynamicAny_DynAny_Forward;
            DynamicAny.Helper.TC_DynAnySeq :=
              CORBA.TypeCode.Internals.Build_Alias_TC
                 (Name => Name_Ü,
                  Id => Id_Ü,
                  Parent => DynamicAny.Helper.TC_IDL_SEQUENCE_DynamicAny_DynAny_Forward);
            CORBA.TypeCode.Internals.Disable_Ref_Counting
              (TC_DynAnySeq);
            CORBA.TypeCode.Internals.Freeze
              (TC_DynAnySeq);
         end if;
      end Initialize_DynAnySeq;

      procedure Raise_MustTruncate_From_Any
        (Item : PolyORB.Any.Any;
         Message : PolyORB.Std.String);

      pragma No_Return (Raise_MustTruncate_From_Any);

      ---------------------------------
      -- Raise_MustTruncate_From_Any --
      ---------------------------------

      procedure Raise_MustTruncate_From_Any
        (Item : PolyORB.Any.Any;
         Message : PolyORB.Std.String)
      is
         Members : constant DynamicAny.MustTruncate_Members :=
           DynamicAny.Helper.From_Any
              (CORBA.Any
                 (Item));
      begin
         PolyORB.Exceptions.User_Raise_Exception
           (MustTruncate'Identity,
            Members,
            Message);
      end Raise_MustTruncate_From_Any;

      MustTruncate_Initialized : PolyORB.Std.Boolean :=
        False;

      -----------------------------
      -- Initialize_MustTruncate --
      -----------------------------

      procedure Initialize_MustTruncate is
         Name_Ü : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("MustTruncate_Members");
         Id_Ü : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("IDL:omg.org/DynamicAny/MustTruncate:1.0");
      begin
         if not MustTruncate_Initialized
         then
            MustTruncate_Initialized :=
              True;
            DynamicAny.Helper.TC_MustTruncate :=
              CORBA.TypeCode.Internals.To_CORBA_Object
                 (PolyORB.Any.TypeCode.Tcf_Except);
            CORBA.Internals.Add_Parameter
              (TC_MustTruncate,
               CORBA.To_Any
                 (Name_Ü));
            CORBA.Internals.Add_Parameter
              (TC_MustTruncate,
               CORBA.To_Any
                 (Id_Ü));
            PolyORB.Exceptions.Register_Exception
              (CORBA.TypeCode.Internals.To_PolyORB_Object
                 (TC_MustTruncate),
               Raise_MustTruncate_From_Any'Access);
            CORBA.TypeCode.Internals.Disable_Ref_Counting
              (TC_MustTruncate);
            CORBA.TypeCode.Internals.Freeze
              (TC_MustTruncate);
         end if;
      end Initialize_MustTruncate;

   end Internals;

   ----------------------------
   -- Unchecked_To_Local_Ref --
   ----------------------------

   function Unchecked_To_Local_Ref
     (The_Ref : CORBA.Object.Ref'Class)
     return DynamicAny.DynAny_Forward.Ref
   is
      Result : DynamicAny.DynAny_Forward.Ref;
   begin
      DynamicAny.DynAny_Forward.Set
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
     return DynamicAny.DynAny_Forward.Ref
   is
   begin
      if (CORBA.Object.Is_Nil
        (The_Ref)
         or else CORBA.Object.Is_A
           (The_Ref,
            "IDL:omg.org/DynamicAny/DynAny:1.0"))
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
     return DynamicAny.FieldName
   is
      Result : constant CORBA.String :=
        CORBA.From_Any
           (Item);
   begin
      return DynamicAny.FieldName
        (Result);
   end From_Any;

   ------------
   -- To_Any --
   ------------

   function To_Any
     (Item : DynamicAny.FieldName)
     return CORBA.Any
   is
      Result : CORBA.Any :=
        CORBA.To_Any
           (CORBA.String
              (Item));
   begin
      CORBA.Internals.Set_Type
        (Result,
         TC_FieldName);
      return Result;
   end To_Any;

   --------------
   -- From_Any --
   --------------

   function From_Any
     (Item : CORBA.Any)
     return DynamicAny.NameValuePair
   is
   begin
      return (id => DynamicAny.Helper.From_Any
        (CORBA.Internals.Get_Aggregate_Element
           (Item,
            DynamicAny.Helper.TC_FieldName,
            0)),
      value => CORBA.From_Any
        (CORBA.Internals.Get_Aggregate_Element
           (Item,
            CORBA.TC_Any,
            1)));
   end From_Any;

   ------------
   -- To_Any --
   ------------

   function To_Any
     (Item : DynamicAny.NameValuePair)
     return CORBA.Any
   is
      Result : constant CORBA.Any :=
        CORBA.Internals.Get_Empty_Any
           (TC_NameValuePair);
   begin
      PolyORB.Any.Set_Value
        (CORBA.Get_Container
           (Result).all,
         new PolyORB.Any.Content'Class'
           (DynamicAny.Helper.Internals.Wrap
              (new DynamicAny.NameValuePair'
                 (Item))),
         False);
      return Result;
   end To_Any;

   function From_Any
     (Item : CORBA.Any)
     return DynamicAny.IDL_SEQUENCE_DynamicAny_NameValuePair.Sequence
     renames DynamicAny.Helper.Internals.IDL_SEQUENCE_DynamicAny_NameValuePair_Helper.From_Any;

   function To_Any
     (Item : DynamicAny.IDL_SEQUENCE_DynamicAny_NameValuePair.Sequence)
     return CORBA.Any
     renames DynamicAny.Helper.Internals.IDL_SEQUENCE_DynamicAny_NameValuePair_Helper.To_Any;

   --------------
   -- From_Any --
   --------------

   function From_Any
     (Item : CORBA.Any)
     return DynamicAny.NameValuePairSeq
   is
      Result : constant DynamicAny.IDL_SEQUENCE_DynamicAny_NameValuePair.Sequence :=
        DynamicAny.Helper.From_Any
           (Item);
   begin
      return DynamicAny.NameValuePairSeq
        (Result);
   end From_Any;

   ------------
   -- To_Any --
   ------------

   function To_Any
     (Item : DynamicAny.NameValuePairSeq)
     return CORBA.Any
   is
      Result : CORBA.Any :=
        DynamicAny.Helper.To_Any
           (DynamicAny.IDL_SEQUENCE_DynamicAny_NameValuePair.Sequence
              (Item));
   begin
      CORBA.Internals.Set_Type
        (Result,
         TC_NameValuePairSeq);
      return Result;
   end To_Any;

   function From_Any
     (Item : CORBA.Any)
     return DynamicAny.IDL_SEQUENCE_any.Sequence
     renames DynamicAny.Helper.Internals.IDL_SEQUENCE_any_Helper.From_Any;

   function To_Any
     (Item : DynamicAny.IDL_SEQUENCE_any.Sequence)
     return CORBA.Any
     renames DynamicAny.Helper.Internals.IDL_SEQUENCE_any_Helper.To_Any;

   --------------
   -- From_Any --
   --------------

   function From_Any
     (Item : CORBA.Any)
     return DynamicAny.AnySeq
   is
      Result : constant DynamicAny.IDL_SEQUENCE_any.Sequence :=
        DynamicAny.Helper.From_Any
           (Item);
   begin
      return DynamicAny.AnySeq
        (Result);
   end From_Any;

   ------------
   -- To_Any --
   ------------

   function To_Any
     (Item : DynamicAny.AnySeq)
     return CORBA.Any
   is
      Result : CORBA.Any :=
        DynamicAny.Helper.To_Any
           (DynamicAny.IDL_SEQUENCE_any.Sequence
              (Item));
   begin
      CORBA.Internals.Set_Type
        (Result,
         TC_AnySeq);
      return Result;
   end To_Any;

   --------------
   -- From_Any --
   --------------

   function From_Any
     (Item : CORBA.Any)
     return DynamicAny.MustTruncate_Members
   is
      Result_Ü : DynamicAny.MustTruncate_Members;
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
     (Item : DynamicAny.MustTruncate_Members)
     return CORBA.Any
   is
      Result_Ü : constant CORBA.Any :=
        CORBA.Internals.Get_Empty_Any_Aggregate
           (DynamicAny.Helper.TC_MustTruncate);
      pragma Warnings (Off);
      pragma Unreferenced (Item);
      pragma Warnings (On);
   begin
      return Result_Ü;
   end To_Any;

   ------------------------
   -- Raise_MustTruncate --
   ------------------------

   procedure Raise_MustTruncate
     (Members : DynamicAny.MustTruncate_Members)
   is
   begin
      PolyORB.Exceptions.User_Raise_Exception
        (MustTruncate'Identity,
         Members);
   end Raise_MustTruncate;

   -----------------------------
   -- Deferred_Initialization --
   -----------------------------

   procedure Deferred_Initialization is
   begin
      DynamicAny.Helper.Internals.Initialize_DynAny;
      DynamicAny.Helper.Internals.Initialize_FieldName;
      DynamicAny.Helper.Internals.Initialize_NameValuePair;
      DynamicAny.Helper.Internals.Initialize_IDL_SEQUENCE_DynamicAny_NameValuePair;
      DynamicAny.Helper.Internals.Initialize_NameValuePairSeq;
      DynamicAny.Helper.Internals.Initialize_NameDynAnyPair;
      DynamicAny.Helper.Internals.Initialize_IDL_SEQUENCE_DynamicAny_NameDynAnyPair;
      DynamicAny.Helper.Internals.Initialize_NameDynAnyPairSeq;
      DynamicAny.Helper.Internals.Initialize_IDL_SEQUENCE_any;
      DynamicAny.Helper.Internals.Initialize_AnySeq;
      DynamicAny.Helper.Internals.Initialize_IDL_SEQUENCE_DynamicAny_DynAny_Forward;
      DynamicAny.Helper.Internals.Initialize_DynAnySeq;
      DynamicAny.Helper.Internals.Initialize_MustTruncate;
   end Deferred_Initialization;

begin
   declare
      use PolyORB.Utils.Strings;
      use PolyORB.Utils.Strings.Lists;
   begin
      PolyORB.Initialization.Register_Module
        (PolyORB.Initialization.Module_Info'
           (Name => +"DynamicAny.Helper",
            Conflicts => PolyORB.Utils.Strings.Lists.Empty,
            Depends => +"any"
               & "corba"
               & "exceptions",
            Provides => PolyORB.Utils.Strings.Lists.Empty,
            Implicit => False,
            Init => Deferred_Initialization'Access,
            Shutdown => null));
   end;
end DynamicAny.Helper;

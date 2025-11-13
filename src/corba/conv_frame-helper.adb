pragma Style_Checks ("NM32766");
pragma Warnings (Off, "use of an anonymous access type allocator");
pragma Warnings (Off, "unnecessary with of ancestor");

---------------------------------------------------
--  This file has been generated automatically from
--  /Users/heathdorn/Documents/Playground/Agents/RefactorTeam/code_refactor/PolyORB/idls/Interop/CONV_FRAME.idl
--  by IAC (IDL to Ada Compiler) 20.0w (rev. 41a9b833).
---------------------------------------------------
--  NOTE: If you modify this file by hand, your
--  changes will be lost when you re-run the
--  IDL to Ada compiler.
---------------------------------------------------

with PolyORB.Std;
with Ada.Unchecked_Conversion;
with PolyORB.Utils.Unchecked_Deallocation;
with PolyORB.Utils.Strings;
with PolyORB.Utils.Strings.Lists;
with PolyORB.Initialization;

package body CONV_FRAME.Helper is

   
   package body Internals is

      ----------
      -- Wrap --
      ----------

      function Wrap
        (X : access CONV_FRAME.CodeSetId)
        return PolyORB.Any.Content'Class
      is
      begin
         return CORBA.Wrap
           (CORBA.Unsigned_Long
              (X.all)'Unrestricted_Access);
      end Wrap;

      CodeSetId_Initialized : PolyORB.Std.Boolean :=
        False;

      --------------------------
      -- Initialize_CodeSetId --
      --------------------------

      procedure Initialize_CodeSetId is
         Name_Ü : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("CodeSetId");
         Id_Ü : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("IDL:omg.org/CONV_FRAME/CodeSetId:1.0");
      begin
         if not CodeSetId_Initialized
         then
            CodeSetId_Initialized :=
              True;
            CONV_FRAME.Helper.TC_CodeSetId :=
              CORBA.TypeCode.Internals.Build_Alias_TC
                 (Name => Name_Ü,
                  Id => Id_Ü,
                  Parent => CORBA.TC_Unsigned_Long);
            CORBA.TypeCode.Internals.Disable_Ref_Counting
              (TC_CodeSetId);
            CORBA.TypeCode.Internals.Freeze
              (TC_CodeSetId);
         end if;
      end Initialize_CodeSetId;

      ----------------------------------------------------
      -- IDL_SEQUENCE_CONV_FRAME_CodeSetId_Element_Wrap --
      ----------------------------------------------------

      function IDL_SEQUENCE_CONV_FRAME_CodeSetId_Element_Wrap
        (X : access CONV_FRAME.CodeSetId)
        return PolyORB.Any.Content'Class
      is
         Overlay_Ü : aliased CORBA.Unsigned_Long;
         for Overlay_Ü'Address use X.all'Address;
         pragma Import (Ada, Overlay_Ü);
      begin
         return CORBA.Wrap
           (Overlay_Ü'Unchecked_Access);
      end IDL_SEQUENCE_CONV_FRAME_CodeSetId_Element_Wrap;

      function Wrap
        (X : access CONV_FRAME.IDL_SEQUENCE_CONV_FRAME_CodeSetId.Sequence)
        return PolyORB.Any.Content'Class
        renames IDL_SEQUENCE_CONV_FRAME_CodeSetId_Helper.Wrap;

      IDL_SEQUENCE_CONV_FRAME_CodeSetId_Initialized : PolyORB.Std.Boolean :=
        False;

      --------------------------------------------------
      -- Initialize_IDL_SEQUENCE_CONV_FRAME_CodeSetId --
      --------------------------------------------------

      procedure Initialize_IDL_SEQUENCE_CONV_FRAME_CodeSetId is
      begin
         if not IDL_SEQUENCE_CONV_FRAME_CodeSetId_Initialized
         then
            IDL_SEQUENCE_CONV_FRAME_CodeSetId_Initialized :=
              True;
            CONV_FRAME.Helper.Internals.Initialize_CodeSetId;
            CONV_FRAME.Helper.TC_IDL_SEQUENCE_CONV_FRAME_CodeSetId :=
              CORBA.TypeCode.Internals.Build_Sequence_TC
                 (CONV_FRAME.Helper.TC_CodeSetId,
                  0);
            CORBA.TypeCode.Internals.Disable_Ref_Counting
              (CONV_FRAME.Helper.TC_IDL_SEQUENCE_CONV_FRAME_CodeSetId);
            IDL_SEQUENCE_CONV_FRAME_CodeSetId_Helper.Initialize
              (Element_TC => CONV_FRAME.Helper.TC_CodeSetId,
               Sequence_TC => CONV_FRAME.Helper.TC_IDL_SEQUENCE_CONV_FRAME_CodeSetId);
            CORBA.TypeCode.Internals.Freeze
              (TC_IDL_SEQUENCE_CONV_FRAME_CodeSetId);
         end if;
      end Initialize_IDL_SEQUENCE_CONV_FRAME_CodeSetId;

      CodeSetIdSeq_Initialized : PolyORB.Std.Boolean :=
        False;

      -----------------------------
      -- Initialize_CodeSetIdSeq --
      -----------------------------

      procedure Initialize_CodeSetIdSeq is
         Name_Ü : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("CodeSetIdSeq");
         Id_Ü : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("IDL:omg.org/CONV_FRAME/CodeSetIdSeq:1.0");
      begin
         if not CodeSetIdSeq_Initialized
         then
            CodeSetIdSeq_Initialized :=
              True;
            CONV_FRAME.Helper.Internals.Initialize_IDL_SEQUENCE_CONV_FRAME_CodeSetId;
            CONV_FRAME.Helper.TC_CodeSetIdSeq :=
              CORBA.TypeCode.Internals.Build_Alias_TC
                 (Name => Name_Ü,
                  Id => Id_Ü,
                  Parent => CONV_FRAME.Helper.TC_IDL_SEQUENCE_CONV_FRAME_CodeSetId);
            CORBA.TypeCode.Internals.Disable_Ref_Counting
              (TC_CodeSetIdSeq);
            CORBA.TypeCode.Internals.Freeze
              (TC_CodeSetIdSeq);
         end if;
      end Initialize_CodeSetIdSeq;

      ---------------------------
      -- Get_Aggregate_Element --
      ---------------------------

      function Get_Aggregate_Element
        (Acc : not null access Content_Ü_CodeSetComponent;
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
                 (CORBA.Unsigned_Long
                    (Acc.V.native_code_set)'Unrestricted_Access);
            when 1 =>
               return CONV_FRAME.Helper.Internals.Wrap
                 (CONV_FRAME.IDL_SEQUENCE_CONV_FRAME_CodeSetId.Sequence
                    (Acc.V.conversion_code_sets)'Unrestricted_Access);
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
        (Acc : Content_Ü_CodeSetComponent)
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
        (Acc : in out Content_Ü_CodeSetComponent;
         Count : PolyORB.Types.Unsigned_Long)
      is
      begin
         null;
      end Set_Aggregate_Count;

      ---------------------
      -- Unchecked_Get_V --
      ---------------------

      function Unchecked_Get_V
        (Acc : not null access Content_Ü_CodeSetComponent)
        return PolyORB.Types.Address
      is
         function To_Address
           is new Ada.Unchecked_Conversion
              (Ptr_Ü_CodeSetComponent,
               PolyORB.Types.Address);

      begin
         return To_Address
           (Acc.V);
      end Unchecked_Get_V;

      -----------
      -- Clone --
      -----------

      function Clone
        (Acc : Content_Ü_CodeSetComponent;
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
               not in Content_Ü_CodeSetComponent)
            then
               return null;
            end if;
            Target :=
              Into;
            Content_Ü_CodeSetComponent
              (Target.all).V.all :=
              Acc.V.all;
         else
            Target :=
              new Content_Ü_CodeSetComponent;
            Content_Ü_CodeSetComponent
              (Target.all).V :=
              new CONV_FRAME.CodeSetComponent'
                 (Acc.V.all);
         end if;
         return Target;
      end Clone;

      --------------------
      -- Finalize_Value --
      --------------------

      procedure Finalize_Value
        (Acc : in out Content_Ü_CodeSetComponent)
      is
         procedure Free
           is new PolyORB.Utils.Unchecked_Deallocation.Free

           (Object => CONV_FRAME.CodeSetComponent,

            Name   => Ptr_Ü_CodeSetComponent);

      begin
         Free
           (Acc.V);
      end Finalize_Value;

      ----------
      -- Wrap --
      ----------

      function Wrap
        (X : access CONV_FRAME.CodeSetComponent)
        return PolyORB.Any.Content'Class
      is
      begin
         return Content_Ü_CodeSetComponent'
           (PolyORB.Any.Aggregate_Content with
            V => Ptr_Ü_CodeSetComponent'
              (X.all'Unchecked_Access));
      end Wrap;

      CodeSetComponent_Initialized : PolyORB.Std.Boolean :=
        False;

      ---------------------------------
      -- Initialize_CodeSetComponent --
      ---------------------------------

      procedure Initialize_CodeSetComponent is
         Name_Ü : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("CodeSetComponent");
         Id_Ü : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("IDL:omg.org/CONV_FRAME/CodeSetComponent:1.0");
         Argument_Name_Ü_native_code_set : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("native_code_set");
         Argument_Name_Ü_conversion_code_sets : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("conversion_code_sets");
      begin
         if not CodeSetComponent_Initialized
         then
            CodeSetComponent_Initialized :=
              True;
            CONV_FRAME.Helper.TC_CodeSetComponent :=
              CORBA.TypeCode.Internals.To_CORBA_Object
                 (PolyORB.Any.TypeCode.Tcf_Struct);
            CORBA.Internals.Add_Parameter
              (TC_CodeSetComponent,
               CORBA.To_Any
                 (Name_Ü));
            CORBA.Internals.Add_Parameter
              (TC_CodeSetComponent,
               CORBA.To_Any
                 (Id_Ü));
            CONV_FRAME.Helper.Internals.Initialize_CodeSetId;
            CORBA.Internals.Add_Parameter
              (TC_CodeSetComponent,
               CORBA.To_Any
                 (CONV_FRAME.Helper.TC_CodeSetId));
            CORBA.Internals.Add_Parameter
              (TC_CodeSetComponent,
               CORBA.To_Any
                 (Argument_Name_Ü_native_code_set));
            CONV_FRAME.Helper.Internals.Initialize_CodeSetIdSeq;
            CORBA.Internals.Add_Parameter
              (TC_CodeSetComponent,
               CORBA.To_Any
                 (CONV_FRAME.Helper.TC_CodeSetIdSeq));
            CORBA.Internals.Add_Parameter
              (TC_CodeSetComponent,
               CORBA.To_Any
                 (Argument_Name_Ü_conversion_code_sets));
            CORBA.TypeCode.Internals.Disable_Ref_Counting
              (TC_CodeSetComponent);
            CORBA.TypeCode.Internals.Freeze
              (TC_CodeSetComponent);
         end if;
      end Initialize_CodeSetComponent;

      ---------------------------
      -- Get_Aggregate_Element --
      ---------------------------

      function Get_Aggregate_Element
        (Acc : not null access Content_Ü_CodeSetComponentInfo;
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
               return CONV_FRAME.Helper.Internals.Wrap
                 (Acc.V.ForCharData'Unrestricted_Access);
            when 1 =>
               return CONV_FRAME.Helper.Internals.Wrap
                 (Acc.V.ForWcharData'Unrestricted_Access);
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
        (Acc : Content_Ü_CodeSetComponentInfo)
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
        (Acc : in out Content_Ü_CodeSetComponentInfo;
         Count : PolyORB.Types.Unsigned_Long)
      is
      begin
         null;
      end Set_Aggregate_Count;

      ---------------------
      -- Unchecked_Get_V --
      ---------------------

      function Unchecked_Get_V
        (Acc : not null access Content_Ü_CodeSetComponentInfo)
        return PolyORB.Types.Address
      is
         function To_Address
           is new Ada.Unchecked_Conversion
              (Ptr_Ü_CodeSetComponentInfo,
               PolyORB.Types.Address);

      begin
         return To_Address
           (Acc.V);
      end Unchecked_Get_V;

      -----------
      -- Clone --
      -----------

      function Clone
        (Acc : Content_Ü_CodeSetComponentInfo;
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
               not in Content_Ü_CodeSetComponentInfo)
            then
               return null;
            end if;
            Target :=
              Into;
            Content_Ü_CodeSetComponentInfo
              (Target.all).V.all :=
              Acc.V.all;
         else
            Target :=
              new Content_Ü_CodeSetComponentInfo;
            Content_Ü_CodeSetComponentInfo
              (Target.all).V :=
              new CONV_FRAME.CodeSetComponentInfo'
                 (Acc.V.all);
         end if;
         return Target;
      end Clone;

      --------------------
      -- Finalize_Value --
      --------------------

      procedure Finalize_Value
        (Acc : in out Content_Ü_CodeSetComponentInfo)
      is
         procedure Free
           is new PolyORB.Utils.Unchecked_Deallocation.Free

           (Object => CONV_FRAME.CodeSetComponentInfo,

            Name   => Ptr_Ü_CodeSetComponentInfo);

      begin
         Free
           (Acc.V);
      end Finalize_Value;

      ----------
      -- Wrap --
      ----------

      function Wrap
        (X : access CONV_FRAME.CodeSetComponentInfo)
        return PolyORB.Any.Content'Class
      is
      begin
         return Content_Ü_CodeSetComponentInfo'
           (PolyORB.Any.Aggregate_Content with
            V => Ptr_Ü_CodeSetComponentInfo'
              (X.all'Unchecked_Access));
      end Wrap;

      CodeSetComponentInfo_Initialized : PolyORB.Std.Boolean :=
        False;

      -------------------------------------
      -- Initialize_CodeSetComponentInfo --
      -------------------------------------

      procedure Initialize_CodeSetComponentInfo is
         Name_Ü : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("CodeSetComponentInfo");
         Id_Ü : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("IDL:omg.org/CONV_FRAME/CodeSetComponentInfo:1.0");
         Argument_Name_Ü_ForCharData : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("ForCharData");
         Argument_Name_Ü_ForWcharData : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("ForWcharData");
      begin
         if not CodeSetComponentInfo_Initialized
         then
            CodeSetComponentInfo_Initialized :=
              True;
            CONV_FRAME.Helper.TC_CodeSetComponentInfo :=
              CORBA.TypeCode.Internals.To_CORBA_Object
                 (PolyORB.Any.TypeCode.Tcf_Struct);
            CORBA.Internals.Add_Parameter
              (TC_CodeSetComponentInfo,
               CORBA.To_Any
                 (Name_Ü));
            CORBA.Internals.Add_Parameter
              (TC_CodeSetComponentInfo,
               CORBA.To_Any
                 (Id_Ü));
            CONV_FRAME.Helper.Internals.Initialize_CodeSetComponent;
            CORBA.Internals.Add_Parameter
              (TC_CodeSetComponentInfo,
               CORBA.To_Any
                 (CONV_FRAME.Helper.TC_CodeSetComponent));
            CORBA.Internals.Add_Parameter
              (TC_CodeSetComponentInfo,
               CORBA.To_Any
                 (Argument_Name_Ü_ForCharData));
            CONV_FRAME.Helper.Internals.Initialize_CodeSetComponent;
            CORBA.Internals.Add_Parameter
              (TC_CodeSetComponentInfo,
               CORBA.To_Any
                 (CONV_FRAME.Helper.TC_CodeSetComponent));
            CORBA.Internals.Add_Parameter
              (TC_CodeSetComponentInfo,
               CORBA.To_Any
                 (Argument_Name_Ü_ForWcharData));
            CORBA.TypeCode.Internals.Disable_Ref_Counting
              (TC_CodeSetComponentInfo);
            CORBA.TypeCode.Internals.Freeze
              (TC_CodeSetComponentInfo);
         end if;
      end Initialize_CodeSetComponentInfo;

      ---------------------------
      -- Get_Aggregate_Element --
      ---------------------------

      function Get_Aggregate_Element
        (Acc : not null access Content_Ü_CodeSetContext;
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
                 (CORBA.Unsigned_Long
                    (Acc.V.char_data)'Unrestricted_Access);
            when 1 =>
               return CORBA.Wrap
                 (CORBA.Unsigned_Long
                    (Acc.V.wchar_data)'Unrestricted_Access);
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
        (Acc : Content_Ü_CodeSetContext)
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
        (Acc : in out Content_Ü_CodeSetContext;
         Count : PolyORB.Types.Unsigned_Long)
      is
      begin
         null;
      end Set_Aggregate_Count;

      ---------------------
      -- Unchecked_Get_V --
      ---------------------

      function Unchecked_Get_V
        (Acc : not null access Content_Ü_CodeSetContext)
        return PolyORB.Types.Address
      is
         function To_Address
           is new Ada.Unchecked_Conversion
              (Ptr_Ü_CodeSetContext,
               PolyORB.Types.Address);

      begin
         return To_Address
           (Acc.V);
      end Unchecked_Get_V;

      -----------
      -- Clone --
      -----------

      function Clone
        (Acc : Content_Ü_CodeSetContext;
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
               not in Content_Ü_CodeSetContext)
            then
               return null;
            end if;
            Target :=
              Into;
            Content_Ü_CodeSetContext
              (Target.all).V.all :=
              Acc.V.all;
         else
            Target :=
              new Content_Ü_CodeSetContext;
            Content_Ü_CodeSetContext
              (Target.all).V :=
              new CONV_FRAME.CodeSetContext'
                 (Acc.V.all);
         end if;
         return Target;
      end Clone;

      --------------------
      -- Finalize_Value --
      --------------------

      procedure Finalize_Value
        (Acc : in out Content_Ü_CodeSetContext)
      is
         procedure Free
           is new PolyORB.Utils.Unchecked_Deallocation.Free

           (Object => CONV_FRAME.CodeSetContext,

            Name   => Ptr_Ü_CodeSetContext);

      begin
         Free
           (Acc.V);
      end Finalize_Value;

      ----------
      -- Wrap --
      ----------

      function Wrap
        (X : access CONV_FRAME.CodeSetContext)
        return PolyORB.Any.Content'Class
      is
      begin
         return Content_Ü_CodeSetContext'
           (PolyORB.Any.Aggregate_Content with
            V => Ptr_Ü_CodeSetContext'
              (X.all'Unchecked_Access));
      end Wrap;

      CodeSetContext_Initialized : PolyORB.Std.Boolean :=
        False;

      -------------------------------
      -- Initialize_CodeSetContext --
      -------------------------------

      procedure Initialize_CodeSetContext is
         Name_Ü : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("CodeSetContext");
         Id_Ü : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("IDL:omg.org/CONV_FRAME/CodeSetContext:1.0");
         Argument_Name_Ü_char_data : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("char_data");
         Argument_Name_Ü_wchar_data : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("wchar_data");
      begin
         if not CodeSetContext_Initialized
         then
            CodeSetContext_Initialized :=
              True;
            CONV_FRAME.Helper.TC_CodeSetContext :=
              CORBA.TypeCode.Internals.To_CORBA_Object
                 (PolyORB.Any.TypeCode.Tcf_Struct);
            CORBA.Internals.Add_Parameter
              (TC_CodeSetContext,
               CORBA.To_Any
                 (Name_Ü));
            CORBA.Internals.Add_Parameter
              (TC_CodeSetContext,
               CORBA.To_Any
                 (Id_Ü));
            CONV_FRAME.Helper.Internals.Initialize_CodeSetId;
            CORBA.Internals.Add_Parameter
              (TC_CodeSetContext,
               CORBA.To_Any
                 (CONV_FRAME.Helper.TC_CodeSetId));
            CORBA.Internals.Add_Parameter
              (TC_CodeSetContext,
               CORBA.To_Any
                 (Argument_Name_Ü_char_data));
            CONV_FRAME.Helper.Internals.Initialize_CodeSetId;
            CORBA.Internals.Add_Parameter
              (TC_CodeSetContext,
               CORBA.To_Any
                 (CONV_FRAME.Helper.TC_CodeSetId));
            CORBA.Internals.Add_Parameter
              (TC_CodeSetContext,
               CORBA.To_Any
                 (Argument_Name_Ü_wchar_data));
            CORBA.TypeCode.Internals.Disable_Ref_Counting
              (TC_CodeSetContext);
            CORBA.TypeCode.Internals.Freeze
              (TC_CodeSetContext);
         end if;
      end Initialize_CodeSetContext;

   end Internals;

   --------------
   -- From_Any --
   --------------

   function From_Any
     (Item : CORBA.Any)
     return CONV_FRAME.CodeSetId
   is
      Result : constant CORBA.Unsigned_Long :=
        CORBA.From_Any
           (Item);
   begin
      return CONV_FRAME.CodeSetId
        (Result);
   end From_Any;

   ------------
   -- To_Any --
   ------------

   function To_Any
     (Item : CONV_FRAME.CodeSetId)
     return CORBA.Any
   is
      Result : CORBA.Any :=
        CORBA.To_Any
           (CORBA.Unsigned_Long
              (Item));
   begin
      CORBA.Internals.Set_Type
        (Result,
         TC_CodeSetId);
      return Result;
   end To_Any;

   function From_Any
     (Item : CORBA.Any)
     return CONV_FRAME.IDL_SEQUENCE_CONV_FRAME_CodeSetId.Sequence
     renames CONV_FRAME.Helper.Internals.IDL_SEQUENCE_CONV_FRAME_CodeSetId_Helper.From_Any;

   function To_Any
     (Item : CONV_FRAME.IDL_SEQUENCE_CONV_FRAME_CodeSetId.Sequence)
     return CORBA.Any
     renames CONV_FRAME.Helper.Internals.IDL_SEQUENCE_CONV_FRAME_CodeSetId_Helper.To_Any;

   --------------
   -- From_Any --
   --------------

   function From_Any
     (Item : CORBA.Any)
     return CONV_FRAME.CodeSetIdSeq
   is
      Result : constant CONV_FRAME.IDL_SEQUENCE_CONV_FRAME_CodeSetId.Sequence :=
        CONV_FRAME.Helper.From_Any
           (Item);
   begin
      return CONV_FRAME.CodeSetIdSeq
        (Result);
   end From_Any;

   ------------
   -- To_Any --
   ------------

   function To_Any
     (Item : CONV_FRAME.CodeSetIdSeq)
     return CORBA.Any
   is
      Result : CORBA.Any :=
        CONV_FRAME.Helper.To_Any
           (CONV_FRAME.IDL_SEQUENCE_CONV_FRAME_CodeSetId.Sequence
              (Item));
   begin
      CORBA.Internals.Set_Type
        (Result,
         TC_CodeSetIdSeq);
      return Result;
   end To_Any;

   --------------
   -- From_Any --
   --------------

   function From_Any
     (Item : CORBA.Any)
     return CONV_FRAME.CodeSetComponent
   is
   begin
      return (native_code_set => CONV_FRAME.Helper.From_Any
        (CORBA.Internals.Get_Aggregate_Element
           (Item,
            CONV_FRAME.Helper.TC_CodeSetId,
            0)),
      conversion_code_sets => CONV_FRAME.Helper.From_Any
        (CORBA.Internals.Get_Aggregate_Element
           (Item,
            CONV_FRAME.Helper.TC_CodeSetIdSeq,
            1)));
   end From_Any;

   ------------
   -- To_Any --
   ------------

   function To_Any
     (Item : CONV_FRAME.CodeSetComponent)
     return CORBA.Any
   is
      Result : constant CORBA.Any :=
        CORBA.Internals.Get_Empty_Any
           (TC_CodeSetComponent);
   begin
      PolyORB.Any.Set_Value
        (CORBA.Get_Container
           (Result).all,
         new PolyORB.Any.Content'Class'
           (CONV_FRAME.Helper.Internals.Wrap
              (new CONV_FRAME.CodeSetComponent'
                 (Item))),
         False);
      return Result;
   end To_Any;

   --------------
   -- From_Any --
   --------------

   function From_Any
     (Item : CORBA.Any)
     return CONV_FRAME.CodeSetComponentInfo
   is
   begin
      return (ForCharData => CONV_FRAME.Helper.From_Any
        (CORBA.Internals.Get_Aggregate_Element
           (Item,
            CONV_FRAME.Helper.TC_CodeSetComponent,
            0)),
      ForWcharData => CONV_FRAME.Helper.From_Any
        (CORBA.Internals.Get_Aggregate_Element
           (Item,
            CONV_FRAME.Helper.TC_CodeSetComponent,
            1)));
   end From_Any;

   ------------
   -- To_Any --
   ------------

   function To_Any
     (Item : CONV_FRAME.CodeSetComponentInfo)
     return CORBA.Any
   is
      Result : constant CORBA.Any :=
        CORBA.Internals.Get_Empty_Any
           (TC_CodeSetComponentInfo);
   begin
      PolyORB.Any.Set_Value
        (CORBA.Get_Container
           (Result).all,
         new PolyORB.Any.Content'Class'
           (CONV_FRAME.Helper.Internals.Wrap
              (new CONV_FRAME.CodeSetComponentInfo'
                 (Item))),
         False);
      return Result;
   end To_Any;

   --------------
   -- From_Any --
   --------------

   function From_Any
     (Item : CORBA.Any)
     return CONV_FRAME.CodeSetContext
   is
   begin
      return (char_data => CONV_FRAME.Helper.From_Any
        (CORBA.Internals.Get_Aggregate_Element
           (Item,
            CONV_FRAME.Helper.TC_CodeSetId,
            0)),
      wchar_data => CONV_FRAME.Helper.From_Any
        (CORBA.Internals.Get_Aggregate_Element
           (Item,
            CONV_FRAME.Helper.TC_CodeSetId,
            1)));
   end From_Any;

   ------------
   -- To_Any --
   ------------

   function To_Any
     (Item : CONV_FRAME.CodeSetContext)
     return CORBA.Any
   is
      Result : constant CORBA.Any :=
        CORBA.Internals.Get_Empty_Any
           (TC_CodeSetContext);
   begin
      PolyORB.Any.Set_Value
        (CORBA.Get_Container
           (Result).all,
         new PolyORB.Any.Content'Class'
           (CONV_FRAME.Helper.Internals.Wrap
              (new CONV_FRAME.CodeSetContext'
                 (Item))),
         False);
      return Result;
   end To_Any;

   -----------------------------
   -- Deferred_Initialization --
   -----------------------------

   procedure Deferred_Initialization is
   begin
      CONV_FRAME.Helper.Internals.Initialize_CodeSetId;
      CONV_FRAME.Helper.Internals.Initialize_IDL_SEQUENCE_CONV_FRAME_CodeSetId;
      CONV_FRAME.Helper.Internals.Initialize_CodeSetIdSeq;
      CONV_FRAME.Helper.Internals.Initialize_CodeSetComponent;
      CONV_FRAME.Helper.Internals.Initialize_CodeSetComponentInfo;
      CONV_FRAME.Helper.Internals.Initialize_CodeSetContext;
   end Deferred_Initialization;

begin
   declare
      use PolyORB.Utils.Strings;
      use PolyORB.Utils.Strings.Lists;
   begin
      PolyORB.Initialization.Register_Module
        (PolyORB.Initialization.Module_Info'
           (Name => +"CONV_FRAME.Helper",
            Conflicts => PolyORB.Utils.Strings.Lists.Empty,
            Depends => +"any"
               & "corba",
            Provides => PolyORB.Utils.Strings.Lists.Empty,
            Implicit => False,
            Init => Deferred_Initialization'Access,
            Shutdown => null));
   end;
end CONV_FRAME.Helper;

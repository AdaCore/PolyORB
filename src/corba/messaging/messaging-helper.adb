pragma Style_Checks ("NM32766");
pragma Warnings (Off, "use of an anonymous access type allocator");
pragma Warnings (Off, "unnecessary with of ancestor");

---------------------------------------------------
--  This file has been generated automatically from
--  /Users/heathdorn/Documents/Playground/Agents/RefactorTeam/code_refactor/PolyORB/idls/Misc/Messaging.idl
--  by IAC (IDL to Ada Compiler) 20.0w (rev. 41a9b833).
---------------------------------------------------
--  NOTE: If you modify this file by hand, your
--  changes will be lost when you re-run the
--  IDL to Ada compiler.
---------------------------------------------------

with PolyORB.Std;
with Ada.Unchecked_Conversion;
with PolyORB.Utils.Unchecked_Deallocation;
with CORBA.Helper;
with PolyORB.Utils.Strings;
with PolyORB.Utils.Strings.Lists;
with PolyORB.Initialization;

package body Messaging.Helper is

   
   package body Internals is

      ----------
      -- Wrap --
      ----------

      function Wrap
        (X : access Messaging.RebindMode)
        return PolyORB.Any.Content'Class
      is
      begin
         return CORBA.Wrap
           (CORBA.Short
              (X.all)'Unrestricted_Access);
      end Wrap;

      RebindMode_Initialized : PolyORB.Std.Boolean :=
        False;

      ---------------------------
      -- Initialize_RebindMode --
      ---------------------------

      procedure Initialize_RebindMode is
         Name_Ü : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("RebindMode");
         Id_Ü : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("IDL:omg.org/Messaging/RebindMode:1.0");
      begin
         if not RebindMode_Initialized
         then
            RebindMode_Initialized :=
              True;
            Messaging.Helper.TC_RebindMode :=
              CORBA.TypeCode.Internals.Build_Alias_TC
                 (Name => Name_Ü,
                  Id => Id_Ü,
                  Parent => CORBA.TC_Short);
            CORBA.TypeCode.Internals.Disable_Ref_Counting
              (TC_RebindMode);
            CORBA.TypeCode.Internals.Freeze
              (TC_RebindMode);
         end if;
      end Initialize_RebindMode;

      ----------
      -- Wrap --
      ----------

      function Wrap
        (X : access Messaging.SyncScope)
        return PolyORB.Any.Content'Class
      is
      begin
         return CORBA.Wrap
           (CORBA.Short
              (X.all)'Unrestricted_Access);
      end Wrap;

      SyncScope_Initialized : PolyORB.Std.Boolean :=
        False;

      --------------------------
      -- Initialize_SyncScope --
      --------------------------

      procedure Initialize_SyncScope is
         Name_Ü : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("SyncScope");
         Id_Ü : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("IDL:omg.org/Messaging/SyncScope:1.0");
      begin
         if not SyncScope_Initialized
         then
            SyncScope_Initialized :=
              True;
            Messaging.Helper.TC_SyncScope :=
              CORBA.TypeCode.Internals.Build_Alias_TC
                 (Name => Name_Ü,
                  Id => Id_Ü,
                  Parent => CORBA.TC_Short);
            CORBA.TypeCode.Internals.Disable_Ref_Counting
              (TC_SyncScope);
            CORBA.TypeCode.Internals.Freeze
              (TC_SyncScope);
         end if;
      end Initialize_SyncScope;

      ----------
      -- Wrap --
      ----------

      function Wrap
        (X : access Messaging.RoutingType)
        return PolyORB.Any.Content'Class
      is
      begin
         return CORBA.Wrap
           (CORBA.Short
              (X.all)'Unrestricted_Access);
      end Wrap;

      RoutingType_Initialized : PolyORB.Std.Boolean :=
        False;

      ----------------------------
      -- Initialize_RoutingType --
      ----------------------------

      procedure Initialize_RoutingType is
         Name_Ü : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("RoutingType");
         Id_Ü : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("IDL:omg.org/Messaging/RoutingType:1.0");
      begin
         if not RoutingType_Initialized
         then
            RoutingType_Initialized :=
              True;
            Messaging.Helper.TC_RoutingType :=
              CORBA.TypeCode.Internals.Build_Alias_TC
                 (Name => Name_Ü,
                  Id => Id_Ü,
                  Parent => CORBA.TC_Short);
            CORBA.TypeCode.Internals.Disable_Ref_Counting
              (TC_RoutingType);
            CORBA.TypeCode.Internals.Freeze
              (TC_RoutingType);
         end if;
      end Initialize_RoutingType;

      ----------
      -- Wrap --
      ----------

      function Wrap
        (X : access Messaging.Priority)
        return PolyORB.Any.Content'Class
      is
      begin
         return CORBA.Wrap
           (CORBA.Short
              (X.all)'Unrestricted_Access);
      end Wrap;

      Priority_Initialized : PolyORB.Std.Boolean :=
        False;

      -------------------------
      -- Initialize_Priority --
      -------------------------

      procedure Initialize_Priority is
         Name_Ü : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("Priority");
         Id_Ü : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("IDL:omg.org/Messaging/Priority:1.0");
      begin
         if not Priority_Initialized
         then
            Priority_Initialized :=
              True;
            Messaging.Helper.TC_Priority :=
              CORBA.TypeCode.Internals.Build_Alias_TC
                 (Name => Name_Ü,
                  Id => Id_Ü,
                  Parent => CORBA.TC_Short);
            CORBA.TypeCode.Internals.Disable_Ref_Counting
              (TC_Priority);
            CORBA.TypeCode.Internals.Freeze
              (TC_Priority);
         end if;
      end Initialize_Priority;

      ----------
      -- Wrap --
      ----------

      function Wrap
        (X : access Messaging.Ordering)
        return PolyORB.Any.Content'Class
      is
      begin
         return CORBA.Wrap
           (CORBA.Unsigned_Short
              (X.all)'Unrestricted_Access);
      end Wrap;

      Ordering_Initialized : PolyORB.Std.Boolean :=
        False;

      -------------------------
      -- Initialize_Ordering --
      -------------------------

      procedure Initialize_Ordering is
         Name_Ü : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("Ordering");
         Id_Ü : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("IDL:omg.org/Messaging/Ordering:1.0");
      begin
         if not Ordering_Initialized
         then
            Ordering_Initialized :=
              True;
            Messaging.Helper.TC_Ordering :=
              CORBA.TypeCode.Internals.Build_Alias_TC
                 (Name => Name_Ü,
                  Id => Id_Ü,
                  Parent => CORBA.TC_Unsigned_Short);
            CORBA.TypeCode.Internals.Disable_Ref_Counting
              (TC_Ordering);
            CORBA.TypeCode.Internals.Freeze
              (TC_Ordering);
         end if;
      end Initialize_Ordering;

      ---------------------------
      -- Get_Aggregate_Element --
      ---------------------------

      function Get_Aggregate_Element
        (Acc : not null access Content_Ü_PriorityRange;
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
                 (CORBA.Short
                    (Acc.V.min)'Unrestricted_Access);
            when 1 =>
               return CORBA.Wrap
                 (CORBA.Short
                    (Acc.V.max)'Unrestricted_Access);
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
        (Acc : Content_Ü_PriorityRange)
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
        (Acc : in out Content_Ü_PriorityRange;
         Count : PolyORB.Types.Unsigned_Long)
      is
      begin
         null;
      end Set_Aggregate_Count;

      ---------------------
      -- Unchecked_Get_V --
      ---------------------

      function Unchecked_Get_V
        (Acc : not null access Content_Ü_PriorityRange)
        return PolyORB.Types.Address
      is
         function To_Address
           is new Ada.Unchecked_Conversion
              (Ptr_Ü_PriorityRange,
               PolyORB.Types.Address);

      begin
         return To_Address
           (Acc.V);
      end Unchecked_Get_V;

      -----------
      -- Clone --
      -----------

      function Clone
        (Acc : Content_Ü_PriorityRange;
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
               not in Content_Ü_PriorityRange)
            then
               return null;
            end if;
            Target :=
              Into;
            Content_Ü_PriorityRange
              (Target.all).V.all :=
              Acc.V.all;
         else
            Target :=
              new Content_Ü_PriorityRange;
            Content_Ü_PriorityRange
              (Target.all).V :=
              new Messaging.PriorityRange'
                 (Acc.V.all);
         end if;
         return Target;
      end Clone;

      --------------------
      -- Finalize_Value --
      --------------------

      procedure Finalize_Value
        (Acc : in out Content_Ü_PriorityRange)
      is
         procedure Free
           is new PolyORB.Utils.Unchecked_Deallocation.Free

           (Object => Messaging.PriorityRange,

            Name   => Ptr_Ü_PriorityRange);

      begin
         Free
           (Acc.V);
      end Finalize_Value;

      ----------
      -- Wrap --
      ----------

      function Wrap
        (X : access Messaging.PriorityRange)
        return PolyORB.Any.Content'Class
      is
      begin
         return Content_Ü_PriorityRange'
           (PolyORB.Any.Aggregate_Content with
            V => Ptr_Ü_PriorityRange'
              (X.all'Unchecked_Access));
      end Wrap;

      PriorityRange_Initialized : PolyORB.Std.Boolean :=
        False;

      ------------------------------
      -- Initialize_PriorityRange --
      ------------------------------

      procedure Initialize_PriorityRange is
         Name_Ü : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("PriorityRange");
         Id_Ü : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("IDL:omg.org/Messaging/PriorityRange:1.0");
         Argument_Name_Ü_min : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("min");
         Argument_Name_Ü_max : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("max");
      begin
         if not PriorityRange_Initialized
         then
            PriorityRange_Initialized :=
              True;
            Messaging.Helper.TC_PriorityRange :=
              CORBA.TypeCode.Internals.To_CORBA_Object
                 (PolyORB.Any.TypeCode.Tcf_Struct);
            CORBA.Internals.Add_Parameter
              (TC_PriorityRange,
               CORBA.To_Any
                 (Name_Ü));
            CORBA.Internals.Add_Parameter
              (TC_PriorityRange,
               CORBA.To_Any
                 (Id_Ü));
            Messaging.Helper.Internals.Initialize_Priority;
            CORBA.Internals.Add_Parameter
              (TC_PriorityRange,
               CORBA.To_Any
                 (Messaging.Helper.TC_Priority));
            CORBA.Internals.Add_Parameter
              (TC_PriorityRange,
               CORBA.To_Any
                 (Argument_Name_Ü_min));
            Messaging.Helper.Internals.Initialize_Priority;
            CORBA.Internals.Add_Parameter
              (TC_PriorityRange,
               CORBA.To_Any
                 (Messaging.Helper.TC_Priority));
            CORBA.Internals.Add_Parameter
              (TC_PriorityRange,
               CORBA.To_Any
                 (Argument_Name_Ü_max));
            CORBA.TypeCode.Internals.Disable_Ref_Counting
              (TC_PriorityRange);
            CORBA.TypeCode.Internals.Freeze
              (TC_PriorityRange);
         end if;
      end Initialize_PriorityRange;

      ---------------------------
      -- Get_Aggregate_Element --
      ---------------------------

      function Get_Aggregate_Element
        (Acc : not null access Content_Ü_RoutingTypeRange;
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
                 (CORBA.Short
                    (Acc.V.min)'Unrestricted_Access);
            when 1 =>
               return CORBA.Wrap
                 (CORBA.Short
                    (Acc.V.max)'Unrestricted_Access);
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
        (Acc : Content_Ü_RoutingTypeRange)
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
        (Acc : in out Content_Ü_RoutingTypeRange;
         Count : PolyORB.Types.Unsigned_Long)
      is
      begin
         null;
      end Set_Aggregate_Count;

      ---------------------
      -- Unchecked_Get_V --
      ---------------------

      function Unchecked_Get_V
        (Acc : not null access Content_Ü_RoutingTypeRange)
        return PolyORB.Types.Address
      is
         function To_Address
           is new Ada.Unchecked_Conversion
              (Ptr_Ü_RoutingTypeRange,
               PolyORB.Types.Address);

      begin
         return To_Address
           (Acc.V);
      end Unchecked_Get_V;

      -----------
      -- Clone --
      -----------

      function Clone
        (Acc : Content_Ü_RoutingTypeRange;
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
               not in Content_Ü_RoutingTypeRange)
            then
               return null;
            end if;
            Target :=
              Into;
            Content_Ü_RoutingTypeRange
              (Target.all).V.all :=
              Acc.V.all;
         else
            Target :=
              new Content_Ü_RoutingTypeRange;
            Content_Ü_RoutingTypeRange
              (Target.all).V :=
              new Messaging.RoutingTypeRange'
                 (Acc.V.all);
         end if;
         return Target;
      end Clone;

      --------------------
      -- Finalize_Value --
      --------------------

      procedure Finalize_Value
        (Acc : in out Content_Ü_RoutingTypeRange)
      is
         procedure Free
           is new PolyORB.Utils.Unchecked_Deallocation.Free

           (Object => Messaging.RoutingTypeRange,

            Name   => Ptr_Ü_RoutingTypeRange);

      begin
         Free
           (Acc.V);
      end Finalize_Value;

      ----------
      -- Wrap --
      ----------

      function Wrap
        (X : access Messaging.RoutingTypeRange)
        return PolyORB.Any.Content'Class
      is
      begin
         return Content_Ü_RoutingTypeRange'
           (PolyORB.Any.Aggregate_Content with
            V => Ptr_Ü_RoutingTypeRange'
              (X.all'Unchecked_Access));
      end Wrap;

      RoutingTypeRange_Initialized : PolyORB.Std.Boolean :=
        False;

      ---------------------------------
      -- Initialize_RoutingTypeRange --
      ---------------------------------

      procedure Initialize_RoutingTypeRange is
         Name_Ü : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("RoutingTypeRange");
         Id_Ü : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("IDL:omg.org/Messaging/RoutingTypeRange:1.0");
         Argument_Name_Ü_min : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("min");
         Argument_Name_Ü_max : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("max");
      begin
         if not RoutingTypeRange_Initialized
         then
            RoutingTypeRange_Initialized :=
              True;
            Messaging.Helper.TC_RoutingTypeRange :=
              CORBA.TypeCode.Internals.To_CORBA_Object
                 (PolyORB.Any.TypeCode.Tcf_Struct);
            CORBA.Internals.Add_Parameter
              (TC_RoutingTypeRange,
               CORBA.To_Any
                 (Name_Ü));
            CORBA.Internals.Add_Parameter
              (TC_RoutingTypeRange,
               CORBA.To_Any
                 (Id_Ü));
            Messaging.Helper.Internals.Initialize_RoutingType;
            CORBA.Internals.Add_Parameter
              (TC_RoutingTypeRange,
               CORBA.To_Any
                 (Messaging.Helper.TC_RoutingType));
            CORBA.Internals.Add_Parameter
              (TC_RoutingTypeRange,
               CORBA.To_Any
                 (Argument_Name_Ü_min));
            Messaging.Helper.Internals.Initialize_RoutingType;
            CORBA.Internals.Add_Parameter
              (TC_RoutingTypeRange,
               CORBA.To_Any
                 (Messaging.Helper.TC_RoutingType));
            CORBA.Internals.Add_Parameter
              (TC_RoutingTypeRange,
               CORBA.To_Any
                 (Argument_Name_Ü_max));
            CORBA.TypeCode.Internals.Disable_Ref_Counting
              (TC_RoutingTypeRange);
            CORBA.TypeCode.Internals.Freeze
              (TC_RoutingTypeRange);
         end if;
      end Initialize_RoutingTypeRange;

      -------------------------------------
      -- IDL_SEQUENCE_octet_Element_Wrap --
      -------------------------------------

      function IDL_SEQUENCE_octet_Element_Wrap
        (X : access CORBA.Octet)
        return PolyORB.Any.Content'Class
      is
      begin
         return CORBA.Wrap
           (X.all'Unrestricted_Access);
      end IDL_SEQUENCE_octet_Element_Wrap;

      function Wrap
        (X : access Messaging.IDL_SEQUENCE_octet.Sequence)
        return PolyORB.Any.Content'Class
        renames IDL_SEQUENCE_octet_Helper.Wrap;

      IDL_SEQUENCE_octet_Initialized : PolyORB.Std.Boolean :=
        False;

      -----------------------------------
      -- Initialize_IDL_SEQUENCE_octet --
      -----------------------------------

      procedure Initialize_IDL_SEQUENCE_octet is
      begin
         if not IDL_SEQUENCE_octet_Initialized
         then
            IDL_SEQUENCE_octet_Initialized :=
              True;
            Messaging.Helper.TC_IDL_SEQUENCE_octet :=
              CORBA.TypeCode.Internals.Build_Sequence_TC
                 (CORBA.TC_Octet,
                  0);
            CORBA.TypeCode.Internals.Disable_Ref_Counting
              (Messaging.Helper.TC_IDL_SEQUENCE_octet);
            IDL_SEQUENCE_octet_Helper.Initialize
              (Element_TC => CORBA.TC_Octet,
               Sequence_TC => Messaging.Helper.TC_IDL_SEQUENCE_octet);
            CORBA.TypeCode.Internals.Freeze
              (TC_IDL_SEQUENCE_octet);
         end if;
      end Initialize_IDL_SEQUENCE_octet;

      IDL_AT_Sequence_octet_Initialized : PolyORB.Std.Boolean :=
        False;

      --------------------------------------
      -- Initialize_IDL_AT_Sequence_octet --
      --------------------------------------

      procedure Initialize_IDL_AT_Sequence_octet is
         Name_Ü : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("IDL_AT_Sequence_octet");
         Id_Ü : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("IDL:omg.org/Messaging/IDL_AT_Sequence_octet:1.0");
      begin
         if not IDL_AT_Sequence_octet_Initialized
         then
            IDL_AT_Sequence_octet_Initialized :=
              True;
            Messaging.Helper.Internals.Initialize_IDL_SEQUENCE_octet;
            Messaging.Helper.TC_IDL_AT_Sequence_octet :=
              CORBA.TypeCode.Internals.Build_Alias_TC
                 (Name => Name_Ü,
                  Id => Id_Ü,
                  Parent => Messaging.Helper.TC_IDL_SEQUENCE_octet);
            CORBA.TypeCode.Internals.Disable_Ref_Counting
              (TC_IDL_AT_Sequence_octet);
            CORBA.TypeCode.Internals.Freeze
              (TC_IDL_AT_Sequence_octet);
         end if;
      end Initialize_IDL_AT_Sequence_octet;

      ---------------------------
      -- Get_Aggregate_Element --
      ---------------------------

      function Get_Aggregate_Element
        (Acc : not null access Content_Ü_PolicyValue;
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
                    (Acc.V.ptype)'Unrestricted_Access);
            when 1 =>
               return Messaging.Helper.Internals.Wrap
                 (Messaging.IDL_SEQUENCE_octet.Sequence
                    (Acc.V.pvalue)'Unrestricted_Access);
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
        (Acc : Content_Ü_PolicyValue)
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
        (Acc : in out Content_Ü_PolicyValue;
         Count : PolyORB.Types.Unsigned_Long)
      is
      begin
         null;
      end Set_Aggregate_Count;

      ---------------------
      -- Unchecked_Get_V --
      ---------------------

      function Unchecked_Get_V
        (Acc : not null access Content_Ü_PolicyValue)
        return PolyORB.Types.Address
      is
         function To_Address
           is new Ada.Unchecked_Conversion
              (Ptr_Ü_PolicyValue,
               PolyORB.Types.Address);

      begin
         return To_Address
           (Acc.V);
      end Unchecked_Get_V;

      -----------
      -- Clone --
      -----------

      function Clone
        (Acc : Content_Ü_PolicyValue;
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
               not in Content_Ü_PolicyValue)
            then
               return null;
            end if;
            Target :=
              Into;
            Content_Ü_PolicyValue
              (Target.all).V.all :=
              Acc.V.all;
         else
            Target :=
              new Content_Ü_PolicyValue;
            Content_Ü_PolicyValue
              (Target.all).V :=
              new Messaging.PolicyValue'
                 (Acc.V.all);
         end if;
         return Target;
      end Clone;

      --------------------
      -- Finalize_Value --
      --------------------

      procedure Finalize_Value
        (Acc : in out Content_Ü_PolicyValue)
      is
         procedure Free
           is new PolyORB.Utils.Unchecked_Deallocation.Free

           (Object => Messaging.PolicyValue,

            Name   => Ptr_Ü_PolicyValue);

      begin
         Free
           (Acc.V);
      end Finalize_Value;

      ----------
      -- Wrap --
      ----------

      function Wrap
        (X : access Messaging.PolicyValue)
        return PolyORB.Any.Content'Class
      is
      begin
         return Content_Ü_PolicyValue'
           (PolyORB.Any.Aggregate_Content with
            V => Ptr_Ü_PolicyValue'
              (X.all'Unchecked_Access));
      end Wrap;

      PolicyValue_Initialized : PolyORB.Std.Boolean :=
        False;

      ----------------------------
      -- Initialize_PolicyValue --
      ----------------------------

      procedure Initialize_PolicyValue is
         Name_Ü : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("PolicyValue");
         Id_Ü : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("IDL:omg.org/Messaging/PolicyValue:1.0");
         Argument_Name_Ü_ptype : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("ptype");
         Argument_Name_Ü_pvalue : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("pvalue");
      begin
         if not PolicyValue_Initialized
         then
            PolicyValue_Initialized :=
              True;
            Messaging.Helper.TC_PolicyValue :=
              CORBA.TypeCode.Internals.To_CORBA_Object
                 (PolyORB.Any.TypeCode.Tcf_Struct);
            CORBA.Internals.Add_Parameter
              (TC_PolicyValue,
               CORBA.To_Any
                 (Name_Ü));
            CORBA.Internals.Add_Parameter
              (TC_PolicyValue,
               CORBA.To_Any
                 (Id_Ü));
            CORBA.Internals.Add_Parameter
              (TC_PolicyValue,
               CORBA.To_Any
                 (CORBA.Helper.TC_PolicyType));
            CORBA.Internals.Add_Parameter
              (TC_PolicyValue,
               CORBA.To_Any
                 (Argument_Name_Ü_ptype));
            Messaging.Helper.Internals.Initialize_IDL_AT_Sequence_octet;
            CORBA.Internals.Add_Parameter
              (TC_PolicyValue,
               CORBA.To_Any
                 (Messaging.Helper.TC_IDL_AT_Sequence_octet));
            CORBA.Internals.Add_Parameter
              (TC_PolicyValue,
               CORBA.To_Any
                 (Argument_Name_Ü_pvalue));
            CORBA.TypeCode.Internals.Disable_Ref_Counting
              (TC_PolicyValue);
            CORBA.TypeCode.Internals.Freeze
              (TC_PolicyValue);
         end if;
      end Initialize_PolicyValue;

      -----------------------------------------------------
      -- IDL_SEQUENCE_Messaging_PolicyValue_Element_Wrap --
      -----------------------------------------------------

      function IDL_SEQUENCE_Messaging_PolicyValue_Element_Wrap
        (X : access Messaging.PolicyValue)
        return PolyORB.Any.Content'Class
      is
      begin
         return Messaging.Helper.Internals.Wrap
           (X.all'Unrestricted_Access);
      end IDL_SEQUENCE_Messaging_PolicyValue_Element_Wrap;

      function Wrap
        (X : access Messaging.IDL_SEQUENCE_Messaging_PolicyValue.Sequence)
        return PolyORB.Any.Content'Class
        renames IDL_SEQUENCE_Messaging_PolicyValue_Helper.Wrap;

      IDL_SEQUENCE_Messaging_PolicyValue_Initialized : PolyORB.Std.Boolean :=
        False;

      ---------------------------------------------------
      -- Initialize_IDL_SEQUENCE_Messaging_PolicyValue --
      ---------------------------------------------------

      procedure Initialize_IDL_SEQUENCE_Messaging_PolicyValue is
      begin
         if not IDL_SEQUENCE_Messaging_PolicyValue_Initialized
         then
            IDL_SEQUENCE_Messaging_PolicyValue_Initialized :=
              True;
            Messaging.Helper.Internals.Initialize_PolicyValue;
            Messaging.Helper.TC_IDL_SEQUENCE_Messaging_PolicyValue :=
              CORBA.TypeCode.Internals.Build_Sequence_TC
                 (Messaging.Helper.TC_PolicyValue,
                  0);
            CORBA.TypeCode.Internals.Disable_Ref_Counting
              (Messaging.Helper.TC_IDL_SEQUENCE_Messaging_PolicyValue);
            IDL_SEQUENCE_Messaging_PolicyValue_Helper.Initialize
              (Element_TC => Messaging.Helper.TC_PolicyValue,
               Sequence_TC => Messaging.Helper.TC_IDL_SEQUENCE_Messaging_PolicyValue);
            CORBA.TypeCode.Internals.Freeze
              (TC_IDL_SEQUENCE_Messaging_PolicyValue);
         end if;
      end Initialize_IDL_SEQUENCE_Messaging_PolicyValue;

      PolicyValueSeq_Initialized : PolyORB.Std.Boolean :=
        False;

      -------------------------------
      -- Initialize_PolicyValueSeq --
      -------------------------------

      procedure Initialize_PolicyValueSeq is
         Name_Ü : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("PolicyValueSeq");
         Id_Ü : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("IDL:omg.org/Messaging/PolicyValueSeq:1.0");
      begin
         if not PolicyValueSeq_Initialized
         then
            PolicyValueSeq_Initialized :=
              True;
            Messaging.Helper.Internals.Initialize_IDL_SEQUENCE_Messaging_PolicyValue;
            Messaging.Helper.TC_PolicyValueSeq :=
              CORBA.TypeCode.Internals.Build_Alias_TC
                 (Name => Name_Ü,
                  Id => Id_Ü,
                  Parent => Messaging.Helper.TC_IDL_SEQUENCE_Messaging_PolicyValue);
            CORBA.TypeCode.Internals.Disable_Ref_Counting
              (TC_PolicyValueSeq);
            CORBA.TypeCode.Internals.Freeze
              (TC_PolicyValueSeq);
         end if;
      end Initialize_PolicyValueSeq;

   end Internals;

   --------------
   -- From_Any --
   --------------

   function From_Any
     (Item : CORBA.Any)
     return Messaging.RebindMode
   is
      Result : constant CORBA.Short :=
        CORBA.From_Any
           (Item);
   begin
      return Messaging.RebindMode
        (Result);
   end From_Any;

   ------------
   -- To_Any --
   ------------

   function To_Any
     (Item : Messaging.RebindMode)
     return CORBA.Any
   is
      Result : CORBA.Any :=
        CORBA.To_Any
           (CORBA.Short
              (Item));
   begin
      CORBA.Internals.Set_Type
        (Result,
         TC_RebindMode);
      return Result;
   end To_Any;

   --------------
   -- From_Any --
   --------------

   function From_Any
     (Item : CORBA.Any)
     return Messaging.SyncScope
   is
      Result : constant CORBA.Short :=
        CORBA.From_Any
           (Item);
   begin
      return Messaging.SyncScope
        (Result);
   end From_Any;

   ------------
   -- To_Any --
   ------------

   function To_Any
     (Item : Messaging.SyncScope)
     return CORBA.Any
   is
      Result : CORBA.Any :=
        CORBA.To_Any
           (CORBA.Short
              (Item));
   begin
      CORBA.Internals.Set_Type
        (Result,
         TC_SyncScope);
      return Result;
   end To_Any;

   --------------
   -- From_Any --
   --------------

   function From_Any
     (Item : CORBA.Any)
     return Messaging.RoutingType
   is
      Result : constant CORBA.Short :=
        CORBA.From_Any
           (Item);
   begin
      return Messaging.RoutingType
        (Result);
   end From_Any;

   ------------
   -- To_Any --
   ------------

   function To_Any
     (Item : Messaging.RoutingType)
     return CORBA.Any
   is
      Result : CORBA.Any :=
        CORBA.To_Any
           (CORBA.Short
              (Item));
   begin
      CORBA.Internals.Set_Type
        (Result,
         TC_RoutingType);
      return Result;
   end To_Any;

   --------------
   -- From_Any --
   --------------

   function From_Any
     (Item : CORBA.Any)
     return Messaging.Priority
   is
      Result : constant CORBA.Short :=
        CORBA.From_Any
           (Item);
   begin
      return Messaging.Priority
        (Result);
   end From_Any;

   ------------
   -- To_Any --
   ------------

   function To_Any
     (Item : Messaging.Priority)
     return CORBA.Any
   is
      Result : CORBA.Any :=
        CORBA.To_Any
           (CORBA.Short
              (Item));
   begin
      CORBA.Internals.Set_Type
        (Result,
         TC_Priority);
      return Result;
   end To_Any;

   --------------
   -- From_Any --
   --------------

   function From_Any
     (Item : CORBA.Any)
     return Messaging.Ordering
   is
      Result : constant CORBA.Unsigned_Short :=
        CORBA.From_Any
           (Item);
   begin
      return Messaging.Ordering
        (Result);
   end From_Any;

   ------------
   -- To_Any --
   ------------

   function To_Any
     (Item : Messaging.Ordering)
     return CORBA.Any
   is
      Result : CORBA.Any :=
        CORBA.To_Any
           (CORBA.Unsigned_Short
              (Item));
   begin
      CORBA.Internals.Set_Type
        (Result,
         TC_Ordering);
      return Result;
   end To_Any;

   --------------
   -- From_Any --
   --------------

   function From_Any
     (Item : CORBA.Any)
     return Messaging.PriorityRange
   is
   begin
      return (min => Messaging.Helper.From_Any
        (CORBA.Internals.Get_Aggregate_Element
           (Item,
            Messaging.Helper.TC_Priority,
            0)),
      max => Messaging.Helper.From_Any
        (CORBA.Internals.Get_Aggregate_Element
           (Item,
            Messaging.Helper.TC_Priority,
            1)));
   end From_Any;

   ------------
   -- To_Any --
   ------------

   function To_Any
     (Item : Messaging.PriorityRange)
     return CORBA.Any
   is
      Result : constant CORBA.Any :=
        CORBA.Internals.Get_Empty_Any
           (TC_PriorityRange);
   begin
      PolyORB.Any.Set_Value
        (CORBA.Get_Container
           (Result).all,
         new PolyORB.Any.Content'Class'
           (Messaging.Helper.Internals.Wrap
              (new Messaging.PriorityRange'
                 (Item))),
         False);
      return Result;
   end To_Any;

   --------------
   -- From_Any --
   --------------

   function From_Any
     (Item : CORBA.Any)
     return Messaging.RoutingTypeRange
   is
   begin
      return (min => Messaging.Helper.From_Any
        (CORBA.Internals.Get_Aggregate_Element
           (Item,
            Messaging.Helper.TC_RoutingType,
            0)),
      max => Messaging.Helper.From_Any
        (CORBA.Internals.Get_Aggregate_Element
           (Item,
            Messaging.Helper.TC_RoutingType,
            1)));
   end From_Any;

   ------------
   -- To_Any --
   ------------

   function To_Any
     (Item : Messaging.RoutingTypeRange)
     return CORBA.Any
   is
      Result : constant CORBA.Any :=
        CORBA.Internals.Get_Empty_Any
           (TC_RoutingTypeRange);
   begin
      PolyORB.Any.Set_Value
        (CORBA.Get_Container
           (Result).all,
         new PolyORB.Any.Content'Class'
           (Messaging.Helper.Internals.Wrap
              (new Messaging.RoutingTypeRange'
                 (Item))),
         False);
      return Result;
   end To_Any;

   function From_Any
     (Item : CORBA.Any)
     return Messaging.IDL_SEQUENCE_octet.Sequence
     renames Messaging.Helper.Internals.IDL_SEQUENCE_octet_Helper.From_Any;

   function To_Any
     (Item : Messaging.IDL_SEQUENCE_octet.Sequence)
     return CORBA.Any
     renames Messaging.Helper.Internals.IDL_SEQUENCE_octet_Helper.To_Any;

   --------------
   -- From_Any --
   --------------

   function From_Any
     (Item : CORBA.Any)
     return Messaging.IDL_AT_Sequence_octet
   is
      Result : constant Messaging.IDL_SEQUENCE_octet.Sequence :=
        Messaging.Helper.From_Any
           (Item);
   begin
      return Messaging.IDL_AT_Sequence_octet
        (Result);
   end From_Any;

   ------------
   -- To_Any --
   ------------

   function To_Any
     (Item : Messaging.IDL_AT_Sequence_octet)
     return CORBA.Any
   is
      Result : CORBA.Any :=
        Messaging.Helper.To_Any
           (Messaging.IDL_SEQUENCE_octet.Sequence
              (Item));
   begin
      CORBA.Internals.Set_Type
        (Result,
         TC_IDL_AT_Sequence_octet);
      return Result;
   end To_Any;

   --------------
   -- From_Any --
   --------------

   function From_Any
     (Item : CORBA.Any)
     return Messaging.PolicyValue
   is
   begin
      return (ptype => CORBA.Helper.From_Any
        (CORBA.Internals.Get_Aggregate_Element
           (Item,
            CORBA.Helper.TC_PolicyType,
            0)),
      pvalue => Messaging.Helper.From_Any
        (CORBA.Internals.Get_Aggregate_Element
           (Item,
            Messaging.Helper.TC_IDL_AT_Sequence_octet,
            1)));
   end From_Any;

   ------------
   -- To_Any --
   ------------

   function To_Any
     (Item : Messaging.PolicyValue)
     return CORBA.Any
   is
      Result : constant CORBA.Any :=
        CORBA.Internals.Get_Empty_Any
           (TC_PolicyValue);
   begin
      PolyORB.Any.Set_Value
        (CORBA.Get_Container
           (Result).all,
         new PolyORB.Any.Content'Class'
           (Messaging.Helper.Internals.Wrap
              (new Messaging.PolicyValue'
                 (Item))),
         False);
      return Result;
   end To_Any;

   function From_Any
     (Item : CORBA.Any)
     return Messaging.IDL_SEQUENCE_Messaging_PolicyValue.Sequence
     renames Messaging.Helper.Internals.IDL_SEQUENCE_Messaging_PolicyValue_Helper.From_Any;

   function To_Any
     (Item : Messaging.IDL_SEQUENCE_Messaging_PolicyValue.Sequence)
     return CORBA.Any
     renames Messaging.Helper.Internals.IDL_SEQUENCE_Messaging_PolicyValue_Helper.To_Any;

   --------------
   -- From_Any --
   --------------

   function From_Any
     (Item : CORBA.Any)
     return Messaging.PolicyValueSeq
   is
      Result : constant Messaging.IDL_SEQUENCE_Messaging_PolicyValue.Sequence :=
        Messaging.Helper.From_Any
           (Item);
   begin
      return Messaging.PolicyValueSeq
        (Result);
   end From_Any;

   ------------
   -- To_Any --
   ------------

   function To_Any
     (Item : Messaging.PolicyValueSeq)
     return CORBA.Any
   is
      Result : CORBA.Any :=
        Messaging.Helper.To_Any
           (Messaging.IDL_SEQUENCE_Messaging_PolicyValue.Sequence
              (Item));
   begin
      CORBA.Internals.Set_Type
        (Result,
         TC_PolicyValueSeq);
      return Result;
   end To_Any;

   -----------------------------
   -- Deferred_Initialization --
   -----------------------------

   procedure Deferred_Initialization is
   begin
      Messaging.Helper.Internals.Initialize_RebindMode;
      Messaging.Helper.Internals.Initialize_SyncScope;
      Messaging.Helper.Internals.Initialize_RoutingType;
      Messaging.Helper.Internals.Initialize_Priority;
      Messaging.Helper.Internals.Initialize_Ordering;
      Messaging.Helper.Internals.Initialize_PriorityRange;
      Messaging.Helper.Internals.Initialize_RoutingTypeRange;
      Messaging.Helper.Internals.Initialize_IDL_SEQUENCE_octet;
      Messaging.Helper.Internals.Initialize_IDL_AT_Sequence_octet;
      Messaging.Helper.Internals.Initialize_PolicyValue;
      Messaging.Helper.Internals.Initialize_IDL_SEQUENCE_Messaging_PolicyValue;
      Messaging.Helper.Internals.Initialize_PolicyValueSeq;
   end Deferred_Initialization;

begin
   declare
      use PolyORB.Utils.Strings;
      use PolyORB.Utils.Strings.Lists;
   begin
      PolyORB.Initialization.Register_Module
        (PolyORB.Initialization.Module_Info'
           (Name => +"Messaging.Helper",
            Conflicts => PolyORB.Utils.Strings.Lists.Empty,
            Depends => +"any"
               & "corba"
               & "corba.helper",
            Provides => PolyORB.Utils.Strings.Lists.Empty,
            Implicit => False,
            Init => Deferred_Initialization'Access,
            Shutdown => null));
   end;
end Messaging.Helper;

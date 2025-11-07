pragma Style_Checks ("NM32766");
pragma Warnings (Off, "use of an anonymous access type allocator");
pragma Warnings (Off, "unnecessary with of ancestor");

---------------------------------------------------
--  This file has been generated automatically from
--  /Users/heathdorn/Documents/Playground/Agents/RefactorTeam/code_refactor/PolyORB/idls/Interop/CSIIOP.idl
--  by IAC (IDL to Ada Compiler) 20.0w (rev. 41a9b833).
---------------------------------------------------
--  NOTE: If you modify this file by hand, your
--  changes will be lost when you re-run the
--  IDL to Ada compiler.
---------------------------------------------------

with PolyORB.Std;
with Ada.Unchecked_Conversion;
with PolyORB.Utils.Unchecked_Deallocation;
with CSI.Helper;
with CSI;
with IOP.Helper;
with PolyORB.Utils.Strings;
with PolyORB.Utils.Strings.Lists;
with PolyORB.Initialization;

package body CSIIOP.Helper is

   
   package body Internals is

      ----------
      -- Wrap --
      ----------

      function Wrap
        (X : access CSIIOP.AssociationOptions)
        return PolyORB.Any.Content'Class
      is
      begin
         return CORBA.Wrap
           (CORBA.Unsigned_Short
              (X.all)'Unrestricted_Access);
      end Wrap;

      AssociationOptions_Initialized : PolyORB.Std.Boolean :=
        False;

      -----------------------------------
      -- Initialize_AssociationOptions --
      -----------------------------------

      procedure Initialize_AssociationOptions is
         Name_Ü : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("AssociationOptions");
         Id_Ü : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("IDL:omg.org/CSIIOP/AssociationOptions:1.0");
      begin
         if not AssociationOptions_Initialized
         then
            AssociationOptions_Initialized :=
              True;
            CSIIOP.Helper.TC_AssociationOptions :=
              CORBA.TypeCode.Internals.Build_Alias_TC
                 (Name => Name_Ü,
                  Id => Id_Ü,
                  Parent => CORBA.TC_Unsigned_Short);
            CORBA.TypeCode.Internals.Disable_Ref_Counting
              (TC_AssociationOptions);
            CORBA.TypeCode.Internals.Freeze
              (TC_AssociationOptions);
         end if;
      end Initialize_AssociationOptions;

      ----------
      -- Wrap --
      ----------

      function Wrap
        (X : access CSIIOP.ServiceConfigurationSyntax)
        return PolyORB.Any.Content'Class
      is
      begin
         return CORBA.Wrap
           (CORBA.Unsigned_Long
              (X.all)'Unrestricted_Access);
      end Wrap;

      ServiceConfigurationSyntax_Initialized : PolyORB.Std.Boolean :=
        False;

      -------------------------------------------
      -- Initialize_ServiceConfigurationSyntax --
      -------------------------------------------

      procedure Initialize_ServiceConfigurationSyntax is
         Name_Ü : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("ServiceConfigurationSyntax");
         Id_Ü : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("IDL:omg.org/CSIIOP/ServiceConfigurationSyntax:1.0");
      begin
         if not ServiceConfigurationSyntax_Initialized
         then
            ServiceConfigurationSyntax_Initialized :=
              True;
            CSIIOP.Helper.TC_ServiceConfigurationSyntax :=
              CORBA.TypeCode.Internals.Build_Alias_TC
                 (Name => Name_Ü,
                  Id => Id_Ü,
                  Parent => CORBA.TC_Unsigned_Long);
            CORBA.TypeCode.Internals.Disable_Ref_Counting
              (TC_ServiceConfigurationSyntax);
            CORBA.TypeCode.Internals.Freeze
              (TC_ServiceConfigurationSyntax);
         end if;
      end Initialize_ServiceConfigurationSyntax;

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
        (X : access CSIIOP.IDL_SEQUENCE_octet.Sequence)
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
            CSIIOP.Helper.TC_IDL_SEQUENCE_octet :=
              CORBA.TypeCode.Internals.Build_Sequence_TC
                 (CORBA.TC_Octet,
                  0);
            CORBA.TypeCode.Internals.Disable_Ref_Counting
              (CSIIOP.Helper.TC_IDL_SEQUENCE_octet);
            IDL_SEQUENCE_octet_Helper.Initialize
              (Element_TC => CORBA.TC_Octet,
               Sequence_TC => CSIIOP.Helper.TC_IDL_SEQUENCE_octet);
            CORBA.TypeCode.Internals.Freeze
              (TC_IDL_SEQUENCE_octet);
         end if;
      end Initialize_IDL_SEQUENCE_octet;

      ServiceSpecificName_Initialized : PolyORB.Std.Boolean :=
        False;

      ------------------------------------
      -- Initialize_ServiceSpecificName --
      ------------------------------------

      procedure Initialize_ServiceSpecificName is
         Name_Ü : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("ServiceSpecificName");
         Id_Ü : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("IDL:omg.org/CSIIOP/ServiceSpecificName:1.0");
      begin
         if not ServiceSpecificName_Initialized
         then
            ServiceSpecificName_Initialized :=
              True;
            CSIIOP.Helper.Internals.Initialize_IDL_SEQUENCE_octet;
            CSIIOP.Helper.TC_ServiceSpecificName :=
              CORBA.TypeCode.Internals.Build_Alias_TC
                 (Name => Name_Ü,
                  Id => Id_Ü,
                  Parent => CSIIOP.Helper.TC_IDL_SEQUENCE_octet);
            CORBA.TypeCode.Internals.Disable_Ref_Counting
              (TC_ServiceSpecificName);
            CORBA.TypeCode.Internals.Freeze
              (TC_ServiceSpecificName);
         end if;
      end Initialize_ServiceSpecificName;

      ---------------------------
      -- Get_Aggregate_Element --
      ---------------------------

      function Get_Aggregate_Element
        (Acc : not null access Content_Ü_ServiceConfiguration;
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
                    (Acc.V.syntax)'Unrestricted_Access);
            when 1 =>
               return CSIIOP.Helper.Internals.Wrap
                 (CSIIOP.IDL_SEQUENCE_octet.Sequence
                    (Acc.V.name)'Unrestricted_Access);
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
        (Acc : Content_Ü_ServiceConfiguration)
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
        (Acc : in out Content_Ü_ServiceConfiguration;
         Count : PolyORB.Types.Unsigned_Long)
      is
      begin
         null;
      end Set_Aggregate_Count;

      ---------------------
      -- Unchecked_Get_V --
      ---------------------

      function Unchecked_Get_V
        (Acc : not null access Content_Ü_ServiceConfiguration)
        return PolyORB.Types.Address
      is
         function To_Address
           is new Ada.Unchecked_Conversion
              (Ptr_Ü_ServiceConfiguration,
               PolyORB.Types.Address);

      begin
         return To_Address
           (Acc.V);
      end Unchecked_Get_V;

      -----------
      -- Clone --
      -----------

      function Clone
        (Acc : Content_Ü_ServiceConfiguration;
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
               not in Content_Ü_ServiceConfiguration)
            then
               return null;
            end if;
            Target :=
              Into;
            Content_Ü_ServiceConfiguration
              (Target.all).V.all :=
              Acc.V.all;
         else
            Target :=
              new Content_Ü_ServiceConfiguration;
            Content_Ü_ServiceConfiguration
              (Target.all).V :=
              new CSIIOP.ServiceConfiguration'
                 (Acc.V.all);
         end if;
         return Target;
      end Clone;

      --------------------
      -- Finalize_Value --
      --------------------

      procedure Finalize_Value
        (Acc : in out Content_Ü_ServiceConfiguration)
      is
         procedure Free
           is new PolyORB.Utils.Unchecked_Deallocation.Free

           (Object => CSIIOP.ServiceConfiguration,

            Name   => Ptr_Ü_ServiceConfiguration);

      begin
         Free
           (Acc.V);
      end Finalize_Value;

      ----------
      -- Wrap --
      ----------

      function Wrap
        (X : access CSIIOP.ServiceConfiguration)
        return PolyORB.Any.Content'Class
      is
      begin
         return Content_Ü_ServiceConfiguration'
           (PolyORB.Any.Aggregate_Content with
            V => Ptr_Ü_ServiceConfiguration'
              (X.all'Unchecked_Access));
      end Wrap;

      ServiceConfiguration_Initialized : PolyORB.Std.Boolean :=
        False;

      -------------------------------------
      -- Initialize_ServiceConfiguration --
      -------------------------------------

      procedure Initialize_ServiceConfiguration is
         Name_Ü : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("ServiceConfiguration");
         Id_Ü : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("IDL:omg.org/CSIIOP/ServiceConfiguration:1.0");
         Argument_Name_Ü_syntax : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("syntax");
         Argument_Name_Ü_name : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("name");
      begin
         if not ServiceConfiguration_Initialized
         then
            ServiceConfiguration_Initialized :=
              True;
            CSIIOP.Helper.TC_ServiceConfiguration :=
              CORBA.TypeCode.Internals.To_CORBA_Object
                 (PolyORB.Any.TypeCode.Tcf_Struct);
            CORBA.Internals.Add_Parameter
              (TC_ServiceConfiguration,
               CORBA.To_Any
                 (Name_Ü));
            CORBA.Internals.Add_Parameter
              (TC_ServiceConfiguration,
               CORBA.To_Any
                 (Id_Ü));
            CSIIOP.Helper.Internals.Initialize_ServiceConfigurationSyntax;
            CORBA.Internals.Add_Parameter
              (TC_ServiceConfiguration,
               CORBA.To_Any
                 (CSIIOP.Helper.TC_ServiceConfigurationSyntax));
            CORBA.Internals.Add_Parameter
              (TC_ServiceConfiguration,
               CORBA.To_Any
                 (Argument_Name_Ü_syntax));
            CSIIOP.Helper.Internals.Initialize_ServiceSpecificName;
            CORBA.Internals.Add_Parameter
              (TC_ServiceConfiguration,
               CORBA.To_Any
                 (CSIIOP.Helper.TC_ServiceSpecificName));
            CORBA.Internals.Add_Parameter
              (TC_ServiceConfiguration,
               CORBA.To_Any
                 (Argument_Name_Ü_name));
            CORBA.TypeCode.Internals.Disable_Ref_Counting
              (TC_ServiceConfiguration);
            CORBA.TypeCode.Internals.Freeze
              (TC_ServiceConfiguration);
         end if;
      end Initialize_ServiceConfiguration;

      -----------------------------------------------------------
      -- IDL_SEQUENCE_CSIIOP_ServiceConfiguration_Element_Wrap --
      -----------------------------------------------------------

      function IDL_SEQUENCE_CSIIOP_ServiceConfiguration_Element_Wrap
        (X : access CSIIOP.ServiceConfiguration)
        return PolyORB.Any.Content'Class
      is
      begin
         return CSIIOP.Helper.Internals.Wrap
           (X.all'Unrestricted_Access);
      end IDL_SEQUENCE_CSIIOP_ServiceConfiguration_Element_Wrap;

      function Wrap
        (X : access CSIIOP.IDL_SEQUENCE_CSIIOP_ServiceConfiguration.Sequence)
        return PolyORB.Any.Content'Class
        renames IDL_SEQUENCE_CSIIOP_ServiceConfiguration_Helper.Wrap;

      IDL_SEQUENCE_CSIIOP_ServiceConfiguration_Initialized : PolyORB.Std.Boolean :=
        False;

      ---------------------------------------------------------
      -- Initialize_IDL_SEQUENCE_CSIIOP_ServiceConfiguration --
      ---------------------------------------------------------

      procedure Initialize_IDL_SEQUENCE_CSIIOP_ServiceConfiguration is
      begin
         if not IDL_SEQUENCE_CSIIOP_ServiceConfiguration_Initialized
         then
            IDL_SEQUENCE_CSIIOP_ServiceConfiguration_Initialized :=
              True;
            CSIIOP.Helper.Internals.Initialize_ServiceConfiguration;
            CSIIOP.Helper.TC_IDL_SEQUENCE_CSIIOP_ServiceConfiguration :=
              CORBA.TypeCode.Internals.Build_Sequence_TC
                 (CSIIOP.Helper.TC_ServiceConfiguration,
                  0);
            CORBA.TypeCode.Internals.Disable_Ref_Counting
              (CSIIOP.Helper.TC_IDL_SEQUENCE_CSIIOP_ServiceConfiguration);
            IDL_SEQUENCE_CSIIOP_ServiceConfiguration_Helper.Initialize
              (Element_TC => CSIIOP.Helper.TC_ServiceConfiguration,
               Sequence_TC => CSIIOP.Helper.TC_IDL_SEQUENCE_CSIIOP_ServiceConfiguration);
            CORBA.TypeCode.Internals.Freeze
              (TC_IDL_SEQUENCE_CSIIOP_ServiceConfiguration);
         end if;
      end Initialize_IDL_SEQUENCE_CSIIOP_ServiceConfiguration;

      ServiceConfigurationList_Initialized : PolyORB.Std.Boolean :=
        False;

      -----------------------------------------
      -- Initialize_ServiceConfigurationList --
      -----------------------------------------

      procedure Initialize_ServiceConfigurationList is
         Name_Ü : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("ServiceConfigurationList");
         Id_Ü : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("IDL:omg.org/CSIIOP/ServiceConfigurationList:1.0");
      begin
         if not ServiceConfigurationList_Initialized
         then
            ServiceConfigurationList_Initialized :=
              True;
            CSIIOP.Helper.Internals.Initialize_IDL_SEQUENCE_CSIIOP_ServiceConfiguration;
            CSIIOP.Helper.TC_ServiceConfigurationList :=
              CORBA.TypeCode.Internals.Build_Alias_TC
                 (Name => Name_Ü,
                  Id => Id_Ü,
                  Parent => CSIIOP.Helper.TC_IDL_SEQUENCE_CSIIOP_ServiceConfiguration);
            CORBA.TypeCode.Internals.Disable_Ref_Counting
              (TC_ServiceConfigurationList);
            CORBA.TypeCode.Internals.Freeze
              (TC_ServiceConfigurationList);
         end if;
      end Initialize_ServiceConfigurationList;

      ---------------------------
      -- Get_Aggregate_Element --
      ---------------------------

      function Get_Aggregate_Element
        (Acc : not null access Content_Ü_AS_ContextSec;
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
                 (CORBA.Unsigned_Short
                    (Acc.V.target_supports)'Unrestricted_Access);
            when 1 =>
               return CORBA.Wrap
                 (CORBA.Unsigned_Short
                    (Acc.V.target_requires)'Unrestricted_Access);
            when 2 =>
               return CSI.Helper.Internals.Wrap
                 (CSI.IDL_SEQUENCE_octet_3.Sequence
                    (Acc.V.client_authentication_mech)'Unrestricted_Access);
            when 3 =>
               return CSI.Helper.Internals.Wrap
                 (CSI.IDL_SEQUENCE_octet_5.Sequence
                    (Acc.V.target_name)'Unrestricted_Access);
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
        (Acc : Content_Ü_AS_ContextSec)
        return PolyORB.Types.Unsigned_Long
      is
         pragma Unreferenced (Acc);
      begin
         return 4;
      end Get_Aggregate_Count;

      -------------------------
      -- Set_Aggregate_Count --
      -------------------------

      procedure Set_Aggregate_Count
        (Acc : in out Content_Ü_AS_ContextSec;
         Count : PolyORB.Types.Unsigned_Long)
      is
      begin
         null;
      end Set_Aggregate_Count;

      ---------------------
      -- Unchecked_Get_V --
      ---------------------

      function Unchecked_Get_V
        (Acc : not null access Content_Ü_AS_ContextSec)
        return PolyORB.Types.Address
      is
         function To_Address
           is new Ada.Unchecked_Conversion
              (Ptr_Ü_AS_ContextSec,
               PolyORB.Types.Address);

      begin
         return To_Address
           (Acc.V);
      end Unchecked_Get_V;

      -----------
      -- Clone --
      -----------

      function Clone
        (Acc : Content_Ü_AS_ContextSec;
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
               not in Content_Ü_AS_ContextSec)
            then
               return null;
            end if;
            Target :=
              Into;
            Content_Ü_AS_ContextSec
              (Target.all).V.all :=
              Acc.V.all;
         else
            Target :=
              new Content_Ü_AS_ContextSec;
            Content_Ü_AS_ContextSec
              (Target.all).V :=
              new CSIIOP.AS_ContextSec'
                 (Acc.V.all);
         end if;
         return Target;
      end Clone;

      --------------------
      -- Finalize_Value --
      --------------------

      procedure Finalize_Value
        (Acc : in out Content_Ü_AS_ContextSec)
      is
         procedure Free
           is new PolyORB.Utils.Unchecked_Deallocation.Free

           (Object => CSIIOP.AS_ContextSec,

            Name   => Ptr_Ü_AS_ContextSec);

      begin
         Free
           (Acc.V);
      end Finalize_Value;

      ----------
      -- Wrap --
      ----------

      function Wrap
        (X : access CSIIOP.AS_ContextSec)
        return PolyORB.Any.Content'Class
      is
      begin
         return Content_Ü_AS_ContextSec'
           (PolyORB.Any.Aggregate_Content with
            V => Ptr_Ü_AS_ContextSec'
              (X.all'Unchecked_Access));
      end Wrap;

      AS_ContextSec_Initialized : PolyORB.Std.Boolean :=
        False;

      ------------------------------
      -- Initialize_AS_ContextSec --
      ------------------------------

      procedure Initialize_AS_ContextSec is
         Name_Ü : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("AS_ContextSec");
         Id_Ü : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("IDL:omg.org/CSIIOP/AS_ContextSec:1.0");
         Argument_Name_Ü_target_supports : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("target_supports");
         Argument_Name_Ü_target_requires : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("target_requires");
         Argument_Name_Ü_client_authentication_mech : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("client_authentication_mech");
         Argument_Name_Ü_target_name : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("target_name");
      begin
         if not AS_ContextSec_Initialized
         then
            AS_ContextSec_Initialized :=
              True;
            CSIIOP.Helper.TC_AS_ContextSec :=
              CORBA.TypeCode.Internals.To_CORBA_Object
                 (PolyORB.Any.TypeCode.Tcf_Struct);
            CORBA.Internals.Add_Parameter
              (TC_AS_ContextSec,
               CORBA.To_Any
                 (Name_Ü));
            CORBA.Internals.Add_Parameter
              (TC_AS_ContextSec,
               CORBA.To_Any
                 (Id_Ü));
            CSIIOP.Helper.Internals.Initialize_AssociationOptions;
            CORBA.Internals.Add_Parameter
              (TC_AS_ContextSec,
               CORBA.To_Any
                 (CSIIOP.Helper.TC_AssociationOptions));
            CORBA.Internals.Add_Parameter
              (TC_AS_ContextSec,
               CORBA.To_Any
                 (Argument_Name_Ü_target_supports));
            CSIIOP.Helper.Internals.Initialize_AssociationOptions;
            CORBA.Internals.Add_Parameter
              (TC_AS_ContextSec,
               CORBA.To_Any
                 (CSIIOP.Helper.TC_AssociationOptions));
            CORBA.Internals.Add_Parameter
              (TC_AS_ContextSec,
               CORBA.To_Any
                 (Argument_Name_Ü_target_requires));
            CSI.Helper.Internals.Initialize_OID;
            CORBA.Internals.Add_Parameter
              (TC_AS_ContextSec,
               CORBA.To_Any
                 (CSI.Helper.TC_OID));
            CORBA.Internals.Add_Parameter
              (TC_AS_ContextSec,
               CORBA.To_Any
                 (Argument_Name_Ü_client_authentication_mech));
            CSI.Helper.Internals.Initialize_GSS_NT_ExportedName;
            CORBA.Internals.Add_Parameter
              (TC_AS_ContextSec,
               CORBA.To_Any
                 (CSI.Helper.TC_GSS_NT_ExportedName));
            CORBA.Internals.Add_Parameter
              (TC_AS_ContextSec,
               CORBA.To_Any
                 (Argument_Name_Ü_target_name));
            CORBA.TypeCode.Internals.Disable_Ref_Counting
              (TC_AS_ContextSec);
            CORBA.TypeCode.Internals.Freeze
              (TC_AS_ContextSec);
         end if;
      end Initialize_AS_ContextSec;

      ---------------------------
      -- Get_Aggregate_Element --
      ---------------------------

      function Get_Aggregate_Element
        (Acc : not null access Content_Ü_SAS_ContextSec;
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
                 (CORBA.Unsigned_Short
                    (Acc.V.target_supports)'Unrestricted_Access);
            when 1 =>
               return CORBA.Wrap
                 (CORBA.Unsigned_Short
                    (Acc.V.target_requires)'Unrestricted_Access);
            when 2 =>
               return CSIIOP.Helper.Internals.Wrap
                 (CSIIOP.IDL_SEQUENCE_CSIIOP_ServiceConfiguration.Sequence
                    (Acc.V.privilege_authorities)'Unrestricted_Access);
            when 3 =>
               return CSI.Helper.Internals.Wrap
                 (CSI.IDL_SEQUENCE_CSI_OID.Sequence
                    (Acc.V.supported_naming_mechanisms)'Unrestricted_Access);
            when 4 =>
               return CORBA.Wrap
                 (CORBA.Unsigned_Long
                    (Acc.V.supported_identity_types)'Unrestricted_Access);
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
        (Acc : Content_Ü_SAS_ContextSec)
        return PolyORB.Types.Unsigned_Long
      is
         pragma Unreferenced (Acc);
      begin
         return 5;
      end Get_Aggregate_Count;

      -------------------------
      -- Set_Aggregate_Count --
      -------------------------

      procedure Set_Aggregate_Count
        (Acc : in out Content_Ü_SAS_ContextSec;
         Count : PolyORB.Types.Unsigned_Long)
      is
      begin
         null;
      end Set_Aggregate_Count;

      ---------------------
      -- Unchecked_Get_V --
      ---------------------

      function Unchecked_Get_V
        (Acc : not null access Content_Ü_SAS_ContextSec)
        return PolyORB.Types.Address
      is
         function To_Address
           is new Ada.Unchecked_Conversion
              (Ptr_Ü_SAS_ContextSec,
               PolyORB.Types.Address);

      begin
         return To_Address
           (Acc.V);
      end Unchecked_Get_V;

      -----------
      -- Clone --
      -----------

      function Clone
        (Acc : Content_Ü_SAS_ContextSec;
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
               not in Content_Ü_SAS_ContextSec)
            then
               return null;
            end if;
            Target :=
              Into;
            Content_Ü_SAS_ContextSec
              (Target.all).V.all :=
              Acc.V.all;
         else
            Target :=
              new Content_Ü_SAS_ContextSec;
            Content_Ü_SAS_ContextSec
              (Target.all).V :=
              new CSIIOP.SAS_ContextSec'
                 (Acc.V.all);
         end if;
         return Target;
      end Clone;

      --------------------
      -- Finalize_Value --
      --------------------

      procedure Finalize_Value
        (Acc : in out Content_Ü_SAS_ContextSec)
      is
         procedure Free
           is new PolyORB.Utils.Unchecked_Deallocation.Free

           (Object => CSIIOP.SAS_ContextSec,

            Name   => Ptr_Ü_SAS_ContextSec);

      begin
         Free
           (Acc.V);
      end Finalize_Value;

      ----------
      -- Wrap --
      ----------

      function Wrap
        (X : access CSIIOP.SAS_ContextSec)
        return PolyORB.Any.Content'Class
      is
      begin
         return Content_Ü_SAS_ContextSec'
           (PolyORB.Any.Aggregate_Content with
            V => Ptr_Ü_SAS_ContextSec'
              (X.all'Unchecked_Access));
      end Wrap;

      SAS_ContextSec_Initialized : PolyORB.Std.Boolean :=
        False;

      -------------------------------
      -- Initialize_SAS_ContextSec --
      -------------------------------

      procedure Initialize_SAS_ContextSec is
         Name_Ü : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("SAS_ContextSec");
         Id_Ü : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("IDL:omg.org/CSIIOP/SAS_ContextSec:1.0");
         Argument_Name_Ü_target_supports : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("target_supports");
         Argument_Name_Ü_target_requires : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("target_requires");
         Argument_Name_Ü_privilege_authorities : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("privilege_authorities");
         Argument_Name_Ü_supported_naming_mechanisms : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("supported_naming_mechanisms");
         Argument_Name_Ü_supported_identity_types : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("supported_identity_types");
      begin
         if not SAS_ContextSec_Initialized
         then
            SAS_ContextSec_Initialized :=
              True;
            CSIIOP.Helper.TC_SAS_ContextSec :=
              CORBA.TypeCode.Internals.To_CORBA_Object
                 (PolyORB.Any.TypeCode.Tcf_Struct);
            CORBA.Internals.Add_Parameter
              (TC_SAS_ContextSec,
               CORBA.To_Any
                 (Name_Ü));
            CORBA.Internals.Add_Parameter
              (TC_SAS_ContextSec,
               CORBA.To_Any
                 (Id_Ü));
            CSIIOP.Helper.Internals.Initialize_AssociationOptions;
            CORBA.Internals.Add_Parameter
              (TC_SAS_ContextSec,
               CORBA.To_Any
                 (CSIIOP.Helper.TC_AssociationOptions));
            CORBA.Internals.Add_Parameter
              (TC_SAS_ContextSec,
               CORBA.To_Any
                 (Argument_Name_Ü_target_supports));
            CSIIOP.Helper.Internals.Initialize_AssociationOptions;
            CORBA.Internals.Add_Parameter
              (TC_SAS_ContextSec,
               CORBA.To_Any
                 (CSIIOP.Helper.TC_AssociationOptions));
            CORBA.Internals.Add_Parameter
              (TC_SAS_ContextSec,
               CORBA.To_Any
                 (Argument_Name_Ü_target_requires));
            CSIIOP.Helper.Internals.Initialize_ServiceConfigurationList;
            CORBA.Internals.Add_Parameter
              (TC_SAS_ContextSec,
               CORBA.To_Any
                 (CSIIOP.Helper.TC_ServiceConfigurationList));
            CORBA.Internals.Add_Parameter
              (TC_SAS_ContextSec,
               CORBA.To_Any
                 (Argument_Name_Ü_privilege_authorities));
            CSI.Helper.Internals.Initialize_OIDList;
            CORBA.Internals.Add_Parameter
              (TC_SAS_ContextSec,
               CORBA.To_Any
                 (CSI.Helper.TC_OIDList));
            CORBA.Internals.Add_Parameter
              (TC_SAS_ContextSec,
               CORBA.To_Any
                 (Argument_Name_Ü_supported_naming_mechanisms));
            CSI.Helper.Internals.Initialize_IdentityTokenType;
            CORBA.Internals.Add_Parameter
              (TC_SAS_ContextSec,
               CORBA.To_Any
                 (CSI.Helper.TC_IdentityTokenType));
            CORBA.Internals.Add_Parameter
              (TC_SAS_ContextSec,
               CORBA.To_Any
                 (Argument_Name_Ü_supported_identity_types));
            CORBA.TypeCode.Internals.Disable_Ref_Counting
              (TC_SAS_ContextSec);
            CORBA.TypeCode.Internals.Freeze
              (TC_SAS_ContextSec);
         end if;
      end Initialize_SAS_ContextSec;

      ---------------------------
      -- Get_Aggregate_Element --
      ---------------------------

      function Get_Aggregate_Element
        (Acc : not null access Content_Ü_CompoundSecMech;
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
                 (CORBA.Unsigned_Short
                    (Acc.V.target_requires)'Unrestricted_Access);
            when 1 =>
               return IOP.Helper.Internals.Wrap
                 (Acc.V.transport_mech'Unrestricted_Access);
            when 2 =>
               return CSIIOP.Helper.Internals.Wrap
                 (Acc.V.as_context_mech'Unrestricted_Access);
            when 3 =>
               return CSIIOP.Helper.Internals.Wrap
                 (Acc.V.sas_context_mech'Unrestricted_Access);
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
        (Acc : Content_Ü_CompoundSecMech)
        return PolyORB.Types.Unsigned_Long
      is
         pragma Unreferenced (Acc);
      begin
         return 4;
      end Get_Aggregate_Count;

      -------------------------
      -- Set_Aggregate_Count --
      -------------------------

      procedure Set_Aggregate_Count
        (Acc : in out Content_Ü_CompoundSecMech;
         Count : PolyORB.Types.Unsigned_Long)
      is
      begin
         null;
      end Set_Aggregate_Count;

      ---------------------
      -- Unchecked_Get_V --
      ---------------------

      function Unchecked_Get_V
        (Acc : not null access Content_Ü_CompoundSecMech)
        return PolyORB.Types.Address
      is
         function To_Address
           is new Ada.Unchecked_Conversion
              (Ptr_Ü_CompoundSecMech,
               PolyORB.Types.Address);

      begin
         return To_Address
           (Acc.V);
      end Unchecked_Get_V;

      -----------
      -- Clone --
      -----------

      function Clone
        (Acc : Content_Ü_CompoundSecMech;
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
               not in Content_Ü_CompoundSecMech)
            then
               return null;
            end if;
            Target :=
              Into;
            Content_Ü_CompoundSecMech
              (Target.all).V.all :=
              Acc.V.all;
         else
            Target :=
              new Content_Ü_CompoundSecMech;
            Content_Ü_CompoundSecMech
              (Target.all).V :=
              new CSIIOP.CompoundSecMech'
                 (Acc.V.all);
         end if;
         return Target;
      end Clone;

      --------------------
      -- Finalize_Value --
      --------------------

      procedure Finalize_Value
        (Acc : in out Content_Ü_CompoundSecMech)
      is
         procedure Free
           is new PolyORB.Utils.Unchecked_Deallocation.Free

           (Object => CSIIOP.CompoundSecMech,

            Name   => Ptr_Ü_CompoundSecMech);

      begin
         Free
           (Acc.V);
      end Finalize_Value;

      ----------
      -- Wrap --
      ----------

      function Wrap
        (X : access CSIIOP.CompoundSecMech)
        return PolyORB.Any.Content'Class
      is
      begin
         return Content_Ü_CompoundSecMech'
           (PolyORB.Any.Aggregate_Content with
            V => Ptr_Ü_CompoundSecMech'
              (X.all'Unchecked_Access));
      end Wrap;

      CompoundSecMech_Initialized : PolyORB.Std.Boolean :=
        False;

      --------------------------------
      -- Initialize_CompoundSecMech --
      --------------------------------

      procedure Initialize_CompoundSecMech is
         Name_Ü : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("CompoundSecMech");
         Id_Ü : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("IDL:omg.org/CSIIOP/CompoundSecMech:1.0");
         Argument_Name_Ü_target_requires : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("target_requires");
         Argument_Name_Ü_transport_mech : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("transport_mech");
         Argument_Name_Ü_as_context_mech : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("as_context_mech");
         Argument_Name_Ü_sas_context_mech : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("sas_context_mech");
      begin
         if not CompoundSecMech_Initialized
         then
            CompoundSecMech_Initialized :=
              True;
            CSIIOP.Helper.TC_CompoundSecMech :=
              CORBA.TypeCode.Internals.To_CORBA_Object
                 (PolyORB.Any.TypeCode.Tcf_Struct);
            CORBA.Internals.Add_Parameter
              (TC_CompoundSecMech,
               CORBA.To_Any
                 (Name_Ü));
            CORBA.Internals.Add_Parameter
              (TC_CompoundSecMech,
               CORBA.To_Any
                 (Id_Ü));
            CSIIOP.Helper.Internals.Initialize_AssociationOptions;
            CORBA.Internals.Add_Parameter
              (TC_CompoundSecMech,
               CORBA.To_Any
                 (CSIIOP.Helper.TC_AssociationOptions));
            CORBA.Internals.Add_Parameter
              (TC_CompoundSecMech,
               CORBA.To_Any
                 (Argument_Name_Ü_target_requires));
            IOP.Helper.Internals.Initialize_TaggedComponent;
            CORBA.Internals.Add_Parameter
              (TC_CompoundSecMech,
               CORBA.To_Any
                 (IOP.Helper.TC_TaggedComponent));
            CORBA.Internals.Add_Parameter
              (TC_CompoundSecMech,
               CORBA.To_Any
                 (Argument_Name_Ü_transport_mech));
            CSIIOP.Helper.Internals.Initialize_AS_ContextSec;
            CORBA.Internals.Add_Parameter
              (TC_CompoundSecMech,
               CORBA.To_Any
                 (CSIIOP.Helper.TC_AS_ContextSec));
            CORBA.Internals.Add_Parameter
              (TC_CompoundSecMech,
               CORBA.To_Any
                 (Argument_Name_Ü_as_context_mech));
            CSIIOP.Helper.Internals.Initialize_SAS_ContextSec;
            CORBA.Internals.Add_Parameter
              (TC_CompoundSecMech,
               CORBA.To_Any
                 (CSIIOP.Helper.TC_SAS_ContextSec));
            CORBA.Internals.Add_Parameter
              (TC_CompoundSecMech,
               CORBA.To_Any
                 (Argument_Name_Ü_sas_context_mech));
            CORBA.TypeCode.Internals.Disable_Ref_Counting
              (TC_CompoundSecMech);
            CORBA.TypeCode.Internals.Freeze
              (TC_CompoundSecMech);
         end if;
      end Initialize_CompoundSecMech;

      ------------------------------------------------------
      -- IDL_SEQUENCE_CSIIOP_CompoundSecMech_Element_Wrap --
      ------------------------------------------------------

      function IDL_SEQUENCE_CSIIOP_CompoundSecMech_Element_Wrap
        (X : access CSIIOP.CompoundSecMech)
        return PolyORB.Any.Content'Class
      is
      begin
         return CSIIOP.Helper.Internals.Wrap
           (X.all'Unrestricted_Access);
      end IDL_SEQUENCE_CSIIOP_CompoundSecMech_Element_Wrap;

      function Wrap
        (X : access CSIIOP.IDL_SEQUENCE_CSIIOP_CompoundSecMech.Sequence)
        return PolyORB.Any.Content'Class
        renames IDL_SEQUENCE_CSIIOP_CompoundSecMech_Helper.Wrap;

      IDL_SEQUENCE_CSIIOP_CompoundSecMech_Initialized : PolyORB.Std.Boolean :=
        False;

      ----------------------------------------------------
      -- Initialize_IDL_SEQUENCE_CSIIOP_CompoundSecMech --
      ----------------------------------------------------

      procedure Initialize_IDL_SEQUENCE_CSIIOP_CompoundSecMech is
      begin
         if not IDL_SEQUENCE_CSIIOP_CompoundSecMech_Initialized
         then
            IDL_SEQUENCE_CSIIOP_CompoundSecMech_Initialized :=
              True;
            CSIIOP.Helper.Internals.Initialize_CompoundSecMech;
            CSIIOP.Helper.TC_IDL_SEQUENCE_CSIIOP_CompoundSecMech :=
              CORBA.TypeCode.Internals.Build_Sequence_TC
                 (CSIIOP.Helper.TC_CompoundSecMech,
                  0);
            CORBA.TypeCode.Internals.Disable_Ref_Counting
              (CSIIOP.Helper.TC_IDL_SEQUENCE_CSIIOP_CompoundSecMech);
            IDL_SEQUENCE_CSIIOP_CompoundSecMech_Helper.Initialize
              (Element_TC => CSIIOP.Helper.TC_CompoundSecMech,
               Sequence_TC => CSIIOP.Helper.TC_IDL_SEQUENCE_CSIIOP_CompoundSecMech);
            CORBA.TypeCode.Internals.Freeze
              (TC_IDL_SEQUENCE_CSIIOP_CompoundSecMech);
         end if;
      end Initialize_IDL_SEQUENCE_CSIIOP_CompoundSecMech;

      CompoundSecMechanisms_Initialized : PolyORB.Std.Boolean :=
        False;

      --------------------------------------
      -- Initialize_CompoundSecMechanisms --
      --------------------------------------

      procedure Initialize_CompoundSecMechanisms is
         Name_Ü : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("CompoundSecMechanisms");
         Id_Ü : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("IDL:omg.org/CSIIOP/CompoundSecMechanisms:1.0");
      begin
         if not CompoundSecMechanisms_Initialized
         then
            CompoundSecMechanisms_Initialized :=
              True;
            CSIIOP.Helper.Internals.Initialize_IDL_SEQUENCE_CSIIOP_CompoundSecMech;
            CSIIOP.Helper.TC_CompoundSecMechanisms :=
              CORBA.TypeCode.Internals.Build_Alias_TC
                 (Name => Name_Ü,
                  Id => Id_Ü,
                  Parent => CSIIOP.Helper.TC_IDL_SEQUENCE_CSIIOP_CompoundSecMech);
            CORBA.TypeCode.Internals.Disable_Ref_Counting
              (TC_CompoundSecMechanisms);
            CORBA.TypeCode.Internals.Freeze
              (TC_CompoundSecMechanisms);
         end if;
      end Initialize_CompoundSecMechanisms;

      ---------------------------
      -- Get_Aggregate_Element --
      ---------------------------

      function Get_Aggregate_Element
        (Acc : not null access Content_Ü_CompoundSecMechList;
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
                 (Acc.V.stateful'Unrestricted_Access);
            when 1 =>
               return CSIIOP.Helper.Internals.Wrap
                 (CSIIOP.IDL_SEQUENCE_CSIIOP_CompoundSecMech.Sequence
                    (Acc.V.mechanism_list)'Unrestricted_Access);
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
        (Acc : Content_Ü_CompoundSecMechList)
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
        (Acc : in out Content_Ü_CompoundSecMechList;
         Count : PolyORB.Types.Unsigned_Long)
      is
      begin
         null;
      end Set_Aggregate_Count;

      ---------------------
      -- Unchecked_Get_V --
      ---------------------

      function Unchecked_Get_V
        (Acc : not null access Content_Ü_CompoundSecMechList)
        return PolyORB.Types.Address
      is
         function To_Address
           is new Ada.Unchecked_Conversion
              (Ptr_Ü_CompoundSecMechList,
               PolyORB.Types.Address);

      begin
         return To_Address
           (Acc.V);
      end Unchecked_Get_V;

      -----------
      -- Clone --
      -----------

      function Clone
        (Acc : Content_Ü_CompoundSecMechList;
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
               not in Content_Ü_CompoundSecMechList)
            then
               return null;
            end if;
            Target :=
              Into;
            Content_Ü_CompoundSecMechList
              (Target.all).V.all :=
              Acc.V.all;
         else
            Target :=
              new Content_Ü_CompoundSecMechList;
            Content_Ü_CompoundSecMechList
              (Target.all).V :=
              new CSIIOP.CompoundSecMechList'
                 (Acc.V.all);
         end if;
         return Target;
      end Clone;

      --------------------
      -- Finalize_Value --
      --------------------

      procedure Finalize_Value
        (Acc : in out Content_Ü_CompoundSecMechList)
      is
         procedure Free
           is new PolyORB.Utils.Unchecked_Deallocation.Free

           (Object => CSIIOP.CompoundSecMechList,

            Name   => Ptr_Ü_CompoundSecMechList);

      begin
         Free
           (Acc.V);
      end Finalize_Value;

      ----------
      -- Wrap --
      ----------

      function Wrap
        (X : access CSIIOP.CompoundSecMechList)
        return PolyORB.Any.Content'Class
      is
      begin
         return Content_Ü_CompoundSecMechList'
           (PolyORB.Any.Aggregate_Content with
            V => Ptr_Ü_CompoundSecMechList'
              (X.all'Unchecked_Access));
      end Wrap;

      CompoundSecMechList_Initialized : PolyORB.Std.Boolean :=
        False;

      ------------------------------------
      -- Initialize_CompoundSecMechList --
      ------------------------------------

      procedure Initialize_CompoundSecMechList is
         Name_Ü : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("CompoundSecMechList");
         Id_Ü : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("IDL:omg.org/CSIIOP/CompoundSecMechList:1.0");
         Argument_Name_Ü_stateful : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("stateful");
         Argument_Name_Ü_mechanism_list : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("mechanism_list");
      begin
         if not CompoundSecMechList_Initialized
         then
            CompoundSecMechList_Initialized :=
              True;
            CSIIOP.Helper.TC_CompoundSecMechList :=
              CORBA.TypeCode.Internals.To_CORBA_Object
                 (PolyORB.Any.TypeCode.Tcf_Struct);
            CORBA.Internals.Add_Parameter
              (TC_CompoundSecMechList,
               CORBA.To_Any
                 (Name_Ü));
            CORBA.Internals.Add_Parameter
              (TC_CompoundSecMechList,
               CORBA.To_Any
                 (Id_Ü));
            CORBA.Internals.Add_Parameter
              (TC_CompoundSecMechList,
               CORBA.To_Any
                 (CORBA.TC_Boolean));
            CORBA.Internals.Add_Parameter
              (TC_CompoundSecMechList,
               CORBA.To_Any
                 (Argument_Name_Ü_stateful));
            CSIIOP.Helper.Internals.Initialize_CompoundSecMechanisms;
            CORBA.Internals.Add_Parameter
              (TC_CompoundSecMechList,
               CORBA.To_Any
                 (CSIIOP.Helper.TC_CompoundSecMechanisms));
            CORBA.Internals.Add_Parameter
              (TC_CompoundSecMechList,
               CORBA.To_Any
                 (Argument_Name_Ü_mechanism_list));
            CORBA.TypeCode.Internals.Disable_Ref_Counting
              (TC_CompoundSecMechList);
            CORBA.TypeCode.Internals.Freeze
              (TC_CompoundSecMechList);
         end if;
      end Initialize_CompoundSecMechList;

      ---------------------------
      -- Get_Aggregate_Element --
      ---------------------------

      function Get_Aggregate_Element
        (Acc : not null access Content_Ü_TransportAddress;
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
                 (Acc.V.host_name'Unrestricted_Access);
            when 1 =>
               return CORBA.Wrap
                 (Acc.V.port'Unrestricted_Access);
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
        (Acc : Content_Ü_TransportAddress)
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
        (Acc : in out Content_Ü_TransportAddress;
         Count : PolyORB.Types.Unsigned_Long)
      is
      begin
         null;
      end Set_Aggregate_Count;

      ---------------------
      -- Unchecked_Get_V --
      ---------------------

      function Unchecked_Get_V
        (Acc : not null access Content_Ü_TransportAddress)
        return PolyORB.Types.Address
      is
         function To_Address
           is new Ada.Unchecked_Conversion
              (Ptr_Ü_TransportAddress,
               PolyORB.Types.Address);

      begin
         return To_Address
           (Acc.V);
      end Unchecked_Get_V;

      -----------
      -- Clone --
      -----------

      function Clone
        (Acc : Content_Ü_TransportAddress;
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
               not in Content_Ü_TransportAddress)
            then
               return null;
            end if;
            Target :=
              Into;
            Content_Ü_TransportAddress
              (Target.all).V.all :=
              Acc.V.all;
         else
            Target :=
              new Content_Ü_TransportAddress;
            Content_Ü_TransportAddress
              (Target.all).V :=
              new CSIIOP.TransportAddress'
                 (Acc.V.all);
         end if;
         return Target;
      end Clone;

      --------------------
      -- Finalize_Value --
      --------------------

      procedure Finalize_Value
        (Acc : in out Content_Ü_TransportAddress)
      is
         procedure Free
           is new PolyORB.Utils.Unchecked_Deallocation.Free

           (Object => CSIIOP.TransportAddress,

            Name   => Ptr_Ü_TransportAddress);

      begin
         Free
           (Acc.V);
      end Finalize_Value;

      ----------
      -- Wrap --
      ----------

      function Wrap
        (X : access CSIIOP.TransportAddress)
        return PolyORB.Any.Content'Class
      is
      begin
         return Content_Ü_TransportAddress'
           (PolyORB.Any.Aggregate_Content with
            V => Ptr_Ü_TransportAddress'
              (X.all'Unchecked_Access));
      end Wrap;

      TransportAddress_Initialized : PolyORB.Std.Boolean :=
        False;

      ---------------------------------
      -- Initialize_TransportAddress --
      ---------------------------------

      procedure Initialize_TransportAddress is
         Name_Ü : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("TransportAddress");
         Id_Ü : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("IDL:omg.org/CSIIOP/TransportAddress:1.0");
         Argument_Name_Ü_host_name : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("host_name");
         Argument_Name_Ü_port : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("port");
      begin
         if not TransportAddress_Initialized
         then
            TransportAddress_Initialized :=
              True;
            CSIIOP.Helper.TC_TransportAddress :=
              CORBA.TypeCode.Internals.To_CORBA_Object
                 (PolyORB.Any.TypeCode.Tcf_Struct);
            CORBA.Internals.Add_Parameter
              (TC_TransportAddress,
               CORBA.To_Any
                 (Name_Ü));
            CORBA.Internals.Add_Parameter
              (TC_TransportAddress,
               CORBA.To_Any
                 (Id_Ü));
            CORBA.Internals.Add_Parameter
              (TC_TransportAddress,
               CORBA.To_Any
                 (CORBA.TC_String));
            CORBA.Internals.Add_Parameter
              (TC_TransportAddress,
               CORBA.To_Any
                 (Argument_Name_Ü_host_name));
            CORBA.Internals.Add_Parameter
              (TC_TransportAddress,
               CORBA.To_Any
                 (CORBA.TC_Unsigned_Short));
            CORBA.Internals.Add_Parameter
              (TC_TransportAddress,
               CORBA.To_Any
                 (Argument_Name_Ü_port));
            CORBA.TypeCode.Internals.Disable_Ref_Counting
              (TC_TransportAddress);
            CORBA.TypeCode.Internals.Freeze
              (TC_TransportAddress);
         end if;
      end Initialize_TransportAddress;

      -------------------------------------------------------
      -- IDL_SEQUENCE_CSIIOP_TransportAddress_Element_Wrap --
      -------------------------------------------------------

      function IDL_SEQUENCE_CSIIOP_TransportAddress_Element_Wrap
        (X : access CSIIOP.TransportAddress)
        return PolyORB.Any.Content'Class
      is
      begin
         return CSIIOP.Helper.Internals.Wrap
           (X.all'Unrestricted_Access);
      end IDL_SEQUENCE_CSIIOP_TransportAddress_Element_Wrap;

      function Wrap
        (X : access CSIIOP.IDL_SEQUENCE_CSIIOP_TransportAddress.Sequence)
        return PolyORB.Any.Content'Class
        renames IDL_SEQUENCE_CSIIOP_TransportAddress_Helper.Wrap;

      IDL_SEQUENCE_CSIIOP_TransportAddress_Initialized : PolyORB.Std.Boolean :=
        False;

      -----------------------------------------------------
      -- Initialize_IDL_SEQUENCE_CSIIOP_TransportAddress --
      -----------------------------------------------------

      procedure Initialize_IDL_SEQUENCE_CSIIOP_TransportAddress is
      begin
         if not IDL_SEQUENCE_CSIIOP_TransportAddress_Initialized
         then
            IDL_SEQUENCE_CSIIOP_TransportAddress_Initialized :=
              True;
            CSIIOP.Helper.Internals.Initialize_TransportAddress;
            CSIIOP.Helper.TC_IDL_SEQUENCE_CSIIOP_TransportAddress :=
              CORBA.TypeCode.Internals.Build_Sequence_TC
                 (CSIIOP.Helper.TC_TransportAddress,
                  0);
            CORBA.TypeCode.Internals.Disable_Ref_Counting
              (CSIIOP.Helper.TC_IDL_SEQUENCE_CSIIOP_TransportAddress);
            IDL_SEQUENCE_CSIIOP_TransportAddress_Helper.Initialize
              (Element_TC => CSIIOP.Helper.TC_TransportAddress,
               Sequence_TC => CSIIOP.Helper.TC_IDL_SEQUENCE_CSIIOP_TransportAddress);
            CORBA.TypeCode.Internals.Freeze
              (TC_IDL_SEQUENCE_CSIIOP_TransportAddress);
         end if;
      end Initialize_IDL_SEQUENCE_CSIIOP_TransportAddress;

      TransportAddressList_Initialized : PolyORB.Std.Boolean :=
        False;

      -------------------------------------
      -- Initialize_TransportAddressList --
      -------------------------------------

      procedure Initialize_TransportAddressList is
         Name_Ü : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("TransportAddressList");
         Id_Ü : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("IDL:omg.org/CSIIOP/TransportAddressList:1.0");
      begin
         if not TransportAddressList_Initialized
         then
            TransportAddressList_Initialized :=
              True;
            CSIIOP.Helper.Internals.Initialize_IDL_SEQUENCE_CSIIOP_TransportAddress;
            CSIIOP.Helper.TC_TransportAddressList :=
              CORBA.TypeCode.Internals.Build_Alias_TC
                 (Name => Name_Ü,
                  Id => Id_Ü,
                  Parent => CSIIOP.Helper.TC_IDL_SEQUENCE_CSIIOP_TransportAddress);
            CORBA.TypeCode.Internals.Disable_Ref_Counting
              (TC_TransportAddressList);
            CORBA.TypeCode.Internals.Freeze
              (TC_TransportAddressList);
         end if;
      end Initialize_TransportAddressList;

      ---------------------------
      -- Get_Aggregate_Element --
      ---------------------------

      function Get_Aggregate_Element
        (Acc : not null access Content_Ü_SECIOP_SEC_TRANS;
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
                 (CORBA.Unsigned_Short
                    (Acc.V.target_supports)'Unrestricted_Access);
            when 1 =>
               return CORBA.Wrap
                 (CORBA.Unsigned_Short
                    (Acc.V.target_requires)'Unrestricted_Access);
            when 2 =>
               return CSI.Helper.Internals.Wrap
                 (CSI.IDL_SEQUENCE_octet_3.Sequence
                    (Acc.V.mech_oid)'Unrestricted_Access);
            when 3 =>
               return CSI.Helper.Internals.Wrap
                 (CSI.IDL_SEQUENCE_octet_5.Sequence
                    (Acc.V.target_name)'Unrestricted_Access);
            when 4 =>
               return CSIIOP.Helper.Internals.Wrap
                 (CSIIOP.IDL_SEQUENCE_CSIIOP_TransportAddress.Sequence
                    (Acc.V.addresses)'Unrestricted_Access);
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
        (Acc : Content_Ü_SECIOP_SEC_TRANS)
        return PolyORB.Types.Unsigned_Long
      is
         pragma Unreferenced (Acc);
      begin
         return 5;
      end Get_Aggregate_Count;

      -------------------------
      -- Set_Aggregate_Count --
      -------------------------

      procedure Set_Aggregate_Count
        (Acc : in out Content_Ü_SECIOP_SEC_TRANS;
         Count : PolyORB.Types.Unsigned_Long)
      is
      begin
         null;
      end Set_Aggregate_Count;

      ---------------------
      -- Unchecked_Get_V --
      ---------------------

      function Unchecked_Get_V
        (Acc : not null access Content_Ü_SECIOP_SEC_TRANS)
        return PolyORB.Types.Address
      is
         function To_Address
           is new Ada.Unchecked_Conversion
              (Ptr_Ü_SECIOP_SEC_TRANS,
               PolyORB.Types.Address);

      begin
         return To_Address
           (Acc.V);
      end Unchecked_Get_V;

      -----------
      -- Clone --
      -----------

      function Clone
        (Acc : Content_Ü_SECIOP_SEC_TRANS;
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
               not in Content_Ü_SECIOP_SEC_TRANS)
            then
               return null;
            end if;
            Target :=
              Into;
            Content_Ü_SECIOP_SEC_TRANS
              (Target.all).V.all :=
              Acc.V.all;
         else
            Target :=
              new Content_Ü_SECIOP_SEC_TRANS;
            Content_Ü_SECIOP_SEC_TRANS
              (Target.all).V :=
              new CSIIOP.SECIOP_SEC_TRANS'
                 (Acc.V.all);
         end if;
         return Target;
      end Clone;

      --------------------
      -- Finalize_Value --
      --------------------

      procedure Finalize_Value
        (Acc : in out Content_Ü_SECIOP_SEC_TRANS)
      is
         procedure Free
           is new PolyORB.Utils.Unchecked_Deallocation.Free

           (Object => CSIIOP.SECIOP_SEC_TRANS,

            Name   => Ptr_Ü_SECIOP_SEC_TRANS);

      begin
         Free
           (Acc.V);
      end Finalize_Value;

      ----------
      -- Wrap --
      ----------

      function Wrap
        (X : access CSIIOP.SECIOP_SEC_TRANS)
        return PolyORB.Any.Content'Class
      is
      begin
         return Content_Ü_SECIOP_SEC_TRANS'
           (PolyORB.Any.Aggregate_Content with
            V => Ptr_Ü_SECIOP_SEC_TRANS'
              (X.all'Unchecked_Access));
      end Wrap;

      SECIOP_SEC_TRANS_Initialized : PolyORB.Std.Boolean :=
        False;

      ---------------------------------
      -- Initialize_SECIOP_SEC_TRANS --
      ---------------------------------

      procedure Initialize_SECIOP_SEC_TRANS is
         Name_Ü : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("SECIOP_SEC_TRANS");
         Id_Ü : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("IDL:omg.org/CSIIOP/SECIOP_SEC_TRANS:1.0");
         Argument_Name_Ü_target_supports : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("target_supports");
         Argument_Name_Ü_target_requires : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("target_requires");
         Argument_Name_Ü_mech_oid : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("mech_oid");
         Argument_Name_Ü_target_name : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("target_name");
         Argument_Name_Ü_addresses : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("addresses");
      begin
         if not SECIOP_SEC_TRANS_Initialized
         then
            SECIOP_SEC_TRANS_Initialized :=
              True;
            CSIIOP.Helper.TC_SECIOP_SEC_TRANS :=
              CORBA.TypeCode.Internals.To_CORBA_Object
                 (PolyORB.Any.TypeCode.Tcf_Struct);
            CORBA.Internals.Add_Parameter
              (TC_SECIOP_SEC_TRANS,
               CORBA.To_Any
                 (Name_Ü));
            CORBA.Internals.Add_Parameter
              (TC_SECIOP_SEC_TRANS,
               CORBA.To_Any
                 (Id_Ü));
            CSIIOP.Helper.Internals.Initialize_AssociationOptions;
            CORBA.Internals.Add_Parameter
              (TC_SECIOP_SEC_TRANS,
               CORBA.To_Any
                 (CSIIOP.Helper.TC_AssociationOptions));
            CORBA.Internals.Add_Parameter
              (TC_SECIOP_SEC_TRANS,
               CORBA.To_Any
                 (Argument_Name_Ü_target_supports));
            CSIIOP.Helper.Internals.Initialize_AssociationOptions;
            CORBA.Internals.Add_Parameter
              (TC_SECIOP_SEC_TRANS,
               CORBA.To_Any
                 (CSIIOP.Helper.TC_AssociationOptions));
            CORBA.Internals.Add_Parameter
              (TC_SECIOP_SEC_TRANS,
               CORBA.To_Any
                 (Argument_Name_Ü_target_requires));
            CSI.Helper.Internals.Initialize_OID;
            CORBA.Internals.Add_Parameter
              (TC_SECIOP_SEC_TRANS,
               CORBA.To_Any
                 (CSI.Helper.TC_OID));
            CORBA.Internals.Add_Parameter
              (TC_SECIOP_SEC_TRANS,
               CORBA.To_Any
                 (Argument_Name_Ü_mech_oid));
            CSI.Helper.Internals.Initialize_GSS_NT_ExportedName;
            CORBA.Internals.Add_Parameter
              (TC_SECIOP_SEC_TRANS,
               CORBA.To_Any
                 (CSI.Helper.TC_GSS_NT_ExportedName));
            CORBA.Internals.Add_Parameter
              (TC_SECIOP_SEC_TRANS,
               CORBA.To_Any
                 (Argument_Name_Ü_target_name));
            CSIIOP.Helper.Internals.Initialize_TransportAddressList;
            CORBA.Internals.Add_Parameter
              (TC_SECIOP_SEC_TRANS,
               CORBA.To_Any
                 (CSIIOP.Helper.TC_TransportAddressList));
            CORBA.Internals.Add_Parameter
              (TC_SECIOP_SEC_TRANS,
               CORBA.To_Any
                 (Argument_Name_Ü_addresses));
            CORBA.TypeCode.Internals.Disable_Ref_Counting
              (TC_SECIOP_SEC_TRANS);
            CORBA.TypeCode.Internals.Freeze
              (TC_SECIOP_SEC_TRANS);
         end if;
      end Initialize_SECIOP_SEC_TRANS;

      ---------------------------
      -- Get_Aggregate_Element --
      ---------------------------

      function Get_Aggregate_Element
        (Acc : not null access Content_Ü_TLS_SEC_TRANS;
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
                 (CORBA.Unsigned_Short
                    (Acc.V.target_supports)'Unrestricted_Access);
            when 1 =>
               return CORBA.Wrap
                 (CORBA.Unsigned_Short
                    (Acc.V.target_requires)'Unrestricted_Access);
            when 2 =>
               return CSIIOP.Helper.Internals.Wrap
                 (CSIIOP.IDL_SEQUENCE_CSIIOP_TransportAddress.Sequence
                    (Acc.V.addresses)'Unrestricted_Access);
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
        (Acc : Content_Ü_TLS_SEC_TRANS)
        return PolyORB.Types.Unsigned_Long
      is
         pragma Unreferenced (Acc);
      begin
         return 3;
      end Get_Aggregate_Count;

      -------------------------
      -- Set_Aggregate_Count --
      -------------------------

      procedure Set_Aggregate_Count
        (Acc : in out Content_Ü_TLS_SEC_TRANS;
         Count : PolyORB.Types.Unsigned_Long)
      is
      begin
         null;
      end Set_Aggregate_Count;

      ---------------------
      -- Unchecked_Get_V --
      ---------------------

      function Unchecked_Get_V
        (Acc : not null access Content_Ü_TLS_SEC_TRANS)
        return PolyORB.Types.Address
      is
         function To_Address
           is new Ada.Unchecked_Conversion
              (Ptr_Ü_TLS_SEC_TRANS,
               PolyORB.Types.Address);

      begin
         return To_Address
           (Acc.V);
      end Unchecked_Get_V;

      -----------
      -- Clone --
      -----------

      function Clone
        (Acc : Content_Ü_TLS_SEC_TRANS;
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
               not in Content_Ü_TLS_SEC_TRANS)
            then
               return null;
            end if;
            Target :=
              Into;
            Content_Ü_TLS_SEC_TRANS
              (Target.all).V.all :=
              Acc.V.all;
         else
            Target :=
              new Content_Ü_TLS_SEC_TRANS;
            Content_Ü_TLS_SEC_TRANS
              (Target.all).V :=
              new CSIIOP.TLS_SEC_TRANS'
                 (Acc.V.all);
         end if;
         return Target;
      end Clone;

      --------------------
      -- Finalize_Value --
      --------------------

      procedure Finalize_Value
        (Acc : in out Content_Ü_TLS_SEC_TRANS)
      is
         procedure Free
           is new PolyORB.Utils.Unchecked_Deallocation.Free

           (Object => CSIIOP.TLS_SEC_TRANS,

            Name   => Ptr_Ü_TLS_SEC_TRANS);

      begin
         Free
           (Acc.V);
      end Finalize_Value;

      ----------
      -- Wrap --
      ----------

      function Wrap
        (X : access CSIIOP.TLS_SEC_TRANS)
        return PolyORB.Any.Content'Class
      is
      begin
         return Content_Ü_TLS_SEC_TRANS'
           (PolyORB.Any.Aggregate_Content with
            V => Ptr_Ü_TLS_SEC_TRANS'
              (X.all'Unchecked_Access));
      end Wrap;

      TLS_SEC_TRANS_Initialized : PolyORB.Std.Boolean :=
        False;

      ------------------------------
      -- Initialize_TLS_SEC_TRANS --
      ------------------------------

      procedure Initialize_TLS_SEC_TRANS is
         Name_Ü : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("TLS_SEC_TRANS");
         Id_Ü : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("IDL:omg.org/CSIIOP/TLS_SEC_TRANS:1.0");
         Argument_Name_Ü_target_supports : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("target_supports");
         Argument_Name_Ü_target_requires : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("target_requires");
         Argument_Name_Ü_addresses : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("addresses");
      begin
         if not TLS_SEC_TRANS_Initialized
         then
            TLS_SEC_TRANS_Initialized :=
              True;
            CSIIOP.Helper.TC_TLS_SEC_TRANS :=
              CORBA.TypeCode.Internals.To_CORBA_Object
                 (PolyORB.Any.TypeCode.Tcf_Struct);
            CORBA.Internals.Add_Parameter
              (TC_TLS_SEC_TRANS,
               CORBA.To_Any
                 (Name_Ü));
            CORBA.Internals.Add_Parameter
              (TC_TLS_SEC_TRANS,
               CORBA.To_Any
                 (Id_Ü));
            CSIIOP.Helper.Internals.Initialize_AssociationOptions;
            CORBA.Internals.Add_Parameter
              (TC_TLS_SEC_TRANS,
               CORBA.To_Any
                 (CSIIOP.Helper.TC_AssociationOptions));
            CORBA.Internals.Add_Parameter
              (TC_TLS_SEC_TRANS,
               CORBA.To_Any
                 (Argument_Name_Ü_target_supports));
            CSIIOP.Helper.Internals.Initialize_AssociationOptions;
            CORBA.Internals.Add_Parameter
              (TC_TLS_SEC_TRANS,
               CORBA.To_Any
                 (CSIIOP.Helper.TC_AssociationOptions));
            CORBA.Internals.Add_Parameter
              (TC_TLS_SEC_TRANS,
               CORBA.To_Any
                 (Argument_Name_Ü_target_requires));
            CSIIOP.Helper.Internals.Initialize_TransportAddressList;
            CORBA.Internals.Add_Parameter
              (TC_TLS_SEC_TRANS,
               CORBA.To_Any
                 (CSIIOP.Helper.TC_TransportAddressList));
            CORBA.Internals.Add_Parameter
              (TC_TLS_SEC_TRANS,
               CORBA.To_Any
                 (Argument_Name_Ü_addresses));
            CORBA.TypeCode.Internals.Disable_Ref_Counting
              (TC_TLS_SEC_TRANS);
            CORBA.TypeCode.Internals.Freeze
              (TC_TLS_SEC_TRANS);
         end if;
      end Initialize_TLS_SEC_TRANS;

   end Internals;

   --------------
   -- From_Any --
   --------------

   function From_Any
     (Item : CORBA.Any)
     return CSIIOP.AssociationOptions
   is
      Result : constant CORBA.Unsigned_Short :=
        CORBA.From_Any
           (Item);
   begin
      return CSIIOP.AssociationOptions
        (Result);
   end From_Any;

   ------------
   -- To_Any --
   ------------

   function To_Any
     (Item : CSIIOP.AssociationOptions)
     return CORBA.Any
   is
      Result : CORBA.Any :=
        CORBA.To_Any
           (CORBA.Unsigned_Short
              (Item));
   begin
      CORBA.Internals.Set_Type
        (Result,
         TC_AssociationOptions);
      return Result;
   end To_Any;

   --------------
   -- From_Any --
   --------------

   function From_Any
     (Item : CORBA.Any)
     return CSIIOP.ServiceConfigurationSyntax
   is
      Result : constant CORBA.Unsigned_Long :=
        CORBA.From_Any
           (Item);
   begin
      return CSIIOP.ServiceConfigurationSyntax
        (Result);
   end From_Any;

   ------------
   -- To_Any --
   ------------

   function To_Any
     (Item : CSIIOP.ServiceConfigurationSyntax)
     return CORBA.Any
   is
      Result : CORBA.Any :=
        CORBA.To_Any
           (CORBA.Unsigned_Long
              (Item));
   begin
      CORBA.Internals.Set_Type
        (Result,
         TC_ServiceConfigurationSyntax);
      return Result;
   end To_Any;

   function From_Any
     (Item : CORBA.Any)
     return CSIIOP.IDL_SEQUENCE_octet.Sequence
     renames CSIIOP.Helper.Internals.IDL_SEQUENCE_octet_Helper.From_Any;

   function To_Any
     (Item : CSIIOP.IDL_SEQUENCE_octet.Sequence)
     return CORBA.Any
     renames CSIIOP.Helper.Internals.IDL_SEQUENCE_octet_Helper.To_Any;

   --------------
   -- From_Any --
   --------------

   function From_Any
     (Item : CORBA.Any)
     return CSIIOP.ServiceSpecificName
   is
      Result : constant CSIIOP.IDL_SEQUENCE_octet.Sequence :=
        CSIIOP.Helper.From_Any
           (Item);
   begin
      return CSIIOP.ServiceSpecificName
        (Result);
   end From_Any;

   ------------
   -- To_Any --
   ------------

   function To_Any
     (Item : CSIIOP.ServiceSpecificName)
     return CORBA.Any
   is
      Result : CORBA.Any :=
        CSIIOP.Helper.To_Any
           (CSIIOP.IDL_SEQUENCE_octet.Sequence
              (Item));
   begin
      CORBA.Internals.Set_Type
        (Result,
         TC_ServiceSpecificName);
      return Result;
   end To_Any;

   --------------
   -- From_Any --
   --------------

   function From_Any
     (Item : CORBA.Any)
     return CSIIOP.ServiceConfiguration
   is
   begin
      return (syntax => CSIIOP.Helper.From_Any
        (CORBA.Internals.Get_Aggregate_Element
           (Item,
            CSIIOP.Helper.TC_ServiceConfigurationSyntax,
            0)),
      name => CSIIOP.Helper.From_Any
        (CORBA.Internals.Get_Aggregate_Element
           (Item,
            CSIIOP.Helper.TC_ServiceSpecificName,
            1)));
   end From_Any;

   ------------
   -- To_Any --
   ------------

   function To_Any
     (Item : CSIIOP.ServiceConfiguration)
     return CORBA.Any
   is
      Result : constant CORBA.Any :=
        CORBA.Internals.Get_Empty_Any
           (TC_ServiceConfiguration);
   begin
      PolyORB.Any.Set_Value
        (CORBA.Get_Container
           (Result).all,
         new PolyORB.Any.Content'Class'
           (CSIIOP.Helper.Internals.Wrap
              (new CSIIOP.ServiceConfiguration'
                 (Item))),
         False);
      return Result;
   end To_Any;

   function From_Any
     (Item : CORBA.Any)
     return CSIIOP.IDL_SEQUENCE_CSIIOP_ServiceConfiguration.Sequence
     renames CSIIOP.Helper.Internals.IDL_SEQUENCE_CSIIOP_ServiceConfiguration_Helper.From_Any;

   function To_Any
     (Item : CSIIOP.IDL_SEQUENCE_CSIIOP_ServiceConfiguration.Sequence)
     return CORBA.Any
     renames CSIIOP.Helper.Internals.IDL_SEQUENCE_CSIIOP_ServiceConfiguration_Helper.To_Any;

   --------------
   -- From_Any --
   --------------

   function From_Any
     (Item : CORBA.Any)
     return CSIIOP.ServiceConfigurationList
   is
      Result : constant CSIIOP.IDL_SEQUENCE_CSIIOP_ServiceConfiguration.Sequence :=
        CSIIOP.Helper.From_Any
           (Item);
   begin
      return CSIIOP.ServiceConfigurationList
        (Result);
   end From_Any;

   ------------
   -- To_Any --
   ------------

   function To_Any
     (Item : CSIIOP.ServiceConfigurationList)
     return CORBA.Any
   is
      Result : CORBA.Any :=
        CSIIOP.Helper.To_Any
           (CSIIOP.IDL_SEQUENCE_CSIIOP_ServiceConfiguration.Sequence
              (Item));
   begin
      CORBA.Internals.Set_Type
        (Result,
         TC_ServiceConfigurationList);
      return Result;
   end To_Any;

   --------------
   -- From_Any --
   --------------

   function From_Any
     (Item : CORBA.Any)
     return CSIIOP.AS_ContextSec
   is
   begin
      return (target_supports => CSIIOP.Helper.From_Any
        (CORBA.Internals.Get_Aggregate_Element
           (Item,
            CSIIOP.Helper.TC_AssociationOptions,
            0)),
      target_requires => CSIIOP.Helper.From_Any
        (CORBA.Internals.Get_Aggregate_Element
           (Item,
            CSIIOP.Helper.TC_AssociationOptions,
            1)),
      client_authentication_mech => CSI.Helper.From_Any
        (CORBA.Internals.Get_Aggregate_Element
           (Item,
            CSI.Helper.TC_OID,
            2)),
      target_name => CSI.Helper.From_Any
        (CORBA.Internals.Get_Aggregate_Element
           (Item,
            CSI.Helper.TC_GSS_NT_ExportedName,
            3)));
   end From_Any;

   ------------
   -- To_Any --
   ------------

   function To_Any
     (Item : CSIIOP.AS_ContextSec)
     return CORBA.Any
   is
      Result : constant CORBA.Any :=
        CORBA.Internals.Get_Empty_Any
           (TC_AS_ContextSec);
   begin
      PolyORB.Any.Set_Value
        (CORBA.Get_Container
           (Result).all,
         new PolyORB.Any.Content'Class'
           (CSIIOP.Helper.Internals.Wrap
              (new CSIIOP.AS_ContextSec'
                 (Item))),
         False);
      return Result;
   end To_Any;

   --------------
   -- From_Any --
   --------------

   function From_Any
     (Item : CORBA.Any)
     return CSIIOP.SAS_ContextSec
   is
   begin
      return (target_supports => CSIIOP.Helper.From_Any
        (CORBA.Internals.Get_Aggregate_Element
           (Item,
            CSIIOP.Helper.TC_AssociationOptions,
            0)),
      target_requires => CSIIOP.Helper.From_Any
        (CORBA.Internals.Get_Aggregate_Element
           (Item,
            CSIIOP.Helper.TC_AssociationOptions,
            1)),
      privilege_authorities => CSIIOP.Helper.From_Any
        (CORBA.Internals.Get_Aggregate_Element
           (Item,
            CSIIOP.Helper.TC_ServiceConfigurationList,
            2)),
      supported_naming_mechanisms => CSI.Helper.From_Any
        (CORBA.Internals.Get_Aggregate_Element
           (Item,
            CSI.Helper.TC_OIDList,
            3)),
      supported_identity_types => CSI.Helper.From_Any
        (CORBA.Internals.Get_Aggregate_Element
           (Item,
            CSI.Helper.TC_IdentityTokenType,
            4)));
   end From_Any;

   ------------
   -- To_Any --
   ------------

   function To_Any
     (Item : CSIIOP.SAS_ContextSec)
     return CORBA.Any
   is
      Result : constant CORBA.Any :=
        CORBA.Internals.Get_Empty_Any
           (TC_SAS_ContextSec);
   begin
      PolyORB.Any.Set_Value
        (CORBA.Get_Container
           (Result).all,
         new PolyORB.Any.Content'Class'
           (CSIIOP.Helper.Internals.Wrap
              (new CSIIOP.SAS_ContextSec'
                 (Item))),
         False);
      return Result;
   end To_Any;

   --------------
   -- From_Any --
   --------------

   function From_Any
     (Item : CORBA.Any)
     return CSIIOP.CompoundSecMech
   is
   begin
      return (target_requires => CSIIOP.Helper.From_Any
        (CORBA.Internals.Get_Aggregate_Element
           (Item,
            CSIIOP.Helper.TC_AssociationOptions,
            0)),
      transport_mech => IOP.Helper.From_Any
        (CORBA.Internals.Get_Aggregate_Element
           (Item,
            IOP.Helper.TC_TaggedComponent,
            1)),
      as_context_mech => CSIIOP.Helper.From_Any
        (CORBA.Internals.Get_Aggregate_Element
           (Item,
            CSIIOP.Helper.TC_AS_ContextSec,
            2)),
      sas_context_mech => CSIIOP.Helper.From_Any
        (CORBA.Internals.Get_Aggregate_Element
           (Item,
            CSIIOP.Helper.TC_SAS_ContextSec,
            3)));
   end From_Any;

   ------------
   -- To_Any --
   ------------

   function To_Any
     (Item : CSIIOP.CompoundSecMech)
     return CORBA.Any
   is
      Result : constant CORBA.Any :=
        CORBA.Internals.Get_Empty_Any
           (TC_CompoundSecMech);
   begin
      PolyORB.Any.Set_Value
        (CORBA.Get_Container
           (Result).all,
         new PolyORB.Any.Content'Class'
           (CSIIOP.Helper.Internals.Wrap
              (new CSIIOP.CompoundSecMech'
                 (Item))),
         False);
      return Result;
   end To_Any;

   function From_Any
     (Item : CORBA.Any)
     return CSIIOP.IDL_SEQUENCE_CSIIOP_CompoundSecMech.Sequence
     renames CSIIOP.Helper.Internals.IDL_SEQUENCE_CSIIOP_CompoundSecMech_Helper.From_Any;

   function To_Any
     (Item : CSIIOP.IDL_SEQUENCE_CSIIOP_CompoundSecMech.Sequence)
     return CORBA.Any
     renames CSIIOP.Helper.Internals.IDL_SEQUENCE_CSIIOP_CompoundSecMech_Helper.To_Any;

   --------------
   -- From_Any --
   --------------

   function From_Any
     (Item : CORBA.Any)
     return CSIIOP.CompoundSecMechanisms
   is
      Result : constant CSIIOP.IDL_SEQUENCE_CSIIOP_CompoundSecMech.Sequence :=
        CSIIOP.Helper.From_Any
           (Item);
   begin
      return CSIIOP.CompoundSecMechanisms
        (Result);
   end From_Any;

   ------------
   -- To_Any --
   ------------

   function To_Any
     (Item : CSIIOP.CompoundSecMechanisms)
     return CORBA.Any
   is
      Result : CORBA.Any :=
        CSIIOP.Helper.To_Any
           (CSIIOP.IDL_SEQUENCE_CSIIOP_CompoundSecMech.Sequence
              (Item));
   begin
      CORBA.Internals.Set_Type
        (Result,
         TC_CompoundSecMechanisms);
      return Result;
   end To_Any;

   --------------
   -- From_Any --
   --------------

   function From_Any
     (Item : CORBA.Any)
     return CSIIOP.CompoundSecMechList
   is
   begin
      return (stateful => CORBA.From_Any
        (CORBA.Internals.Get_Aggregate_Element
           (Item,
            CORBA.TC_Boolean,
            0)),
      mechanism_list => CSIIOP.Helper.From_Any
        (CORBA.Internals.Get_Aggregate_Element
           (Item,
            CSIIOP.Helper.TC_CompoundSecMechanisms,
            1)));
   end From_Any;

   ------------
   -- To_Any --
   ------------

   function To_Any
     (Item : CSIIOP.CompoundSecMechList)
     return CORBA.Any
   is
      Result : constant CORBA.Any :=
        CORBA.Internals.Get_Empty_Any
           (TC_CompoundSecMechList);
   begin
      PolyORB.Any.Set_Value
        (CORBA.Get_Container
           (Result).all,
         new PolyORB.Any.Content'Class'
           (CSIIOP.Helper.Internals.Wrap
              (new CSIIOP.CompoundSecMechList'
                 (Item))),
         False);
      return Result;
   end To_Any;

   --------------
   -- From_Any --
   --------------

   function From_Any
     (Item : CORBA.Any)
     return CSIIOP.TransportAddress
   is
   begin
      return (host_name => CORBA.From_Any
        (CORBA.Internals.Get_Aggregate_Element
           (Item,
            CORBA.TC_String,
            0)),
      port => CORBA.From_Any
        (CORBA.Internals.Get_Aggregate_Element
           (Item,
            CORBA.TC_Unsigned_Short,
            1)));
   end From_Any;

   ------------
   -- To_Any --
   ------------

   function To_Any
     (Item : CSIIOP.TransportAddress)
     return CORBA.Any
   is
      Result : constant CORBA.Any :=
        CORBA.Internals.Get_Empty_Any
           (TC_TransportAddress);
   begin
      PolyORB.Any.Set_Value
        (CORBA.Get_Container
           (Result).all,
         new PolyORB.Any.Content'Class'
           (CSIIOP.Helper.Internals.Wrap
              (new CSIIOP.TransportAddress'
                 (Item))),
         False);
      return Result;
   end To_Any;

   function From_Any
     (Item : CORBA.Any)
     return CSIIOP.IDL_SEQUENCE_CSIIOP_TransportAddress.Sequence
     renames CSIIOP.Helper.Internals.IDL_SEQUENCE_CSIIOP_TransportAddress_Helper.From_Any;

   function To_Any
     (Item : CSIIOP.IDL_SEQUENCE_CSIIOP_TransportAddress.Sequence)
     return CORBA.Any
     renames CSIIOP.Helper.Internals.IDL_SEQUENCE_CSIIOP_TransportAddress_Helper.To_Any;

   --------------
   -- From_Any --
   --------------

   function From_Any
     (Item : CORBA.Any)
     return CSIIOP.TransportAddressList
   is
      Result : constant CSIIOP.IDL_SEQUENCE_CSIIOP_TransportAddress.Sequence :=
        CSIIOP.Helper.From_Any
           (Item);
   begin
      return CSIIOP.TransportAddressList
        (Result);
   end From_Any;

   ------------
   -- To_Any --
   ------------

   function To_Any
     (Item : CSIIOP.TransportAddressList)
     return CORBA.Any
   is
      Result : CORBA.Any :=
        CSIIOP.Helper.To_Any
           (CSIIOP.IDL_SEQUENCE_CSIIOP_TransportAddress.Sequence
              (Item));
   begin
      CORBA.Internals.Set_Type
        (Result,
         TC_TransportAddressList);
      return Result;
   end To_Any;

   --------------
   -- From_Any --
   --------------

   function From_Any
     (Item : CORBA.Any)
     return CSIIOP.SECIOP_SEC_TRANS
   is
   begin
      return (target_supports => CSIIOP.Helper.From_Any
        (CORBA.Internals.Get_Aggregate_Element
           (Item,
            CSIIOP.Helper.TC_AssociationOptions,
            0)),
      target_requires => CSIIOP.Helper.From_Any
        (CORBA.Internals.Get_Aggregate_Element
           (Item,
            CSIIOP.Helper.TC_AssociationOptions,
            1)),
      mech_oid => CSI.Helper.From_Any
        (CORBA.Internals.Get_Aggregate_Element
           (Item,
            CSI.Helper.TC_OID,
            2)),
      target_name => CSI.Helper.From_Any
        (CORBA.Internals.Get_Aggregate_Element
           (Item,
            CSI.Helper.TC_GSS_NT_ExportedName,
            3)),
      addresses => CSIIOP.Helper.From_Any
        (CORBA.Internals.Get_Aggregate_Element
           (Item,
            CSIIOP.Helper.TC_TransportAddressList,
            4)));
   end From_Any;

   ------------
   -- To_Any --
   ------------

   function To_Any
     (Item : CSIIOP.SECIOP_SEC_TRANS)
     return CORBA.Any
   is
      Result : constant CORBA.Any :=
        CORBA.Internals.Get_Empty_Any
           (TC_SECIOP_SEC_TRANS);
   begin
      PolyORB.Any.Set_Value
        (CORBA.Get_Container
           (Result).all,
         new PolyORB.Any.Content'Class'
           (CSIIOP.Helper.Internals.Wrap
              (new CSIIOP.SECIOP_SEC_TRANS'
                 (Item))),
         False);
      return Result;
   end To_Any;

   --------------
   -- From_Any --
   --------------

   function From_Any
     (Item : CORBA.Any)
     return CSIIOP.TLS_SEC_TRANS
   is
   begin
      return (target_supports => CSIIOP.Helper.From_Any
        (CORBA.Internals.Get_Aggregate_Element
           (Item,
            CSIIOP.Helper.TC_AssociationOptions,
            0)),
      target_requires => CSIIOP.Helper.From_Any
        (CORBA.Internals.Get_Aggregate_Element
           (Item,
            CSIIOP.Helper.TC_AssociationOptions,
            1)),
      addresses => CSIIOP.Helper.From_Any
        (CORBA.Internals.Get_Aggregate_Element
           (Item,
            CSIIOP.Helper.TC_TransportAddressList,
            2)));
   end From_Any;

   ------------
   -- To_Any --
   ------------

   function To_Any
     (Item : CSIIOP.TLS_SEC_TRANS)
     return CORBA.Any
   is
      Result : constant CORBA.Any :=
        CORBA.Internals.Get_Empty_Any
           (TC_TLS_SEC_TRANS);
   begin
      PolyORB.Any.Set_Value
        (CORBA.Get_Container
           (Result).all,
         new PolyORB.Any.Content'Class'
           (CSIIOP.Helper.Internals.Wrap
              (new CSIIOP.TLS_SEC_TRANS'
                 (Item))),
         False);
      return Result;
   end To_Any;

   -----------------------------
   -- Deferred_Initialization --
   -----------------------------

   procedure Deferred_Initialization is
   begin
      CSIIOP.Helper.Internals.Initialize_AssociationOptions;
      CSIIOP.Helper.Internals.Initialize_ServiceConfigurationSyntax;
      CSIIOP.Helper.Internals.Initialize_IDL_SEQUENCE_octet;
      CSIIOP.Helper.Internals.Initialize_ServiceSpecificName;
      CSIIOP.Helper.Internals.Initialize_ServiceConfiguration;
      CSIIOP.Helper.Internals.Initialize_IDL_SEQUENCE_CSIIOP_ServiceConfiguration;
      CSIIOP.Helper.Internals.Initialize_ServiceConfigurationList;
      CSIIOP.Helper.Internals.Initialize_AS_ContextSec;
      CSIIOP.Helper.Internals.Initialize_SAS_ContextSec;
      CSIIOP.Helper.Internals.Initialize_CompoundSecMech;
      CSIIOP.Helper.Internals.Initialize_IDL_SEQUENCE_CSIIOP_CompoundSecMech;
      CSIIOP.Helper.Internals.Initialize_CompoundSecMechanisms;
      CSIIOP.Helper.Internals.Initialize_CompoundSecMechList;
      CSIIOP.Helper.Internals.Initialize_TransportAddress;
      CSIIOP.Helper.Internals.Initialize_IDL_SEQUENCE_CSIIOP_TransportAddress;
      CSIIOP.Helper.Internals.Initialize_TransportAddressList;
      CSIIOP.Helper.Internals.Initialize_SECIOP_SEC_TRANS;
      CSIIOP.Helper.Internals.Initialize_TLS_SEC_TRANS;
   end Deferred_Initialization;

begin
   declare
      use PolyORB.Utils.Strings;
      use PolyORB.Utils.Strings.Lists;
   begin
      PolyORB.Initialization.Register_Module
        (PolyORB.Initialization.Module_Info'
           (Name => +"CSIIOP.Helper",
            Conflicts => PolyORB.Utils.Strings.Lists.Empty,
            Depends => +"any"
               & "corba",
            Provides => PolyORB.Utils.Strings.Lists.Empty,
            Implicit => False,
            Init => Deferred_Initialization'Access,
            Shutdown => null));
   end;
end CSIIOP.Helper;

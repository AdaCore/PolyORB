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
with CORBA.IDL_Sequences.Helper;
with CORBA.IDL_Sequences;
with Ada.Unchecked_Conversion;
with PolyORB.Utils.Unchecked_Deallocation;
with PolyORB.Utils.Strings;
with PolyORB.Utils.Strings.Lists;
with PolyORB.Initialization;

package body IOP.Helper is

   
   package body Internals is

      ----------
      -- Wrap --
      ----------

      function Wrap
        (X : access IOP.ProfileId)
        return PolyORB.Any.Content'Class
      is
      begin
         return CORBA.Wrap
           (CORBA.Unsigned_Long
              (X.all)'Unrestricted_Access);
      end Wrap;

      ProfileId_Initialized : PolyORB.Std.Boolean :=
        False;

      --------------------------
      -- Initialize_ProfileId --
      --------------------------

      procedure Initialize_ProfileId is
         Name_Ü : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("ProfileId");
         Id_Ü : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("IDL:omg.org/IOP/ProfileId:1.0");
      begin
         if not ProfileId_Initialized
         then
            ProfileId_Initialized :=
              True;
            IOP.Helper.TC_ProfileId :=
              CORBA.TypeCode.Internals.Build_Alias_TC
                 (Name => Name_Ü,
                  Id => Id_Ü,
                  Parent => CORBA.TC_Unsigned_Long);
            CORBA.TypeCode.Internals.Disable_Ref_Counting
              (TC_ProfileId);
            CORBA.TypeCode.Internals.Freeze
              (TC_ProfileId);
         end if;
      end Initialize_ProfileId;

      ProfileData_Initialized : PolyORB.Std.Boolean :=
        False;

      ----------------------------
      -- Initialize_ProfileData --
      ----------------------------

      procedure Initialize_ProfileData is
         Name_Ü : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("ProfileData");
         Id_Ü : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("IDL:omg.org/IOP/ProfileData:1.0");
      begin
         if not ProfileData_Initialized
         then
            ProfileData_Initialized :=
              True;
            IOP.Helper.TC_ProfileData :=
              CORBA.TypeCode.Internals.Build_Alias_TC
                 (Name => Name_Ü,
                  Id => Id_Ü,
                  Parent => CORBA.IDL_Sequences.Helper.TC_OctetSeq);
            CORBA.TypeCode.Internals.Disable_Ref_Counting
              (TC_ProfileData);
            CORBA.TypeCode.Internals.Freeze
              (TC_ProfileData);
         end if;
      end Initialize_ProfileData;

      ---------------------------
      -- Get_Aggregate_Element --
      ---------------------------

      function Get_Aggregate_Element
        (Acc : not null access Content_Ü_TaggedProfile;
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
                    (Acc.V.tag)'Unrestricted_Access);
            when 1 =>
               return CORBA.IDL_Sequences.Helper.Wrap
                 (CORBA.IDL_Sequences.IDL_SEQUENCE_Octet.Sequence
                    (Acc.V.profile_data)'Unrestricted_Access);
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
        (Acc : Content_Ü_TaggedProfile)
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
        (Acc : in out Content_Ü_TaggedProfile;
         Count : PolyORB.Types.Unsigned_Long)
      is
      begin
         null;
      end Set_Aggregate_Count;

      ---------------------
      -- Unchecked_Get_V --
      ---------------------

      function Unchecked_Get_V
        (Acc : not null access Content_Ü_TaggedProfile)
        return PolyORB.Types.Address
      is
         function To_Address
           is new Ada.Unchecked_Conversion
              (Ptr_Ü_TaggedProfile,
               PolyORB.Types.Address);

      begin
         return To_Address
           (Acc.V);
      end Unchecked_Get_V;

      -----------
      -- Clone --
      -----------

      function Clone
        (Acc : Content_Ü_TaggedProfile;
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
               not in Content_Ü_TaggedProfile)
            then
               return null;
            end if;
            Target :=
              Into;
            Content_Ü_TaggedProfile
              (Target.all).V.all :=
              Acc.V.all;
         else
            Target :=
              new Content_Ü_TaggedProfile;
            Content_Ü_TaggedProfile
              (Target.all).V :=
              new IOP.TaggedProfile'
                 (Acc.V.all);
         end if;
         return Target;
      end Clone;

      --------------------
      -- Finalize_Value --
      --------------------

      procedure Finalize_Value
        (Acc : in out Content_Ü_TaggedProfile)
      is
         procedure Free
           is new PolyORB.Utils.Unchecked_Deallocation.Free

           (Object => IOP.TaggedProfile,

            Name   => Ptr_Ü_TaggedProfile);

      begin
         Free
           (Acc.V);
      end Finalize_Value;

      ----------
      -- Wrap --
      ----------

      function Wrap
        (X : access IOP.TaggedProfile)
        return PolyORB.Any.Content'Class
      is
      begin
         return Content_Ü_TaggedProfile'
           (PolyORB.Any.Aggregate_Content with
            V => Ptr_Ü_TaggedProfile'
              (X.all'Unchecked_Access));
      end Wrap;

      TaggedProfile_Initialized : PolyORB.Std.Boolean :=
        False;

      ------------------------------
      -- Initialize_TaggedProfile --
      ------------------------------

      procedure Initialize_TaggedProfile is
         Name_Ü : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("TaggedProfile");
         Id_Ü : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("IDL:omg.org/IOP/TaggedProfile:1.0");
         Argument_Name_Ü_tag : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("tag");
         Argument_Name_Ü_profile_data : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("profile_data");
      begin
         if not TaggedProfile_Initialized
         then
            TaggedProfile_Initialized :=
              True;
            IOP.Helper.TC_TaggedProfile :=
              CORBA.TypeCode.Internals.To_CORBA_Object
                 (PolyORB.Any.TypeCode.Tcf_Struct);
            CORBA.Internals.Add_Parameter
              (TC_TaggedProfile,
               CORBA.To_Any
                 (Name_Ü));
            CORBA.Internals.Add_Parameter
              (TC_TaggedProfile,
               CORBA.To_Any
                 (Id_Ü));
            IOP.Helper.Internals.Initialize_ProfileId;
            CORBA.Internals.Add_Parameter
              (TC_TaggedProfile,
               CORBA.To_Any
                 (IOP.Helper.TC_ProfileId));
            CORBA.Internals.Add_Parameter
              (TC_TaggedProfile,
               CORBA.To_Any
                 (Argument_Name_Ü_tag));
            IOP.Helper.Internals.Initialize_ProfileData;
            CORBA.Internals.Add_Parameter
              (TC_TaggedProfile,
               CORBA.To_Any
                 (IOP.Helper.TC_ProfileData));
            CORBA.Internals.Add_Parameter
              (TC_TaggedProfile,
               CORBA.To_Any
                 (Argument_Name_Ü_profile_data));
            CORBA.TypeCode.Internals.Disable_Ref_Counting
              (TC_TaggedProfile);
            CORBA.TypeCode.Internals.Freeze
              (TC_TaggedProfile);
         end if;
      end Initialize_TaggedProfile;

      -------------------------------------------------
      -- IDL_SEQUENCE_IOP_TaggedProfile_Element_Wrap --
      -------------------------------------------------

      function IDL_SEQUENCE_IOP_TaggedProfile_Element_Wrap
        (X : access IOP.TaggedProfile)
        return PolyORB.Any.Content'Class
      is
      begin
         return IOP.Helper.Internals.Wrap
           (X.all'Unrestricted_Access);
      end IDL_SEQUENCE_IOP_TaggedProfile_Element_Wrap;

      function Wrap
        (X : access IOP.IDL_SEQUENCE_IOP_TaggedProfile.Sequence)
        return PolyORB.Any.Content'Class
        renames IDL_SEQUENCE_IOP_TaggedProfile_Helper.Wrap;

      IDL_SEQUENCE_IOP_TaggedProfile_Initialized : PolyORB.Std.Boolean :=
        False;

      -----------------------------------------------
      -- Initialize_IDL_SEQUENCE_IOP_TaggedProfile --
      -----------------------------------------------

      procedure Initialize_IDL_SEQUENCE_IOP_TaggedProfile is
      begin
         if not IDL_SEQUENCE_IOP_TaggedProfile_Initialized
         then
            IDL_SEQUENCE_IOP_TaggedProfile_Initialized :=
              True;
            IOP.Helper.Internals.Initialize_TaggedProfile;
            IOP.Helper.TC_IDL_SEQUENCE_IOP_TaggedProfile :=
              CORBA.TypeCode.Internals.Build_Sequence_TC
                 (IOP.Helper.TC_TaggedProfile,
                  0);
            CORBA.TypeCode.Internals.Disable_Ref_Counting
              (IOP.Helper.TC_IDL_SEQUENCE_IOP_TaggedProfile);
            IDL_SEQUENCE_IOP_TaggedProfile_Helper.Initialize
              (Element_TC => IOP.Helper.TC_TaggedProfile,
               Sequence_TC => IOP.Helper.TC_IDL_SEQUENCE_IOP_TaggedProfile);
            CORBA.TypeCode.Internals.Freeze
              (TC_IDL_SEQUENCE_IOP_TaggedProfile);
         end if;
      end Initialize_IDL_SEQUENCE_IOP_TaggedProfile;

      TaggedProfileSeq_Initialized : PolyORB.Std.Boolean :=
        False;

      ---------------------------------
      -- Initialize_TaggedProfileSeq --
      ---------------------------------

      procedure Initialize_TaggedProfileSeq is
         Name_Ü : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("TaggedProfileSeq");
         Id_Ü : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("IDL:omg.org/IOP/TaggedProfileSeq:1.0");
      begin
         if not TaggedProfileSeq_Initialized
         then
            TaggedProfileSeq_Initialized :=
              True;
            IOP.Helper.Internals.Initialize_IDL_SEQUENCE_IOP_TaggedProfile;
            IOP.Helper.TC_TaggedProfileSeq :=
              CORBA.TypeCode.Internals.Build_Alias_TC
                 (Name => Name_Ü,
                  Id => Id_Ü,
                  Parent => IOP.Helper.TC_IDL_SEQUENCE_IOP_TaggedProfile);
            CORBA.TypeCode.Internals.Disable_Ref_Counting
              (TC_TaggedProfileSeq);
            CORBA.TypeCode.Internals.Freeze
              (TC_TaggedProfileSeq);
         end if;
      end Initialize_TaggedProfileSeq;

      ---------------------------
      -- Get_Aggregate_Element --
      ---------------------------

      function Get_Aggregate_Element
        (Acc : not null access Content_Ü_IOR;
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
                 (Acc.V.type_id'Unrestricted_Access);
            when 1 =>
               return IOP.Helper.Internals.Wrap
                 (IOP.IDL_SEQUENCE_IOP_TaggedProfile.Sequence
                    (Acc.V.profiles)'Unrestricted_Access);
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
        (Acc : Content_Ü_IOR)
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
        (Acc : in out Content_Ü_IOR;
         Count : PolyORB.Types.Unsigned_Long)
      is
      begin
         null;
      end Set_Aggregate_Count;

      ---------------------
      -- Unchecked_Get_V --
      ---------------------

      function Unchecked_Get_V
        (Acc : not null access Content_Ü_IOR)
        return PolyORB.Types.Address
      is
         function To_Address
           is new Ada.Unchecked_Conversion
              (Ptr_Ü_IOR,
               PolyORB.Types.Address);

      begin
         return To_Address
           (Acc.V);
      end Unchecked_Get_V;

      -----------
      -- Clone --
      -----------

      function Clone
        (Acc : Content_Ü_IOR;
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
               not in Content_Ü_IOR)
            then
               return null;
            end if;
            Target :=
              Into;
            Content_Ü_IOR
              (Target.all).V.all :=
              Acc.V.all;
         else
            Target :=
              new Content_Ü_IOR;
            Content_Ü_IOR
              (Target.all).V :=
              new IOP.IOR'
                 (Acc.V.all);
         end if;
         return Target;
      end Clone;

      --------------------
      -- Finalize_Value --
      --------------------

      procedure Finalize_Value
        (Acc : in out Content_Ü_IOR)
      is
         procedure Free
           is new PolyORB.Utils.Unchecked_Deallocation.Free

           (Object => IOP.IOR,

            Name   => Ptr_Ü_IOR);

      begin
         Free
           (Acc.V);
      end Finalize_Value;

      ----------
      -- Wrap --
      ----------

      function Wrap
        (X : access IOP.IOR)
        return PolyORB.Any.Content'Class
      is
      begin
         return Content_Ü_IOR'
           (PolyORB.Any.Aggregate_Content with
            V => Ptr_Ü_IOR'
              (X.all'Unchecked_Access));
      end Wrap;

      IOR_Initialized : PolyORB.Std.Boolean :=
        False;

      --------------------
      -- Initialize_IOR --
      --------------------

      procedure Initialize_IOR is
         Name_Ü : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("IOR");
         Id_Ü : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("IDL:omg.org/IOP/IOR:1.0");
         Argument_Name_Ü_type_id : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("type_id");
         Argument_Name_Ü_profiles : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("profiles");
      begin
         if not IOR_Initialized
         then
            IOR_Initialized :=
              True;
            IOP.Helper.TC_IOR :=
              CORBA.TypeCode.Internals.To_CORBA_Object
                 (PolyORB.Any.TypeCode.Tcf_Struct);
            CORBA.Internals.Add_Parameter
              (TC_IOR,
               CORBA.To_Any
                 (Name_Ü));
            CORBA.Internals.Add_Parameter
              (TC_IOR,
               CORBA.To_Any
                 (Id_Ü));
            CORBA.Internals.Add_Parameter
              (TC_IOR,
               CORBA.To_Any
                 (CORBA.TC_String));
            CORBA.Internals.Add_Parameter
              (TC_IOR,
               CORBA.To_Any
                 (Argument_Name_Ü_type_id));
            IOP.Helper.Internals.Initialize_TaggedProfileSeq;
            CORBA.Internals.Add_Parameter
              (TC_IOR,
               CORBA.To_Any
                 (IOP.Helper.TC_TaggedProfileSeq));
            CORBA.Internals.Add_Parameter
              (TC_IOR,
               CORBA.To_Any
                 (Argument_Name_Ü_profiles));
            CORBA.TypeCode.Internals.Disable_Ref_Counting
              (TC_IOR);
            CORBA.TypeCode.Internals.Freeze
              (TC_IOR);
         end if;
      end Initialize_IOR;

      ----------
      -- Wrap --
      ----------

      function Wrap
        (X : access IOP.ComponentId)
        return PolyORB.Any.Content'Class
      is
      begin
         return CORBA.Wrap
           (CORBA.Unsigned_Long
              (X.all)'Unrestricted_Access);
      end Wrap;

      ComponentId_Initialized : PolyORB.Std.Boolean :=
        False;

      ----------------------------
      -- Initialize_ComponentId --
      ----------------------------

      procedure Initialize_ComponentId is
         Name_Ü : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("ComponentId");
         Id_Ü : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("IDL:omg.org/IOP/ComponentId:1.0");
      begin
         if not ComponentId_Initialized
         then
            ComponentId_Initialized :=
              True;
            IOP.Helper.TC_ComponentId :=
              CORBA.TypeCode.Internals.Build_Alias_TC
                 (Name => Name_Ü,
                  Id => Id_Ü,
                  Parent => CORBA.TC_Unsigned_Long);
            CORBA.TypeCode.Internals.Disable_Ref_Counting
              (TC_ComponentId);
            CORBA.TypeCode.Internals.Freeze
              (TC_ComponentId);
         end if;
      end Initialize_ComponentId;

      ComponentData_Initialized : PolyORB.Std.Boolean :=
        False;

      ------------------------------
      -- Initialize_ComponentData --
      ------------------------------

      procedure Initialize_ComponentData is
         Name_Ü : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("ComponentData");
         Id_Ü : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("IDL:omg.org/IOP/ComponentData:1.0");
      begin
         if not ComponentData_Initialized
         then
            ComponentData_Initialized :=
              True;
            IOP.Helper.TC_ComponentData :=
              CORBA.TypeCode.Internals.Build_Alias_TC
                 (Name => Name_Ü,
                  Id => Id_Ü,
                  Parent => CORBA.IDL_Sequences.Helper.TC_OctetSeq);
            CORBA.TypeCode.Internals.Disable_Ref_Counting
              (TC_ComponentData);
            CORBA.TypeCode.Internals.Freeze
              (TC_ComponentData);
         end if;
      end Initialize_ComponentData;

      ---------------------------
      -- Get_Aggregate_Element --
      ---------------------------

      function Get_Aggregate_Element
        (Acc : not null access Content_Ü_TaggedComponent;
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
                    (Acc.V.tag)'Unrestricted_Access);
            when 1 =>
               return CORBA.IDL_Sequences.Helper.Wrap
                 (CORBA.IDL_Sequences.IDL_SEQUENCE_Octet.Sequence
                    (Acc.V.component_data)'Unrestricted_Access);
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
        (Acc : Content_Ü_TaggedComponent)
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
        (Acc : in out Content_Ü_TaggedComponent;
         Count : PolyORB.Types.Unsigned_Long)
      is
      begin
         null;
      end Set_Aggregate_Count;

      ---------------------
      -- Unchecked_Get_V --
      ---------------------

      function Unchecked_Get_V
        (Acc : not null access Content_Ü_TaggedComponent)
        return PolyORB.Types.Address
      is
         function To_Address
           is new Ada.Unchecked_Conversion
              (Ptr_Ü_TaggedComponent,
               PolyORB.Types.Address);

      begin
         return To_Address
           (Acc.V);
      end Unchecked_Get_V;

      -----------
      -- Clone --
      -----------

      function Clone
        (Acc : Content_Ü_TaggedComponent;
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
               not in Content_Ü_TaggedComponent)
            then
               return null;
            end if;
            Target :=
              Into;
            Content_Ü_TaggedComponent
              (Target.all).V.all :=
              Acc.V.all;
         else
            Target :=
              new Content_Ü_TaggedComponent;
            Content_Ü_TaggedComponent
              (Target.all).V :=
              new IOP.TaggedComponent'
                 (Acc.V.all);
         end if;
         return Target;
      end Clone;

      --------------------
      -- Finalize_Value --
      --------------------

      procedure Finalize_Value
        (Acc : in out Content_Ü_TaggedComponent)
      is
         procedure Free
           is new PolyORB.Utils.Unchecked_Deallocation.Free

           (Object => IOP.TaggedComponent,

            Name   => Ptr_Ü_TaggedComponent);

      begin
         Free
           (Acc.V);
      end Finalize_Value;

      ----------
      -- Wrap --
      ----------

      function Wrap
        (X : access IOP.TaggedComponent)
        return PolyORB.Any.Content'Class
      is
      begin
         return Content_Ü_TaggedComponent'
           (PolyORB.Any.Aggregate_Content with
            V => Ptr_Ü_TaggedComponent'
              (X.all'Unchecked_Access));
      end Wrap;

      TaggedComponent_Initialized : PolyORB.Std.Boolean :=
        False;

      --------------------------------
      -- Initialize_TaggedComponent --
      --------------------------------

      procedure Initialize_TaggedComponent is
         Name_Ü : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("TaggedComponent");
         Id_Ü : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("IDL:omg.org/IOP/TaggedComponent:1.0");
         Argument_Name_Ü_tag : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("tag");
         Argument_Name_Ü_component_data : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("component_data");
      begin
         if not TaggedComponent_Initialized
         then
            TaggedComponent_Initialized :=
              True;
            IOP.Helper.TC_TaggedComponent :=
              CORBA.TypeCode.Internals.To_CORBA_Object
                 (PolyORB.Any.TypeCode.Tcf_Struct);
            CORBA.Internals.Add_Parameter
              (TC_TaggedComponent,
               CORBA.To_Any
                 (Name_Ü));
            CORBA.Internals.Add_Parameter
              (TC_TaggedComponent,
               CORBA.To_Any
                 (Id_Ü));
            IOP.Helper.Internals.Initialize_ComponentId;
            CORBA.Internals.Add_Parameter
              (TC_TaggedComponent,
               CORBA.To_Any
                 (IOP.Helper.TC_ComponentId));
            CORBA.Internals.Add_Parameter
              (TC_TaggedComponent,
               CORBA.To_Any
                 (Argument_Name_Ü_tag));
            IOP.Helper.Internals.Initialize_ComponentData;
            CORBA.Internals.Add_Parameter
              (TC_TaggedComponent,
               CORBA.To_Any
                 (IOP.Helper.TC_ComponentData));
            CORBA.Internals.Add_Parameter
              (TC_TaggedComponent,
               CORBA.To_Any
                 (Argument_Name_Ü_component_data));
            CORBA.TypeCode.Internals.Disable_Ref_Counting
              (TC_TaggedComponent);
            CORBA.TypeCode.Internals.Freeze
              (TC_TaggedComponent);
         end if;
      end Initialize_TaggedComponent;

      ---------------------------------------------------
      -- IDL_SEQUENCE_IOP_TaggedComponent_Element_Wrap --
      ---------------------------------------------------

      function IDL_SEQUENCE_IOP_TaggedComponent_Element_Wrap
        (X : access IOP.TaggedComponent)
        return PolyORB.Any.Content'Class
      is
      begin
         return IOP.Helper.Internals.Wrap
           (X.all'Unrestricted_Access);
      end IDL_SEQUENCE_IOP_TaggedComponent_Element_Wrap;

      function Wrap
        (X : access IOP.IDL_SEQUENCE_IOP_TaggedComponent.Sequence)
        return PolyORB.Any.Content'Class
        renames IDL_SEQUENCE_IOP_TaggedComponent_Helper.Wrap;

      IDL_SEQUENCE_IOP_TaggedComponent_Initialized : PolyORB.Std.Boolean :=
        False;

      -------------------------------------------------
      -- Initialize_IDL_SEQUENCE_IOP_TaggedComponent --
      -------------------------------------------------

      procedure Initialize_IDL_SEQUENCE_IOP_TaggedComponent is
      begin
         if not IDL_SEQUENCE_IOP_TaggedComponent_Initialized
         then
            IDL_SEQUENCE_IOP_TaggedComponent_Initialized :=
              True;
            IOP.Helper.Internals.Initialize_TaggedComponent;
            IOP.Helper.TC_IDL_SEQUENCE_IOP_TaggedComponent :=
              CORBA.TypeCode.Internals.Build_Sequence_TC
                 (IOP.Helper.TC_TaggedComponent,
                  0);
            CORBA.TypeCode.Internals.Disable_Ref_Counting
              (IOP.Helper.TC_IDL_SEQUENCE_IOP_TaggedComponent);
            IDL_SEQUENCE_IOP_TaggedComponent_Helper.Initialize
              (Element_TC => IOP.Helper.TC_TaggedComponent,
               Sequence_TC => IOP.Helper.TC_IDL_SEQUENCE_IOP_TaggedComponent);
            CORBA.TypeCode.Internals.Freeze
              (TC_IDL_SEQUENCE_IOP_TaggedComponent);
         end if;
      end Initialize_IDL_SEQUENCE_IOP_TaggedComponent;

      TaggedComponentSeq_Initialized : PolyORB.Std.Boolean :=
        False;

      -----------------------------------
      -- Initialize_TaggedComponentSeq --
      -----------------------------------

      procedure Initialize_TaggedComponentSeq is
         Name_Ü : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("TaggedComponentSeq");
         Id_Ü : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("IDL:omg.org/IOP/TaggedComponentSeq:1.0");
      begin
         if not TaggedComponentSeq_Initialized
         then
            TaggedComponentSeq_Initialized :=
              True;
            IOP.Helper.Internals.Initialize_IDL_SEQUENCE_IOP_TaggedComponent;
            IOP.Helper.TC_TaggedComponentSeq :=
              CORBA.TypeCode.Internals.Build_Alias_TC
                 (Name => Name_Ü,
                  Id => Id_Ü,
                  Parent => IOP.Helper.TC_IDL_SEQUENCE_IOP_TaggedComponent);
            CORBA.TypeCode.Internals.Disable_Ref_Counting
              (TC_TaggedComponentSeq);
            CORBA.TypeCode.Internals.Freeze
              (TC_TaggedComponentSeq);
         end if;
      end Initialize_TaggedComponentSeq;

      ObjectKey_Initialized : PolyORB.Std.Boolean :=
        False;

      --------------------------
      -- Initialize_ObjectKey --
      --------------------------

      procedure Initialize_ObjectKey is
         Name_Ü : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("ObjectKey");
         Id_Ü : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("IDL:omg.org/IOP/ObjectKey:1.0");
      begin
         if not ObjectKey_Initialized
         then
            ObjectKey_Initialized :=
              True;
            IOP.Helper.TC_ObjectKey :=
              CORBA.TypeCode.Internals.Build_Alias_TC
                 (Name => Name_Ü,
                  Id => Id_Ü,
                  Parent => CORBA.IDL_Sequences.Helper.TC_OctetSeq);
            CORBA.TypeCode.Internals.Disable_Ref_Counting
              (TC_ObjectKey);
            CORBA.TypeCode.Internals.Freeze
              (TC_ObjectKey);
         end if;
      end Initialize_ObjectKey;

      -----------------------------------------------------
      -- IDL_SEQUENCE_IOP_TaggedComponent_1_Element_Wrap --
      -----------------------------------------------------

      function IDL_SEQUENCE_IOP_TaggedComponent_1_Element_Wrap
        (X : access IOP.TaggedComponent)
        return PolyORB.Any.Content'Class
      is
      begin
         return IOP.Helper.Internals.Wrap
           (X.all'Unrestricted_Access);
      end IDL_SEQUENCE_IOP_TaggedComponent_1_Element_Wrap;

      function Wrap
        (X : access IOP.IDL_SEQUENCE_IOP_TaggedComponent_1.Sequence)
        return PolyORB.Any.Content'Class
        renames IDL_SEQUENCE_IOP_TaggedComponent_1_Helper.Wrap;

      IDL_SEQUENCE_IOP_TaggedComponent_1_Initialized : PolyORB.Std.Boolean :=
        False;

      ---------------------------------------------------
      -- Initialize_IDL_SEQUENCE_IOP_TaggedComponent_1 --
      ---------------------------------------------------

      procedure Initialize_IDL_SEQUENCE_IOP_TaggedComponent_1 is
      begin
         if not IDL_SEQUENCE_IOP_TaggedComponent_1_Initialized
         then
            IDL_SEQUENCE_IOP_TaggedComponent_1_Initialized :=
              True;
            IOP.Helper.Internals.Initialize_TaggedComponent;
            IOP.Helper.TC_IDL_SEQUENCE_IOP_TaggedComponent_1 :=
              CORBA.TypeCode.Internals.Build_Sequence_TC
                 (IOP.Helper.TC_TaggedComponent,
                  0);
            CORBA.TypeCode.Internals.Disable_Ref_Counting
              (IOP.Helper.TC_IDL_SEQUENCE_IOP_TaggedComponent_1);
            IDL_SEQUENCE_IOP_TaggedComponent_1_Helper.Initialize
              (Element_TC => IOP.Helper.TC_TaggedComponent,
               Sequence_TC => IOP.Helper.TC_IDL_SEQUENCE_IOP_TaggedComponent_1);
            CORBA.TypeCode.Internals.Freeze
              (TC_IDL_SEQUENCE_IOP_TaggedComponent_1);
         end if;
      end Initialize_IDL_SEQUENCE_IOP_TaggedComponent_1;

      MultipleComponentProfile_Initialized : PolyORB.Std.Boolean :=
        False;

      -----------------------------------------
      -- Initialize_MultipleComponentProfile --
      -----------------------------------------

      procedure Initialize_MultipleComponentProfile is
         Name_Ü : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("MultipleComponentProfile");
         Id_Ü : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("IDL:omg.org/IOP/MultipleComponentProfile:1.0");
      begin
         if not MultipleComponentProfile_Initialized
         then
            MultipleComponentProfile_Initialized :=
              True;
            IOP.Helper.Internals.Initialize_IDL_SEQUENCE_IOP_TaggedComponent_1;
            IOP.Helper.TC_MultipleComponentProfile :=
              CORBA.TypeCode.Internals.Build_Alias_TC
                 (Name => Name_Ü,
                  Id => Id_Ü,
                  Parent => IOP.Helper.TC_IDL_SEQUENCE_IOP_TaggedComponent_1);
            CORBA.TypeCode.Internals.Disable_Ref_Counting
              (TC_MultipleComponentProfile);
            CORBA.TypeCode.Internals.Freeze
              (TC_MultipleComponentProfile);
         end if;
      end Initialize_MultipleComponentProfile;

      ContextData_Initialized : PolyORB.Std.Boolean :=
        False;

      ----------------------------
      -- Initialize_ContextData --
      ----------------------------

      procedure Initialize_ContextData is
         Name_Ü : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("ContextData");
         Id_Ü : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("IDL:omg.org/IOP/ContextData:1.0");
      begin
         if not ContextData_Initialized
         then
            ContextData_Initialized :=
              True;
            IOP.Helper.TC_ContextData :=
              CORBA.TypeCode.Internals.Build_Alias_TC
                 (Name => Name_Ü,
                  Id => Id_Ü,
                  Parent => CORBA.IDL_Sequences.Helper.TC_OctetSeq);
            CORBA.TypeCode.Internals.Disable_Ref_Counting
              (TC_ContextData);
            CORBA.TypeCode.Internals.Freeze
              (TC_ContextData);
         end if;
      end Initialize_ContextData;

      ----------
      -- Wrap --
      ----------

      function Wrap
        (X : access IOP.ServiceId)
        return PolyORB.Any.Content'Class
      is
      begin
         return CORBA.Wrap
           (CORBA.Unsigned_Long
              (X.all)'Unrestricted_Access);
      end Wrap;

      ServiceId_Initialized : PolyORB.Std.Boolean :=
        False;

      --------------------------
      -- Initialize_ServiceId --
      --------------------------

      procedure Initialize_ServiceId is
         Name_Ü : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("ServiceId");
         Id_Ü : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("IDL:omg.org/IOP/ServiceId:1.0");
      begin
         if not ServiceId_Initialized
         then
            ServiceId_Initialized :=
              True;
            IOP.Helper.TC_ServiceId :=
              CORBA.TypeCode.Internals.Build_Alias_TC
                 (Name => Name_Ü,
                  Id => Id_Ü,
                  Parent => CORBA.TC_Unsigned_Long);
            CORBA.TypeCode.Internals.Disable_Ref_Counting
              (TC_ServiceId);
            CORBA.TypeCode.Internals.Freeze
              (TC_ServiceId);
         end if;
      end Initialize_ServiceId;

      ---------------------------
      -- Get_Aggregate_Element --
      ---------------------------

      function Get_Aggregate_Element
        (Acc : not null access Content_Ü_ServiceContext;
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
                    (Acc.V.context_id)'Unrestricted_Access);
            when 1 =>
               return CORBA.IDL_Sequences.Helper.Wrap
                 (CORBA.IDL_Sequences.IDL_SEQUENCE_Octet.Sequence
                    (Acc.V.context_data)'Unrestricted_Access);
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
        (Acc : Content_Ü_ServiceContext)
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
        (Acc : in out Content_Ü_ServiceContext;
         Count : PolyORB.Types.Unsigned_Long)
      is
      begin
         null;
      end Set_Aggregate_Count;

      ---------------------
      -- Unchecked_Get_V --
      ---------------------

      function Unchecked_Get_V
        (Acc : not null access Content_Ü_ServiceContext)
        return PolyORB.Types.Address
      is
         function To_Address
           is new Ada.Unchecked_Conversion
              (Ptr_Ü_ServiceContext,
               PolyORB.Types.Address);

      begin
         return To_Address
           (Acc.V);
      end Unchecked_Get_V;

      -----------
      -- Clone --
      -----------

      function Clone
        (Acc : Content_Ü_ServiceContext;
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
               not in Content_Ü_ServiceContext)
            then
               return null;
            end if;
            Target :=
              Into;
            Content_Ü_ServiceContext
              (Target.all).V.all :=
              Acc.V.all;
         else
            Target :=
              new Content_Ü_ServiceContext;
            Content_Ü_ServiceContext
              (Target.all).V :=
              new IOP.ServiceContext'
                 (Acc.V.all);
         end if;
         return Target;
      end Clone;

      --------------------
      -- Finalize_Value --
      --------------------

      procedure Finalize_Value
        (Acc : in out Content_Ü_ServiceContext)
      is
         procedure Free
           is new PolyORB.Utils.Unchecked_Deallocation.Free

           (Object => IOP.ServiceContext,

            Name   => Ptr_Ü_ServiceContext);

      begin
         Free
           (Acc.V);
      end Finalize_Value;

      ----------
      -- Wrap --
      ----------

      function Wrap
        (X : access IOP.ServiceContext)
        return PolyORB.Any.Content'Class
      is
      begin
         return Content_Ü_ServiceContext'
           (PolyORB.Any.Aggregate_Content with
            V => Ptr_Ü_ServiceContext'
              (X.all'Unchecked_Access));
      end Wrap;

      ServiceContext_Initialized : PolyORB.Std.Boolean :=
        False;

      -------------------------------
      -- Initialize_ServiceContext --
      -------------------------------

      procedure Initialize_ServiceContext is
         Name_Ü : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("ServiceContext");
         Id_Ü : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("IDL:omg.org/IOP/ServiceContext:1.0");
         Argument_Name_Ü_context_id : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("context_id");
         Argument_Name_Ü_context_data : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("context_data");
      begin
         if not ServiceContext_Initialized
         then
            ServiceContext_Initialized :=
              True;
            IOP.Helper.TC_ServiceContext :=
              CORBA.TypeCode.Internals.To_CORBA_Object
                 (PolyORB.Any.TypeCode.Tcf_Struct);
            CORBA.Internals.Add_Parameter
              (TC_ServiceContext,
               CORBA.To_Any
                 (Name_Ü));
            CORBA.Internals.Add_Parameter
              (TC_ServiceContext,
               CORBA.To_Any
                 (Id_Ü));
            IOP.Helper.Internals.Initialize_ServiceId;
            CORBA.Internals.Add_Parameter
              (TC_ServiceContext,
               CORBA.To_Any
                 (IOP.Helper.TC_ServiceId));
            CORBA.Internals.Add_Parameter
              (TC_ServiceContext,
               CORBA.To_Any
                 (Argument_Name_Ü_context_id));
            IOP.Helper.Internals.Initialize_ContextData;
            CORBA.Internals.Add_Parameter
              (TC_ServiceContext,
               CORBA.To_Any
                 (IOP.Helper.TC_ContextData));
            CORBA.Internals.Add_Parameter
              (TC_ServiceContext,
               CORBA.To_Any
                 (Argument_Name_Ü_context_data));
            CORBA.TypeCode.Internals.Disable_Ref_Counting
              (TC_ServiceContext);
            CORBA.TypeCode.Internals.Freeze
              (TC_ServiceContext);
         end if;
      end Initialize_ServiceContext;

      --------------------------------------------------
      -- IDL_SEQUENCE_IOP_ServiceContext_Element_Wrap --
      --------------------------------------------------

      function IDL_SEQUENCE_IOP_ServiceContext_Element_Wrap
        (X : access IOP.ServiceContext)
        return PolyORB.Any.Content'Class
      is
      begin
         return IOP.Helper.Internals.Wrap
           (X.all'Unrestricted_Access);
      end IDL_SEQUENCE_IOP_ServiceContext_Element_Wrap;

      function Wrap
        (X : access IOP.IDL_SEQUENCE_IOP_ServiceContext.Sequence)
        return PolyORB.Any.Content'Class
        renames IDL_SEQUENCE_IOP_ServiceContext_Helper.Wrap;

      IDL_SEQUENCE_IOP_ServiceContext_Initialized : PolyORB.Std.Boolean :=
        False;

      ------------------------------------------------
      -- Initialize_IDL_SEQUENCE_IOP_ServiceContext --
      ------------------------------------------------

      procedure Initialize_IDL_SEQUENCE_IOP_ServiceContext is
      begin
         if not IDL_SEQUENCE_IOP_ServiceContext_Initialized
         then
            IDL_SEQUENCE_IOP_ServiceContext_Initialized :=
              True;
            IOP.Helper.Internals.Initialize_ServiceContext;
            IOP.Helper.TC_IDL_SEQUENCE_IOP_ServiceContext :=
              CORBA.TypeCode.Internals.Build_Sequence_TC
                 (IOP.Helper.TC_ServiceContext,
                  0);
            CORBA.TypeCode.Internals.Disable_Ref_Counting
              (IOP.Helper.TC_IDL_SEQUENCE_IOP_ServiceContext);
            IDL_SEQUENCE_IOP_ServiceContext_Helper.Initialize
              (Element_TC => IOP.Helper.TC_ServiceContext,
               Sequence_TC => IOP.Helper.TC_IDL_SEQUENCE_IOP_ServiceContext);
            CORBA.TypeCode.Internals.Freeze
              (TC_IDL_SEQUENCE_IOP_ServiceContext);
         end if;
      end Initialize_IDL_SEQUENCE_IOP_ServiceContext;

      ServiceContextList_Initialized : PolyORB.Std.Boolean :=
        False;

      -----------------------------------
      -- Initialize_ServiceContextList --
      -----------------------------------

      procedure Initialize_ServiceContextList is
         Name_Ü : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("ServiceContextList");
         Id_Ü : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("IDL:omg.org/IOP/ServiceContextList:1.0");
      begin
         if not ServiceContextList_Initialized
         then
            ServiceContextList_Initialized :=
              True;
            IOP.Helper.Internals.Initialize_IDL_SEQUENCE_IOP_ServiceContext;
            IOP.Helper.TC_ServiceContextList :=
              CORBA.TypeCode.Internals.Build_Alias_TC
                 (Name => Name_Ü,
                  Id => Id_Ü,
                  Parent => IOP.Helper.TC_IDL_SEQUENCE_IOP_ServiceContext);
            CORBA.TypeCode.Internals.Disable_Ref_Counting
              (TC_ServiceContextList);
            CORBA.TypeCode.Internals.Freeze
              (TC_ServiceContextList);
         end if;
      end Initialize_ServiceContextList;

      ----------
      -- Wrap --
      ----------

      function Wrap
        (X : access IOP.EncodingFormat)
        return PolyORB.Any.Content'Class
      is
      begin
         return CORBA.Wrap
           (CORBA.Short
              (X.all)'Unrestricted_Access);
      end Wrap;

      EncodingFormat_Initialized : PolyORB.Std.Boolean :=
        False;

      -------------------------------
      -- Initialize_EncodingFormat --
      -------------------------------

      procedure Initialize_EncodingFormat is
         Name_Ü : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("EncodingFormat");
         Id_Ü : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("IDL:omg.org/IOP/EncodingFormat:1.0");
      begin
         if not EncodingFormat_Initialized
         then
            EncodingFormat_Initialized :=
              True;
            IOP.Helper.TC_EncodingFormat :=
              CORBA.TypeCode.Internals.Build_Alias_TC
                 (Name => Name_Ü,
                  Id => Id_Ü,
                  Parent => CORBA.TC_Short);
            CORBA.TypeCode.Internals.Disable_Ref_Counting
              (TC_EncodingFormat);
            CORBA.TypeCode.Internals.Freeze
              (TC_EncodingFormat);
         end if;
      end Initialize_EncodingFormat;

      ---------------------------
      -- Get_Aggregate_Element --
      ---------------------------

      function Get_Aggregate_Element
        (Acc : not null access Content_Ü_Encoding;
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
                    (Acc.V.format)'Unrestricted_Access);
            when 1 =>
               return CORBA.Wrap
                 (Acc.V.major_version'Unrestricted_Access);
            when 2 =>
               return CORBA.Wrap
                 (Acc.V.minor_version'Unrestricted_Access);
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
        (Acc : Content_Ü_Encoding)
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
        (Acc : in out Content_Ü_Encoding;
         Count : PolyORB.Types.Unsigned_Long)
      is
      begin
         null;
      end Set_Aggregate_Count;

      ---------------------
      -- Unchecked_Get_V --
      ---------------------

      function Unchecked_Get_V
        (Acc : not null access Content_Ü_Encoding)
        return PolyORB.Types.Address
      is
         function To_Address
           is new Ada.Unchecked_Conversion
              (Ptr_Ü_Encoding,
               PolyORB.Types.Address);

      begin
         return To_Address
           (Acc.V);
      end Unchecked_Get_V;

      -----------
      -- Clone --
      -----------

      function Clone
        (Acc : Content_Ü_Encoding;
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
               not in Content_Ü_Encoding)
            then
               return null;
            end if;
            Target :=
              Into;
            Content_Ü_Encoding
              (Target.all).V.all :=
              Acc.V.all;
         else
            Target :=
              new Content_Ü_Encoding;
            Content_Ü_Encoding
              (Target.all).V :=
              new IOP.Encoding'
                 (Acc.V.all);
         end if;
         return Target;
      end Clone;

      --------------------
      -- Finalize_Value --
      --------------------

      procedure Finalize_Value
        (Acc : in out Content_Ü_Encoding)
      is
         procedure Free
           is new PolyORB.Utils.Unchecked_Deallocation.Free

           (Object => IOP.Encoding,

            Name   => Ptr_Ü_Encoding);

      begin
         Free
           (Acc.V);
      end Finalize_Value;

      ----------
      -- Wrap --
      ----------

      function Wrap
        (X : access IOP.Encoding)
        return PolyORB.Any.Content'Class
      is
      begin
         return Content_Ü_Encoding'
           (PolyORB.Any.Aggregate_Content with
            V => Ptr_Ü_Encoding'
              (X.all'Unchecked_Access));
      end Wrap;

      Encoding_Initialized : PolyORB.Std.Boolean :=
        False;

      -------------------------
      -- Initialize_Encoding --
      -------------------------

      procedure Initialize_Encoding is
         Name_Ü : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("Encoding");
         Id_Ü : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("IDL:omg.org/IOP/Encoding:1.0");
         Argument_Name_Ü_format : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("format");
         Argument_Name_Ü_major_version : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("major_version");
         Argument_Name_Ü_minor_version : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("minor_version");
      begin
         if not Encoding_Initialized
         then
            Encoding_Initialized :=
              True;
            IOP.Helper.TC_Encoding :=
              CORBA.TypeCode.Internals.To_CORBA_Object
                 (PolyORB.Any.TypeCode.Tcf_Struct);
            CORBA.Internals.Add_Parameter
              (TC_Encoding,
               CORBA.To_Any
                 (Name_Ü));
            CORBA.Internals.Add_Parameter
              (TC_Encoding,
               CORBA.To_Any
                 (Id_Ü));
            IOP.Helper.Internals.Initialize_EncodingFormat;
            CORBA.Internals.Add_Parameter
              (TC_Encoding,
               CORBA.To_Any
                 (IOP.Helper.TC_EncodingFormat));
            CORBA.Internals.Add_Parameter
              (TC_Encoding,
               CORBA.To_Any
                 (Argument_Name_Ü_format));
            CORBA.Internals.Add_Parameter
              (TC_Encoding,
               CORBA.To_Any
                 (CORBA.TC_Octet));
            CORBA.Internals.Add_Parameter
              (TC_Encoding,
               CORBA.To_Any
                 (Argument_Name_Ü_major_version));
            CORBA.Internals.Add_Parameter
              (TC_Encoding,
               CORBA.To_Any
                 (CORBA.TC_Octet));
            CORBA.Internals.Add_Parameter
              (TC_Encoding,
               CORBA.To_Any
                 (Argument_Name_Ü_minor_version));
            CORBA.TypeCode.Internals.Disable_Ref_Counting
              (TC_Encoding);
            CORBA.TypeCode.Internals.Freeze
              (TC_Encoding);
         end if;
      end Initialize_Encoding;

   end Internals;

   --------------
   -- From_Any --
   --------------

   function From_Any
     (Item : CORBA.Any)
     return IOP.ProfileId
   is
      Result : constant CORBA.Unsigned_Long :=
        CORBA.From_Any
           (Item);
   begin
      return IOP.ProfileId
        (Result);
   end From_Any;

   ------------
   -- To_Any --
   ------------

   function To_Any
     (Item : IOP.ProfileId)
     return CORBA.Any
   is
      Result : CORBA.Any :=
        CORBA.To_Any
           (CORBA.Unsigned_Long
              (Item));
   begin
      CORBA.Internals.Set_Type
        (Result,
         TC_ProfileId);
      return Result;
   end To_Any;

   --------------
   -- From_Any --
   --------------

   function From_Any
     (Item : CORBA.Any)
     return IOP.ProfileData
   is
      Result : constant CORBA.IDL_Sequences.OctetSeq :=
        CORBA.IDL_Sequences.Helper.From_Any
           (Item);
   begin
      return IOP.ProfileData
        (Result);
   end From_Any;

   ------------
   -- To_Any --
   ------------

   function To_Any
     (Item : IOP.ProfileData)
     return CORBA.Any
   is
      Result : CORBA.Any :=
        CORBA.IDL_Sequences.Helper.To_Any
           (CORBA.IDL_Sequences.OctetSeq
              (Item));
   begin
      CORBA.Internals.Set_Type
        (Result,
         TC_ProfileData);
      return Result;
   end To_Any;

   --------------
   -- From_Any --
   --------------

   function From_Any
     (Item : CORBA.Any)
     return IOP.TaggedProfile
   is
   begin
      return (tag => IOP.Helper.From_Any
        (CORBA.Internals.Get_Aggregate_Element
           (Item,
            IOP.Helper.TC_ProfileId,
            0)),
      profile_data => IOP.Helper.From_Any
        (CORBA.Internals.Get_Aggregate_Element
           (Item,
            IOP.Helper.TC_ProfileData,
            1)));
   end From_Any;

   ------------
   -- To_Any --
   ------------

   function To_Any
     (Item : IOP.TaggedProfile)
     return CORBA.Any
   is
      Result : constant CORBA.Any :=
        CORBA.Internals.Get_Empty_Any
           (TC_TaggedProfile);
   begin
      PolyORB.Any.Set_Value
        (CORBA.Get_Container
           (Result).all,
         new PolyORB.Any.Content'Class'
           (IOP.Helper.Internals.Wrap
              (new IOP.TaggedProfile'
                 (Item))),
         False);
      return Result;
   end To_Any;

   function From_Any
     (Item : CORBA.Any)
     return IOP.IDL_SEQUENCE_IOP_TaggedProfile.Sequence
     renames IOP.Helper.Internals.IDL_SEQUENCE_IOP_TaggedProfile_Helper.From_Any;

   function To_Any
     (Item : IOP.IDL_SEQUENCE_IOP_TaggedProfile.Sequence)
     return CORBA.Any
     renames IOP.Helper.Internals.IDL_SEQUENCE_IOP_TaggedProfile_Helper.To_Any;

   --------------
   -- From_Any --
   --------------

   function From_Any
     (Item : CORBA.Any)
     return IOP.TaggedProfileSeq
   is
      Result : constant IOP.IDL_SEQUENCE_IOP_TaggedProfile.Sequence :=
        IOP.Helper.From_Any
           (Item);
   begin
      return IOP.TaggedProfileSeq
        (Result);
   end From_Any;

   ------------
   -- To_Any --
   ------------

   function To_Any
     (Item : IOP.TaggedProfileSeq)
     return CORBA.Any
   is
      Result : CORBA.Any :=
        IOP.Helper.To_Any
           (IOP.IDL_SEQUENCE_IOP_TaggedProfile.Sequence
              (Item));
   begin
      CORBA.Internals.Set_Type
        (Result,
         TC_TaggedProfileSeq);
      return Result;
   end To_Any;

   --------------
   -- From_Any --
   --------------

   function From_Any
     (Item : CORBA.Any)
     return IOP.IOR
   is
   begin
      return (type_id => CORBA.From_Any
        (CORBA.Internals.Get_Aggregate_Element
           (Item,
            CORBA.TC_String,
            0)),
      profiles => IOP.Helper.From_Any
        (CORBA.Internals.Get_Aggregate_Element
           (Item,
            IOP.Helper.TC_TaggedProfileSeq,
            1)));
   end From_Any;

   ------------
   -- To_Any --
   ------------

   function To_Any
     (Item : IOP.IOR)
     return CORBA.Any
   is
      Result : constant CORBA.Any :=
        CORBA.Internals.Get_Empty_Any
           (TC_IOR);
   begin
      PolyORB.Any.Set_Value
        (CORBA.Get_Container
           (Result).all,
         new PolyORB.Any.Content'Class'
           (IOP.Helper.Internals.Wrap
              (new IOP.IOR'
                 (Item))),
         False);
      return Result;
   end To_Any;

   --------------
   -- From_Any --
   --------------

   function From_Any
     (Item : CORBA.Any)
     return IOP.ComponentId
   is
      Result : constant CORBA.Unsigned_Long :=
        CORBA.From_Any
           (Item);
   begin
      return IOP.ComponentId
        (Result);
   end From_Any;

   ------------
   -- To_Any --
   ------------

   function To_Any
     (Item : IOP.ComponentId)
     return CORBA.Any
   is
      Result : CORBA.Any :=
        CORBA.To_Any
           (CORBA.Unsigned_Long
              (Item));
   begin
      CORBA.Internals.Set_Type
        (Result,
         TC_ComponentId);
      return Result;
   end To_Any;

   --------------
   -- From_Any --
   --------------

   function From_Any
     (Item : CORBA.Any)
     return IOP.ComponentData
   is
      Result : constant CORBA.IDL_Sequences.OctetSeq :=
        CORBA.IDL_Sequences.Helper.From_Any
           (Item);
   begin
      return IOP.ComponentData
        (Result);
   end From_Any;

   ------------
   -- To_Any --
   ------------

   function To_Any
     (Item : IOP.ComponentData)
     return CORBA.Any
   is
      Result : CORBA.Any :=
        CORBA.IDL_Sequences.Helper.To_Any
           (CORBA.IDL_Sequences.OctetSeq
              (Item));
   begin
      CORBA.Internals.Set_Type
        (Result,
         TC_ComponentData);
      return Result;
   end To_Any;

   --------------
   -- From_Any --
   --------------

   function From_Any
     (Item : CORBA.Any)
     return IOP.TaggedComponent
   is
   begin
      return (tag => IOP.Helper.From_Any
        (CORBA.Internals.Get_Aggregate_Element
           (Item,
            IOP.Helper.TC_ComponentId,
            0)),
      component_data => IOP.Helper.From_Any
        (CORBA.Internals.Get_Aggregate_Element
           (Item,
            IOP.Helper.TC_ComponentData,
            1)));
   end From_Any;

   ------------
   -- To_Any --
   ------------

   function To_Any
     (Item : IOP.TaggedComponent)
     return CORBA.Any
   is
      Result : constant CORBA.Any :=
        CORBA.Internals.Get_Empty_Any
           (TC_TaggedComponent);
   begin
      PolyORB.Any.Set_Value
        (CORBA.Get_Container
           (Result).all,
         new PolyORB.Any.Content'Class'
           (IOP.Helper.Internals.Wrap
              (new IOP.TaggedComponent'
                 (Item))),
         False);
      return Result;
   end To_Any;

   function From_Any
     (Item : CORBA.Any)
     return IOP.IDL_SEQUENCE_IOP_TaggedComponent.Sequence
     renames IOP.Helper.Internals.IDL_SEQUENCE_IOP_TaggedComponent_Helper.From_Any;

   function To_Any
     (Item : IOP.IDL_SEQUENCE_IOP_TaggedComponent.Sequence)
     return CORBA.Any
     renames IOP.Helper.Internals.IDL_SEQUENCE_IOP_TaggedComponent_Helper.To_Any;

   --------------
   -- From_Any --
   --------------

   function From_Any
     (Item : CORBA.Any)
     return IOP.TaggedComponentSeq
   is
      Result : constant IOP.IDL_SEQUENCE_IOP_TaggedComponent.Sequence :=
        IOP.Helper.From_Any
           (Item);
   begin
      return IOP.TaggedComponentSeq
        (Result);
   end From_Any;

   ------------
   -- To_Any --
   ------------

   function To_Any
     (Item : IOP.TaggedComponentSeq)
     return CORBA.Any
   is
      Result : CORBA.Any :=
        IOP.Helper.To_Any
           (IOP.IDL_SEQUENCE_IOP_TaggedComponent.Sequence
              (Item));
   begin
      CORBA.Internals.Set_Type
        (Result,
         TC_TaggedComponentSeq);
      return Result;
   end To_Any;

   --------------
   -- From_Any --
   --------------

   function From_Any
     (Item : CORBA.Any)
     return IOP.ObjectKey
   is
      Result : constant CORBA.IDL_Sequences.OctetSeq :=
        CORBA.IDL_Sequences.Helper.From_Any
           (Item);
   begin
      return IOP.ObjectKey
        (Result);
   end From_Any;

   ------------
   -- To_Any --
   ------------

   function To_Any
     (Item : IOP.ObjectKey)
     return CORBA.Any
   is
      Result : CORBA.Any :=
        CORBA.IDL_Sequences.Helper.To_Any
           (CORBA.IDL_Sequences.OctetSeq
              (Item));
   begin
      CORBA.Internals.Set_Type
        (Result,
         TC_ObjectKey);
      return Result;
   end To_Any;

   function From_Any
     (Item : CORBA.Any)
     return IOP.IDL_SEQUENCE_IOP_TaggedComponent_1.Sequence
     renames IOP.Helper.Internals.IDL_SEQUENCE_IOP_TaggedComponent_1_Helper.From_Any;

   function To_Any
     (Item : IOP.IDL_SEQUENCE_IOP_TaggedComponent_1.Sequence)
     return CORBA.Any
     renames IOP.Helper.Internals.IDL_SEQUENCE_IOP_TaggedComponent_1_Helper.To_Any;

   --------------
   -- From_Any --
   --------------

   function From_Any
     (Item : CORBA.Any)
     return IOP.MultipleComponentProfile
   is
      Result : constant IOP.IDL_SEQUENCE_IOP_TaggedComponent_1.Sequence :=
        IOP.Helper.From_Any
           (Item);
   begin
      return IOP.MultipleComponentProfile
        (Result);
   end From_Any;

   ------------
   -- To_Any --
   ------------

   function To_Any
     (Item : IOP.MultipleComponentProfile)
     return CORBA.Any
   is
      Result : CORBA.Any :=
        IOP.Helper.To_Any
           (IOP.IDL_SEQUENCE_IOP_TaggedComponent_1.Sequence
              (Item));
   begin
      CORBA.Internals.Set_Type
        (Result,
         TC_MultipleComponentProfile);
      return Result;
   end To_Any;

   --------------
   -- From_Any --
   --------------

   function From_Any
     (Item : CORBA.Any)
     return IOP.ContextData
   is
      Result : constant CORBA.IDL_Sequences.OctetSeq :=
        CORBA.IDL_Sequences.Helper.From_Any
           (Item);
   begin
      return IOP.ContextData
        (Result);
   end From_Any;

   ------------
   -- To_Any --
   ------------

   function To_Any
     (Item : IOP.ContextData)
     return CORBA.Any
   is
      Result : CORBA.Any :=
        CORBA.IDL_Sequences.Helper.To_Any
           (CORBA.IDL_Sequences.OctetSeq
              (Item));
   begin
      CORBA.Internals.Set_Type
        (Result,
         TC_ContextData);
      return Result;
   end To_Any;

   --------------
   -- From_Any --
   --------------

   function From_Any
     (Item : CORBA.Any)
     return IOP.ServiceId
   is
      Result : constant CORBA.Unsigned_Long :=
        CORBA.From_Any
           (Item);
   begin
      return IOP.ServiceId
        (Result);
   end From_Any;

   ------------
   -- To_Any --
   ------------

   function To_Any
     (Item : IOP.ServiceId)
     return CORBA.Any
   is
      Result : CORBA.Any :=
        CORBA.To_Any
           (CORBA.Unsigned_Long
              (Item));
   begin
      CORBA.Internals.Set_Type
        (Result,
         TC_ServiceId);
      return Result;
   end To_Any;

   --------------
   -- From_Any --
   --------------

   function From_Any
     (Item : CORBA.Any)
     return IOP.ServiceContext
   is
   begin
      return (context_id => IOP.Helper.From_Any
        (CORBA.Internals.Get_Aggregate_Element
           (Item,
            IOP.Helper.TC_ServiceId,
            0)),
      context_data => IOP.Helper.From_Any
        (CORBA.Internals.Get_Aggregate_Element
           (Item,
            IOP.Helper.TC_ContextData,
            1)));
   end From_Any;

   ------------
   -- To_Any --
   ------------

   function To_Any
     (Item : IOP.ServiceContext)
     return CORBA.Any
   is
      Result : constant CORBA.Any :=
        CORBA.Internals.Get_Empty_Any
           (TC_ServiceContext);
   begin
      PolyORB.Any.Set_Value
        (CORBA.Get_Container
           (Result).all,
         new PolyORB.Any.Content'Class'
           (IOP.Helper.Internals.Wrap
              (new IOP.ServiceContext'
                 (Item))),
         False);
      return Result;
   end To_Any;

   function From_Any
     (Item : CORBA.Any)
     return IOP.IDL_SEQUENCE_IOP_ServiceContext.Sequence
     renames IOP.Helper.Internals.IDL_SEQUENCE_IOP_ServiceContext_Helper.From_Any;

   function To_Any
     (Item : IOP.IDL_SEQUENCE_IOP_ServiceContext.Sequence)
     return CORBA.Any
     renames IOP.Helper.Internals.IDL_SEQUENCE_IOP_ServiceContext_Helper.To_Any;

   --------------
   -- From_Any --
   --------------

   function From_Any
     (Item : CORBA.Any)
     return IOP.ServiceContextList
   is
      Result : constant IOP.IDL_SEQUENCE_IOP_ServiceContext.Sequence :=
        IOP.Helper.From_Any
           (Item);
   begin
      return IOP.ServiceContextList
        (Result);
   end From_Any;

   ------------
   -- To_Any --
   ------------

   function To_Any
     (Item : IOP.ServiceContextList)
     return CORBA.Any
   is
      Result : CORBA.Any :=
        IOP.Helper.To_Any
           (IOP.IDL_SEQUENCE_IOP_ServiceContext.Sequence
              (Item));
   begin
      CORBA.Internals.Set_Type
        (Result,
         TC_ServiceContextList);
      return Result;
   end To_Any;

   --------------
   -- From_Any --
   --------------

   function From_Any
     (Item : CORBA.Any)
     return IOP.EncodingFormat
   is
      Result : constant CORBA.Short :=
        CORBA.From_Any
           (Item);
   begin
      return IOP.EncodingFormat
        (Result);
   end From_Any;

   ------------
   -- To_Any --
   ------------

   function To_Any
     (Item : IOP.EncodingFormat)
     return CORBA.Any
   is
      Result : CORBA.Any :=
        CORBA.To_Any
           (CORBA.Short
              (Item));
   begin
      CORBA.Internals.Set_Type
        (Result,
         TC_EncodingFormat);
      return Result;
   end To_Any;

   --------------
   -- From_Any --
   --------------

   function From_Any
     (Item : CORBA.Any)
     return IOP.Encoding
   is
   begin
      return (format => IOP.Helper.From_Any
        (CORBA.Internals.Get_Aggregate_Element
           (Item,
            IOP.Helper.TC_EncodingFormat,
            0)),
      major_version => CORBA.From_Any
        (CORBA.Internals.Get_Aggregate_Element
           (Item,
            CORBA.TC_Octet,
            1)),
      minor_version => CORBA.From_Any
        (CORBA.Internals.Get_Aggregate_Element
           (Item,
            CORBA.TC_Octet,
            2)));
   end From_Any;

   ------------
   -- To_Any --
   ------------

   function To_Any
     (Item : IOP.Encoding)
     return CORBA.Any
   is
      Result : constant CORBA.Any :=
        CORBA.Internals.Get_Empty_Any
           (TC_Encoding);
   begin
      PolyORB.Any.Set_Value
        (CORBA.Get_Container
           (Result).all,
         new PolyORB.Any.Content'Class'
           (IOP.Helper.Internals.Wrap
              (new IOP.Encoding'
                 (Item))),
         False);
      return Result;
   end To_Any;

   -----------------------------
   -- Deferred_Initialization --
   -----------------------------

   procedure Deferred_Initialization is
   begin
      IOP.Helper.Internals.Initialize_ProfileId;
      IOP.Helper.Internals.Initialize_ProfileData;
      IOP.Helper.Internals.Initialize_TaggedProfile;
      IOP.Helper.Internals.Initialize_IDL_SEQUENCE_IOP_TaggedProfile;
      IOP.Helper.Internals.Initialize_TaggedProfileSeq;
      IOP.Helper.Internals.Initialize_IOR;
      IOP.Helper.Internals.Initialize_ComponentId;
      IOP.Helper.Internals.Initialize_ComponentData;
      IOP.Helper.Internals.Initialize_TaggedComponent;
      IOP.Helper.Internals.Initialize_IDL_SEQUENCE_IOP_TaggedComponent;
      IOP.Helper.Internals.Initialize_TaggedComponentSeq;
      IOP.Helper.Internals.Initialize_ObjectKey;
      IOP.Helper.Internals.Initialize_IDL_SEQUENCE_IOP_TaggedComponent_1;
      IOP.Helper.Internals.Initialize_MultipleComponentProfile;
      IOP.Helper.Internals.Initialize_ContextData;
      IOP.Helper.Internals.Initialize_ServiceId;
      IOP.Helper.Internals.Initialize_ServiceContext;
      IOP.Helper.Internals.Initialize_IDL_SEQUENCE_IOP_ServiceContext;
      IOP.Helper.Internals.Initialize_ServiceContextList;
      IOP.Helper.Internals.Initialize_EncodingFormat;
      IOP.Helper.Internals.Initialize_Encoding;
   end Deferred_Initialization;

begin
   declare
      use PolyORB.Utils.Strings;
      use PolyORB.Utils.Strings.Lists;
   begin
      PolyORB.Initialization.Register_Module
        (PolyORB.Initialization.Module_Info'
           (Name => +"IOP.Helper",
            Conflicts => PolyORB.Utils.Strings.Lists.Empty,
            Depends => +"any"
               & "corba",
            Provides => PolyORB.Utils.Strings.Lists.Empty,
            Implicit => False,
            Init => Deferred_Initialization'Access,
            Shutdown => null));
   end;
end IOP.Helper;

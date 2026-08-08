pragma Style_Checks ("NM32766");
pragma Warnings (Off, "use of an anonymous access type allocator");
pragma Warnings (Off, "unnecessary with of ancestor");

---------------------------------------------------
--  This file has been generated automatically from
--  /Users/heathdorn/Documents/Playground/Agents/RefactorTeam/code_refactor/PolyORB/idls/Interop/GSSUP.idl
--  by IAC (IDL to Ada Compiler) 20.0w (rev. 41a9b833).
---------------------------------------------------
--  NOTE: If you modify this file by hand, your
--  changes will be lost when you re-run the
--  IDL to Ada compiler.
---------------------------------------------------

with CSI.Helper;
with CSI;
with Ada.Unchecked_Conversion;
with PolyORB.Utils.Unchecked_Deallocation;
with PolyORB.Std;
with PolyORB.Utils.Strings;
with PolyORB.Utils.Strings.Lists;
with PolyORB.Initialization;

package body GSSUP.Helper is

   
   package body Internals is

      ---------------------------
      -- Get_Aggregate_Element --
      ---------------------------

      function Get_Aggregate_Element
        (Acc : not null access Content_Ü_InitialContextToken;
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
               return CSI.Helper.Internals.Wrap
                 (CSI.IDL_SEQUENCE_octet_2.Sequence
                    (Acc.V.username)'Unrestricted_Access);
            when 1 =>
               return CSI.Helper.Internals.Wrap
                 (CSI.IDL_SEQUENCE_octet_2.Sequence
                    (Acc.V.password)'Unrestricted_Access);
            when 2 =>
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
        (Acc : Content_Ü_InitialContextToken)
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
        (Acc : in out Content_Ü_InitialContextToken;
         Count : PolyORB.Types.Unsigned_Long)
      is
      begin
         null;
      end Set_Aggregate_Count;

      ---------------------
      -- Unchecked_Get_V --
      ---------------------

      function Unchecked_Get_V
        (Acc : not null access Content_Ü_InitialContextToken)
        return PolyORB.Types.Address
      is
         function To_Address
           is new Ada.Unchecked_Conversion
              (Ptr_Ü_InitialContextToken,
               PolyORB.Types.Address);

      begin
         return To_Address
           (Acc.V);
      end Unchecked_Get_V;

      -----------
      -- Clone --
      -----------

      function Clone
        (Acc : Content_Ü_InitialContextToken;
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
               not in Content_Ü_InitialContextToken)
            then
               return null;
            end if;
            Target :=
              Into;
            Content_Ü_InitialContextToken
              (Target.all).V.all :=
              Acc.V.all;
         else
            Target :=
              new Content_Ü_InitialContextToken;
            Content_Ü_InitialContextToken
              (Target.all).V :=
              new GSSUP.InitialContextToken'
                 (Acc.V.all);
         end if;
         return Target;
      end Clone;

      --------------------
      -- Finalize_Value --
      --------------------

      procedure Finalize_Value
        (Acc : in out Content_Ü_InitialContextToken)
      is
         procedure Free
           is new PolyORB.Utils.Unchecked_Deallocation.Free

           (Object => GSSUP.InitialContextToken,

            Name   => Ptr_Ü_InitialContextToken);

      begin
         Free
           (Acc.V);
      end Finalize_Value;

      ----------
      -- Wrap --
      ----------

      function Wrap
        (X : access GSSUP.InitialContextToken)
        return PolyORB.Any.Content'Class
      is
      begin
         return Content_Ü_InitialContextToken'
           (PolyORB.Any.Aggregate_Content with
            V => Ptr_Ü_InitialContextToken'
              (X.all'Unchecked_Access));
      end Wrap;

      InitialContextToken_Initialized : PolyORB.Std.Boolean :=
        False;

      ------------------------------------
      -- Initialize_InitialContextToken --
      ------------------------------------

      procedure Initialize_InitialContextToken is
         Name_Ü : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("InitialContextToken");
         Id_Ü : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("IDL:omg.org/GSSUP/InitialContextToken:1.0");
         Argument_Name_Ü_username : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("username");
         Argument_Name_Ü_password : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("password");
         Argument_Name_Ü_target_name : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("target_name");
      begin
         if not InitialContextToken_Initialized
         then
            InitialContextToken_Initialized :=
              True;
            GSSUP.Helper.TC_InitialContextToken :=
              CORBA.TypeCode.Internals.To_CORBA_Object
                 (PolyORB.Any.TypeCode.Tcf_Struct);
            CORBA.Internals.Add_Parameter
              (TC_InitialContextToken,
               CORBA.To_Any
                 (Name_Ü));
            CORBA.Internals.Add_Parameter
              (TC_InitialContextToken,
               CORBA.To_Any
                 (Id_Ü));
            CSI.Helper.Internals.Initialize_UTF8String;
            CORBA.Internals.Add_Parameter
              (TC_InitialContextToken,
               CORBA.To_Any
                 (CSI.Helper.TC_UTF8String));
            CORBA.Internals.Add_Parameter
              (TC_InitialContextToken,
               CORBA.To_Any
                 (Argument_Name_Ü_username));
            CSI.Helper.Internals.Initialize_UTF8String;
            CORBA.Internals.Add_Parameter
              (TC_InitialContextToken,
               CORBA.To_Any
                 (CSI.Helper.TC_UTF8String));
            CORBA.Internals.Add_Parameter
              (TC_InitialContextToken,
               CORBA.To_Any
                 (Argument_Name_Ü_password));
            CSI.Helper.Internals.Initialize_GSS_NT_ExportedName;
            CORBA.Internals.Add_Parameter
              (TC_InitialContextToken,
               CORBA.To_Any
                 (CSI.Helper.TC_GSS_NT_ExportedName));
            CORBA.Internals.Add_Parameter
              (TC_InitialContextToken,
               CORBA.To_Any
                 (Argument_Name_Ü_target_name));
            CORBA.TypeCode.Internals.Disable_Ref_Counting
              (TC_InitialContextToken);
            CORBA.TypeCode.Internals.Freeze
              (TC_InitialContextToken);
         end if;
      end Initialize_InitialContextToken;

      ----------
      -- Wrap --
      ----------

      function Wrap
        (X : access GSSUP.ErrorCode)
        return PolyORB.Any.Content'Class
      is
      begin
         return CORBA.Wrap
           (CORBA.Unsigned_Long
              (X.all)'Unrestricted_Access);
      end Wrap;

      ErrorCode_Initialized : PolyORB.Std.Boolean :=
        False;

      --------------------------
      -- Initialize_ErrorCode --
      --------------------------

      procedure Initialize_ErrorCode is
         Name_Ü : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("ErrorCode");
         Id_Ü : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("IDL:omg.org/GSSUP/ErrorCode:1.0");
      begin
         if not ErrorCode_Initialized
         then
            ErrorCode_Initialized :=
              True;
            GSSUP.Helper.TC_ErrorCode :=
              CORBA.TypeCode.Internals.Build_Alias_TC
                 (Name => Name_Ü,
                  Id => Id_Ü,
                  Parent => CORBA.TC_Unsigned_Long);
            CORBA.TypeCode.Internals.Disable_Ref_Counting
              (TC_ErrorCode);
            CORBA.TypeCode.Internals.Freeze
              (TC_ErrorCode);
         end if;
      end Initialize_ErrorCode;

      ---------------------------
      -- Get_Aggregate_Element --
      ---------------------------

      function Get_Aggregate_Element
        (Acc : not null access Content_Ü_ErrorToken;
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
                    (Acc.V.error_code)'Unrestricted_Access);
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
        (Acc : Content_Ü_ErrorToken)
        return PolyORB.Types.Unsigned_Long
      is
         pragma Unreferenced (Acc);
      begin
         return 1;
      end Get_Aggregate_Count;

      -------------------------
      -- Set_Aggregate_Count --
      -------------------------

      procedure Set_Aggregate_Count
        (Acc : in out Content_Ü_ErrorToken;
         Count : PolyORB.Types.Unsigned_Long)
      is
      begin
         null;
      end Set_Aggregate_Count;

      ---------------------
      -- Unchecked_Get_V --
      ---------------------

      function Unchecked_Get_V
        (Acc : not null access Content_Ü_ErrorToken)
        return PolyORB.Types.Address
      is
         function To_Address
           is new Ada.Unchecked_Conversion
              (Ptr_Ü_ErrorToken,
               PolyORB.Types.Address);

      begin
         return To_Address
           (Acc.V);
      end Unchecked_Get_V;

      -----------
      -- Clone --
      -----------

      function Clone
        (Acc : Content_Ü_ErrorToken;
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
               not in Content_Ü_ErrorToken)
            then
               return null;
            end if;
            Target :=
              Into;
            Content_Ü_ErrorToken
              (Target.all).V.all :=
              Acc.V.all;
         else
            Target :=
              new Content_Ü_ErrorToken;
            Content_Ü_ErrorToken
              (Target.all).V :=
              new GSSUP.ErrorToken'
                 (Acc.V.all);
         end if;
         return Target;
      end Clone;

      --------------------
      -- Finalize_Value --
      --------------------

      procedure Finalize_Value
        (Acc : in out Content_Ü_ErrorToken)
      is
         procedure Free
           is new PolyORB.Utils.Unchecked_Deallocation.Free

           (Object => GSSUP.ErrorToken,

            Name   => Ptr_Ü_ErrorToken);

      begin
         Free
           (Acc.V);
      end Finalize_Value;

      ----------
      -- Wrap --
      ----------

      function Wrap
        (X : access GSSUP.ErrorToken)
        return PolyORB.Any.Content'Class
      is
      begin
         return Content_Ü_ErrorToken'
           (PolyORB.Any.Aggregate_Content with
            V => Ptr_Ü_ErrorToken'
              (X.all'Unchecked_Access));
      end Wrap;

      ErrorToken_Initialized : PolyORB.Std.Boolean :=
        False;

      ---------------------------
      -- Initialize_ErrorToken --
      ---------------------------

      procedure Initialize_ErrorToken is
         Name_Ü : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("ErrorToken");
         Id_Ü : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("IDL:omg.org/GSSUP/ErrorToken:1.0");
         Argument_Name_Ü_error_code : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("error_code");
      begin
         if not ErrorToken_Initialized
         then
            ErrorToken_Initialized :=
              True;
            GSSUP.Helper.TC_ErrorToken :=
              CORBA.TypeCode.Internals.To_CORBA_Object
                 (PolyORB.Any.TypeCode.Tcf_Struct);
            CORBA.Internals.Add_Parameter
              (TC_ErrorToken,
               CORBA.To_Any
                 (Name_Ü));
            CORBA.Internals.Add_Parameter
              (TC_ErrorToken,
               CORBA.To_Any
                 (Id_Ü));
            GSSUP.Helper.Internals.Initialize_ErrorCode;
            CORBA.Internals.Add_Parameter
              (TC_ErrorToken,
               CORBA.To_Any
                 (GSSUP.Helper.TC_ErrorCode));
            CORBA.Internals.Add_Parameter
              (TC_ErrorToken,
               CORBA.To_Any
                 (Argument_Name_Ü_error_code));
            CORBA.TypeCode.Internals.Disable_Ref_Counting
              (TC_ErrorToken);
            CORBA.TypeCode.Internals.Freeze
              (TC_ErrorToken);
         end if;
      end Initialize_ErrorToken;

   end Internals;

   --------------
   -- From_Any --
   --------------

   function From_Any
     (Item : CORBA.Any)
     return GSSUP.InitialContextToken
   is
   begin
      return (username => CSI.Helper.From_Any
        (CORBA.Internals.Get_Aggregate_Element
           (Item,
            CSI.Helper.TC_UTF8String,
            0)),
      password => CSI.Helper.From_Any
        (CORBA.Internals.Get_Aggregate_Element
           (Item,
            CSI.Helper.TC_UTF8String,
            1)),
      target_name => CSI.Helper.From_Any
        (CORBA.Internals.Get_Aggregate_Element
           (Item,
            CSI.Helper.TC_GSS_NT_ExportedName,
            2)));
   end From_Any;

   ------------
   -- To_Any --
   ------------

   function To_Any
     (Item : GSSUP.InitialContextToken)
     return CORBA.Any
   is
      Result : constant CORBA.Any :=
        CORBA.Internals.Get_Empty_Any
           (TC_InitialContextToken);
   begin
      PolyORB.Any.Set_Value
        (CORBA.Get_Container
           (Result).all,
         new PolyORB.Any.Content'Class'
           (GSSUP.Helper.Internals.Wrap
              (new GSSUP.InitialContextToken'
                 (Item))),
         False);
      return Result;
   end To_Any;

   --------------
   -- From_Any --
   --------------

   function From_Any
     (Item : CORBA.Any)
     return GSSUP.ErrorCode
   is
      Result : constant CORBA.Unsigned_Long :=
        CORBA.From_Any
           (Item);
   begin
      return GSSUP.ErrorCode
        (Result);
   end From_Any;

   ------------
   -- To_Any --
   ------------

   function To_Any
     (Item : GSSUP.ErrorCode)
     return CORBA.Any
   is
      Result : CORBA.Any :=
        CORBA.To_Any
           (CORBA.Unsigned_Long
              (Item));
   begin
      CORBA.Internals.Set_Type
        (Result,
         TC_ErrorCode);
      return Result;
   end To_Any;

   --------------
   -- From_Any --
   --------------

   function From_Any
     (Item : CORBA.Any)
     return GSSUP.ErrorToken
   is
   begin
      return (error_code => GSSUP.Helper.From_Any
        (CORBA.Internals.Get_Aggregate_Element
           (Item,
            GSSUP.Helper.TC_ErrorCode,
            0)));
   end From_Any;

   ------------
   -- To_Any --
   ------------

   function To_Any
     (Item : GSSUP.ErrorToken)
     return CORBA.Any
   is
      Result : constant CORBA.Any :=
        CORBA.Internals.Get_Empty_Any
           (TC_ErrorToken);
   begin
      PolyORB.Any.Set_Value
        (CORBA.Get_Container
           (Result).all,
         new PolyORB.Any.Content'Class'
           (GSSUP.Helper.Internals.Wrap
              (new GSSUP.ErrorToken'
                 (Item))),
         False);
      return Result;
   end To_Any;

   -----------------------------
   -- Deferred_Initialization --
   -----------------------------

   procedure Deferred_Initialization is
   begin
      GSSUP.Helper.Internals.Initialize_InitialContextToken;
      GSSUP.Helper.Internals.Initialize_ErrorCode;
      GSSUP.Helper.Internals.Initialize_ErrorToken;
   end Deferred_Initialization;

begin
   declare
      use PolyORB.Utils.Strings;
      use PolyORB.Utils.Strings.Lists;
   begin
      PolyORB.Initialization.Register_Module
        (PolyORB.Initialization.Module_Info'
           (Name => +"GSSUP.Helper",
            Conflicts => PolyORB.Utils.Strings.Lists.Empty,
            Depends => +"any"
               & "corba",
            Provides => PolyORB.Utils.Strings.Lists.Empty,
            Implicit => False,
            Init => Deferred_Initialization'Access,
            Shutdown => null));
   end;
end GSSUP.Helper;

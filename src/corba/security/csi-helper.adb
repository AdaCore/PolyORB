pragma Style_Checks ("NM32766");
pragma Warnings (Off, "use of an anonymous access type allocator");
pragma Warnings (Off, "unnecessary with of ancestor");

---------------------------------------------------
--  This file has been generated automatically from
--  /Users/heathdorn/Documents/Playground/Agents/RefactorTeam/code_refactor/PolyORB/idls/Interop/CSI.idl
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

package body CSI.Helper is

   
   package body Internals is

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
        (X : access CSI.IDL_SEQUENCE_octet.Sequence)
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
            CSI.Helper.TC_IDL_SEQUENCE_octet :=
              CORBA.TypeCode.Internals.Build_Sequence_TC
                 (CORBA.TC_Octet,
                  0);
            CORBA.TypeCode.Internals.Disable_Ref_Counting
              (CSI.Helper.TC_IDL_SEQUENCE_octet);
            IDL_SEQUENCE_octet_Helper.Initialize
              (Element_TC => CORBA.TC_Octet,
               Sequence_TC => CSI.Helper.TC_IDL_SEQUENCE_octet);
            CORBA.TypeCode.Internals.Freeze
              (TC_IDL_SEQUENCE_octet);
         end if;
      end Initialize_IDL_SEQUENCE_octet;

      X509CertificateChain_Initialized : PolyORB.Std.Boolean :=
        False;

      -------------------------------------
      -- Initialize_X509CertificateChain --
      -------------------------------------

      procedure Initialize_X509CertificateChain is
         Name_Ü : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("X509CertificateChain");
         Id_Ü : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("IDL:omg.org/CSI/X509CertificateChain:1.0");
      begin
         if not X509CertificateChain_Initialized
         then
            X509CertificateChain_Initialized :=
              True;
            CSI.Helper.Internals.Initialize_IDL_SEQUENCE_octet;
            CSI.Helper.TC_X509CertificateChain :=
              CORBA.TypeCode.Internals.Build_Alias_TC
                 (Name => Name_Ü,
                  Id => Id_Ü,
                  Parent => CSI.Helper.TC_IDL_SEQUENCE_octet);
            CORBA.TypeCode.Internals.Disable_Ref_Counting
              (TC_X509CertificateChain);
            CORBA.TypeCode.Internals.Freeze
              (TC_X509CertificateChain);
         end if;
      end Initialize_X509CertificateChain;

      ---------------------------------------
      -- IDL_SEQUENCE_octet_1_Element_Wrap --
      ---------------------------------------

      function IDL_SEQUENCE_octet_1_Element_Wrap
        (X : access CORBA.Octet)
        return PolyORB.Any.Content'Class
      is
      begin
         return CORBA.Wrap
           (X.all'Unrestricted_Access);
      end IDL_SEQUENCE_octet_1_Element_Wrap;

      function Wrap
        (X : access CSI.IDL_SEQUENCE_octet_1.Sequence)
        return PolyORB.Any.Content'Class
        renames IDL_SEQUENCE_octet_1_Helper.Wrap;

      IDL_SEQUENCE_octet_1_Initialized : PolyORB.Std.Boolean :=
        False;

      -------------------------------------
      -- Initialize_IDL_SEQUENCE_octet_1 --
      -------------------------------------

      procedure Initialize_IDL_SEQUENCE_octet_1 is
      begin
         if not IDL_SEQUENCE_octet_1_Initialized
         then
            IDL_SEQUENCE_octet_1_Initialized :=
              True;
            CSI.Helper.TC_IDL_SEQUENCE_octet_1 :=
              CORBA.TypeCode.Internals.Build_Sequence_TC
                 (CORBA.TC_Octet,
                  0);
            CORBA.TypeCode.Internals.Disable_Ref_Counting
              (CSI.Helper.TC_IDL_SEQUENCE_octet_1);
            IDL_SEQUENCE_octet_1_Helper.Initialize
              (Element_TC => CORBA.TC_Octet,
               Sequence_TC => CSI.Helper.TC_IDL_SEQUENCE_octet_1);
            CORBA.TypeCode.Internals.Freeze
              (TC_IDL_SEQUENCE_octet_1);
         end if;
      end Initialize_IDL_SEQUENCE_octet_1;

      X501DistinguishedName_Initialized : PolyORB.Std.Boolean :=
        False;

      --------------------------------------
      -- Initialize_X501DistinguishedName --
      --------------------------------------

      procedure Initialize_X501DistinguishedName is
         Name_Ü : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("X501DistinguishedName");
         Id_Ü : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("IDL:omg.org/CSI/X501DistinguishedName:1.0");
      begin
         if not X501DistinguishedName_Initialized
         then
            X501DistinguishedName_Initialized :=
              True;
            CSI.Helper.Internals.Initialize_IDL_SEQUENCE_octet_1;
            CSI.Helper.TC_X501DistinguishedName :=
              CORBA.TypeCode.Internals.Build_Alias_TC
                 (Name => Name_Ü,
                  Id => Id_Ü,
                  Parent => CSI.Helper.TC_IDL_SEQUENCE_octet_1);
            CORBA.TypeCode.Internals.Disable_Ref_Counting
              (TC_X501DistinguishedName);
            CORBA.TypeCode.Internals.Freeze
              (TC_X501DistinguishedName);
         end if;
      end Initialize_X501DistinguishedName;

      ---------------------------------------
      -- IDL_SEQUENCE_octet_2_Element_Wrap --
      ---------------------------------------

      function IDL_SEQUENCE_octet_2_Element_Wrap
        (X : access CORBA.Octet)
        return PolyORB.Any.Content'Class
      is
      begin
         return CORBA.Wrap
           (X.all'Unrestricted_Access);
      end IDL_SEQUENCE_octet_2_Element_Wrap;

      function Wrap
        (X : access CSI.IDL_SEQUENCE_octet_2.Sequence)
        return PolyORB.Any.Content'Class
        renames IDL_SEQUENCE_octet_2_Helper.Wrap;

      IDL_SEQUENCE_octet_2_Initialized : PolyORB.Std.Boolean :=
        False;

      -------------------------------------
      -- Initialize_IDL_SEQUENCE_octet_2 --
      -------------------------------------

      procedure Initialize_IDL_SEQUENCE_octet_2 is
      begin
         if not IDL_SEQUENCE_octet_2_Initialized
         then
            IDL_SEQUENCE_octet_2_Initialized :=
              True;
            CSI.Helper.TC_IDL_SEQUENCE_octet_2 :=
              CORBA.TypeCode.Internals.Build_Sequence_TC
                 (CORBA.TC_Octet,
                  0);
            CORBA.TypeCode.Internals.Disable_Ref_Counting
              (CSI.Helper.TC_IDL_SEQUENCE_octet_2);
            IDL_SEQUENCE_octet_2_Helper.Initialize
              (Element_TC => CORBA.TC_Octet,
               Sequence_TC => CSI.Helper.TC_IDL_SEQUENCE_octet_2);
            CORBA.TypeCode.Internals.Freeze
              (TC_IDL_SEQUENCE_octet_2);
         end if;
      end Initialize_IDL_SEQUENCE_octet_2;

      UTF8String_Initialized : PolyORB.Std.Boolean :=
        False;

      ---------------------------
      -- Initialize_UTF8String --
      ---------------------------

      procedure Initialize_UTF8String is
         Name_Ü : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("UTF8String");
         Id_Ü : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("IDL:omg.org/CSI/UTF8String:1.0");
      begin
         if not UTF8String_Initialized
         then
            UTF8String_Initialized :=
              True;
            CSI.Helper.Internals.Initialize_IDL_SEQUENCE_octet_2;
            CSI.Helper.TC_UTF8String :=
              CORBA.TypeCode.Internals.Build_Alias_TC
                 (Name => Name_Ü,
                  Id => Id_Ü,
                  Parent => CSI.Helper.TC_IDL_SEQUENCE_octet_2);
            CORBA.TypeCode.Internals.Disable_Ref_Counting
              (TC_UTF8String);
            CORBA.TypeCode.Internals.Freeze
              (TC_UTF8String);
         end if;
      end Initialize_UTF8String;

      ---------------------------------------
      -- IDL_SEQUENCE_octet_3_Element_Wrap --
      ---------------------------------------

      function IDL_SEQUENCE_octet_3_Element_Wrap
        (X : access CORBA.Octet)
        return PolyORB.Any.Content'Class
      is
      begin
         return CORBA.Wrap
           (X.all'Unrestricted_Access);
      end IDL_SEQUENCE_octet_3_Element_Wrap;

      function Wrap
        (X : access CSI.IDL_SEQUENCE_octet_3.Sequence)
        return PolyORB.Any.Content'Class
        renames IDL_SEQUENCE_octet_3_Helper.Wrap;

      IDL_SEQUENCE_octet_3_Initialized : PolyORB.Std.Boolean :=
        False;

      -------------------------------------
      -- Initialize_IDL_SEQUENCE_octet_3 --
      -------------------------------------

      procedure Initialize_IDL_SEQUENCE_octet_3 is
      begin
         if not IDL_SEQUENCE_octet_3_Initialized
         then
            IDL_SEQUENCE_octet_3_Initialized :=
              True;
            CSI.Helper.TC_IDL_SEQUENCE_octet_3 :=
              CORBA.TypeCode.Internals.Build_Sequence_TC
                 (CORBA.TC_Octet,
                  0);
            CORBA.TypeCode.Internals.Disable_Ref_Counting
              (CSI.Helper.TC_IDL_SEQUENCE_octet_3);
            IDL_SEQUENCE_octet_3_Helper.Initialize
              (Element_TC => CORBA.TC_Octet,
               Sequence_TC => CSI.Helper.TC_IDL_SEQUENCE_octet_3);
            CORBA.TypeCode.Internals.Freeze
              (TC_IDL_SEQUENCE_octet_3);
         end if;
      end Initialize_IDL_SEQUENCE_octet_3;

      OID_Initialized : PolyORB.Std.Boolean :=
        False;

      --------------------
      -- Initialize_OID --
      --------------------

      procedure Initialize_OID is
         Name_Ü : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("OID");
         Id_Ü : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("IDL:omg.org/CSI/OID:1.0");
      begin
         if not OID_Initialized
         then
            OID_Initialized :=
              True;
            CSI.Helper.Internals.Initialize_IDL_SEQUENCE_octet_3;
            CSI.Helper.TC_OID :=
              CORBA.TypeCode.Internals.Build_Alias_TC
                 (Name => Name_Ü,
                  Id => Id_Ü,
                  Parent => CSI.Helper.TC_IDL_SEQUENCE_octet_3);
            CORBA.TypeCode.Internals.Disable_Ref_Counting
              (TC_OID);
            CORBA.TypeCode.Internals.Freeze
              (TC_OID);
         end if;
      end Initialize_OID;

      ---------------------------------------
      -- IDL_SEQUENCE_CSI_OID_Element_Wrap --
      ---------------------------------------

      function IDL_SEQUENCE_CSI_OID_Element_Wrap
        (X : access CSI.OID)
        return PolyORB.Any.Content'Class
      is
      begin
         return CSI.Helper.Internals.Wrap
           (CSI.IDL_SEQUENCE_octet_3.Sequence
              (X.all)'Unrestricted_Access);
      end IDL_SEQUENCE_CSI_OID_Element_Wrap;

      function Wrap
        (X : access CSI.IDL_SEQUENCE_CSI_OID.Sequence)
        return PolyORB.Any.Content'Class
        renames IDL_SEQUENCE_CSI_OID_Helper.Wrap;

      IDL_SEQUENCE_CSI_OID_Initialized : PolyORB.Std.Boolean :=
        False;

      -------------------------------------
      -- Initialize_IDL_SEQUENCE_CSI_OID --
      -------------------------------------

      procedure Initialize_IDL_SEQUENCE_CSI_OID is
      begin
         if not IDL_SEQUENCE_CSI_OID_Initialized
         then
            IDL_SEQUENCE_CSI_OID_Initialized :=
              True;
            CSI.Helper.Internals.Initialize_OID;
            CSI.Helper.TC_IDL_SEQUENCE_CSI_OID :=
              CORBA.TypeCode.Internals.Build_Sequence_TC
                 (CSI.Helper.TC_OID,
                  0);
            CORBA.TypeCode.Internals.Disable_Ref_Counting
              (CSI.Helper.TC_IDL_SEQUENCE_CSI_OID);
            IDL_SEQUENCE_CSI_OID_Helper.Initialize
              (Element_TC => CSI.Helper.TC_OID,
               Sequence_TC => CSI.Helper.TC_IDL_SEQUENCE_CSI_OID);
            CORBA.TypeCode.Internals.Freeze
              (TC_IDL_SEQUENCE_CSI_OID);
         end if;
      end Initialize_IDL_SEQUENCE_CSI_OID;

      OIDList_Initialized : PolyORB.Std.Boolean :=
        False;

      ------------------------
      -- Initialize_OIDList --
      ------------------------

      procedure Initialize_OIDList is
         Name_Ü : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("OIDList");
         Id_Ü : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("IDL:omg.org/CSI/OIDList:1.0");
      begin
         if not OIDList_Initialized
         then
            OIDList_Initialized :=
              True;
            CSI.Helper.Internals.Initialize_IDL_SEQUENCE_CSI_OID;
            CSI.Helper.TC_OIDList :=
              CORBA.TypeCode.Internals.Build_Alias_TC
                 (Name => Name_Ü,
                  Id => Id_Ü,
                  Parent => CSI.Helper.TC_IDL_SEQUENCE_CSI_OID);
            CORBA.TypeCode.Internals.Disable_Ref_Counting
              (TC_OIDList);
            CORBA.TypeCode.Internals.Freeze
              (TC_OIDList);
         end if;
      end Initialize_OIDList;

      ---------------------------------------
      -- IDL_SEQUENCE_octet_4_Element_Wrap --
      ---------------------------------------

      function IDL_SEQUENCE_octet_4_Element_Wrap
        (X : access CORBA.Octet)
        return PolyORB.Any.Content'Class
      is
      begin
         return CORBA.Wrap
           (X.all'Unrestricted_Access);
      end IDL_SEQUENCE_octet_4_Element_Wrap;

      function Wrap
        (X : access CSI.IDL_SEQUENCE_octet_4.Sequence)
        return PolyORB.Any.Content'Class
        renames IDL_SEQUENCE_octet_4_Helper.Wrap;

      IDL_SEQUENCE_octet_4_Initialized : PolyORB.Std.Boolean :=
        False;

      -------------------------------------
      -- Initialize_IDL_SEQUENCE_octet_4 --
      -------------------------------------

      procedure Initialize_IDL_SEQUENCE_octet_4 is
      begin
         if not IDL_SEQUENCE_octet_4_Initialized
         then
            IDL_SEQUENCE_octet_4_Initialized :=
              True;
            CSI.Helper.TC_IDL_SEQUENCE_octet_4 :=
              CORBA.TypeCode.Internals.Build_Sequence_TC
                 (CORBA.TC_Octet,
                  0);
            CORBA.TypeCode.Internals.Disable_Ref_Counting
              (CSI.Helper.TC_IDL_SEQUENCE_octet_4);
            IDL_SEQUENCE_octet_4_Helper.Initialize
              (Element_TC => CORBA.TC_Octet,
               Sequence_TC => CSI.Helper.TC_IDL_SEQUENCE_octet_4);
            CORBA.TypeCode.Internals.Freeze
              (TC_IDL_SEQUENCE_octet_4);
         end if;
      end Initialize_IDL_SEQUENCE_octet_4;

      GSSToken_Initialized : PolyORB.Std.Boolean :=
        False;

      -------------------------
      -- Initialize_GSSToken --
      -------------------------

      procedure Initialize_GSSToken is
         Name_Ü : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("GSSToken");
         Id_Ü : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("IDL:omg.org/CSI/GSSToken:1.0");
      begin
         if not GSSToken_Initialized
         then
            GSSToken_Initialized :=
              True;
            CSI.Helper.Internals.Initialize_IDL_SEQUENCE_octet_4;
            CSI.Helper.TC_GSSToken :=
              CORBA.TypeCode.Internals.Build_Alias_TC
                 (Name => Name_Ü,
                  Id => Id_Ü,
                  Parent => CSI.Helper.TC_IDL_SEQUENCE_octet_4);
            CORBA.TypeCode.Internals.Disable_Ref_Counting
              (TC_GSSToken);
            CORBA.TypeCode.Internals.Freeze
              (TC_GSSToken);
         end if;
      end Initialize_GSSToken;

      ---------------------------------------
      -- IDL_SEQUENCE_octet_5_Element_Wrap --
      ---------------------------------------

      function IDL_SEQUENCE_octet_5_Element_Wrap
        (X : access CORBA.Octet)
        return PolyORB.Any.Content'Class
      is
      begin
         return CORBA.Wrap
           (X.all'Unrestricted_Access);
      end IDL_SEQUENCE_octet_5_Element_Wrap;

      function Wrap
        (X : access CSI.IDL_SEQUENCE_octet_5.Sequence)
        return PolyORB.Any.Content'Class
        renames IDL_SEQUENCE_octet_5_Helper.Wrap;

      IDL_SEQUENCE_octet_5_Initialized : PolyORB.Std.Boolean :=
        False;

      -------------------------------------
      -- Initialize_IDL_SEQUENCE_octet_5 --
      -------------------------------------

      procedure Initialize_IDL_SEQUENCE_octet_5 is
      begin
         if not IDL_SEQUENCE_octet_5_Initialized
         then
            IDL_SEQUENCE_octet_5_Initialized :=
              True;
            CSI.Helper.TC_IDL_SEQUENCE_octet_5 :=
              CORBA.TypeCode.Internals.Build_Sequence_TC
                 (CORBA.TC_Octet,
                  0);
            CORBA.TypeCode.Internals.Disable_Ref_Counting
              (CSI.Helper.TC_IDL_SEQUENCE_octet_5);
            IDL_SEQUENCE_octet_5_Helper.Initialize
              (Element_TC => CORBA.TC_Octet,
               Sequence_TC => CSI.Helper.TC_IDL_SEQUENCE_octet_5);
            CORBA.TypeCode.Internals.Freeze
              (TC_IDL_SEQUENCE_octet_5);
         end if;
      end Initialize_IDL_SEQUENCE_octet_5;

      GSS_NT_ExportedName_Initialized : PolyORB.Std.Boolean :=
        False;

      ------------------------------------
      -- Initialize_GSS_NT_ExportedName --
      ------------------------------------

      procedure Initialize_GSS_NT_ExportedName is
         Name_Ü : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("GSS_NT_ExportedName");
         Id_Ü : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("IDL:omg.org/CSI/GSS_NT_ExportedName:1.0");
      begin
         if not GSS_NT_ExportedName_Initialized
         then
            GSS_NT_ExportedName_Initialized :=
              True;
            CSI.Helper.Internals.Initialize_IDL_SEQUENCE_octet_5;
            CSI.Helper.TC_GSS_NT_ExportedName :=
              CORBA.TypeCode.Internals.Build_Alias_TC
                 (Name => Name_Ü,
                  Id => Id_Ü,
                  Parent => CSI.Helper.TC_IDL_SEQUENCE_octet_5);
            CORBA.TypeCode.Internals.Disable_Ref_Counting
              (TC_GSS_NT_ExportedName);
            CORBA.TypeCode.Internals.Freeze
              (TC_GSS_NT_ExportedName);
         end if;
      end Initialize_GSS_NT_ExportedName;

      -------------------------------------------------------
      -- IDL_SEQUENCE_CSI_GSS_NT_ExportedName_Element_Wrap --
      -------------------------------------------------------

      function IDL_SEQUENCE_CSI_GSS_NT_ExportedName_Element_Wrap
        (X : access CSI.GSS_NT_ExportedName)
        return PolyORB.Any.Content'Class
      is
      begin
         return CSI.Helper.Internals.Wrap
           (CSI.IDL_SEQUENCE_octet_5.Sequence
              (X.all)'Unrestricted_Access);
      end IDL_SEQUENCE_CSI_GSS_NT_ExportedName_Element_Wrap;

      function Wrap
        (X : access CSI.IDL_SEQUENCE_CSI_GSS_NT_ExportedName.Sequence)
        return PolyORB.Any.Content'Class
        renames IDL_SEQUENCE_CSI_GSS_NT_ExportedName_Helper.Wrap;

      IDL_SEQUENCE_CSI_GSS_NT_ExportedName_Initialized : PolyORB.Std.Boolean :=
        False;

      -----------------------------------------------------
      -- Initialize_IDL_SEQUENCE_CSI_GSS_NT_ExportedName --
      -----------------------------------------------------

      procedure Initialize_IDL_SEQUENCE_CSI_GSS_NT_ExportedName is
      begin
         if not IDL_SEQUENCE_CSI_GSS_NT_ExportedName_Initialized
         then
            IDL_SEQUENCE_CSI_GSS_NT_ExportedName_Initialized :=
              True;
            CSI.Helper.Internals.Initialize_GSS_NT_ExportedName;
            CSI.Helper.TC_IDL_SEQUENCE_CSI_GSS_NT_ExportedName :=
              CORBA.TypeCode.Internals.Build_Sequence_TC
                 (CSI.Helper.TC_GSS_NT_ExportedName,
                  0);
            CORBA.TypeCode.Internals.Disable_Ref_Counting
              (CSI.Helper.TC_IDL_SEQUENCE_CSI_GSS_NT_ExportedName);
            IDL_SEQUENCE_CSI_GSS_NT_ExportedName_Helper.Initialize
              (Element_TC => CSI.Helper.TC_GSS_NT_ExportedName,
               Sequence_TC => CSI.Helper.TC_IDL_SEQUENCE_CSI_GSS_NT_ExportedName);
            CORBA.TypeCode.Internals.Freeze
              (TC_IDL_SEQUENCE_CSI_GSS_NT_ExportedName);
         end if;
      end Initialize_IDL_SEQUENCE_CSI_GSS_NT_ExportedName;

      GSS_NT_ExportedNameList_Initialized : PolyORB.Std.Boolean :=
        False;

      ----------------------------------------
      -- Initialize_GSS_NT_ExportedNameList --
      ----------------------------------------

      procedure Initialize_GSS_NT_ExportedNameList is
         Name_Ü : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("GSS_NT_ExportedNameList");
         Id_Ü : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("IDL:omg.org/CSI/GSS_NT_ExportedNameList:1.0");
      begin
         if not GSS_NT_ExportedNameList_Initialized
         then
            GSS_NT_ExportedNameList_Initialized :=
              True;
            CSI.Helper.Internals.Initialize_IDL_SEQUENCE_CSI_GSS_NT_ExportedName;
            CSI.Helper.TC_GSS_NT_ExportedNameList :=
              CORBA.TypeCode.Internals.Build_Alias_TC
                 (Name => Name_Ü,
                  Id => Id_Ü,
                  Parent => CSI.Helper.TC_IDL_SEQUENCE_CSI_GSS_NT_ExportedName);
            CORBA.TypeCode.Internals.Disable_Ref_Counting
              (TC_GSS_NT_ExportedNameList);
            CORBA.TypeCode.Internals.Freeze
              (TC_GSS_NT_ExportedNameList);
         end if;
      end Initialize_GSS_NT_ExportedNameList;

      ----------
      -- Wrap --
      ----------

      function Wrap
        (X : access CSI.MsgType)
        return PolyORB.Any.Content'Class
      is
      begin
         return CORBA.Wrap
           (CORBA.Short
              (X.all)'Unrestricted_Access);
      end Wrap;

      MsgType_Initialized : PolyORB.Std.Boolean :=
        False;

      ------------------------
      -- Initialize_MsgType --
      ------------------------

      procedure Initialize_MsgType is
         Name_Ü : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("MsgType");
         Id_Ü : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("IDL:omg.org/CSI/MsgType:1.0");
      begin
         if not MsgType_Initialized
         then
            MsgType_Initialized :=
              True;
            CSI.Helper.TC_MsgType :=
              CORBA.TypeCode.Internals.Build_Alias_TC
                 (Name => Name_Ü,
                  Id => Id_Ü,
                  Parent => CORBA.TC_Short);
            CORBA.TypeCode.Internals.Disable_Ref_Counting
              (TC_MsgType);
            CORBA.TypeCode.Internals.Freeze
              (TC_MsgType);
         end if;
      end Initialize_MsgType;

      ----------
      -- Wrap --
      ----------

      function Wrap
        (X : access CSI.ContextId)
        return PolyORB.Any.Content'Class
      is
      begin
         return CORBA.Wrap
           (CORBA.Unsigned_Long_Long
              (X.all)'Unrestricted_Access);
      end Wrap;

      ContextId_Initialized : PolyORB.Std.Boolean :=
        False;

      --------------------------
      -- Initialize_ContextId --
      --------------------------

      procedure Initialize_ContextId is
         Name_Ü : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("ContextId");
         Id_Ü : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("IDL:omg.org/CSI/ContextId:1.0");
      begin
         if not ContextId_Initialized
         then
            ContextId_Initialized :=
              True;
            CSI.Helper.TC_ContextId :=
              CORBA.TypeCode.Internals.Build_Alias_TC
                 (Name => Name_Ü,
                  Id => Id_Ü,
                  Parent => CORBA.TC_Unsigned_Long_Long);
            CORBA.TypeCode.Internals.Disable_Ref_Counting
              (TC_ContextId);
            CORBA.TypeCode.Internals.Freeze
              (TC_ContextId);
         end if;
      end Initialize_ContextId;

      ----------
      -- Wrap --
      ----------

      function Wrap
        (X : access CSI.AuthorizationElementType)
        return PolyORB.Any.Content'Class
      is
      begin
         return CORBA.Wrap
           (CORBA.Unsigned_Long
              (X.all)'Unrestricted_Access);
      end Wrap;

      AuthorizationElementType_Initialized : PolyORB.Std.Boolean :=
        False;

      -----------------------------------------
      -- Initialize_AuthorizationElementType --
      -----------------------------------------

      procedure Initialize_AuthorizationElementType is
         Name_Ü : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("AuthorizationElementType");
         Id_Ü : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("IDL:omg.org/CSI/AuthorizationElementType:1.0");
      begin
         if not AuthorizationElementType_Initialized
         then
            AuthorizationElementType_Initialized :=
              True;
            CSI.Helper.TC_AuthorizationElementType :=
              CORBA.TypeCode.Internals.Build_Alias_TC
                 (Name => Name_Ü,
                  Id => Id_Ü,
                  Parent => CORBA.TC_Unsigned_Long);
            CORBA.TypeCode.Internals.Disable_Ref_Counting
              (TC_AuthorizationElementType);
            CORBA.TypeCode.Internals.Freeze
              (TC_AuthorizationElementType);
         end if;
      end Initialize_AuthorizationElementType;

      ---------------------------------------
      -- IDL_SEQUENCE_octet_6_Element_Wrap --
      ---------------------------------------

      function IDL_SEQUENCE_octet_6_Element_Wrap
        (X : access CORBA.Octet)
        return PolyORB.Any.Content'Class
      is
      begin
         return CORBA.Wrap
           (X.all'Unrestricted_Access);
      end IDL_SEQUENCE_octet_6_Element_Wrap;

      function Wrap
        (X : access CSI.IDL_SEQUENCE_octet_6.Sequence)
        return PolyORB.Any.Content'Class
        renames IDL_SEQUENCE_octet_6_Helper.Wrap;

      IDL_SEQUENCE_octet_6_Initialized : PolyORB.Std.Boolean :=
        False;

      -------------------------------------
      -- Initialize_IDL_SEQUENCE_octet_6 --
      -------------------------------------

      procedure Initialize_IDL_SEQUENCE_octet_6 is
      begin
         if not IDL_SEQUENCE_octet_6_Initialized
         then
            IDL_SEQUENCE_octet_6_Initialized :=
              True;
            CSI.Helper.TC_IDL_SEQUENCE_octet_6 :=
              CORBA.TypeCode.Internals.Build_Sequence_TC
                 (CORBA.TC_Octet,
                  0);
            CORBA.TypeCode.Internals.Disable_Ref_Counting
              (CSI.Helper.TC_IDL_SEQUENCE_octet_6);
            IDL_SEQUENCE_octet_6_Helper.Initialize
              (Element_TC => CORBA.TC_Octet,
               Sequence_TC => CSI.Helper.TC_IDL_SEQUENCE_octet_6);
            CORBA.TypeCode.Internals.Freeze
              (TC_IDL_SEQUENCE_octet_6);
         end if;
      end Initialize_IDL_SEQUENCE_octet_6;

      AuthorizationElementContents_Initialized : PolyORB.Std.Boolean :=
        False;

      ---------------------------------------------
      -- Initialize_AuthorizationElementContents --
      ---------------------------------------------

      procedure Initialize_AuthorizationElementContents is
         Name_Ü : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("AuthorizationElementContents");
         Id_Ü : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("IDL:omg.org/CSI/AuthorizationElementContents:1.0");
      begin
         if not AuthorizationElementContents_Initialized
         then
            AuthorizationElementContents_Initialized :=
              True;
            CSI.Helper.Internals.Initialize_IDL_SEQUENCE_octet_6;
            CSI.Helper.TC_AuthorizationElementContents :=
              CORBA.TypeCode.Internals.Build_Alias_TC
                 (Name => Name_Ü,
                  Id => Id_Ü,
                  Parent => CSI.Helper.TC_IDL_SEQUENCE_octet_6);
            CORBA.TypeCode.Internals.Disable_Ref_Counting
              (TC_AuthorizationElementContents);
            CORBA.TypeCode.Internals.Freeze
              (TC_AuthorizationElementContents);
         end if;
      end Initialize_AuthorizationElementContents;

      ---------------------------
      -- Get_Aggregate_Element --
      ---------------------------

      function Get_Aggregate_Element
        (Acc : not null access Content_Ü_AuthorizationElement;
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
                    (Acc.V.the_type)'Unrestricted_Access);
            when 1 =>
               return CSI.Helper.Internals.Wrap
                 (CSI.IDL_SEQUENCE_octet_6.Sequence
                    (Acc.V.the_element)'Unrestricted_Access);
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
        (Acc : Content_Ü_AuthorizationElement)
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
        (Acc : in out Content_Ü_AuthorizationElement;
         Count : PolyORB.Types.Unsigned_Long)
      is
      begin
         null;
      end Set_Aggregate_Count;

      ---------------------
      -- Unchecked_Get_V --
      ---------------------

      function Unchecked_Get_V
        (Acc : not null access Content_Ü_AuthorizationElement)
        return PolyORB.Types.Address
      is
         function To_Address
           is new Ada.Unchecked_Conversion
              (Ptr_Ü_AuthorizationElement,
               PolyORB.Types.Address);

      begin
         return To_Address
           (Acc.V);
      end Unchecked_Get_V;

      -----------
      -- Clone --
      -----------

      function Clone
        (Acc : Content_Ü_AuthorizationElement;
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
               not in Content_Ü_AuthorizationElement)
            then
               return null;
            end if;
            Target :=
              Into;
            Content_Ü_AuthorizationElement
              (Target.all).V.all :=
              Acc.V.all;
         else
            Target :=
              new Content_Ü_AuthorizationElement;
            Content_Ü_AuthorizationElement
              (Target.all).V :=
              new CSI.AuthorizationElement'
                 (Acc.V.all);
         end if;
         return Target;
      end Clone;

      --------------------
      -- Finalize_Value --
      --------------------

      procedure Finalize_Value
        (Acc : in out Content_Ü_AuthorizationElement)
      is
         procedure Free
           is new PolyORB.Utils.Unchecked_Deallocation.Free

           (Object => CSI.AuthorizationElement,

            Name   => Ptr_Ü_AuthorizationElement);

      begin
         Free
           (Acc.V);
      end Finalize_Value;

      ----------
      -- Wrap --
      ----------

      function Wrap
        (X : access CSI.AuthorizationElement)
        return PolyORB.Any.Content'Class
      is
      begin
         return Content_Ü_AuthorizationElement'
           (PolyORB.Any.Aggregate_Content with
            V => Ptr_Ü_AuthorizationElement'
              (X.all'Unchecked_Access));
      end Wrap;

      AuthorizationElement_Initialized : PolyORB.Std.Boolean :=
        False;

      -------------------------------------
      -- Initialize_AuthorizationElement --
      -------------------------------------

      procedure Initialize_AuthorizationElement is
         Name_Ü : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("AuthorizationElement");
         Id_Ü : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("IDL:omg.org/CSI/AuthorizationElement:1.0");
         Argument_Name_Ü_the_type : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("the_type");
         Argument_Name_Ü_the_element : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("the_element");
      begin
         if not AuthorizationElement_Initialized
         then
            AuthorizationElement_Initialized :=
              True;
            CSI.Helper.TC_AuthorizationElement :=
              CORBA.TypeCode.Internals.To_CORBA_Object
                 (PolyORB.Any.TypeCode.Tcf_Struct);
            CORBA.Internals.Add_Parameter
              (TC_AuthorizationElement,
               CORBA.To_Any
                 (Name_Ü));
            CORBA.Internals.Add_Parameter
              (TC_AuthorizationElement,
               CORBA.To_Any
                 (Id_Ü));
            CSI.Helper.Internals.Initialize_AuthorizationElementType;
            CORBA.Internals.Add_Parameter
              (TC_AuthorizationElement,
               CORBA.To_Any
                 (CSI.Helper.TC_AuthorizationElementType));
            CORBA.Internals.Add_Parameter
              (TC_AuthorizationElement,
               CORBA.To_Any
                 (Argument_Name_Ü_the_type));
            CSI.Helper.Internals.Initialize_AuthorizationElementContents;
            CORBA.Internals.Add_Parameter
              (TC_AuthorizationElement,
               CORBA.To_Any
                 (CSI.Helper.TC_AuthorizationElementContents));
            CORBA.Internals.Add_Parameter
              (TC_AuthorizationElement,
               CORBA.To_Any
                 (Argument_Name_Ü_the_element));
            CORBA.TypeCode.Internals.Disable_Ref_Counting
              (TC_AuthorizationElement);
            CORBA.TypeCode.Internals.Freeze
              (TC_AuthorizationElement);
         end if;
      end Initialize_AuthorizationElement;

      --------------------------------------------------------
      -- IDL_SEQUENCE_CSI_AuthorizationElement_Element_Wrap --
      --------------------------------------------------------

      function IDL_SEQUENCE_CSI_AuthorizationElement_Element_Wrap
        (X : access CSI.AuthorizationElement)
        return PolyORB.Any.Content'Class
      is
      begin
         return CSI.Helper.Internals.Wrap
           (X.all'Unrestricted_Access);
      end IDL_SEQUENCE_CSI_AuthorizationElement_Element_Wrap;

      function Wrap
        (X : access CSI.IDL_SEQUENCE_CSI_AuthorizationElement.Sequence)
        return PolyORB.Any.Content'Class
        renames IDL_SEQUENCE_CSI_AuthorizationElement_Helper.Wrap;

      IDL_SEQUENCE_CSI_AuthorizationElement_Initialized : PolyORB.Std.Boolean :=
        False;

      ------------------------------------------------------
      -- Initialize_IDL_SEQUENCE_CSI_AuthorizationElement --
      ------------------------------------------------------

      procedure Initialize_IDL_SEQUENCE_CSI_AuthorizationElement is
      begin
         if not IDL_SEQUENCE_CSI_AuthorizationElement_Initialized
         then
            IDL_SEQUENCE_CSI_AuthorizationElement_Initialized :=
              True;
            CSI.Helper.Internals.Initialize_AuthorizationElement;
            CSI.Helper.TC_IDL_SEQUENCE_CSI_AuthorizationElement :=
              CORBA.TypeCode.Internals.Build_Sequence_TC
                 (CSI.Helper.TC_AuthorizationElement,
                  0);
            CORBA.TypeCode.Internals.Disable_Ref_Counting
              (CSI.Helper.TC_IDL_SEQUENCE_CSI_AuthorizationElement);
            IDL_SEQUENCE_CSI_AuthorizationElement_Helper.Initialize
              (Element_TC => CSI.Helper.TC_AuthorizationElement,
               Sequence_TC => CSI.Helper.TC_IDL_SEQUENCE_CSI_AuthorizationElement);
            CORBA.TypeCode.Internals.Freeze
              (TC_IDL_SEQUENCE_CSI_AuthorizationElement);
         end if;
      end Initialize_IDL_SEQUENCE_CSI_AuthorizationElement;

      AuthorizationToken_Initialized : PolyORB.Std.Boolean :=
        False;

      -----------------------------------
      -- Initialize_AuthorizationToken --
      -----------------------------------

      procedure Initialize_AuthorizationToken is
         Name_Ü : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("AuthorizationToken");
         Id_Ü : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("IDL:omg.org/CSI/AuthorizationToken:1.0");
      begin
         if not AuthorizationToken_Initialized
         then
            AuthorizationToken_Initialized :=
              True;
            CSI.Helper.Internals.Initialize_IDL_SEQUENCE_CSI_AuthorizationElement;
            CSI.Helper.TC_AuthorizationToken :=
              CORBA.TypeCode.Internals.Build_Alias_TC
                 (Name => Name_Ü,
                  Id => Id_Ü,
                  Parent => CSI.Helper.TC_IDL_SEQUENCE_CSI_AuthorizationElement);
            CORBA.TypeCode.Internals.Disable_Ref_Counting
              (TC_AuthorizationToken);
            CORBA.TypeCode.Internals.Freeze
              (TC_AuthorizationToken);
         end if;
      end Initialize_AuthorizationToken;

      ----------
      -- Wrap --
      ----------

      function Wrap
        (X : access CSI.IdentityTokenType)
        return PolyORB.Any.Content'Class
      is
      begin
         return CORBA.Wrap
           (CORBA.Unsigned_Long
              (X.all)'Unrestricted_Access);
      end Wrap;

      IdentityTokenType_Initialized : PolyORB.Std.Boolean :=
        False;

      ----------------------------------
      -- Initialize_IdentityTokenType --
      ----------------------------------

      procedure Initialize_IdentityTokenType is
         Name_Ü : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("IdentityTokenType");
         Id_Ü : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("IDL:omg.org/CSI/IdentityTokenType:1.0");
      begin
         if not IdentityTokenType_Initialized
         then
            IdentityTokenType_Initialized :=
              True;
            CSI.Helper.TC_IdentityTokenType :=
              CORBA.TypeCode.Internals.Build_Alias_TC
                 (Name => Name_Ü,
                  Id => Id_Ü,
                  Parent => CORBA.TC_Unsigned_Long);
            CORBA.TypeCode.Internals.Disable_Ref_Counting
              (TC_IdentityTokenType);
            CORBA.TypeCode.Internals.Freeze
              (TC_IdentityTokenType);
         end if;
      end Initialize_IdentityTokenType;

      ---------------------------------------
      -- IDL_SEQUENCE_octet_7_Element_Wrap --
      ---------------------------------------

      function IDL_SEQUENCE_octet_7_Element_Wrap
        (X : access CORBA.Octet)
        return PolyORB.Any.Content'Class
      is
      begin
         return CORBA.Wrap
           (X.all'Unrestricted_Access);
      end IDL_SEQUENCE_octet_7_Element_Wrap;

      function Wrap
        (X : access CSI.IDL_SEQUENCE_octet_7.Sequence)
        return PolyORB.Any.Content'Class
        renames IDL_SEQUENCE_octet_7_Helper.Wrap;

      IDL_SEQUENCE_octet_7_Initialized : PolyORB.Std.Boolean :=
        False;

      -------------------------------------
      -- Initialize_IDL_SEQUENCE_octet_7 --
      -------------------------------------

      procedure Initialize_IDL_SEQUENCE_octet_7 is
      begin
         if not IDL_SEQUENCE_octet_7_Initialized
         then
            IDL_SEQUENCE_octet_7_Initialized :=
              True;
            CSI.Helper.TC_IDL_SEQUENCE_octet_7 :=
              CORBA.TypeCode.Internals.Build_Sequence_TC
                 (CORBA.TC_Octet,
                  0);
            CORBA.TypeCode.Internals.Disable_Ref_Counting
              (CSI.Helper.TC_IDL_SEQUENCE_octet_7);
            IDL_SEQUENCE_octet_7_Helper.Initialize
              (Element_TC => CORBA.TC_Octet,
               Sequence_TC => CSI.Helper.TC_IDL_SEQUENCE_octet_7);
            CORBA.TypeCode.Internals.Freeze
              (TC_IDL_SEQUENCE_octet_7);
         end if;
      end Initialize_IDL_SEQUENCE_octet_7;

      IdentityExtension_Initialized : PolyORB.Std.Boolean :=
        False;

      ----------------------------------
      -- Initialize_IdentityExtension --
      ----------------------------------

      procedure Initialize_IdentityExtension is
         Name_Ü : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("IdentityExtension");
         Id_Ü : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("IDL:omg.org/CSI/IdentityExtension:1.0");
      begin
         if not IdentityExtension_Initialized
         then
            IdentityExtension_Initialized :=
              True;
            CSI.Helper.Internals.Initialize_IDL_SEQUENCE_octet_7;
            CSI.Helper.TC_IdentityExtension :=
              CORBA.TypeCode.Internals.Build_Alias_TC
                 (Name => Name_Ü,
                  Id => Id_Ü,
                  Parent => CSI.Helper.TC_IDL_SEQUENCE_octet_7);
            CORBA.TypeCode.Internals.Disable_Ref_Counting
              (TC_IdentityExtension);
            CORBA.TypeCode.Internals.Freeze
              (TC_IdentityExtension);
         end if;
      end Initialize_IdentityExtension;

      ---------------------------
      -- Get_Aggregate_Element --
      ---------------------------

      function Get_Aggregate_Element
        (Acc : not null access Content_Ü_IdentityToken;
         Tc : PolyORB.Any.TypeCode.Object_Ptr;
         Index : PolyORB.Types.Unsigned_Long;
         Mech : not null access PolyORB.Any.Mechanism)
        return PolyORB.Any.Content'Class
      is
         use type PolyORB.Any.Mechanism;
         pragma Suppress (Validity_Check);
         use type PolyORB.Types.Unsigned_Long;
         pragma Unreferenced (Tc);
      begin
         if (Index
            = 0)
         then
            Mech.all :=
              PolyORB.Any.By_Value;
            Acc.Switch_Cache :=
              Acc.V.Switch;
            return CORBA.Wrap
              (CORBA.Unsigned_Long
                 (Acc.Switch_Cache)'Unrestricted_Access);
         else
            pragma Assert ((Index
               = 1));
            Mech.all :=
              PolyORB.Any.By_Reference;
            case Acc.V.Switch is
               when 0 =>
                  return CORBA.Wrap
                    (Acc.V.absent'Unrestricted_Access);
               when 1 =>
                  return CORBA.Wrap
                    (Acc.V.anonymous'Unrestricted_Access);
               when 2 =>
                  return CSI.Helper.Internals.Wrap
                    (CSI.IDL_SEQUENCE_octet_5.Sequence
                       (Acc.V.principal_name)'Unrestricted_Access);
               when 4 =>
                  return CSI.Helper.Internals.Wrap
                    (CSI.IDL_SEQUENCE_octet.Sequence
                       (Acc.V.certificate_chain)'Unrestricted_Access);
               when 8 =>
                  return CSI.Helper.Internals.Wrap
                    (CSI.IDL_SEQUENCE_octet_1.Sequence
                       (Acc.V.dn)'Unrestricted_Access);
               pragma Warnings (Off);
               when others =>
                  return CSI.Helper.Internals.Wrap
                    (CSI.IDL_SEQUENCE_octet_7.Sequence
                       (Acc.V.id)'Unrestricted_Access);
               pragma Warnings (On);

            end case;
         end if;
      end Get_Aggregate_Element;

      ---------------------------
      -- Set_Aggregate_Element --
      ---------------------------

      procedure Set_Aggregate_Element
        (Acc : in out Content_Ü_IdentityToken;
         Tc : PolyORB.Any.TypeCode.Object_Ptr;
         Index : PolyORB.Types.Unsigned_Long;
         From_C : in out PolyORB.Any.Any_Container'Class)
      is
         use type PolyORB.Types.Unsigned_Long;
         pragma Assert ((Index
            = 0));
         New_Switch : constant CSI.IdentityTokenType :=
           CSI.IdentityTokenType
              (CORBA.Unsigned_Long'
                 (CORBA.From_Any
                    (From_C)));
         New_Union : CSI.IdentityToken
           (Switch => New_Switch);
         --  Use default initialization
         pragma Warnings (Off, New_Union);
         pragma Suppress (Discriminant_Check);
         pragma Unreferenced (Tc);
      begin
         Acc.V.all :=
           New_Union;
      end Set_Aggregate_Element;

      -------------------------
      -- Get_Aggregate_Count --
      -------------------------

      function Get_Aggregate_Count
        (Acc : Content_Ü_IdentityToken)
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
        (Acc : in out Content_Ü_IdentityToken;
         Count : PolyORB.Types.Unsigned_Long)
      is
      begin
         null;
      end Set_Aggregate_Count;

      ---------------------
      -- Unchecked_Get_V --
      ---------------------

      function Unchecked_Get_V
        (Acc : not null access Content_Ü_IdentityToken)
        return PolyORB.Types.Address
      is
         function To_Address
           is new Ada.Unchecked_Conversion
              (Ptr_Ü_IdentityToken,
               PolyORB.Types.Address);

      begin
         return To_Address
           (Acc.V);
      end Unchecked_Get_V;

      -----------
      -- Clone --
      -----------

      function Clone
        (Acc : Content_Ü_IdentityToken;
         Into : PolyORB.Any.Content_Ptr := null)
        return PolyORB.Any.Content_Ptr
      is
         use type PolyORB.Any.Content_Ptr;
         Target : PolyORB.Any.Content_Ptr;
         pragma Suppress (Discriminant_Check);
      begin
         if (Into
            /= null)
         then
            if (Into.all
               not in Content_Ü_IdentityToken)
            then
               return null;
            end if;
            Target :=
              Into;
            Content_Ü_IdentityToken
              (Target.all).V.all :=
              Acc.V.all;
         else
            Target :=
              new Content_Ü_IdentityToken;
            Content_Ü_IdentityToken
              (Target.all).V :=
              new CSI.IdentityToken'
                 (Acc.V.all);
         end if;
         Content_Ü_IdentityToken
           (Target.all).Switch_Cache :=
           Acc.Switch_Cache;
         return Target;
      end Clone;

      --------------------
      -- Finalize_Value --
      --------------------

      procedure Finalize_Value
        (Acc : in out Content_Ü_IdentityToken)
      is
         procedure Free
           is new PolyORB.Utils.Unchecked_Deallocation.Free

           (Object => CSI.IdentityToken,

            Name   => Ptr_Ü_IdentityToken);

      begin
         Free
           (Acc.V);
      end Finalize_Value;

      ----------
      -- Wrap --
      ----------

      function Wrap
        (X : access CSI.IdentityToken)
        return PolyORB.Any.Content'Class
      is
      begin
         return Content_Ü_IdentityToken'
           (PolyORB.Any.Aggregate_Content with
            V => Ptr_Ü_IdentityToken'
              (X.all'Unchecked_Access),
            Switch_Cache => X.Switch);
      end Wrap;

      IdentityToken_Initialized : PolyORB.Std.Boolean :=
        False;

      ------------------------------
      -- Initialize_IdentityToken --
      ------------------------------

      procedure Initialize_IdentityToken is
         Name_Ü : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("IdentityToken");
         Id_Ü : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("IDL:omg.org/CSI/IdentityToken:1.0");
         Argument_Name_Ü_absent : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("absent");
         Argument_Name_Ü_anonymous : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("anonymous");
         Argument_Name_Ü_principal_name : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("principal_name");
         Argument_Name_Ü_certificate_chain : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("certificate_chain");
         Argument_Name_Ü_dn : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("dn");
         Argument_Name_Ü_id : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("id");
      begin
         if not IdentityToken_Initialized
         then
            IdentityToken_Initialized :=
              True;
            CSI.Helper.TC_IdentityToken :=
              CORBA.TypeCode.Internals.To_CORBA_Object
                 (PolyORB.Any.TypeCode.Tcf_Union);
            CORBA.Internals.Add_Parameter
              (TC_IdentityToken,
               CORBA.To_Any
                 (Name_Ü));
            CORBA.Internals.Add_Parameter
              (TC_IdentityToken,
               CORBA.To_Any
                 (Id_Ü));
            CSI.Helper.Internals.Initialize_IdentityTokenType;
            CORBA.Internals.Add_Parameter
              (TC_IdentityToken,
               CORBA.To_Any
                 (CSI.Helper.TC_IdentityTokenType));
            CSI.Helper.Internals.Initialize_GSS_NT_ExportedName;
            CSI.Helper.Internals.Initialize_X509CertificateChain;
            CSI.Helper.Internals.Initialize_X501DistinguishedName;
            CSI.Helper.Internals.Initialize_IdentityExtension;
            CORBA.Internals.Add_Parameter
              (TC_IdentityToken,
               CORBA.To_Any
                 (CORBA.Long
                    (5)));
            CORBA.Internals.Add_Parameter
              (TC_IdentityToken,
               CSI.Helper.To_Any
                 (CSI.IdentityTokenType'
                    (0)));
            CORBA.Internals.Add_Parameter
              (TC_IdentityToken,
               CORBA.To_Any
                 (CORBA.TC_Boolean));
            CORBA.Internals.Add_Parameter
              (TC_IdentityToken,
               CORBA.To_Any
                 (Argument_Name_Ü_absent));
            CORBA.Internals.Add_Parameter
              (TC_IdentityToken,
               CSI.Helper.To_Any
                 (CSI.IdentityTokenType'
                    (1)));
            CORBA.Internals.Add_Parameter
              (TC_IdentityToken,
               CORBA.To_Any
                 (CORBA.TC_Boolean));
            CORBA.Internals.Add_Parameter
              (TC_IdentityToken,
               CORBA.To_Any
                 (Argument_Name_Ü_anonymous));
            CORBA.Internals.Add_Parameter
              (TC_IdentityToken,
               CSI.Helper.To_Any
                 (CSI.IdentityTokenType'
                    (2)));
            CORBA.Internals.Add_Parameter
              (TC_IdentityToken,
               CORBA.To_Any
                 (CSI.Helper.TC_GSS_NT_ExportedName));
            CORBA.Internals.Add_Parameter
              (TC_IdentityToken,
               CORBA.To_Any
                 (Argument_Name_Ü_principal_name));
            CORBA.Internals.Add_Parameter
              (TC_IdentityToken,
               CSI.Helper.To_Any
                 (CSI.IdentityTokenType'
                    (4)));
            CORBA.Internals.Add_Parameter
              (TC_IdentityToken,
               CORBA.To_Any
                 (CSI.Helper.TC_X509CertificateChain));
            CORBA.Internals.Add_Parameter
              (TC_IdentityToken,
               CORBA.To_Any
                 (Argument_Name_Ü_certificate_chain));
            CORBA.Internals.Add_Parameter
              (TC_IdentityToken,
               CSI.Helper.To_Any
                 (CSI.IdentityTokenType'
                    (8)));
            CORBA.Internals.Add_Parameter
              (TC_IdentityToken,
               CORBA.To_Any
                 (CSI.Helper.TC_X501DistinguishedName));
            CORBA.Internals.Add_Parameter
              (TC_IdentityToken,
               CORBA.To_Any
                 (Argument_Name_Ü_dn));
            CORBA.Internals.Add_Parameter
              (TC_IdentityToken,
               CSI.Helper.To_Any
                 (CSI.IdentityTokenType'First));
            CORBA.Internals.Add_Parameter
              (TC_IdentityToken,
               CORBA.To_Any
                 (CSI.Helper.TC_IdentityExtension));
            CORBA.Internals.Add_Parameter
              (TC_IdentityToken,
               CORBA.To_Any
                 (Argument_Name_Ü_id));
            CORBA.TypeCode.Internals.Disable_Ref_Counting
              (TC_IdentityToken);
            CORBA.TypeCode.Internals.Freeze
              (TC_IdentityToken);
         end if;
      end Initialize_IdentityToken;

      ---------------------------
      -- Get_Aggregate_Element --
      ---------------------------

      function Get_Aggregate_Element
        (Acc : not null access Content_Ü_EstablishContext;
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
                 (CORBA.Unsigned_Long_Long
                    (Acc.V.client_context_id)'Unrestricted_Access);
            when 1 =>
               return CSI.Helper.Internals.Wrap
                 (CSI.IDL_SEQUENCE_CSI_AuthorizationElement.Sequence
                    (Acc.V.authorization_token)'Unrestricted_Access);
            when 2 =>
               return CSI.Helper.Internals.Wrap
                 (Acc.V.identity_token'Unrestricted_Access);
            when 3 =>
               return CSI.Helper.Internals.Wrap
                 (CSI.IDL_SEQUENCE_octet_4.Sequence
                    (Acc.V.client_authentication_token)'Unrestricted_Access);
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
        (Acc : Content_Ü_EstablishContext)
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
        (Acc : in out Content_Ü_EstablishContext;
         Count : PolyORB.Types.Unsigned_Long)
      is
      begin
         null;
      end Set_Aggregate_Count;

      ---------------------
      -- Unchecked_Get_V --
      ---------------------

      function Unchecked_Get_V
        (Acc : not null access Content_Ü_EstablishContext)
        return PolyORB.Types.Address
      is
         function To_Address
           is new Ada.Unchecked_Conversion
              (Ptr_Ü_EstablishContext,
               PolyORB.Types.Address);

      begin
         return To_Address
           (Acc.V);
      end Unchecked_Get_V;

      -----------
      -- Clone --
      -----------

      function Clone
        (Acc : Content_Ü_EstablishContext;
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
               not in Content_Ü_EstablishContext)
            then
               return null;
            end if;
            Target :=
              Into;
            Content_Ü_EstablishContext
              (Target.all).V.all :=
              Acc.V.all;
         else
            Target :=
              new Content_Ü_EstablishContext;
            Content_Ü_EstablishContext
              (Target.all).V :=
              new CSI.EstablishContext'
                 (Acc.V.all);
         end if;
         return Target;
      end Clone;

      --------------------
      -- Finalize_Value --
      --------------------

      procedure Finalize_Value
        (Acc : in out Content_Ü_EstablishContext)
      is
         procedure Free
           is new PolyORB.Utils.Unchecked_Deallocation.Free

           (Object => CSI.EstablishContext,

            Name   => Ptr_Ü_EstablishContext);

      begin
         Free
           (Acc.V);
      end Finalize_Value;

      ----------
      -- Wrap --
      ----------

      function Wrap
        (X : access CSI.EstablishContext)
        return PolyORB.Any.Content'Class
      is
      begin
         return Content_Ü_EstablishContext'
           (PolyORB.Any.Aggregate_Content with
            V => Ptr_Ü_EstablishContext'
              (X.all'Unchecked_Access));
      end Wrap;

      EstablishContext_Initialized : PolyORB.Std.Boolean :=
        False;

      ---------------------------------
      -- Initialize_EstablishContext --
      ---------------------------------

      procedure Initialize_EstablishContext is
         Name_Ü : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("EstablishContext");
         Id_Ü : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("IDL:omg.org/CSI/EstablishContext:1.0");
         Argument_Name_Ü_client_context_id : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("client_context_id");
         Argument_Name_Ü_authorization_token : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("authorization_token");
         Argument_Name_Ü_identity_token : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("identity_token");
         Argument_Name_Ü_client_authentication_token : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("client_authentication_token");
      begin
         if not EstablishContext_Initialized
         then
            EstablishContext_Initialized :=
              True;
            CSI.Helper.TC_EstablishContext :=
              CORBA.TypeCode.Internals.To_CORBA_Object
                 (PolyORB.Any.TypeCode.Tcf_Struct);
            CORBA.Internals.Add_Parameter
              (TC_EstablishContext,
               CORBA.To_Any
                 (Name_Ü));
            CORBA.Internals.Add_Parameter
              (TC_EstablishContext,
               CORBA.To_Any
                 (Id_Ü));
            CSI.Helper.Internals.Initialize_ContextId;
            CORBA.Internals.Add_Parameter
              (TC_EstablishContext,
               CORBA.To_Any
                 (CSI.Helper.TC_ContextId));
            CORBA.Internals.Add_Parameter
              (TC_EstablishContext,
               CORBA.To_Any
                 (Argument_Name_Ü_client_context_id));
            CSI.Helper.Internals.Initialize_AuthorizationToken;
            CORBA.Internals.Add_Parameter
              (TC_EstablishContext,
               CORBA.To_Any
                 (CSI.Helper.TC_AuthorizationToken));
            CORBA.Internals.Add_Parameter
              (TC_EstablishContext,
               CORBA.To_Any
                 (Argument_Name_Ü_authorization_token));
            CSI.Helper.Internals.Initialize_IdentityToken;
            CORBA.Internals.Add_Parameter
              (TC_EstablishContext,
               CORBA.To_Any
                 (CSI.Helper.TC_IdentityToken));
            CORBA.Internals.Add_Parameter
              (TC_EstablishContext,
               CORBA.To_Any
                 (Argument_Name_Ü_identity_token));
            CSI.Helper.Internals.Initialize_GSSToken;
            CORBA.Internals.Add_Parameter
              (TC_EstablishContext,
               CORBA.To_Any
                 (CSI.Helper.TC_GSSToken));
            CORBA.Internals.Add_Parameter
              (TC_EstablishContext,
               CORBA.To_Any
                 (Argument_Name_Ü_client_authentication_token));
            CORBA.TypeCode.Internals.Disable_Ref_Counting
              (TC_EstablishContext);
            CORBA.TypeCode.Internals.Freeze
              (TC_EstablishContext);
         end if;
      end Initialize_EstablishContext;

      ---------------------------
      -- Get_Aggregate_Element --
      ---------------------------

      function Get_Aggregate_Element
        (Acc : not null access Content_Ü_CompleteEstablishContext;
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
                 (CORBA.Unsigned_Long_Long
                    (Acc.V.client_context_id)'Unrestricted_Access);
            when 1 =>
               return CORBA.Wrap
                 (Acc.V.context_stateful'Unrestricted_Access);
            when 2 =>
               return CSI.Helper.Internals.Wrap
                 (CSI.IDL_SEQUENCE_octet_4.Sequence
                    (Acc.V.final_context_token)'Unrestricted_Access);
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
        (Acc : Content_Ü_CompleteEstablishContext)
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
        (Acc : in out Content_Ü_CompleteEstablishContext;
         Count : PolyORB.Types.Unsigned_Long)
      is
      begin
         null;
      end Set_Aggregate_Count;

      ---------------------
      -- Unchecked_Get_V --
      ---------------------

      function Unchecked_Get_V
        (Acc : not null access Content_Ü_CompleteEstablishContext)
        return PolyORB.Types.Address
      is
         function To_Address
           is new Ada.Unchecked_Conversion
              (Ptr_Ü_CompleteEstablishContext,
               PolyORB.Types.Address);

      begin
         return To_Address
           (Acc.V);
      end Unchecked_Get_V;

      -----------
      -- Clone --
      -----------

      function Clone
        (Acc : Content_Ü_CompleteEstablishContext;
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
               not in Content_Ü_CompleteEstablishContext)
            then
               return null;
            end if;
            Target :=
              Into;
            Content_Ü_CompleteEstablishContext
              (Target.all).V.all :=
              Acc.V.all;
         else
            Target :=
              new Content_Ü_CompleteEstablishContext;
            Content_Ü_CompleteEstablishContext
              (Target.all).V :=
              new CSI.CompleteEstablishContext'
                 (Acc.V.all);
         end if;
         return Target;
      end Clone;

      --------------------
      -- Finalize_Value --
      --------------------

      procedure Finalize_Value
        (Acc : in out Content_Ü_CompleteEstablishContext)
      is
         procedure Free
           is new PolyORB.Utils.Unchecked_Deallocation.Free

           (Object => CSI.CompleteEstablishContext,

            Name   => Ptr_Ü_CompleteEstablishContext);

      begin
         Free
           (Acc.V);
      end Finalize_Value;

      ----------
      -- Wrap --
      ----------

      function Wrap
        (X : access CSI.CompleteEstablishContext)
        return PolyORB.Any.Content'Class
      is
      begin
         return Content_Ü_CompleteEstablishContext'
           (PolyORB.Any.Aggregate_Content with
            V => Ptr_Ü_CompleteEstablishContext'
              (X.all'Unchecked_Access));
      end Wrap;

      CompleteEstablishContext_Initialized : PolyORB.Std.Boolean :=
        False;

      -----------------------------------------
      -- Initialize_CompleteEstablishContext --
      -----------------------------------------

      procedure Initialize_CompleteEstablishContext is
         Name_Ü : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("CompleteEstablishContext");
         Id_Ü : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("IDL:omg.org/CSI/CompleteEstablishContext:1.0");
         Argument_Name_Ü_client_context_id : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("client_context_id");
         Argument_Name_Ü_context_stateful : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("context_stateful");
         Argument_Name_Ü_final_context_token : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("final_context_token");
      begin
         if not CompleteEstablishContext_Initialized
         then
            CompleteEstablishContext_Initialized :=
              True;
            CSI.Helper.TC_CompleteEstablishContext :=
              CORBA.TypeCode.Internals.To_CORBA_Object
                 (PolyORB.Any.TypeCode.Tcf_Struct);
            CORBA.Internals.Add_Parameter
              (TC_CompleteEstablishContext,
               CORBA.To_Any
                 (Name_Ü));
            CORBA.Internals.Add_Parameter
              (TC_CompleteEstablishContext,
               CORBA.To_Any
                 (Id_Ü));
            CSI.Helper.Internals.Initialize_ContextId;
            CORBA.Internals.Add_Parameter
              (TC_CompleteEstablishContext,
               CORBA.To_Any
                 (CSI.Helper.TC_ContextId));
            CORBA.Internals.Add_Parameter
              (TC_CompleteEstablishContext,
               CORBA.To_Any
                 (Argument_Name_Ü_client_context_id));
            CORBA.Internals.Add_Parameter
              (TC_CompleteEstablishContext,
               CORBA.To_Any
                 (CORBA.TC_Boolean));
            CORBA.Internals.Add_Parameter
              (TC_CompleteEstablishContext,
               CORBA.To_Any
                 (Argument_Name_Ü_context_stateful));
            CSI.Helper.Internals.Initialize_GSSToken;
            CORBA.Internals.Add_Parameter
              (TC_CompleteEstablishContext,
               CORBA.To_Any
                 (CSI.Helper.TC_GSSToken));
            CORBA.Internals.Add_Parameter
              (TC_CompleteEstablishContext,
               CORBA.To_Any
                 (Argument_Name_Ü_final_context_token));
            CORBA.TypeCode.Internals.Disable_Ref_Counting
              (TC_CompleteEstablishContext);
            CORBA.TypeCode.Internals.Freeze
              (TC_CompleteEstablishContext);
         end if;
      end Initialize_CompleteEstablishContext;

      ---------------------------
      -- Get_Aggregate_Element --
      ---------------------------

      function Get_Aggregate_Element
        (Acc : not null access Content_Ü_ContextError;
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
                 (CORBA.Unsigned_Long_Long
                    (Acc.V.client_context_id)'Unrestricted_Access);
            when 1 =>
               return CORBA.Wrap
                 (Acc.V.major_status'Unrestricted_Access);
            when 2 =>
               return CORBA.Wrap
                 (Acc.V.minor_status'Unrestricted_Access);
            when 3 =>
               return CSI.Helper.Internals.Wrap
                 (CSI.IDL_SEQUENCE_octet_4.Sequence
                    (Acc.V.error_token)'Unrestricted_Access);
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
        (Acc : Content_Ü_ContextError)
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
        (Acc : in out Content_Ü_ContextError;
         Count : PolyORB.Types.Unsigned_Long)
      is
      begin
         null;
      end Set_Aggregate_Count;

      ---------------------
      -- Unchecked_Get_V --
      ---------------------

      function Unchecked_Get_V
        (Acc : not null access Content_Ü_ContextError)
        return PolyORB.Types.Address
      is
         function To_Address
           is new Ada.Unchecked_Conversion
              (Ptr_Ü_ContextError,
               PolyORB.Types.Address);

      begin
         return To_Address
           (Acc.V);
      end Unchecked_Get_V;

      -----------
      -- Clone --
      -----------

      function Clone
        (Acc : Content_Ü_ContextError;
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
               not in Content_Ü_ContextError)
            then
               return null;
            end if;
            Target :=
              Into;
            Content_Ü_ContextError
              (Target.all).V.all :=
              Acc.V.all;
         else
            Target :=
              new Content_Ü_ContextError;
            Content_Ü_ContextError
              (Target.all).V :=
              new CSI.ContextError'
                 (Acc.V.all);
         end if;
         return Target;
      end Clone;

      --------------------
      -- Finalize_Value --
      --------------------

      procedure Finalize_Value
        (Acc : in out Content_Ü_ContextError)
      is
         procedure Free
           is new PolyORB.Utils.Unchecked_Deallocation.Free

           (Object => CSI.ContextError,

            Name   => Ptr_Ü_ContextError);

      begin
         Free
           (Acc.V);
      end Finalize_Value;

      ----------
      -- Wrap --
      ----------

      function Wrap
        (X : access CSI.ContextError)
        return PolyORB.Any.Content'Class
      is
      begin
         return Content_Ü_ContextError'
           (PolyORB.Any.Aggregate_Content with
            V => Ptr_Ü_ContextError'
              (X.all'Unchecked_Access));
      end Wrap;

      ContextError_Initialized : PolyORB.Std.Boolean :=
        False;

      -----------------------------
      -- Initialize_ContextError --
      -----------------------------

      procedure Initialize_ContextError is
         Name_Ü : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("ContextError");
         Id_Ü : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("IDL:omg.org/CSI/ContextError:1.0");
         Argument_Name_Ü_client_context_id : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("client_context_id");
         Argument_Name_Ü_major_status : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("major_status");
         Argument_Name_Ü_minor_status : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("minor_status");
         Argument_Name_Ü_error_token : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("error_token");
      begin
         if not ContextError_Initialized
         then
            ContextError_Initialized :=
              True;
            CSI.Helper.TC_ContextError :=
              CORBA.TypeCode.Internals.To_CORBA_Object
                 (PolyORB.Any.TypeCode.Tcf_Struct);
            CORBA.Internals.Add_Parameter
              (TC_ContextError,
               CORBA.To_Any
                 (Name_Ü));
            CORBA.Internals.Add_Parameter
              (TC_ContextError,
               CORBA.To_Any
                 (Id_Ü));
            CSI.Helper.Internals.Initialize_ContextId;
            CORBA.Internals.Add_Parameter
              (TC_ContextError,
               CORBA.To_Any
                 (CSI.Helper.TC_ContextId));
            CORBA.Internals.Add_Parameter
              (TC_ContextError,
               CORBA.To_Any
                 (Argument_Name_Ü_client_context_id));
            CORBA.Internals.Add_Parameter
              (TC_ContextError,
               CORBA.To_Any
                 (CORBA.TC_Long));
            CORBA.Internals.Add_Parameter
              (TC_ContextError,
               CORBA.To_Any
                 (Argument_Name_Ü_major_status));
            CORBA.Internals.Add_Parameter
              (TC_ContextError,
               CORBA.To_Any
                 (CORBA.TC_Long));
            CORBA.Internals.Add_Parameter
              (TC_ContextError,
               CORBA.To_Any
                 (Argument_Name_Ü_minor_status));
            CSI.Helper.Internals.Initialize_GSSToken;
            CORBA.Internals.Add_Parameter
              (TC_ContextError,
               CORBA.To_Any
                 (CSI.Helper.TC_GSSToken));
            CORBA.Internals.Add_Parameter
              (TC_ContextError,
               CORBA.To_Any
                 (Argument_Name_Ü_error_token));
            CORBA.TypeCode.Internals.Disable_Ref_Counting
              (TC_ContextError);
            CORBA.TypeCode.Internals.Freeze
              (TC_ContextError);
         end if;
      end Initialize_ContextError;

      ---------------------------
      -- Get_Aggregate_Element --
      ---------------------------

      function Get_Aggregate_Element
        (Acc : not null access Content_Ü_MessageInContext;
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
                 (CORBA.Unsigned_Long_Long
                    (Acc.V.client_context_id)'Unrestricted_Access);
            when 1 =>
               return CORBA.Wrap
                 (Acc.V.discard_context'Unrestricted_Access);
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
        (Acc : Content_Ü_MessageInContext)
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
        (Acc : in out Content_Ü_MessageInContext;
         Count : PolyORB.Types.Unsigned_Long)
      is
      begin
         null;
      end Set_Aggregate_Count;

      ---------------------
      -- Unchecked_Get_V --
      ---------------------

      function Unchecked_Get_V
        (Acc : not null access Content_Ü_MessageInContext)
        return PolyORB.Types.Address
      is
         function To_Address
           is new Ada.Unchecked_Conversion
              (Ptr_Ü_MessageInContext,
               PolyORB.Types.Address);

      begin
         return To_Address
           (Acc.V);
      end Unchecked_Get_V;

      -----------
      -- Clone --
      -----------

      function Clone
        (Acc : Content_Ü_MessageInContext;
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
               not in Content_Ü_MessageInContext)
            then
               return null;
            end if;
            Target :=
              Into;
            Content_Ü_MessageInContext
              (Target.all).V.all :=
              Acc.V.all;
         else
            Target :=
              new Content_Ü_MessageInContext;
            Content_Ü_MessageInContext
              (Target.all).V :=
              new CSI.MessageInContext'
                 (Acc.V.all);
         end if;
         return Target;
      end Clone;

      --------------------
      -- Finalize_Value --
      --------------------

      procedure Finalize_Value
        (Acc : in out Content_Ü_MessageInContext)
      is
         procedure Free
           is new PolyORB.Utils.Unchecked_Deallocation.Free

           (Object => CSI.MessageInContext,

            Name   => Ptr_Ü_MessageInContext);

      begin
         Free
           (Acc.V);
      end Finalize_Value;

      ----------
      -- Wrap --
      ----------

      function Wrap
        (X : access CSI.MessageInContext)
        return PolyORB.Any.Content'Class
      is
      begin
         return Content_Ü_MessageInContext'
           (PolyORB.Any.Aggregate_Content with
            V => Ptr_Ü_MessageInContext'
              (X.all'Unchecked_Access));
      end Wrap;

      MessageInContext_Initialized : PolyORB.Std.Boolean :=
        False;

      ---------------------------------
      -- Initialize_MessageInContext --
      ---------------------------------

      procedure Initialize_MessageInContext is
         Name_Ü : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("MessageInContext");
         Id_Ü : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("IDL:omg.org/CSI/MessageInContext:1.0");
         Argument_Name_Ü_client_context_id : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("client_context_id");
         Argument_Name_Ü_discard_context : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("discard_context");
      begin
         if not MessageInContext_Initialized
         then
            MessageInContext_Initialized :=
              True;
            CSI.Helper.TC_MessageInContext :=
              CORBA.TypeCode.Internals.To_CORBA_Object
                 (PolyORB.Any.TypeCode.Tcf_Struct);
            CORBA.Internals.Add_Parameter
              (TC_MessageInContext,
               CORBA.To_Any
                 (Name_Ü));
            CORBA.Internals.Add_Parameter
              (TC_MessageInContext,
               CORBA.To_Any
                 (Id_Ü));
            CSI.Helper.Internals.Initialize_ContextId;
            CORBA.Internals.Add_Parameter
              (TC_MessageInContext,
               CORBA.To_Any
                 (CSI.Helper.TC_ContextId));
            CORBA.Internals.Add_Parameter
              (TC_MessageInContext,
               CORBA.To_Any
                 (Argument_Name_Ü_client_context_id));
            CORBA.Internals.Add_Parameter
              (TC_MessageInContext,
               CORBA.To_Any
                 (CORBA.TC_Boolean));
            CORBA.Internals.Add_Parameter
              (TC_MessageInContext,
               CORBA.To_Any
                 (Argument_Name_Ü_discard_context));
            CORBA.TypeCode.Internals.Disable_Ref_Counting
              (TC_MessageInContext);
            CORBA.TypeCode.Internals.Freeze
              (TC_MessageInContext);
         end if;
      end Initialize_MessageInContext;

      ---------------------------
      -- Get_Aggregate_Element --
      ---------------------------

      function Get_Aggregate_Element
        (Acc : not null access Content_Ü_SASContextBody;
         Tc : PolyORB.Any.TypeCode.Object_Ptr;
         Index : PolyORB.Types.Unsigned_Long;
         Mech : not null access PolyORB.Any.Mechanism)
        return PolyORB.Any.Content'Class
      is
         use type PolyORB.Any.Mechanism;
         pragma Suppress (Validity_Check);
         use type PolyORB.Types.Unsigned_Long;
         pragma Unreferenced (Tc);
      begin
         if (Index
            = 0)
         then
            Mech.all :=
              PolyORB.Any.By_Value;
            Acc.Switch_Cache :=
              Acc.V.Switch;
            return CORBA.Wrap
              (CORBA.Short
                 (Acc.Switch_Cache)'Unrestricted_Access);
         else
            pragma Assert ((Index
               = 1));
            Mech.all :=
              PolyORB.Any.By_Reference;
            case Acc.V.Switch is
               when 0 =>
                  return CSI.Helper.Internals.Wrap
                    (Acc.V.establish_msg'Unrestricted_Access);
               when 1 =>
                  return CSI.Helper.Internals.Wrap
                    (Acc.V.complete_msg'Unrestricted_Access);
               when 4 =>
                  return CSI.Helper.Internals.Wrap
                    (Acc.V.error_msg'Unrestricted_Access);
               when 5 =>
                  return CSI.Helper.Internals.Wrap
                    (Acc.V.in_context_msg'Unrestricted_Access);
               pragma Warnings (Off);
               when others =>
                  null;
               pragma Warnings (On);

            end case;
         end if;
      end Get_Aggregate_Element;

      ---------------------------
      -- Set_Aggregate_Element --
      ---------------------------

      procedure Set_Aggregate_Element
        (Acc : in out Content_Ü_SASContextBody;
         Tc : PolyORB.Any.TypeCode.Object_Ptr;
         Index : PolyORB.Types.Unsigned_Long;
         From_C : in out PolyORB.Any.Any_Container'Class)
      is
         use type PolyORB.Types.Unsigned_Long;
         pragma Assert ((Index
            = 0));
         New_Switch : constant CSI.MsgType :=
           CSI.MsgType
              (CORBA.Short'
                 (CORBA.From_Any
                    (From_C)));
         New_Union : CSI.SASContextBody
           (Switch => New_Switch);
         --  Use default initialization
         pragma Warnings (Off, New_Union);
         pragma Suppress (Discriminant_Check);
         pragma Unreferenced (Tc);
      begin
         Acc.V.all :=
           New_Union;
      end Set_Aggregate_Element;

      -------------------------
      -- Get_Aggregate_Count --
      -------------------------

      function Get_Aggregate_Count
        (Acc : Content_Ü_SASContextBody)
        return PolyORB.Types.Unsigned_Long
      is
      begin
         case Acc.V.Switch is
            when 0 =>
               return 2;
            when 1 =>
               return 2;
            when 4 =>
               return 2;
            when 5 =>
               return 2;
            pragma Warnings (Off);
            when others =>
               return 1;
            pragma Warnings (On);

         end case;
      end Get_Aggregate_Count;

      -------------------------
      -- Set_Aggregate_Count --
      -------------------------

      procedure Set_Aggregate_Count
        (Acc : in out Content_Ü_SASContextBody;
         Count : PolyORB.Types.Unsigned_Long)
      is
      begin
         null;
      end Set_Aggregate_Count;

      ---------------------
      -- Unchecked_Get_V --
      ---------------------

      function Unchecked_Get_V
        (Acc : not null access Content_Ü_SASContextBody)
        return PolyORB.Types.Address
      is
         function To_Address
           is new Ada.Unchecked_Conversion
              (Ptr_Ü_SASContextBody,
               PolyORB.Types.Address);

      begin
         return To_Address
           (Acc.V);
      end Unchecked_Get_V;

      -----------
      -- Clone --
      -----------

      function Clone
        (Acc : Content_Ü_SASContextBody;
         Into : PolyORB.Any.Content_Ptr := null)
        return PolyORB.Any.Content_Ptr
      is
         use type PolyORB.Any.Content_Ptr;
         Target : PolyORB.Any.Content_Ptr;
         pragma Suppress (Discriminant_Check);
      begin
         if (Into
            /= null)
         then
            if (Into.all
               not in Content_Ü_SASContextBody)
            then
               return null;
            end if;
            Target :=
              Into;
            Content_Ü_SASContextBody
              (Target.all).V.all :=
              Acc.V.all;
         else
            Target :=
              new Content_Ü_SASContextBody;
            Content_Ü_SASContextBody
              (Target.all).V :=
              new CSI.SASContextBody'
                 (Acc.V.all);
         end if;
         Content_Ü_SASContextBody
           (Target.all).Switch_Cache :=
           Acc.Switch_Cache;
         return Target;
      end Clone;

      --------------------
      -- Finalize_Value --
      --------------------

      procedure Finalize_Value
        (Acc : in out Content_Ü_SASContextBody)
      is
         procedure Free
           is new PolyORB.Utils.Unchecked_Deallocation.Free

           (Object => CSI.SASContextBody,

            Name   => Ptr_Ü_SASContextBody);

      begin
         Free
           (Acc.V);
      end Finalize_Value;

      ----------
      -- Wrap --
      ----------

      function Wrap
        (X : access CSI.SASContextBody)
        return PolyORB.Any.Content'Class
      is
      begin
         return Content_Ü_SASContextBody'
           (PolyORB.Any.Aggregate_Content with
            V => Ptr_Ü_SASContextBody'
              (X.all'Unchecked_Access),
            Switch_Cache => X.Switch);
      end Wrap;

      SASContextBody_Initialized : PolyORB.Std.Boolean :=
        False;

      -------------------------------
      -- Initialize_SASContextBody --
      -------------------------------

      procedure Initialize_SASContextBody is
         Name_Ü : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("SASContextBody");
         Id_Ü : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("IDL:omg.org/CSI/SASContextBody:1.0");
         Argument_Name_Ü_establish_msg : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("establish_msg");
         Argument_Name_Ü_complete_msg : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("complete_msg");
         Argument_Name_Ü_error_msg : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("error_msg");
         Argument_Name_Ü_in_context_msg : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("in_context_msg");
      begin
         if not SASContextBody_Initialized
         then
            SASContextBody_Initialized :=
              True;
            CSI.Helper.TC_SASContextBody :=
              CORBA.TypeCode.Internals.To_CORBA_Object
                 (PolyORB.Any.TypeCode.Tcf_Union);
            CORBA.Internals.Add_Parameter
              (TC_SASContextBody,
               CORBA.To_Any
                 (Name_Ü));
            CORBA.Internals.Add_Parameter
              (TC_SASContextBody,
               CORBA.To_Any
                 (Id_Ü));
            CSI.Helper.Internals.Initialize_MsgType;
            CORBA.Internals.Add_Parameter
              (TC_SASContextBody,
               CORBA.To_Any
                 (CSI.Helper.TC_MsgType));
            CSI.Helper.Internals.Initialize_EstablishContext;
            CSI.Helper.Internals.Initialize_CompleteEstablishContext;
            CSI.Helper.Internals.Initialize_ContextError;
            CSI.Helper.Internals.Initialize_MessageInContext;
            CORBA.Internals.Add_Parameter
              (TC_SASContextBody,
               CORBA.To_Any
                 (CORBA.Long
                    (-1)));
            CORBA.Internals.Add_Parameter
              (TC_SASContextBody,
               CSI.Helper.To_Any
                 (CSI.MsgType'
                    (0)));
            CORBA.Internals.Add_Parameter
              (TC_SASContextBody,
               CORBA.To_Any
                 (CSI.Helper.TC_EstablishContext));
            CORBA.Internals.Add_Parameter
              (TC_SASContextBody,
               CORBA.To_Any
                 (Argument_Name_Ü_establish_msg));
            CORBA.Internals.Add_Parameter
              (TC_SASContextBody,
               CSI.Helper.To_Any
                 (CSI.MsgType'
                    (1)));
            CORBA.Internals.Add_Parameter
              (TC_SASContextBody,
               CORBA.To_Any
                 (CSI.Helper.TC_CompleteEstablishContext));
            CORBA.Internals.Add_Parameter
              (TC_SASContextBody,
               CORBA.To_Any
                 (Argument_Name_Ü_complete_msg));
            CORBA.Internals.Add_Parameter
              (TC_SASContextBody,
               CSI.Helper.To_Any
                 (CSI.MsgType'
                    (4)));
            CORBA.Internals.Add_Parameter
              (TC_SASContextBody,
               CORBA.To_Any
                 (CSI.Helper.TC_ContextError));
            CORBA.Internals.Add_Parameter
              (TC_SASContextBody,
               CORBA.To_Any
                 (Argument_Name_Ü_error_msg));
            CORBA.Internals.Add_Parameter
              (TC_SASContextBody,
               CSI.Helper.To_Any
                 (CSI.MsgType'
                    (5)));
            CORBA.Internals.Add_Parameter
              (TC_SASContextBody,
               CORBA.To_Any
                 (CSI.Helper.TC_MessageInContext));
            CORBA.Internals.Add_Parameter
              (TC_SASContextBody,
               CORBA.To_Any
                 (Argument_Name_Ü_in_context_msg));
            CORBA.TypeCode.Internals.Disable_Ref_Counting
              (TC_SASContextBody);
            CORBA.TypeCode.Internals.Freeze
              (TC_SASContextBody);
         end if;
      end Initialize_SASContextBody;

      ----------
      -- Wrap --
      ----------

      function Wrap
        (X : access CSI.StringOID)
        return PolyORB.Any.Content'Class
      is
      begin
         return CORBA.Wrap
           (CORBA.String
              (X.all)'Unrestricted_Access);
      end Wrap;

      StringOID_Initialized : PolyORB.Std.Boolean :=
        False;

      --------------------------
      -- Initialize_StringOID --
      --------------------------

      procedure Initialize_StringOID is
         Name_Ü : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("StringOID");
         Id_Ü : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("IDL:omg.org/CSI/StringOID:1.0");
      begin
         if not StringOID_Initialized
         then
            StringOID_Initialized :=
              True;
            CSI.Helper.TC_StringOID :=
              CORBA.TypeCode.Internals.Build_Alias_TC
                 (Name => Name_Ü,
                  Id => Id_Ü,
                  Parent => CORBA.TC_String);
            CORBA.TypeCode.Internals.Disable_Ref_Counting
              (TC_StringOID);
            CORBA.TypeCode.Internals.Freeze
              (TC_StringOID);
         end if;
      end Initialize_StringOID;

   end Internals;

   function From_Any
     (Item : CORBA.Any)
     return CSI.IDL_SEQUENCE_octet.Sequence
     renames CSI.Helper.Internals.IDL_SEQUENCE_octet_Helper.From_Any;

   function To_Any
     (Item : CSI.IDL_SEQUENCE_octet.Sequence)
     return CORBA.Any
     renames CSI.Helper.Internals.IDL_SEQUENCE_octet_Helper.To_Any;

   --------------
   -- From_Any --
   --------------

   function From_Any
     (Item : CORBA.Any)
     return CSI.X509CertificateChain
   is
      Result : constant CSI.IDL_SEQUENCE_octet.Sequence :=
        CSI.Helper.From_Any
           (Item);
   begin
      return CSI.X509CertificateChain
        (Result);
   end From_Any;

   ------------
   -- To_Any --
   ------------

   function To_Any
     (Item : CSI.X509CertificateChain)
     return CORBA.Any
   is
      Result : CORBA.Any :=
        CSI.Helper.To_Any
           (CSI.IDL_SEQUENCE_octet.Sequence
              (Item));
   begin
      CORBA.Internals.Set_Type
        (Result,
         TC_X509CertificateChain);
      return Result;
   end To_Any;

   function From_Any
     (Item : CORBA.Any)
     return CSI.IDL_SEQUENCE_octet_1.Sequence
     renames CSI.Helper.Internals.IDL_SEQUENCE_octet_1_Helper.From_Any;

   function To_Any
     (Item : CSI.IDL_SEQUENCE_octet_1.Sequence)
     return CORBA.Any
     renames CSI.Helper.Internals.IDL_SEQUENCE_octet_1_Helper.To_Any;

   --------------
   -- From_Any --
   --------------

   function From_Any
     (Item : CORBA.Any)
     return CSI.X501DistinguishedName
   is
      Result : constant CSI.IDL_SEQUENCE_octet_1.Sequence :=
        CSI.Helper.From_Any
           (Item);
   begin
      return CSI.X501DistinguishedName
        (Result);
   end From_Any;

   ------------
   -- To_Any --
   ------------

   function To_Any
     (Item : CSI.X501DistinguishedName)
     return CORBA.Any
   is
      Result : CORBA.Any :=
        CSI.Helper.To_Any
           (CSI.IDL_SEQUENCE_octet_1.Sequence
              (Item));
   begin
      CORBA.Internals.Set_Type
        (Result,
         TC_X501DistinguishedName);
      return Result;
   end To_Any;

   function From_Any
     (Item : CORBA.Any)
     return CSI.IDL_SEQUENCE_octet_2.Sequence
     renames CSI.Helper.Internals.IDL_SEQUENCE_octet_2_Helper.From_Any;

   function To_Any
     (Item : CSI.IDL_SEQUENCE_octet_2.Sequence)
     return CORBA.Any
     renames CSI.Helper.Internals.IDL_SEQUENCE_octet_2_Helper.To_Any;

   --------------
   -- From_Any --
   --------------

   function From_Any
     (Item : CORBA.Any)
     return CSI.UTF8String
   is
      Result : constant CSI.IDL_SEQUENCE_octet_2.Sequence :=
        CSI.Helper.From_Any
           (Item);
   begin
      return CSI.UTF8String
        (Result);
   end From_Any;

   ------------
   -- To_Any --
   ------------

   function To_Any
     (Item : CSI.UTF8String)
     return CORBA.Any
   is
      Result : CORBA.Any :=
        CSI.Helper.To_Any
           (CSI.IDL_SEQUENCE_octet_2.Sequence
              (Item));
   begin
      CORBA.Internals.Set_Type
        (Result,
         TC_UTF8String);
      return Result;
   end To_Any;

   function From_Any
     (Item : CORBA.Any)
     return CSI.IDL_SEQUENCE_octet_3.Sequence
     renames CSI.Helper.Internals.IDL_SEQUENCE_octet_3_Helper.From_Any;

   function To_Any
     (Item : CSI.IDL_SEQUENCE_octet_3.Sequence)
     return CORBA.Any
     renames CSI.Helper.Internals.IDL_SEQUENCE_octet_3_Helper.To_Any;

   --------------
   -- From_Any --
   --------------

   function From_Any
     (Item : CORBA.Any)
     return CSI.OID
   is
      Result : constant CSI.IDL_SEQUENCE_octet_3.Sequence :=
        CSI.Helper.From_Any
           (Item);
   begin
      return CSI.OID
        (Result);
   end From_Any;

   ------------
   -- To_Any --
   ------------

   function To_Any
     (Item : CSI.OID)
     return CORBA.Any
   is
      Result : CORBA.Any :=
        CSI.Helper.To_Any
           (CSI.IDL_SEQUENCE_octet_3.Sequence
              (Item));
   begin
      CORBA.Internals.Set_Type
        (Result,
         TC_OID);
      return Result;
   end To_Any;

   function From_Any
     (Item : CORBA.Any)
     return CSI.IDL_SEQUENCE_CSI_OID.Sequence
     renames CSI.Helper.Internals.IDL_SEQUENCE_CSI_OID_Helper.From_Any;

   function To_Any
     (Item : CSI.IDL_SEQUENCE_CSI_OID.Sequence)
     return CORBA.Any
     renames CSI.Helper.Internals.IDL_SEQUENCE_CSI_OID_Helper.To_Any;

   --------------
   -- From_Any --
   --------------

   function From_Any
     (Item : CORBA.Any)
     return CSI.OIDList
   is
      Result : constant CSI.IDL_SEQUENCE_CSI_OID.Sequence :=
        CSI.Helper.From_Any
           (Item);
   begin
      return CSI.OIDList
        (Result);
   end From_Any;

   ------------
   -- To_Any --
   ------------

   function To_Any
     (Item : CSI.OIDList)
     return CORBA.Any
   is
      Result : CORBA.Any :=
        CSI.Helper.To_Any
           (CSI.IDL_SEQUENCE_CSI_OID.Sequence
              (Item));
   begin
      CORBA.Internals.Set_Type
        (Result,
         TC_OIDList);
      return Result;
   end To_Any;

   function From_Any
     (Item : CORBA.Any)
     return CSI.IDL_SEQUENCE_octet_4.Sequence
     renames CSI.Helper.Internals.IDL_SEQUENCE_octet_4_Helper.From_Any;

   function To_Any
     (Item : CSI.IDL_SEQUENCE_octet_4.Sequence)
     return CORBA.Any
     renames CSI.Helper.Internals.IDL_SEQUENCE_octet_4_Helper.To_Any;

   --------------
   -- From_Any --
   --------------

   function From_Any
     (Item : CORBA.Any)
     return CSI.GSSToken
   is
      Result : constant CSI.IDL_SEQUENCE_octet_4.Sequence :=
        CSI.Helper.From_Any
           (Item);
   begin
      return CSI.GSSToken
        (Result);
   end From_Any;

   ------------
   -- To_Any --
   ------------

   function To_Any
     (Item : CSI.GSSToken)
     return CORBA.Any
   is
      Result : CORBA.Any :=
        CSI.Helper.To_Any
           (CSI.IDL_SEQUENCE_octet_4.Sequence
              (Item));
   begin
      CORBA.Internals.Set_Type
        (Result,
         TC_GSSToken);
      return Result;
   end To_Any;

   function From_Any
     (Item : CORBA.Any)
     return CSI.IDL_SEQUENCE_octet_5.Sequence
     renames CSI.Helper.Internals.IDL_SEQUENCE_octet_5_Helper.From_Any;

   function To_Any
     (Item : CSI.IDL_SEQUENCE_octet_5.Sequence)
     return CORBA.Any
     renames CSI.Helper.Internals.IDL_SEQUENCE_octet_5_Helper.To_Any;

   --------------
   -- From_Any --
   --------------

   function From_Any
     (Item : CORBA.Any)
     return CSI.GSS_NT_ExportedName
   is
      Result : constant CSI.IDL_SEQUENCE_octet_5.Sequence :=
        CSI.Helper.From_Any
           (Item);
   begin
      return CSI.GSS_NT_ExportedName
        (Result);
   end From_Any;

   ------------
   -- To_Any --
   ------------

   function To_Any
     (Item : CSI.GSS_NT_ExportedName)
     return CORBA.Any
   is
      Result : CORBA.Any :=
        CSI.Helper.To_Any
           (CSI.IDL_SEQUENCE_octet_5.Sequence
              (Item));
   begin
      CORBA.Internals.Set_Type
        (Result,
         TC_GSS_NT_ExportedName);
      return Result;
   end To_Any;

   function From_Any
     (Item : CORBA.Any)
     return CSI.IDL_SEQUENCE_CSI_GSS_NT_ExportedName.Sequence
     renames CSI.Helper.Internals.IDL_SEQUENCE_CSI_GSS_NT_ExportedName_Helper.From_Any;

   function To_Any
     (Item : CSI.IDL_SEQUENCE_CSI_GSS_NT_ExportedName.Sequence)
     return CORBA.Any
     renames CSI.Helper.Internals.IDL_SEQUENCE_CSI_GSS_NT_ExportedName_Helper.To_Any;

   --------------
   -- From_Any --
   --------------

   function From_Any
     (Item : CORBA.Any)
     return CSI.GSS_NT_ExportedNameList
   is
      Result : constant CSI.IDL_SEQUENCE_CSI_GSS_NT_ExportedName.Sequence :=
        CSI.Helper.From_Any
           (Item);
   begin
      return CSI.GSS_NT_ExportedNameList
        (Result);
   end From_Any;

   ------------
   -- To_Any --
   ------------

   function To_Any
     (Item : CSI.GSS_NT_ExportedNameList)
     return CORBA.Any
   is
      Result : CORBA.Any :=
        CSI.Helper.To_Any
           (CSI.IDL_SEQUENCE_CSI_GSS_NT_ExportedName.Sequence
              (Item));
   begin
      CORBA.Internals.Set_Type
        (Result,
         TC_GSS_NT_ExportedNameList);
      return Result;
   end To_Any;

   --------------
   -- From_Any --
   --------------

   function From_Any
     (Item : CORBA.Any)
     return CSI.MsgType
   is
      Result : constant CORBA.Short :=
        CORBA.From_Any
           (Item);
   begin
      return CSI.MsgType
        (Result);
   end From_Any;

   ------------
   -- To_Any --
   ------------

   function To_Any
     (Item : CSI.MsgType)
     return CORBA.Any
   is
      Result : CORBA.Any :=
        CORBA.To_Any
           (CORBA.Short
              (Item));
   begin
      CORBA.Internals.Set_Type
        (Result,
         TC_MsgType);
      return Result;
   end To_Any;

   --------------
   -- From_Any --
   --------------

   function From_Any
     (Item : CORBA.Any)
     return CSI.ContextId
   is
      Result : constant CORBA.Unsigned_Long_Long :=
        CORBA.From_Any
           (Item);
   begin
      return CSI.ContextId
        (Result);
   end From_Any;

   ------------
   -- To_Any --
   ------------

   function To_Any
     (Item : CSI.ContextId)
     return CORBA.Any
   is
      Result : CORBA.Any :=
        CORBA.To_Any
           (CORBA.Unsigned_Long_Long
              (Item));
   begin
      CORBA.Internals.Set_Type
        (Result,
         TC_ContextId);
      return Result;
   end To_Any;

   --------------
   -- From_Any --
   --------------

   function From_Any
     (Item : CORBA.Any)
     return CSI.AuthorizationElementType
   is
      Result : constant CORBA.Unsigned_Long :=
        CORBA.From_Any
           (Item);
   begin
      return CSI.AuthorizationElementType
        (Result);
   end From_Any;

   ------------
   -- To_Any --
   ------------

   function To_Any
     (Item : CSI.AuthorizationElementType)
     return CORBA.Any
   is
      Result : CORBA.Any :=
        CORBA.To_Any
           (CORBA.Unsigned_Long
              (Item));
   begin
      CORBA.Internals.Set_Type
        (Result,
         TC_AuthorizationElementType);
      return Result;
   end To_Any;

   function From_Any
     (Item : CORBA.Any)
     return CSI.IDL_SEQUENCE_octet_6.Sequence
     renames CSI.Helper.Internals.IDL_SEQUENCE_octet_6_Helper.From_Any;

   function To_Any
     (Item : CSI.IDL_SEQUENCE_octet_6.Sequence)
     return CORBA.Any
     renames CSI.Helper.Internals.IDL_SEQUENCE_octet_6_Helper.To_Any;

   --------------
   -- From_Any --
   --------------

   function From_Any
     (Item : CORBA.Any)
     return CSI.AuthorizationElementContents
   is
      Result : constant CSI.IDL_SEQUENCE_octet_6.Sequence :=
        CSI.Helper.From_Any
           (Item);
   begin
      return CSI.AuthorizationElementContents
        (Result);
   end From_Any;

   ------------
   -- To_Any --
   ------------

   function To_Any
     (Item : CSI.AuthorizationElementContents)
     return CORBA.Any
   is
      Result : CORBA.Any :=
        CSI.Helper.To_Any
           (CSI.IDL_SEQUENCE_octet_6.Sequence
              (Item));
   begin
      CORBA.Internals.Set_Type
        (Result,
         TC_AuthorizationElementContents);
      return Result;
   end To_Any;

   --------------
   -- From_Any --
   --------------

   function From_Any
     (Item : CORBA.Any)
     return CSI.AuthorizationElement
   is
   begin
      return (the_type => CSI.Helper.From_Any
        (CORBA.Internals.Get_Aggregate_Element
           (Item,
            CSI.Helper.TC_AuthorizationElementType,
            0)),
      the_element => CSI.Helper.From_Any
        (CORBA.Internals.Get_Aggregate_Element
           (Item,
            CSI.Helper.TC_AuthorizationElementContents,
            1)));
   end From_Any;

   ------------
   -- To_Any --
   ------------

   function To_Any
     (Item : CSI.AuthorizationElement)
     return CORBA.Any
   is
      Result : constant CORBA.Any :=
        CORBA.Internals.Get_Empty_Any
           (TC_AuthorizationElement);
   begin
      PolyORB.Any.Set_Value
        (CORBA.Get_Container
           (Result).all,
         new PolyORB.Any.Content'Class'
           (CSI.Helper.Internals.Wrap
              (new CSI.AuthorizationElement'
                 (Item))),
         False);
      return Result;
   end To_Any;

   function From_Any
     (Item : CORBA.Any)
     return CSI.IDL_SEQUENCE_CSI_AuthorizationElement.Sequence
     renames CSI.Helper.Internals.IDL_SEQUENCE_CSI_AuthorizationElement_Helper.From_Any;

   function To_Any
     (Item : CSI.IDL_SEQUENCE_CSI_AuthorizationElement.Sequence)
     return CORBA.Any
     renames CSI.Helper.Internals.IDL_SEQUENCE_CSI_AuthorizationElement_Helper.To_Any;

   --------------
   -- From_Any --
   --------------

   function From_Any
     (Item : CORBA.Any)
     return CSI.AuthorizationToken
   is
      Result : constant CSI.IDL_SEQUENCE_CSI_AuthorizationElement.Sequence :=
        CSI.Helper.From_Any
           (Item);
   begin
      return CSI.AuthorizationToken
        (Result);
   end From_Any;

   ------------
   -- To_Any --
   ------------

   function To_Any
     (Item : CSI.AuthorizationToken)
     return CORBA.Any
   is
      Result : CORBA.Any :=
        CSI.Helper.To_Any
           (CSI.IDL_SEQUENCE_CSI_AuthorizationElement.Sequence
              (Item));
   begin
      CORBA.Internals.Set_Type
        (Result,
         TC_AuthorizationToken);
      return Result;
   end To_Any;

   --------------
   -- From_Any --
   --------------

   function From_Any
     (Item : CORBA.Any)
     return CSI.IdentityTokenType
   is
      Result : constant CORBA.Unsigned_Long :=
        CORBA.From_Any
           (Item);
   begin
      return CSI.IdentityTokenType
        (Result);
   end From_Any;

   ------------
   -- To_Any --
   ------------

   function To_Any
     (Item : CSI.IdentityTokenType)
     return CORBA.Any
   is
      Result : CORBA.Any :=
        CORBA.To_Any
           (CORBA.Unsigned_Long
              (Item));
   begin
      CORBA.Internals.Set_Type
        (Result,
         TC_IdentityTokenType);
      return Result;
   end To_Any;

   function From_Any
     (Item : CORBA.Any)
     return CSI.IDL_SEQUENCE_octet_7.Sequence
     renames CSI.Helper.Internals.IDL_SEQUENCE_octet_7_Helper.From_Any;

   function To_Any
     (Item : CSI.IDL_SEQUENCE_octet_7.Sequence)
     return CORBA.Any
     renames CSI.Helper.Internals.IDL_SEQUENCE_octet_7_Helper.To_Any;

   --------------
   -- From_Any --
   --------------

   function From_Any
     (Item : CORBA.Any)
     return CSI.IdentityExtension
   is
      Result : constant CSI.IDL_SEQUENCE_octet_7.Sequence :=
        CSI.Helper.From_Any
           (Item);
   begin
      return CSI.IdentityExtension
        (Result);
   end From_Any;

   ------------
   -- To_Any --
   ------------

   function To_Any
     (Item : CSI.IdentityExtension)
     return CORBA.Any
   is
      Result : CORBA.Any :=
        CSI.Helper.To_Any
           (CSI.IDL_SEQUENCE_octet_7.Sequence
              (Item));
   begin
      CORBA.Internals.Set_Type
        (Result,
         TC_IdentityExtension);
      return Result;
   end To_Any;

   --------------
   -- From_Any --
   --------------

   function From_Any
     (Item : CORBA.Any)
     return CSI.IdentityToken
   is
      Label_Any_Ü : constant CORBA.Any :=
        CORBA.Internals.Get_Aggregate_Element
           (Item,
            CSI.Helper.TC_IdentityTokenType,
            CORBA.Unsigned_Long
              (0));
      Label_Ü : constant CSI.IdentityTokenType :=
        CSI.Helper.From_Any
           (Label_Any_Ü);
      Result : CSI.IdentityToken
        (Label_Ü);
      Index_Ü : CORBA.Any;
   begin
      case Label_Ü is
         when 0 =>
            Index_Ü :=
              CORBA.Internals.Get_Aggregate_Element
                 (Item,
                  CORBA.TC_Boolean,
                  CORBA.Unsigned_Long
                    (1));
            Result.absent :=
              CORBA.From_Any
                 (Index_Ü);
         when 1 =>
            Index_Ü :=
              CORBA.Internals.Get_Aggregate_Element
                 (Item,
                  CORBA.TC_Boolean,
                  CORBA.Unsigned_Long
                    (1));
            Result.anonymous :=
              CORBA.From_Any
                 (Index_Ü);
         when 2 =>
            Index_Ü :=
              CORBA.Internals.Get_Aggregate_Element
                 (Item,
                  CSI.Helper.TC_GSS_NT_ExportedName,
                  CORBA.Unsigned_Long
                    (1));
            Result.principal_name :=
              CSI.Helper.From_Any
                 (Index_Ü);
         when 4 =>
            Index_Ü :=
              CORBA.Internals.Get_Aggregate_Element
                 (Item,
                  CSI.Helper.TC_X509CertificateChain,
                  CORBA.Unsigned_Long
                    (1));
            Result.certificate_chain :=
              CSI.Helper.From_Any
                 (Index_Ü);
         when 8 =>
            Index_Ü :=
              CORBA.Internals.Get_Aggregate_Element
                 (Item,
                  CSI.Helper.TC_X501DistinguishedName,
                  CORBA.Unsigned_Long
                    (1));
            Result.dn :=
              CSI.Helper.From_Any
                 (Index_Ü);
         pragma Warnings (Off);
         when others =>
            Index_Ü :=
              CORBA.Internals.Get_Aggregate_Element
                 (Item,
                  CSI.Helper.TC_IdentityExtension,
                  CORBA.Unsigned_Long
                    (1));
            Result.id :=
              CSI.Helper.From_Any
                 (Index_Ü);
         pragma Warnings (On);

      end case;
      return Result;
   end From_Any;

   ------------
   -- To_Any --
   ------------

   function To_Any
     (Item : CSI.IdentityToken)
     return CORBA.Any
   is
      Result : constant CORBA.Any :=
        CORBA.Internals.Get_Empty_Any
           (TC_IdentityToken);
   begin
      PolyORB.Any.Set_Value
        (CORBA.Get_Container
           (Result).all,
         new PolyORB.Any.Content'Class'
           (CSI.Helper.Internals.Wrap
              (new CSI.IdentityToken'
                 (Item))),
         False);
      return Result;
   end To_Any;

   --------------
   -- From_Any --
   --------------

   function From_Any
     (Item : CORBA.Any)
     return CSI.EstablishContext
   is
   begin
      return (client_context_id => CSI.Helper.From_Any
        (CORBA.Internals.Get_Aggregate_Element
           (Item,
            CSI.Helper.TC_ContextId,
            0)),
      authorization_token => CSI.Helper.From_Any
        (CORBA.Internals.Get_Aggregate_Element
           (Item,
            CSI.Helper.TC_AuthorizationToken,
            1)),
      identity_token => CSI.Helper.From_Any
        (CORBA.Internals.Get_Aggregate_Element
           (Item,
            CSI.Helper.TC_IdentityToken,
            2)),
      client_authentication_token => CSI.Helper.From_Any
        (CORBA.Internals.Get_Aggregate_Element
           (Item,
            CSI.Helper.TC_GSSToken,
            3)));
   end From_Any;

   ------------
   -- To_Any --
   ------------

   function To_Any
     (Item : CSI.EstablishContext)
     return CORBA.Any
   is
      Result : constant CORBA.Any :=
        CORBA.Internals.Get_Empty_Any
           (TC_EstablishContext);
   begin
      PolyORB.Any.Set_Value
        (CORBA.Get_Container
           (Result).all,
         new PolyORB.Any.Content'Class'
           (CSI.Helper.Internals.Wrap
              (new CSI.EstablishContext'
                 (Item))),
         False);
      return Result;
   end To_Any;

   --------------
   -- From_Any --
   --------------

   function From_Any
     (Item : CORBA.Any)
     return CSI.CompleteEstablishContext
   is
   begin
      return (client_context_id => CSI.Helper.From_Any
        (CORBA.Internals.Get_Aggregate_Element
           (Item,
            CSI.Helper.TC_ContextId,
            0)),
      context_stateful => CORBA.From_Any
        (CORBA.Internals.Get_Aggregate_Element
           (Item,
            CORBA.TC_Boolean,
            1)),
      final_context_token => CSI.Helper.From_Any
        (CORBA.Internals.Get_Aggregate_Element
           (Item,
            CSI.Helper.TC_GSSToken,
            2)));
   end From_Any;

   ------------
   -- To_Any --
   ------------

   function To_Any
     (Item : CSI.CompleteEstablishContext)
     return CORBA.Any
   is
      Result : constant CORBA.Any :=
        CORBA.Internals.Get_Empty_Any
           (TC_CompleteEstablishContext);
   begin
      PolyORB.Any.Set_Value
        (CORBA.Get_Container
           (Result).all,
         new PolyORB.Any.Content'Class'
           (CSI.Helper.Internals.Wrap
              (new CSI.CompleteEstablishContext'
                 (Item))),
         False);
      return Result;
   end To_Any;

   --------------
   -- From_Any --
   --------------

   function From_Any
     (Item : CORBA.Any)
     return CSI.ContextError
   is
   begin
      return (client_context_id => CSI.Helper.From_Any
        (CORBA.Internals.Get_Aggregate_Element
           (Item,
            CSI.Helper.TC_ContextId,
            0)),
      major_status => CORBA.From_Any
        (CORBA.Internals.Get_Aggregate_Element
           (Item,
            CORBA.TC_Long,
            1)),
      minor_status => CORBA.From_Any
        (CORBA.Internals.Get_Aggregate_Element
           (Item,
            CORBA.TC_Long,
            2)),
      error_token => CSI.Helper.From_Any
        (CORBA.Internals.Get_Aggregate_Element
           (Item,
            CSI.Helper.TC_GSSToken,
            3)));
   end From_Any;

   ------------
   -- To_Any --
   ------------

   function To_Any
     (Item : CSI.ContextError)
     return CORBA.Any
   is
      Result : constant CORBA.Any :=
        CORBA.Internals.Get_Empty_Any
           (TC_ContextError);
   begin
      PolyORB.Any.Set_Value
        (CORBA.Get_Container
           (Result).all,
         new PolyORB.Any.Content'Class'
           (CSI.Helper.Internals.Wrap
              (new CSI.ContextError'
                 (Item))),
         False);
      return Result;
   end To_Any;

   --------------
   -- From_Any --
   --------------

   function From_Any
     (Item : CORBA.Any)
     return CSI.MessageInContext
   is
   begin
      return (client_context_id => CSI.Helper.From_Any
        (CORBA.Internals.Get_Aggregate_Element
           (Item,
            CSI.Helper.TC_ContextId,
            0)),
      discard_context => CORBA.From_Any
        (CORBA.Internals.Get_Aggregate_Element
           (Item,
            CORBA.TC_Boolean,
            1)));
   end From_Any;

   ------------
   -- To_Any --
   ------------

   function To_Any
     (Item : CSI.MessageInContext)
     return CORBA.Any
   is
      Result : constant CORBA.Any :=
        CORBA.Internals.Get_Empty_Any
           (TC_MessageInContext);
   begin
      PolyORB.Any.Set_Value
        (CORBA.Get_Container
           (Result).all,
         new PolyORB.Any.Content'Class'
           (CSI.Helper.Internals.Wrap
              (new CSI.MessageInContext'
                 (Item))),
         False);
      return Result;
   end To_Any;

   --------------
   -- From_Any --
   --------------

   function From_Any
     (Item : CORBA.Any)
     return CSI.SASContextBody
   is
      Label_Any_Ü : constant CORBA.Any :=
        CORBA.Internals.Get_Aggregate_Element
           (Item,
            CSI.Helper.TC_MsgType,
            CORBA.Unsigned_Long
              (0));
      Label_Ü : constant CSI.MsgType :=
        CSI.Helper.From_Any
           (Label_Any_Ü);
      Result : CSI.SASContextBody
        (Label_Ü);
      Index_Ü : CORBA.Any;
   begin
      case Label_Ü is
         when 0 =>
            Index_Ü :=
              CORBA.Internals.Get_Aggregate_Element
                 (Item,
                  CSI.Helper.TC_EstablishContext,
                  CORBA.Unsigned_Long
                    (1));
            Result.establish_msg :=
              CSI.Helper.From_Any
                 (Index_Ü);
         when 1 =>
            Index_Ü :=
              CORBA.Internals.Get_Aggregate_Element
                 (Item,
                  CSI.Helper.TC_CompleteEstablishContext,
                  CORBA.Unsigned_Long
                    (1));
            Result.complete_msg :=
              CSI.Helper.From_Any
                 (Index_Ü);
         when 4 =>
            Index_Ü :=
              CORBA.Internals.Get_Aggregate_Element
                 (Item,
                  CSI.Helper.TC_ContextError,
                  CORBA.Unsigned_Long
                    (1));
            Result.error_msg :=
              CSI.Helper.From_Any
                 (Index_Ü);
         when 5 =>
            Index_Ü :=
              CORBA.Internals.Get_Aggregate_Element
                 (Item,
                  CSI.Helper.TC_MessageInContext,
                  CORBA.Unsigned_Long
                    (1));
            Result.in_context_msg :=
              CSI.Helper.From_Any
                 (Index_Ü);
         pragma Warnings (Off);
         when others =>
            null;
         pragma Warnings (On);

      end case;
      return Result;
   end From_Any;

   ------------
   -- To_Any --
   ------------

   function To_Any
     (Item : CSI.SASContextBody)
     return CORBA.Any
   is
      Result : constant CORBA.Any :=
        CORBA.Internals.Get_Empty_Any
           (TC_SASContextBody);
   begin
      PolyORB.Any.Set_Value
        (CORBA.Get_Container
           (Result).all,
         new PolyORB.Any.Content'Class'
           (CSI.Helper.Internals.Wrap
              (new CSI.SASContextBody'
                 (Item))),
         False);
      return Result;
   end To_Any;

   --------------
   -- From_Any --
   --------------

   function From_Any
     (Item : CORBA.Any)
     return CSI.StringOID
   is
      Result : constant CORBA.String :=
        CORBA.From_Any
           (Item);
   begin
      return CSI.StringOID
        (Result);
   end From_Any;

   ------------
   -- To_Any --
   ------------

   function To_Any
     (Item : CSI.StringOID)
     return CORBA.Any
   is
      Result : CORBA.Any :=
        CORBA.To_Any
           (CORBA.String
              (Item));
   begin
      CORBA.Internals.Set_Type
        (Result,
         TC_StringOID);
      return Result;
   end To_Any;

   -----------------------------
   -- Deferred_Initialization --
   -----------------------------

   procedure Deferred_Initialization is
   begin
      CSI.Helper.Internals.Initialize_IDL_SEQUENCE_octet;
      CSI.Helper.Internals.Initialize_X509CertificateChain;
      CSI.Helper.Internals.Initialize_IDL_SEQUENCE_octet_1;
      CSI.Helper.Internals.Initialize_X501DistinguishedName;
      CSI.Helper.Internals.Initialize_IDL_SEQUENCE_octet_2;
      CSI.Helper.Internals.Initialize_UTF8String;
      CSI.Helper.Internals.Initialize_IDL_SEQUENCE_octet_3;
      CSI.Helper.Internals.Initialize_OID;
      CSI.Helper.Internals.Initialize_IDL_SEQUENCE_CSI_OID;
      CSI.Helper.Internals.Initialize_OIDList;
      CSI.Helper.Internals.Initialize_IDL_SEQUENCE_octet_4;
      CSI.Helper.Internals.Initialize_GSSToken;
      CSI.Helper.Internals.Initialize_IDL_SEQUENCE_octet_5;
      CSI.Helper.Internals.Initialize_GSS_NT_ExportedName;
      CSI.Helper.Internals.Initialize_IDL_SEQUENCE_CSI_GSS_NT_ExportedName;
      CSI.Helper.Internals.Initialize_GSS_NT_ExportedNameList;
      CSI.Helper.Internals.Initialize_MsgType;
      CSI.Helper.Internals.Initialize_ContextId;
      CSI.Helper.Internals.Initialize_AuthorizationElementType;
      CSI.Helper.Internals.Initialize_IDL_SEQUENCE_octet_6;
      CSI.Helper.Internals.Initialize_AuthorizationElementContents;
      CSI.Helper.Internals.Initialize_AuthorizationElement;
      CSI.Helper.Internals.Initialize_IDL_SEQUENCE_CSI_AuthorizationElement;
      CSI.Helper.Internals.Initialize_AuthorizationToken;
      CSI.Helper.Internals.Initialize_IdentityTokenType;
      CSI.Helper.Internals.Initialize_IDL_SEQUENCE_octet_7;
      CSI.Helper.Internals.Initialize_IdentityExtension;
      CSI.Helper.Internals.Initialize_IdentityToken;
      CSI.Helper.Internals.Initialize_EstablishContext;
      CSI.Helper.Internals.Initialize_CompleteEstablishContext;
      CSI.Helper.Internals.Initialize_ContextError;
      CSI.Helper.Internals.Initialize_MessageInContext;
      CSI.Helper.Internals.Initialize_SASContextBody;
      CSI.Helper.Internals.Initialize_StringOID;
   end Deferred_Initialization;

begin
   declare
      use PolyORB.Utils.Strings;
      use PolyORB.Utils.Strings.Lists;
   begin
      PolyORB.Initialization.Register_Module
        (PolyORB.Initialization.Module_Info'
           (Name => +"CSI.Helper",
            Conflicts => PolyORB.Utils.Strings.Lists.Empty,
            Depends => +"any"
               & "corba",
            Provides => PolyORB.Utils.Strings.Lists.Empty,
            Implicit => False,
            Init => Deferred_Initialization'Access,
            Shutdown => null));
   end;
end CSI.Helper;

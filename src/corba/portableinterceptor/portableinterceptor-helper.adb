pragma Style_Checks ("NM32766");
pragma Warnings (Off, "use of an anonymous access type allocator");
pragma Warnings (Off, "unnecessary with of ancestor");

---------------------------------------------------
--  This file has been generated automatically from
--  /Users/heathdorn/Documents/Playground/Agents/RefactorTeam/code_refactor/PolyORB/idls/Misc/PortableInterceptor.idl
--  by IAC (IDL to Ada Compiler) 20.0w (rev. 41a9b833).
---------------------------------------------------
--  NOTE: If you modify this file by hand, your
--  changes will be lost when you re-run the
--  IDL to Ada compiler.
---------------------------------------------------

with PolyORB.Std;
with PolyORB.Exceptions;
with CORBA.Object.Helper;
with CORBA.IDL_Sequences.Helper;
with CORBA.IDL_Sequences;
with PolyORB.Utils.Strings;
with PolyORB.Utils.Strings.Lists;
with PolyORB.Initialization;

package body PortableInterceptor.Helper is

   
   package body Internals is

      procedure Raise_ForwardRequest_From_Any
        (Item : PolyORB.Any.Any;
         Message : PolyORB.Std.String);

      pragma No_Return (Raise_ForwardRequest_From_Any);

      -----------------------------------
      -- Raise_ForwardRequest_From_Any --
      -----------------------------------

      procedure Raise_ForwardRequest_From_Any
        (Item : PolyORB.Any.Any;
         Message : PolyORB.Std.String)
      is
         Members : constant PortableInterceptor.ForwardRequest_Members :=
           PortableInterceptor.Helper.From_Any
              (CORBA.Any
                 (Item));
      begin
         PolyORB.Exceptions.User_Raise_Exception
           (ForwardRequest'Identity,
            Members,
            Message);
      end Raise_ForwardRequest_From_Any;

      ForwardRequest_Initialized : PolyORB.Std.Boolean :=
        False;

      -------------------------------
      -- Initialize_ForwardRequest --
      -------------------------------

      procedure Initialize_ForwardRequest is
         Name_Ü : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("ForwardRequest_Members");
         Id_Ü : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("IDL:omg.org/PortableInterceptor/ForwardRequest:1.0");
         Arg_Name_forward : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("forward");
      begin
         if not ForwardRequest_Initialized
         then
            ForwardRequest_Initialized :=
              True;
            PortableInterceptor.Helper.TC_ForwardRequest :=
              CORBA.TypeCode.Internals.To_CORBA_Object
                 (PolyORB.Any.TypeCode.Tcf_Except);
            CORBA.Internals.Add_Parameter
              (TC_ForwardRequest,
               CORBA.To_Any
                 (Name_Ü));
            CORBA.Internals.Add_Parameter
              (TC_ForwardRequest,
               CORBA.To_Any
                 (Id_Ü));
            CORBA.Internals.Add_Parameter
              (TC_ForwardRequest,
               CORBA.To_Any
                 (CORBA.Object.Helper.TC_Object));
            CORBA.Internals.Add_Parameter
              (TC_ForwardRequest,
               CORBA.To_Any
                 (Arg_Name_forward));
            PolyORB.Exceptions.Register_Exception
              (CORBA.TypeCode.Internals.To_PolyORB_Object
                 (TC_ForwardRequest),
               Raise_ForwardRequest_From_Any'Access);
            CORBA.TypeCode.Internals.Disable_Ref_Counting
              (TC_ForwardRequest);
            CORBA.TypeCode.Internals.Freeze
              (TC_ForwardRequest);
         end if;
      end Initialize_ForwardRequest;

      ----------
      -- Wrap --
      ----------

      function Wrap
        (X : access PortableInterceptor.ReplyStatus)
        return PolyORB.Any.Content'Class
      is
      begin
         return CORBA.Wrap
           (CORBA.Short
              (X.all)'Unrestricted_Access);
      end Wrap;

      ReplyStatus_Initialized : PolyORB.Std.Boolean :=
        False;

      ----------------------------
      -- Initialize_ReplyStatus --
      ----------------------------

      procedure Initialize_ReplyStatus is
         Name_Ü : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("ReplyStatus");
         Id_Ü : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("IDL:omg.org/PortableInterceptor/ReplyStatus:1.0");
      begin
         if not ReplyStatus_Initialized
         then
            ReplyStatus_Initialized :=
              True;
            PortableInterceptor.Helper.TC_ReplyStatus :=
              CORBA.TypeCode.Internals.Build_Alias_TC
                 (Name => Name_Ü,
                  Id => Id_Ü,
                  Parent => CORBA.TC_Short);
            CORBA.TypeCode.Internals.Disable_Ref_Counting
              (TC_ReplyStatus);
            CORBA.TypeCode.Internals.Freeze
              (TC_ReplyStatus);
         end if;
      end Initialize_ReplyStatus;

      ----------
      -- Wrap --
      ----------

      function Wrap
        (X : access PortableInterceptor.SlotId)
        return PolyORB.Any.Content'Class
      is
      begin
         return CORBA.Wrap
           (CORBA.Unsigned_Long
              (X.all)'Unrestricted_Access);
      end Wrap;

      SlotId_Initialized : PolyORB.Std.Boolean :=
        False;

      -----------------------
      -- Initialize_SlotId --
      -----------------------

      procedure Initialize_SlotId is
         Name_Ü : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("SlotId");
         Id_Ü : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("IDL:omg.org/PortableInterceptor/SlotId:1.0");
      begin
         if not SlotId_Initialized
         then
            SlotId_Initialized :=
              True;
            PortableInterceptor.Helper.TC_SlotId :=
              CORBA.TypeCode.Internals.Build_Alias_TC
                 (Name => Name_Ü,
                  Id => Id_Ü,
                  Parent => CORBA.TC_Unsigned_Long);
            CORBA.TypeCode.Internals.Disable_Ref_Counting
              (TC_SlotId);
            CORBA.TypeCode.Internals.Freeze
              (TC_SlotId);
         end if;
      end Initialize_SlotId;

      procedure Raise_InvalidSlot_From_Any
        (Item : PolyORB.Any.Any;
         Message : PolyORB.Std.String);

      pragma No_Return (Raise_InvalidSlot_From_Any);

      --------------------------------
      -- Raise_InvalidSlot_From_Any --
      --------------------------------

      procedure Raise_InvalidSlot_From_Any
        (Item : PolyORB.Any.Any;
         Message : PolyORB.Std.String)
      is
         Members : constant PortableInterceptor.InvalidSlot_Members :=
           PortableInterceptor.Helper.From_Any
              (CORBA.Any
                 (Item));
      begin
         PolyORB.Exceptions.User_Raise_Exception
           (InvalidSlot'Identity,
            Members,
            Message);
      end Raise_InvalidSlot_From_Any;

      InvalidSlot_Initialized : PolyORB.Std.Boolean :=
        False;

      ----------------------------
      -- Initialize_InvalidSlot --
      ----------------------------

      procedure Initialize_InvalidSlot is
         Name_Ü : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("InvalidSlot_Members");
         Id_Ü : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("IDL:omg.org/PortableInterceptor/InvalidSlot:1.0");
      begin
         if not InvalidSlot_Initialized
         then
            InvalidSlot_Initialized :=
              True;
            PortableInterceptor.Helper.TC_InvalidSlot :=
              CORBA.TypeCode.Internals.To_CORBA_Object
                 (PolyORB.Any.TypeCode.Tcf_Except);
            CORBA.Internals.Add_Parameter
              (TC_InvalidSlot,
               CORBA.To_Any
                 (Name_Ü));
            CORBA.Internals.Add_Parameter
              (TC_InvalidSlot,
               CORBA.To_Any
                 (Id_Ü));
            PolyORB.Exceptions.Register_Exception
              (CORBA.TypeCode.Internals.To_PolyORB_Object
                 (TC_InvalidSlot),
               Raise_InvalidSlot_From_Any'Access);
            CORBA.TypeCode.Internals.Disable_Ref_Counting
              (TC_InvalidSlot);
            CORBA.TypeCode.Internals.Freeze
              (TC_InvalidSlot);
         end if;
      end Initialize_InvalidSlot;

      ----------
      -- Wrap --
      ----------

      function Wrap
        (X : access PortableInterceptor.ServerId)
        return PolyORB.Any.Content'Class
      is
      begin
         return CORBA.Wrap
           (CORBA.String
              (X.all)'Unrestricted_Access);
      end Wrap;

      ServerId_Initialized : PolyORB.Std.Boolean :=
        False;

      -------------------------
      -- Initialize_ServerId --
      -------------------------

      procedure Initialize_ServerId is
         Name_Ü : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("ServerId");
         Id_Ü : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("IDL:omg.org/PortableInterceptor/ServerId:1.0");
      begin
         if not ServerId_Initialized
         then
            ServerId_Initialized :=
              True;
            PortableInterceptor.Helper.TC_ServerId :=
              CORBA.TypeCode.Internals.Build_Alias_TC
                 (Name => Name_Ü,
                  Id => Id_Ü,
                  Parent => CORBA.TC_String);
            CORBA.TypeCode.Internals.Disable_Ref_Counting
              (TC_ServerId);
            CORBA.TypeCode.Internals.Freeze
              (TC_ServerId);
         end if;
      end Initialize_ServerId;

      ----------
      -- Wrap --
      ----------

      function Wrap
        (X : access PortableInterceptor.ORBId)
        return PolyORB.Any.Content'Class
      is
      begin
         return CORBA.Wrap
           (CORBA.String
              (X.all)'Unrestricted_Access);
      end Wrap;

      ORBId_Initialized : PolyORB.Std.Boolean :=
        False;

      ----------------------
      -- Initialize_ORBId --
      ----------------------

      procedure Initialize_ORBId is
         Name_Ü : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("ORBId");
         Id_Ü : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("IDL:omg.org/PortableInterceptor/ORBId:1.0");
      begin
         if not ORBId_Initialized
         then
            ORBId_Initialized :=
              True;
            PortableInterceptor.Helper.TC_ORBId :=
              CORBA.TypeCode.Internals.Build_Alias_TC
                 (Name => Name_Ü,
                  Id => Id_Ü,
                  Parent => CORBA.TC_String);
            CORBA.TypeCode.Internals.Disable_Ref_Counting
              (TC_ORBId);
            CORBA.TypeCode.Internals.Freeze
              (TC_ORBId);
         end if;
      end Initialize_ORBId;

      AdapterName_Initialized : PolyORB.Std.Boolean :=
        False;

      ----------------------------
      -- Initialize_AdapterName --
      ----------------------------

      procedure Initialize_AdapterName is
         Name_Ü : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("AdapterName");
         Id_Ü : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("IDL:omg.org/PortableInterceptor/AdapterName:1.0");
      begin
         if not AdapterName_Initialized
         then
            AdapterName_Initialized :=
              True;
            PortableInterceptor.Helper.TC_AdapterName :=
              CORBA.TypeCode.Internals.Build_Alias_TC
                 (Name => Name_Ü,
                  Id => Id_Ü,
                  Parent => CORBA.IDL_Sequences.Helper.TC_StringSeq);
            CORBA.TypeCode.Internals.Disable_Ref_Counting
              (TC_AdapterName);
            CORBA.TypeCode.Internals.Freeze
              (TC_AdapterName);
         end if;
      end Initialize_AdapterName;

      ObjectId_Initialized : PolyORB.Std.Boolean :=
        False;

      -------------------------
      -- Initialize_ObjectId --
      -------------------------

      procedure Initialize_ObjectId is
         Name_Ü : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("ObjectId");
         Id_Ü : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("IDL:omg.org/PortableInterceptor/ObjectId:1.0");
      begin
         if not ObjectId_Initialized
         then
            ObjectId_Initialized :=
              True;
            PortableInterceptor.Helper.TC_ObjectId :=
              CORBA.TypeCode.Internals.Build_Alias_TC
                 (Name => Name_Ü,
                  Id => Id_Ü,
                  Parent => CORBA.IDL_Sequences.Helper.TC_OctetSeq);
            CORBA.TypeCode.Internals.Disable_Ref_Counting
              (TC_ObjectId);
            CORBA.TypeCode.Internals.Freeze
              (TC_ObjectId);
         end if;
      end Initialize_ObjectId;

      ----------
      -- Wrap --
      ----------

      function Wrap
        (X : access PortableInterceptor.AdapterManagerId)
        return PolyORB.Any.Content'Class
      is
      begin
         return CORBA.Wrap
           (CORBA.String
              (X.all)'Unrestricted_Access);
      end Wrap;

      AdapterManagerId_Initialized : PolyORB.Std.Boolean :=
        False;

      ---------------------------------
      -- Initialize_AdapterManagerId --
      ---------------------------------

      procedure Initialize_AdapterManagerId is
         Name_Ü : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("AdapterManagerId");
         Id_Ü : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("IDL:omg.org/PortableInterceptor/AdapterManagerId:1.0");
      begin
         if not AdapterManagerId_Initialized
         then
            AdapterManagerId_Initialized :=
              True;
            PortableInterceptor.Helper.TC_AdapterManagerId :=
              CORBA.TypeCode.Internals.Build_Alias_TC
                 (Name => Name_Ü,
                  Id => Id_Ü,
                  Parent => CORBA.TC_String);
            CORBA.TypeCode.Internals.Disable_Ref_Counting
              (TC_AdapterManagerId);
            CORBA.TypeCode.Internals.Freeze
              (TC_AdapterManagerId);
         end if;
      end Initialize_AdapterManagerId;

      ----------
      -- Wrap --
      ----------

      function Wrap
        (X : access PortableInterceptor.AdapterState)
        return PolyORB.Any.Content'Class
      is
      begin
         return CORBA.Wrap
           (CORBA.Short
              (X.all)'Unrestricted_Access);
      end Wrap;

      AdapterState_Initialized : PolyORB.Std.Boolean :=
        False;

      -----------------------------
      -- Initialize_AdapterState --
      -----------------------------

      procedure Initialize_AdapterState is
         Name_Ü : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("AdapterState");
         Id_Ü : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("IDL:omg.org/PortableInterceptor/AdapterState:1.0");
      begin
         if not AdapterState_Initialized
         then
            AdapterState_Initialized :=
              True;
            PortableInterceptor.Helper.TC_AdapterState :=
              CORBA.TypeCode.Internals.Build_Alias_TC
                 (Name => Name_Ü,
                  Id => Id_Ü,
                  Parent => CORBA.TC_Short);
            CORBA.TypeCode.Internals.Disable_Ref_Counting
              (TC_AdapterState);
            CORBA.TypeCode.Internals.Freeze
              (TC_AdapterState);
         end if;
      end Initialize_AdapterState;

   end Internals;

   --------------
   -- From_Any --
   --------------

   function From_Any
     (Item : CORBA.Any)
     return PortableInterceptor.ForwardRequest_Members
   is
   begin
      return (forward => CORBA.Object.Helper.From_Any
        (CORBA.Internals.Get_Aggregate_Element
           (Item,
            CORBA.Object.Helper.TC_Object,
            0)));
   end From_Any;

   ------------
   -- To_Any --
   ------------

   function To_Any
     (Item : PortableInterceptor.ForwardRequest_Members)
     return CORBA.Any
   is
      Result_Ü : CORBA.Any :=
        CORBA.Internals.Get_Empty_Any_Aggregate
           (PortableInterceptor.Helper.TC_ForwardRequest);
   begin
      CORBA.Internals.Add_Aggregate_Element
        (Result_Ü,
         CORBA.Object.Helper.To_Any
           (Item.forward));
      return Result_Ü;
   end To_Any;

   --------------------------
   -- Raise_ForwardRequest --
   --------------------------

   procedure Raise_ForwardRequest
     (Members : PortableInterceptor.ForwardRequest_Members)
   is
   begin
      PolyORB.Exceptions.User_Raise_Exception
        (ForwardRequest'Identity,
         Members);
   end Raise_ForwardRequest;

   --------------
   -- From_Any --
   --------------

   function From_Any
     (Item : CORBA.Any)
     return PortableInterceptor.ReplyStatus
   is
      Result : constant CORBA.Short :=
        CORBA.From_Any
           (Item);
   begin
      return PortableInterceptor.ReplyStatus
        (Result);
   end From_Any;

   ------------
   -- To_Any --
   ------------

   function To_Any
     (Item : PortableInterceptor.ReplyStatus)
     return CORBA.Any
   is
      Result : CORBA.Any :=
        CORBA.To_Any
           (CORBA.Short
              (Item));
   begin
      CORBA.Internals.Set_Type
        (Result,
         TC_ReplyStatus);
      return Result;
   end To_Any;

   --------------
   -- From_Any --
   --------------

   function From_Any
     (Item : CORBA.Any)
     return PortableInterceptor.SlotId
   is
      Result : constant CORBA.Unsigned_Long :=
        CORBA.From_Any
           (Item);
   begin
      return PortableInterceptor.SlotId
        (Result);
   end From_Any;

   ------------
   -- To_Any --
   ------------

   function To_Any
     (Item : PortableInterceptor.SlotId)
     return CORBA.Any
   is
      Result : CORBA.Any :=
        CORBA.To_Any
           (CORBA.Unsigned_Long
              (Item));
   begin
      CORBA.Internals.Set_Type
        (Result,
         TC_SlotId);
      return Result;
   end To_Any;

   --------------
   -- From_Any --
   --------------

   function From_Any
     (Item : CORBA.Any)
     return PortableInterceptor.InvalidSlot_Members
   is
      Result_Ü : PortableInterceptor.InvalidSlot_Members;
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
     (Item : PortableInterceptor.InvalidSlot_Members)
     return CORBA.Any
   is
      Result_Ü : constant CORBA.Any :=
        CORBA.Internals.Get_Empty_Any_Aggregate
           (PortableInterceptor.Helper.TC_InvalidSlot);
      pragma Warnings (Off);
      pragma Unreferenced (Item);
      pragma Warnings (On);
   begin
      return Result_Ü;
   end To_Any;

   -----------------------
   -- Raise_InvalidSlot --
   -----------------------

   procedure Raise_InvalidSlot
     (Members : PortableInterceptor.InvalidSlot_Members)
   is
   begin
      PolyORB.Exceptions.User_Raise_Exception
        (InvalidSlot'Identity,
         Members);
   end Raise_InvalidSlot;

   --------------
   -- From_Any --
   --------------

   function From_Any
     (Item : CORBA.Any)
     return PortableInterceptor.ServerId
   is
      Result : constant CORBA.String :=
        CORBA.From_Any
           (Item);
   begin
      return PortableInterceptor.ServerId
        (Result);
   end From_Any;

   ------------
   -- To_Any --
   ------------

   function To_Any
     (Item : PortableInterceptor.ServerId)
     return CORBA.Any
   is
      Result : CORBA.Any :=
        CORBA.To_Any
           (CORBA.String
              (Item));
   begin
      CORBA.Internals.Set_Type
        (Result,
         TC_ServerId);
      return Result;
   end To_Any;

   --------------
   -- From_Any --
   --------------

   function From_Any
     (Item : CORBA.Any)
     return PortableInterceptor.ORBId
   is
      Result : constant CORBA.String :=
        CORBA.From_Any
           (Item);
   begin
      return PortableInterceptor.ORBId
        (Result);
   end From_Any;

   ------------
   -- To_Any --
   ------------

   function To_Any
     (Item : PortableInterceptor.ORBId)
     return CORBA.Any
   is
      Result : CORBA.Any :=
        CORBA.To_Any
           (CORBA.String
              (Item));
   begin
      CORBA.Internals.Set_Type
        (Result,
         TC_ORBId);
      return Result;
   end To_Any;

   --------------
   -- From_Any --
   --------------

   function From_Any
     (Item : CORBA.Any)
     return PortableInterceptor.AdapterName
   is
      Result : constant CORBA.IDL_Sequences.StringSeq :=
        CORBA.IDL_Sequences.Helper.From_Any
           (Item);
   begin
      return PortableInterceptor.AdapterName
        (Result);
   end From_Any;

   ------------
   -- To_Any --
   ------------

   function To_Any
     (Item : PortableInterceptor.AdapterName)
     return CORBA.Any
   is
      Result : CORBA.Any :=
        CORBA.IDL_Sequences.Helper.To_Any
           (CORBA.IDL_Sequences.StringSeq
              (Item));
   begin
      CORBA.Internals.Set_Type
        (Result,
         TC_AdapterName);
      return Result;
   end To_Any;

   --------------
   -- From_Any --
   --------------

   function From_Any
     (Item : CORBA.Any)
     return PortableInterceptor.ObjectId
   is
      Result : constant CORBA.IDL_Sequences.OctetSeq :=
        CORBA.IDL_Sequences.Helper.From_Any
           (Item);
   begin
      return PortableInterceptor.ObjectId
        (Result);
   end From_Any;

   ------------
   -- To_Any --
   ------------

   function To_Any
     (Item : PortableInterceptor.ObjectId)
     return CORBA.Any
   is
      Result : CORBA.Any :=
        CORBA.IDL_Sequences.Helper.To_Any
           (CORBA.IDL_Sequences.OctetSeq
              (Item));
   begin
      CORBA.Internals.Set_Type
        (Result,
         TC_ObjectId);
      return Result;
   end To_Any;

   --------------
   -- From_Any --
   --------------

   function From_Any
     (Item : CORBA.Any)
     return PortableInterceptor.AdapterManagerId
   is
      Result : constant CORBA.String :=
        CORBA.From_Any
           (Item);
   begin
      return PortableInterceptor.AdapterManagerId
        (Result);
   end From_Any;

   ------------
   -- To_Any --
   ------------

   function To_Any
     (Item : PortableInterceptor.AdapterManagerId)
     return CORBA.Any
   is
      Result : CORBA.Any :=
        CORBA.To_Any
           (CORBA.String
              (Item));
   begin
      CORBA.Internals.Set_Type
        (Result,
         TC_AdapterManagerId);
      return Result;
   end To_Any;

   --------------
   -- From_Any --
   --------------

   function From_Any
     (Item : CORBA.Any)
     return PortableInterceptor.AdapterState
   is
      Result : constant CORBA.Short :=
        CORBA.From_Any
           (Item);
   begin
      return PortableInterceptor.AdapterState
        (Result);
   end From_Any;

   ------------
   -- To_Any --
   ------------

   function To_Any
     (Item : PortableInterceptor.AdapterState)
     return CORBA.Any
   is
      Result : CORBA.Any :=
        CORBA.To_Any
           (CORBA.Short
              (Item));
   begin
      CORBA.Internals.Set_Type
        (Result,
         TC_AdapterState);
      return Result;
   end To_Any;

   -----------------------------
   -- Deferred_Initialization --
   -----------------------------

   procedure Deferred_Initialization is
   begin
      PortableInterceptor.Helper.Internals.Initialize_ForwardRequest;
      PortableInterceptor.Helper.Internals.Initialize_ReplyStatus;
      PortableInterceptor.Helper.Internals.Initialize_SlotId;
      PortableInterceptor.Helper.Internals.Initialize_InvalidSlot;
      PortableInterceptor.Helper.Internals.Initialize_ServerId;
      PortableInterceptor.Helper.Internals.Initialize_ORBId;
      PortableInterceptor.Helper.Internals.Initialize_AdapterName;
      PortableInterceptor.Helper.Internals.Initialize_ObjectId;
      PortableInterceptor.Helper.Internals.Initialize_AdapterManagerId;
      PortableInterceptor.Helper.Internals.Initialize_AdapterState;
   end Deferred_Initialization;

begin
   declare
      use PolyORB.Utils.Strings;
      use PolyORB.Utils.Strings.Lists;
   begin
      PolyORB.Initialization.Register_Module
        (PolyORB.Initialization.Module_Info'
           (Name => +"PortableInterceptor.Helper",
            Conflicts => PolyORB.Utils.Strings.Lists.Empty,
            Depends => +"any"
               & "corba.object"
               & "exceptions"
               & "corba",
            Provides => PolyORB.Utils.Strings.Lists.Empty,
            Implicit => False,
            Init => Deferred_Initialization'Access,
            Shutdown => null));
   end;
end PortableInterceptor.Helper;

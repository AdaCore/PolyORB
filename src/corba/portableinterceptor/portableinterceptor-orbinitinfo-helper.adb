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
with PolyORB.Utils.Strings;
with PolyORB.Utils.Strings.Lists;
with PolyORB.Initialization;

package body PortableInterceptor.ORBInitInfo.Helper is

   
   package body Internals is

      ORBInitInfo_Initialized : PolyORB.Std.Boolean :=
        False;

      ----------------------------
      -- Initialize_ORBInitInfo --
      ----------------------------

      procedure Initialize_ORBInitInfo is
         Name_Ü : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("ORBInitInfo");
         Id_Ü : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("IDL:omg.org/PortableInterceptor/ORBInitInfo:1.0");
      begin
         if not ORBInitInfo_Initialized
         then
            ORBInitInfo_Initialized :=
              True;
            PortableInterceptor.ORBInitInfo.Helper.TC_ORBInitInfo :=
              CORBA.TypeCode.Internals.To_CORBA_Object
                 (PolyORB.Any.TypeCode.Tcf_Object);
            CORBA.Internals.Add_Parameter
              (TC_ORBInitInfo,
               CORBA.To_Any
                 (Name_Ü));
            CORBA.Internals.Add_Parameter
              (TC_ORBInitInfo,
               CORBA.To_Any
                 (Id_Ü));
            CORBA.TypeCode.Internals.Disable_Ref_Counting
              (TC_ORBInitInfo);
            CORBA.TypeCode.Internals.Freeze
              (TC_ORBInitInfo);
         end if;
      end Initialize_ORBInitInfo;

      ----------
      -- Wrap --
      ----------

      function Wrap
        (X : access PortableInterceptor.ORBInitInfo.ObjectId)
        return PolyORB.Any.Content'Class
      is
      begin
         return CORBA.Wrap
           (CORBA.String
              (X.all)'Unrestricted_Access);
      end Wrap;

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
              ("IDL:omg.org/PortableInterceptor/ORBInitInfo/ObjectId:1.0");
      begin
         if not ObjectId_Initialized
         then
            ObjectId_Initialized :=
              True;
            PortableInterceptor.ORBInitInfo.Helper.TC_ObjectId :=
              CORBA.TypeCode.Internals.Build_Alias_TC
                 (Name => Name_Ü,
                  Id => Id_Ü,
                  Parent => CORBA.TC_String);
            CORBA.TypeCode.Internals.Disable_Ref_Counting
              (TC_ObjectId);
            CORBA.TypeCode.Internals.Freeze
              (TC_ObjectId);
         end if;
      end Initialize_ObjectId;

      procedure Raise_DuplicateName_From_Any
        (Item : PolyORB.Any.Any;
         Message : PolyORB.Std.String);

      pragma No_Return (Raise_DuplicateName_From_Any);

      ----------------------------------
      -- Raise_DuplicateName_From_Any --
      ----------------------------------

      procedure Raise_DuplicateName_From_Any
        (Item : PolyORB.Any.Any;
         Message : PolyORB.Std.String)
      is
         Members : constant PortableInterceptor.ORBInitInfo.DuplicateName_Members :=
           PortableInterceptor.ORBInitInfo.Helper.From_Any
              (CORBA.Any
                 (Item));
      begin
         PolyORB.Exceptions.User_Raise_Exception
           (DuplicateName'Identity,
            Members,
            Message);
      end Raise_DuplicateName_From_Any;

      DuplicateName_Initialized : PolyORB.Std.Boolean :=
        False;

      ------------------------------
      -- Initialize_DuplicateName --
      ------------------------------

      procedure Initialize_DuplicateName is
         Name_Ü : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("DuplicateName_Members");
         Id_Ü : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("IDL:omg.org/PortableInterceptor/ORBInitInfo/DuplicateName:1.0");
         Arg_Name_name : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("name");
      begin
         if not DuplicateName_Initialized
         then
            DuplicateName_Initialized :=
              True;
            PortableInterceptor.ORBInitInfo.Helper.TC_DuplicateName :=
              CORBA.TypeCode.Internals.To_CORBA_Object
                 (PolyORB.Any.TypeCode.Tcf_Except);
            CORBA.Internals.Add_Parameter
              (TC_DuplicateName,
               CORBA.To_Any
                 (Name_Ü));
            CORBA.Internals.Add_Parameter
              (TC_DuplicateName,
               CORBA.To_Any
                 (Id_Ü));
            CORBA.Internals.Add_Parameter
              (TC_DuplicateName,
               CORBA.To_Any
                 (CORBA.TC_String));
            CORBA.Internals.Add_Parameter
              (TC_DuplicateName,
               CORBA.To_Any
                 (Arg_Name_name));
            PolyORB.Exceptions.Register_Exception
              (CORBA.TypeCode.Internals.To_PolyORB_Object
                 (TC_DuplicateName),
               Raise_DuplicateName_From_Any'Access);
            CORBA.TypeCode.Internals.Disable_Ref_Counting
              (TC_DuplicateName);
            CORBA.TypeCode.Internals.Freeze
              (TC_DuplicateName);
         end if;
      end Initialize_DuplicateName;

      procedure Raise_InvalidName_From_Any
        (Item : PolyORB.Any.Any;
         Message : PolyORB.Std.String);

      pragma No_Return (Raise_InvalidName_From_Any);

      --------------------------------
      -- Raise_InvalidName_From_Any --
      --------------------------------

      procedure Raise_InvalidName_From_Any
        (Item : PolyORB.Any.Any;
         Message : PolyORB.Std.String)
      is
         Members : constant PortableInterceptor.ORBInitInfo.InvalidName_Members :=
           PortableInterceptor.ORBInitInfo.Helper.From_Any
              (CORBA.Any
                 (Item));
      begin
         PolyORB.Exceptions.User_Raise_Exception
           (InvalidName'Identity,
            Members,
            Message);
      end Raise_InvalidName_From_Any;

      InvalidName_Initialized : PolyORB.Std.Boolean :=
        False;

      ----------------------------
      -- Initialize_InvalidName --
      ----------------------------

      procedure Initialize_InvalidName is
         Name_Ü : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("InvalidName_Members");
         Id_Ü : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("IDL:omg.org/PortableInterceptor/ORBInitInfo/InvalidName:1.0");
      begin
         if not InvalidName_Initialized
         then
            InvalidName_Initialized :=
              True;
            PortableInterceptor.ORBInitInfo.Helper.TC_InvalidName :=
              CORBA.TypeCode.Internals.To_CORBA_Object
                 (PolyORB.Any.TypeCode.Tcf_Except);
            CORBA.Internals.Add_Parameter
              (TC_InvalidName,
               CORBA.To_Any
                 (Name_Ü));
            CORBA.Internals.Add_Parameter
              (TC_InvalidName,
               CORBA.To_Any
                 (Id_Ü));
            PolyORB.Exceptions.Register_Exception
              (CORBA.TypeCode.Internals.To_PolyORB_Object
                 (TC_InvalidName),
               Raise_InvalidName_From_Any'Access);
            CORBA.TypeCode.Internals.Disable_Ref_Counting
              (TC_InvalidName);
            CORBA.TypeCode.Internals.Freeze
              (TC_InvalidName);
         end if;
      end Initialize_InvalidName;

   end Internals;

   ----------------------------
   -- Unchecked_To_Local_Ref --
   ----------------------------

   function Unchecked_To_Local_Ref
     (The_Ref : CORBA.Object.Ref'Class)
     return PortableInterceptor.ORBInitInfo.Local_Ref
   is
      Result : PortableInterceptor.ORBInitInfo.Local_Ref;
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
     return PortableInterceptor.ORBInitInfo.Local_Ref
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
     return PortableInterceptor.ORBInitInfo.ObjectId
   is
      Result : constant CORBA.String :=
        CORBA.From_Any
           (Item);
   begin
      return PortableInterceptor.ORBInitInfo.ObjectId
        (Result);
   end From_Any;

   ------------
   -- To_Any --
   ------------

   function To_Any
     (Item : PortableInterceptor.ORBInitInfo.ObjectId)
     return CORBA.Any
   is
      Result : CORBA.Any :=
        CORBA.To_Any
           (CORBA.String
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
     return PortableInterceptor.ORBInitInfo.DuplicateName_Members
   is
   begin
      return (name => CORBA.From_Any
        (CORBA.Internals.Get_Aggregate_Element
           (Item,
            CORBA.TC_String,
            0)));
   end From_Any;

   ------------
   -- To_Any --
   ------------

   function To_Any
     (Item : PortableInterceptor.ORBInitInfo.DuplicateName_Members)
     return CORBA.Any
   is
      Result_Ü : CORBA.Any :=
        CORBA.Internals.Get_Empty_Any_Aggregate
           (PortableInterceptor.ORBInitInfo.Helper.TC_DuplicateName);
   begin
      CORBA.Internals.Add_Aggregate_Element
        (Result_Ü,
         CORBA.To_Any
           (Item.name));
      return Result_Ü;
   end To_Any;

   -------------------------
   -- Raise_DuplicateName --
   -------------------------

   procedure Raise_DuplicateName
     (Members : PortableInterceptor.ORBInitInfo.DuplicateName_Members)
   is
   begin
      PolyORB.Exceptions.User_Raise_Exception
        (DuplicateName'Identity,
         Members);
   end Raise_DuplicateName;

   --------------
   -- From_Any --
   --------------

   function From_Any
     (Item : CORBA.Any)
     return PortableInterceptor.ORBInitInfo.InvalidName_Members
   is
      Result_Ü : PortableInterceptor.ORBInitInfo.InvalidName_Members;
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
     (Item : PortableInterceptor.ORBInitInfo.InvalidName_Members)
     return CORBA.Any
   is
      Result_Ü : constant CORBA.Any :=
        CORBA.Internals.Get_Empty_Any_Aggregate
           (PortableInterceptor.ORBInitInfo.Helper.TC_InvalidName);
      pragma Warnings (Off);
      pragma Unreferenced (Item);
      pragma Warnings (On);
   begin
      return Result_Ü;
   end To_Any;

   -----------------------
   -- Raise_InvalidName --
   -----------------------

   procedure Raise_InvalidName
     (Members : PortableInterceptor.ORBInitInfo.InvalidName_Members)
   is
   begin
      PolyORB.Exceptions.User_Raise_Exception
        (InvalidName'Identity,
         Members);
   end Raise_InvalidName;

   -----------------------------
   -- Deferred_Initialization --
   -----------------------------

   procedure Deferred_Initialization is
   begin
      PortableInterceptor.ORBInitInfo.Helper.Internals.Initialize_ORBInitInfo;
      PortableInterceptor.ORBInitInfo.Helper.Internals.Initialize_ObjectId;
      PortableInterceptor.ORBInitInfo.Helper.Internals.Initialize_DuplicateName;
      PortableInterceptor.ORBInitInfo.Helper.Internals.Initialize_InvalidName;
   end Deferred_Initialization;

begin
   declare
      use PolyORB.Utils.Strings;
      use PolyORB.Utils.Strings.Lists;
   begin
      PolyORB.Initialization.Register_Module
        (PolyORB.Initialization.Module_Info'
           (Name => +"PortableInterceptor.ORBInitInfo.Helper",
            Conflicts => PolyORB.Utils.Strings.Lists.Empty,
            Depends => +"any"
               & "corba"
               & "exceptions",
            Provides => PolyORB.Utils.Strings.Lists.Empty,
            Implicit => False,
            Init => Deferred_Initialization'Access,
            Shutdown => null));
   end;
end PortableInterceptor.ORBInitInfo.Helper;

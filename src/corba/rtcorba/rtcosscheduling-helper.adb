pragma Style_Checks ("NM32766");
pragma Warnings (Off, "use of an anonymous access type allocator");
pragma Warnings (Off, "unnecessary with of ancestor");

---------------------------------------------------
--  This file has been generated automatically from
--  /Users/heathdorn/Documents/Playground/Agents/RefactorTeam/code_refactor/PolyORB/idls/RTCORBA/RTCosScheduling.idl
--  by IAC (IDL to Ada Compiler) 20.0w (rev. 41a9b833).
---------------------------------------------------
--  NOTE: If you modify this file by hand, your
--  changes will be lost when you re-run the
--  IDL to Ada compiler.
---------------------------------------------------

with PolyORB.Any;
with PolyORB.Std;
with PolyORB.Exceptions;
with PolyORB.Utils.Strings;
with PolyORB.Utils.Strings.Lists;
with PolyORB.Initialization;

package body RTCosScheduling.Helper is

   
   package body Internals is

      procedure Raise_UnknownName_From_Any
        (Item : PolyORB.Any.Any;
         Message : PolyORB.Std.String);

      pragma No_Return (Raise_UnknownName_From_Any);

      --------------------------------
      -- Raise_UnknownName_From_Any --
      --------------------------------

      procedure Raise_UnknownName_From_Any
        (Item : PolyORB.Any.Any;
         Message : PolyORB.Std.String)
      is
         Members : constant RTCosScheduling.UnknownName_Members :=
           RTCosScheduling.Helper.From_Any
              (CORBA.Any
                 (Item));
      begin
         PolyORB.Exceptions.User_Raise_Exception
           (UnknownName'Identity,
            Members,
            Message);
      end Raise_UnknownName_From_Any;

      UnknownName_Initialized : PolyORB.Std.Boolean :=
        False;

      ----------------------------
      -- Initialize_UnknownName --
      ----------------------------

      procedure Initialize_UnknownName is
         Name_Ü : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("UnknownName_Members");
         Id_Ü : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("IDL:RTCosScheduling/UnknownName:1.0");
      begin
         if not UnknownName_Initialized
         then
            UnknownName_Initialized :=
              True;
            RTCosScheduling.Helper.TC_UnknownName :=
              CORBA.TypeCode.Internals.To_CORBA_Object
                 (PolyORB.Any.TypeCode.Tcf_Except);
            CORBA.Internals.Add_Parameter
              (TC_UnknownName,
               CORBA.To_Any
                 (Name_Ü));
            CORBA.Internals.Add_Parameter
              (TC_UnknownName,
               CORBA.To_Any
                 (Id_Ü));
            PolyORB.Exceptions.Register_Exception
              (CORBA.TypeCode.Internals.To_PolyORB_Object
                 (TC_UnknownName),
               Raise_UnknownName_From_Any'Access);
            CORBA.TypeCode.Internals.Disable_Ref_Counting
              (TC_UnknownName);
            CORBA.TypeCode.Internals.Freeze
              (TC_UnknownName);
         end if;
      end Initialize_UnknownName;

   end Internals;

   --------------
   -- From_Any --
   --------------

   function From_Any
     (Item : CORBA.Any)
     return RTCosScheduling.UnknownName_Members
   is
      Result_Ü : RTCosScheduling.UnknownName_Members;
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
     (Item : RTCosScheduling.UnknownName_Members)
     return CORBA.Any
   is
      Result_Ü : constant CORBA.Any :=
        CORBA.Internals.Get_Empty_Any_Aggregate
           (RTCosScheduling.Helper.TC_UnknownName);
      pragma Warnings (Off);
      pragma Unreferenced (Item);
      pragma Warnings (On);
   begin
      return Result_Ü;
   end To_Any;

   -----------------------
   -- Raise_UnknownName --
   -----------------------

   procedure Raise_UnknownName
     (Members : RTCosScheduling.UnknownName_Members)
   is
   begin
      PolyORB.Exceptions.User_Raise_Exception
        (UnknownName'Identity,
         Members);
   end Raise_UnknownName;

   -----------------------------
   -- Deferred_Initialization --
   -----------------------------

   procedure Deferred_Initialization is
   begin
      RTCosScheduling.Helper.Internals.Initialize_UnknownName;
   end Deferred_Initialization;

begin
   declare
      use PolyORB.Utils.Strings;
      use PolyORB.Utils.Strings.Lists;
   begin
      PolyORB.Initialization.Register_Module
        (PolyORB.Initialization.Module_Info'
           (Name => +"RTCosScheduling.Helper",
            Conflicts => PolyORB.Utils.Strings.Lists.Empty,
            Depends => +"any"
               & "exceptions",
            Provides => PolyORB.Utils.Strings.Lists.Empty,
            Implicit => False,
            Init => Deferred_Initialization'Access,
            Shutdown => null));
   end;
end RTCosScheduling.Helper;

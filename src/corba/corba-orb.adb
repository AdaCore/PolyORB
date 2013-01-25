------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                            C O R B A . O R B                             --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2001-2013, Free Software Foundation, Inc.          --
--                                                                          --
-- This is free software;  you can redistribute it  and/or modify it  under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for  more details.                                               --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
--                  PolyORB is maintained by AdaCore                        --
--                     (email: sales@adacore.com)                           --
--                                                                          --
------------------------------------------------------------------------------

--  The following subprograms still have to be implemented :
--
--  Get_Default_Context --
--  Create_Fixed_Tc --
--  Create_Native_Tc --
--  Create_Recursive_Sequence_Tc --

with Ada.Command_Line;
with Ada.Exceptions;

with PolyORB.Initial_References;
with PolyORB.CORBA_P.Local;
with PolyORB.CORBA_P.ORB_Init;
with PolyORB.CORBA_P.Policy_Management;

with PolyORB.Initialization;

with PolyORB.Log;
with PolyORB.ORB;
with PolyORB.Parameters;
with PolyORB.References.IOR;
with PolyORB.Setup;
with PolyORB.Utils.Strings.Lists;

package body CORBA.ORB is

   use PolyORB.Log;
   use PolyORB.ORB;
   use PolyORB.Setup;

   package L is new PolyORB.Log.Facility_Log ("corba.orb");
   procedure O (Message : Standard.String; Level : Log_Level := Debug)
     renames L.Output;
   function C (Level : Log_Level := Debug) return Boolean
     renames L.Enabled;

   procedure Register_Initial_Reference
     (Identifier : ObjectId;
      IOR        : CORBA.String);
   --  Register an initial reference from an IOR given
   --  through the configuration subsystem.

   function ORB_Init_Initial_References
     (Value : Standard.String)
     return Boolean;
   --  Initialisation routine for the InitRef suffix

   ---------------------------------
   -- ORB_Init_Initial_References --
   ---------------------------------

   function ORB_Init_Initial_References
     (Value : Standard.String)
     return Boolean
   is
      Pos : constant Integer := PolyORB.Utils.Find (Value, Value'First, '=');

   begin
      if Pos = Value'Last + 1 then
         Raise_Bad_Param (Default_Sys_Member);

      else
         pragma Debug (C, O ("Registering " & Value (Value'First .. Pos - 1)
                          & " with " & Value (Pos + 1 .. Value'Last)));

         Register_Initial_Reference
           (To_CORBA_String (Value (Value'First .. Pos - 1)),
            To_CORBA_String (Value (Pos + 1 .. Value'Last)));

         return True;
      end if;
   end ORB_Init_Initial_References;

   ----------------------------
   -- Command_Line_Arguments --
   ----------------------------

   function Command_Line_Arguments return Arg_List is
      use Ada.Command_Line;

      Result : Arg_List;
   begin
      for J in 1 .. Argument_Count loop
         Append (Result, CORBA.To_CORBA_String (Argument (J)));
      end loop;

      return Result;
   end Command_Line_Arguments;

   ----------
   -- Init --
   ----------

   procedure Init
     (ORB_Identifier : ORBid;
      Argv           : in out Arg_List)
   is
      pragma Unreferenced (ORB_Identifier);

      use PolyORB.CORBA_P.ORB_Init;
      use PolyORB.Initialization;

      Pos : Natural := 1;

      ORB_Prefix : constant Standard.String := "-ORB";

      Not_Initialized_One : Boolean := False;

   begin
      --  Implementation Note: We first run Initialize_World to allow packages
      --  to register helper routines to parse specific command line arguments.

      if not Is_Initialized then
         Initialize_World;
      end if;

      pragma Debug (C, O ("Init: enter"));

      while Pos <= Length (Argv) loop
         declare
            Suffix : constant Standard.String :=
              To_Standard_String (Get_Element (Argv, Pos));

            Initialized : Boolean := False;
            Space_Index : Positive;

         begin
            pragma Debug (C, O ("Processing " & Suffix));

            if PolyORB.Utils.Has_Prefix (Suffix, ORB_Prefix) then

               pragma Debug
                 (C, O ("Possible suffix is "
                     & Suffix (Suffix'First + ORB_Prefix'Length
                               .. Suffix'Last)));

               Space_Index :=
                 PolyORB.Utils.Find_Whitespace (Suffix, Suffix'First);

               --  Test if parameter is -ORB<suffix><whitespace><value>

               if Space_Index <= Suffix'Last then
                  Initialized
                    := PolyORB.CORBA_P.ORB_Init.Initialize
                    (Suffix (Suffix'First + ORB_Prefix'Length
                             .. Suffix'Last));

                  if Initialized then
                     Delete (Argv, Pos, Pos);
                  end if;

               --  Test if parameter is -ORB<suffix> and next argument is
               --  a <value>.

               elsif Pos < Length (Argv) then
                  declare
                     Value : constant Standard.String :=
                       To_Standard_String
                         (Get_Element (Argv, Pos + 1));

                  begin
                     pragma Debug
                       (C, O ("Try to initialize ("
                           & Suffix (Suffix'First + ORB_Prefix'Length
                                     .. Suffix'Last)
                           & "," & Value & ")"));

                     Initialized :=
                       PolyORB.CORBA_P.ORB_Init.Initialize
                         (Suffix (Suffix'First + ORB_Prefix'Length
                               .. Suffix'Last),
                          Value);

                     if Initialized then
                        Delete (Argv, Pos, Pos + 1);
                     end if;
                  end;
               end if;

               --  Test if parameter is -ORB<suffix><value>

               if not Initialized then
                  Initialized :=
                    PolyORB.CORBA_P.ORB_Init.Initialize
                      (Suffix (Suffix'First + ORB_Prefix'Length
                            .. Suffix'Last));

                  if Initialized then
                     Delete (Argv, Pos, Pos);
                  end if;
               end if;

               if not Initialized then
                  Not_Initialized_One := True;
                  Delete (Argv, Pos, Pos);
               end if;

            else
               Pos := Pos + 1;
            end if;
         end;
      end loop;

      if Not_Initialized_One then
         Raise_Bad_Param (Default_Sys_Member);
      end if;

      pragma Debug (C, O ("Init: leave"));
   end Init;

   ---------------------
   -- Create_Alias_Tc --
   ---------------------

   function Create_Alias_Tc
     (Id            : CORBA.RepositoryId;
      Name          : CORBA.Identifier;
      Original_Type : CORBA.TypeCode.Object)
      return CORBA.TypeCode.Object
   is
   begin
      return CORBA.TypeCode.Internals.Build_Alias_TC
        (Id => CORBA.String (Id), Name => CORBA.String (Name),
         Parent => Original_Type);
   end Create_Alias_Tc;

   ---------------------
   -- Create_Array_Tc --
   ---------------------

   function Create_Array_Tc
     (Length       : CORBA.Unsigned_Long;
      Element_Type : CORBA.TypeCode.Object) return CORBA.TypeCode.Object
   is
   begin
      return TypeCode.Internals.To_CORBA_Object
        (PolyORB.Any.TypeCode.Build_Complex_TC (Tk_Array,
         (PolyORB.Any.To_Any (PolyORB.Types.Unsigned_Long (Length)),
          PolyORB.Any.Any (To_Any (Element_Type)))));
   end Create_Array_Tc;

   ---------------------
   -- Create_Fixed_Tc --
   ---------------------

   function Create_Fixed_Tc
     (IDL_Digits : CORBA.Unsigned_Short;
      scale      : CORBA.Short) return CORBA.TypeCode.Object is
   begin
      return TypeCode.Internals.To_CORBA_Object
        (PolyORB.Any.TypeCode.Build_Complex_TC (Tk_Fixed,
         (PolyORB.Any.To_Any (PolyORB.Types.Unsigned_Short (IDL_Digits)),
          PolyORB.Any.To_Any (PolyORB.Types.Short (scale)))));
   end Create_Fixed_Tc;

   -------------------------
   -- Create_Interface_Tc --
   -------------------------

   function Create_Interface_Tc
     (Id   : CORBA.RepositoryId;
      Name : CORBA.Identifier)
      return CORBA.TypeCode.Object is
   begin
      return TypeCode.Internals.To_CORBA_Object
        (PolyORB.Any.TypeCode.Build_Complex_TC (Tk_Objref,
         (PolyORB.Any.To_Any (PolyORB.Types.String (Name)),
          PolyORB.Any.To_Any (PolyORB.Types.String (Id)))));
   end Create_Interface_Tc;

   -----------------
   -- Create_List --
   -----------------

   procedure Create_List
     (Count    : CORBA.Long;
      New_List : out CORBA.NVList.Ref)
   is
      pragma Unreferenced (Count);
      Result : CORBA.NVList.Ref;
   begin
      New_List := Result;
   end Create_List;

   procedure Create_List (New_List : out CORBA.ExceptionList.Ref)
     renames CORBA.ExceptionList.Create_List;

   ----------------------
   -- Create_Native_Tc --
   ----------------------

   function Create_Native_Tc
     (Id   : CORBA.RepositoryId;
      Name : CORBA.Identifier)
      return CORBA.TypeCode.Object is
   begin
      return TypeCode.Internals.To_CORBA_Object
        (PolyORB.Any.TypeCode.Build_Complex_TC (Tk_Native,
         (PolyORB.Any.To_Any (PolyORB.Types.String (Name)),
          PolyORB.Any.To_Any (PolyORB.Types.String (Id)))));
   end Create_Native_Tc;

   ----------------------------------
   -- Create_Recursive_Sequence_Tc --
   ----------------------------------

   function Create_Recursive_Sequence_Tc
     (Bound  : CORBA.Unsigned_Long;
      Offset : CORBA.Unsigned_Long)
      return CORBA.TypeCode.Object is
   begin
      raise Program_Error;
      pragma Warnings (Off);
      return Create_Recursive_Sequence_Tc (Bound, Offset);
      --  "Possible infinite recursion".
      pragma Warnings (On);
   end Create_Recursive_Sequence_Tc;

   ------------------------
   -- Create_Sequence_Tc --
   ------------------------

   function Create_Sequence_Tc
     (Bound        : CORBA.Unsigned_Long;
      Element_Type : CORBA.TypeCode.Object) return CORBA.TypeCode.Object
   is
   begin
      return TypeCode.Internals.To_CORBA_Object
        (PolyORB.Any.TypeCode.Build_Complex_TC (Tk_Sequence,
         (PolyORB.Any.To_Any (PolyORB.Types.Unsigned_Long (Bound)),
          PolyORB.Any.Any (To_Any (Element_Type)))));
   end Create_Sequence_Tc;

   ----------------------
   -- Create_String_Tc --
   ----------------------

   function Create_String_Tc
     (Bound : CORBA.Unsigned_Long) return CORBA.TypeCode.Object is
   begin
      return TypeCode.Internals.To_CORBA_Object
        (PolyORB.Any.TypeCode.Build_String_TC
         (PolyORB.Types.Unsigned_Long (Bound)));
   end Create_String_Tc;

   -----------------------
   -- Create_Wstring_Tc --
   -----------------------

   function Create_Wstring_Tc
     (Bound : CORBA.Unsigned_Long) return CORBA.TypeCode.Object is
   begin
      return TypeCode.Internals.To_CORBA_Object
        (PolyORB.Any.TypeCode.Build_Wstring_TC
         (PolyORB.Types.Unsigned_Long (Bound)));
   end Create_Wstring_Tc;

   -------------------------
   -- Get_Default_Context --
   -------------------------

   function Get_Default_Context return CORBA.Context.Ref is
      R : CORBA.Context.Ref;
   begin
      raise Program_Error;
      --  Not implemented
      return R;
   end Get_Default_Context;

   -----------------------------
   -- Get_Service_Information --
   -----------------------------

   procedure Get_Service_Information
     (Service_Type        : CORBA.ServiceType;
      Service_Information :    out ServiceInformation;
      Returns             :    out CORBA.Boolean)
   is
      pragma Unreferenced (Service_Type);

      Null_Service_Information : constant ServiceInformation :=
        ServiceInformation'(IDL_SEQUENCE_ServiceOption.Null_Sequence,
                            IDL_SEQUENCE_ServiceDetail.Null_Sequence);

   begin

      --  Service information is not (yet) supported, we return false
      --  for all values of Service_Type.

      Service_Information := Null_Service_Information;
      Returns := False;

   end Get_Service_Information;

   ---------------------------
   -- List_Initial_Services --
   ---------------------------

   function List_Initial_Services return ObjectIdList
   is
      use PolyORB.Initial_References;
      use PolyORB.Utils.Strings.Lists;

      Services_List : List := List_Initial_Services;

      Result : ObjectIdList;

      It : Iterator := First (Services_List);

   begin
      pragma Debug (C, O ("List_Initial_Services: enter"));

      while not Last (It) loop
         pragma Debug (C, O ("Service name: " & Value (It).all));
         IDL_SEQUENCE_ObjectId.Append
           (IDL_SEQUENCE_ObjectId.Sequence (Result),
            To_CORBA_String (Value (It).all));
         Next (It);
      end loop;

      Deallocate (Services_List);

      pragma Debug (C, O ("List_Initial_Services: end"));
      return Result;
   end List_Initial_Services;

   ------------------
   -- Perform_Work --
   ------------------

   procedure Perform_Work is
   begin
      Perform_Work (The_ORB);
   end Perform_Work;

   --------------------------------
   -- Register_Initial_Reference --
   --------------------------------

   procedure Register_Initial_Reference
     (Identifier : ObjectId;
      Ref        : CORBA.Object.Ref)
   is
      use CORBA.Object;
      use PolyORB.Initial_References;

      Id : constant Standard.String := To_Standard_String (Identifier);
      PO_Ref : PolyORB.References.Ref;

   begin
      pragma Debug (C, O ("Register_Initial_Reference: " & Id));

      --  If string id is empty or id is already registered,
      --  then raise InvalidName.

      if Id = ""
        or else not Resolve_Initial_References (Id).Is_Nil
      then
         Raise_InvalidName (InvalidName_Members'(null record));
      end if;

      --  If Ref is null, then raise Bad_Param with minor code 27

      if Is_Nil (Ref) then
         Raise_Bad_Param (
           System_Exception_Members'(Minor     => 27,
                                     Completed => Completed_No));
      end if;

      PO_Ref.Set (Ref.Entity_Of);
      Register_Initial_Reference (Id, PO_Ref);
   end Register_Initial_Reference;

   procedure Register_Initial_Reference
     (Identifier : ObjectId;
      IOR        : CORBA.String)
   is
      Ref : CORBA.Object.Ref;
   begin
      CORBA.ORB.String_To_Object (IOR, Ref);
      Register_Initial_Reference (Identifier, Ref);
   end Register_Initial_Reference;

   --------------------------------
   -- Resolve_Initial_References --
   --------------------------------

   function Resolve_Initial_References
     (Identifier : ObjectId)
     return CORBA.Object.Ref
   is
      use CORBA.Object;
      use PolyORB.Initial_References;

      Id : constant Standard.String := To_Standard_String (Identifier);

      Result : CORBA.Object.Ref;
   begin
      pragma Debug (C, O ("Resolve_Initial_References: " & Id));

      Result.Set (Resolve_Initial_References (Id).Entity_Of);
      if Is_Nil (Result) then
         Raise_InvalidName (InvalidName_Members'(null record));
      end if;
      return Result;
   end Resolve_Initial_References;

   ---------
   -- Run --
   ---------

   procedure Run is
   begin
      PolyORB.ORB.Run (The_ORB, May_Exit => False);
   end Run;

   --------------
   -- Shutdown --
   --------------

   procedure Shutdown (Wait_For_Completion : Boolean) is
   begin
      Shutdown (The_ORB, Wait_For_Completion);
   end Shutdown;

   ----------------------
   -- Object_To_String --
   ----------------------

   function Object_To_String
     (Obj : CORBA.Object.Ref'Class) return CORBA.String
   is
      use PolyORB.References.IOR;
   begin
      if CORBA.Object.Is_Nil (Obj) then
         CORBA.Raise_Inv_Objref (Default_Sys_Member);
      end if;

      if PolyORB.CORBA_P.Local.Is_Local (Obj) then
         Raise_Marshal (Marshal_Members'(Minor     => 4,
                                         Completed => Completed_No));
      end if;

      return To_CORBA_String
        (Object_To_String
         (CORBA.Object.Internals.To_PolyORB_Ref (CORBA.Object.Ref (Obj))));
   end Object_To_String;

   ----------------------
   -- String_To_Object --
   ----------------------

   procedure String_To_Object
     (From : CORBA.String;
      To   : in out CORBA.Object.Ref'Class) is
   begin
      declare
         use PolyORB.References;

         My_Ref : Ref;
      begin
         String_To_Object (To_Standard_String (From), My_Ref);
         CORBA.Object.Set (To, Entity_Of (My_Ref));
      end;

   exception
      when others =>
         CORBA.Raise_Bad_Param (CORBA.Default_Sys_Member);
   end String_To_Object;

   ------------------
   -- Work_Pending --
   ------------------

   function Work_Pending return Boolean is
   begin
      return Work_Pending (The_ORB);
   end Work_Pending;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (ORB_Name : Standard.String)
   is
      pragma Warnings (Off);
      pragma Unreferenced (ORB_Name);
      pragma Warnings (On);

      use PolyORB.Initialization;
   begin
      if not Is_Initialized then
         Initialize_World;
      end if;
   end Initialize;

   -------------------
   -- Create_Policy --
   -------------------

   function Create_Policy
     (The_Type : PolicyType;
      Val      :    Any)
     return CORBA.Policy.Ref
   is
      use PolyORB.CORBA_P.Policy_Management;

      Factory : Policy_Factory;

   begin
      if not Is_Registered (The_Type) then
         Raise_PolicyError ((Reason => BAD_POLICY));
      end if;

      Factory := Get_Policy_Factory (The_Type);

      if Factory = null then
         Raise_PolicyError ((Reason => UNSUPPORTED_POLICY));
      end if;

      return Factory (The_Type, Val);
   end Create_Policy;

   -----------------
   -- Get_Members --
   -----------------

   procedure Get_Members
     (From : Ada.Exceptions.Exception_Occurrence;
      To   : out InvalidName_Members)
   is
      use Ada.Exceptions;

   begin
      if Exception_Identity (From) /= InvalidName'Identity then
         CORBA.Raise_Bad_Param (CORBA.Default_Sys_Member);
      end if;

      To := InvalidName_Members'
        (CORBA.IDL_Exception_Members with null record);
   end Get_Members;

   -----------------------
   -- Raise_InvalidName --
   -----------------------

   procedure Raise_InvalidName
     (Excp_Memb : InvalidName_Members)
   is
      pragma Unreferenced (Excp_Memb);
   begin
      raise InvalidName;
   end Raise_InvalidName;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize;

   procedure Initialize
   is
      Naming_IOR : constant Standard.String :=
        PolyORB.Parameters.Get_Conf
        (Section => "corba", Key => "name_service", Default => "");
      InterfaceRepository_IOR : constant Standard.String :=
        PolyORB.Parameters.Get_Conf
        (Section => "corba", Key => "ir_service", Default => "");
      PolicyDomainManager_IOR : constant Standard.String :=
        PolyORB.Parameters.Get_Conf
        (Section => "corba", Key => "policy_domain_manager", Default => "");
      ReplicationManager_IOR  : constant Standard.String :=
        PolyORB.Parameters.Get_Conf
        (Section => "corba", Key => "replication_manager", Default => "");

   begin
      --  Register initial reference for NamingService

      if Naming_IOR /= "" then
         --  Standard CORBA3 name

         Register_Initial_Reference
           (To_CORBA_String ("NameService"), To_CORBA_String (Naming_IOR));

         --  Legacy compatibility synonym

         Register_Initial_Reference
           (To_CORBA_String ("NamingService"), To_CORBA_String (Naming_IOR));
      end if;

      --  Register initial reference for Interface Repository

      if InterfaceRepository_IOR /= "" then
         Register_Initial_Reference
           (To_CORBA_String ("InterfaceRepository"),
            To_CORBA_String (InterfaceRepository_IOR));
      end if;

      --  Register initial reference for Policy Domain Manager

      if PolicyDomainManager_IOR /= "" then
         Register_Initial_Reference
           (To_CORBA_String ("PolyORBPolicyDomainManager"),
            To_CORBA_String (PolicyDomainManager_IOR));
      end if;

      --  Register initial reference for Replication Manager

      if ReplicationManager_IOR /= "" then
         Register_Initial_Reference
           (To_CORBA_String ("ReplicationManager"),
            To_CORBA_String (ReplicationManager_IOR));
      end if;

      PolyORB.CORBA_P.ORB_Init.Register
        ("InitRef", ORB_Init_Initial_References'Access);
   end Initialize;

   use PolyORB.Initialization;
   use PolyORB.Utils.Strings;
   use PolyORB.Utils.Strings.Lists;

begin
   Register_Module
     (Module_Info'
      (Name      => +"corba.orb",
       Conflicts => Empty,
       Depends   => +"orb" & "initial_references",
       Provides  => Empty,
       Implicit  => False,
       Init      => Initialize'Access,
       Shutdown  => null));
end CORBA.ORB;

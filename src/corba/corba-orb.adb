------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                            C O R B A . O R B                             --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2001-2006, Free Software Foundation, Inc.          --
--                                                                          --
-- PolyORB is free software; you  can  redistribute  it and/or modify it    --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. PolyORB is distributed  in the hope that it will be  useful,    --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with PolyORB; see file COPYING. If    --
-- not, write to the Free Software Foundation, 51 Franklin Street, Fifth    --
-- Floor, Boston, MA 02111-1301, USA.                                       --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
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

with PolyORB.CORBA_P.Initial_References;
with PolyORB.CORBA_P.Local;
with PolyORB.CORBA_P.ORB_Init;
with PolyORB.CORBA_P.Policy_Management;

with PolyORB.Initialization;

with PolyORB.Log;
with PolyORB.ORB;
with PolyORB.Objects;
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
   pragma Unreferenced (C); --  For conditional pragma Debug

   procedure Register_Initial_Reference
     (Identifier : ObjectId;
      IOR        : String);
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
         pragma Debug (O ("Registering " & Value (Value'First .. Pos - 1)
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

      --  Implementation Note: We first run Initialize_World to allow
      --  packages to register helper routines to parse specific
      --  command line arguments.

      if not Is_Initialized then
         Initialize_World;
      end if;

      pragma Debug (O ("Init: enter"));

      while Pos <= Length (Argv) loop
         declare
            Suffix : constant Standard.String
              := To_Standard_String (Element_Of (Argv, Pos));

            Initialized : Boolean := False;
            Space_Index : Positive;

         begin
            pragma Debug (O ("Processing " & Suffix));

            if PolyORB.Utils.Has_Prefix (Suffix, ORB_Prefix) then

               pragma Debug
                 (O ("Possible suffix is "
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
                     Value : constant Standard.String
                       := To_Standard_String (Element_Of (Argv, Pos + 1));

                  begin
                     pragma Debug
                       (O ("Try to initialize ("
                           & Suffix (Suffix'First + ORB_Prefix'Length
                                     .. Suffix'Last)
                           & "," & Value & ")"));

                     Initialized
                       := PolyORB.CORBA_P.ORB_Init.Initialize
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
                  Initialized
                    := PolyORB.CORBA_P.ORB_Init.Initialize
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

      pragma Debug (O ("Init: leave"));
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
      Result : CORBA.TypeCode.Object
        := CORBA.TypeCode.Internals.To_CORBA_Object
        (PolyORB.Any.TypeCode.TC_Alias);

   begin
      CORBA.TypeCode.Internals.Add_Parameter
        (Result, CORBA.To_Any (CORBA.String (Name)));
      CORBA.TypeCode.Internals.Add_Parameter
        (Result, CORBA.To_Any (CORBA.String (Id)));
      CORBA.TypeCode.Internals.Add_Parameter
        (Result, CORBA.To_Any (Original_Type));

      return Result;
   end Create_Alias_Tc;

   ---------------------
   -- Create_Array_Tc --
   ---------------------

   function Create_Array_Tc
     (Length       : CORBA.Unsigned_Long;
      Element_Type : CORBA.TypeCode.Object)
      return CORBA.TypeCode.Object
   is
      Result : CORBA.TypeCode.Object
        := CORBA.TypeCode.Internals.To_CORBA_Object
        (PolyORB.Any.TypeCode.TC_Array);

   begin
      CORBA.TypeCode.Internals.Add_Parameter (Result, CORBA.To_Any (Length));
      CORBA.TypeCode.Internals.Add_Parameter
        (Result, CORBA.To_Any (Element_Type));

      return Result;
   end Create_Array_Tc;

   ---------------------
   -- Create_Fixed_Tc --
   ---------------------

   function Create_Fixed_Tc
     (IDL_Digits : CORBA.Unsigned_Short;
      scale      : CORBA.Short)
      return CORBA.TypeCode.Object is
   begin
      raise Program_Error;
      pragma Warnings (Off);
      return Create_Fixed_Tc (IDL_Digits, scale);
      --  "Possible infinite recursion".
      pragma Warnings (On);
   end Create_Fixed_Tc;

   -------------------------
   -- Create_Interface_Tc --
   -------------------------

   function Create_Interface_Tc
     (Id   : CORBA.RepositoryId;
      Name : CORBA.Identifier)
      return CORBA.TypeCode.Object
   is
      Result : TypeCode.Object
        := CORBA.TypeCode.Internals.To_CORBA_Object
        (PolyORB.Any.TypeCode.TC_Object);

   begin
      CORBA.TypeCode.Internals.Add_Parameter
        (Result, CORBA.To_Any (CORBA.String (Name)));
      CORBA.TypeCode.Internals.Add_Parameter
        (Result, CORBA.To_Any (CORBA.String (Id)));
      return Result;
   end Create_Interface_Tc;

   -----------------
   -- Create_List --
   -----------------

   procedure Create_List
     (Count    : CORBA.Long;
      New_List :    out CORBA.NVList.Ref)
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
     (Id   : RepositoryId;
      Name : Identifier)
      return CORBA.TypeCode.Object is
   begin
      raise Program_Error;
      pragma Warnings (Off);
      return Create_Native_Tc (Id, Name);
      --  "Possible infinite recursion".
      pragma Warnings (On);
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
      Element_Type : CORBA.TypeCode.Object)
      return CORBA.TypeCode.Object
   is
      Result : CORBA.TypeCode.Object
        := CORBA.TypeCode.Internals.To_CORBA_Object
        (PolyORB.Any.TypeCode.TC_Sequence);

   begin
      CORBA.TypeCode.Internals.Add_Parameter (Result, CORBA.To_Any (Bound));
      CORBA.TypeCode.Internals.Add_Parameter
        (Result, CORBA.To_Any (Element_Type));

      return Result;
   end Create_Sequence_Tc;

   ----------------------
   -- Create_String_Tc --
   ----------------------

   function Create_String_Tc
     (Bound : CORBA.Unsigned_Long)
      return CORBA.TypeCode.Object
   is
      Result : CORBA.TypeCode.Object := CORBA.TC_String;
   begin
      CORBA.TypeCode.Internals.Add_Parameter (Result, CORBA.To_Any (Bound));

      return Result;
   end Create_String_Tc;

   -----------------------
   -- Create_Wstring_Tc --
   -----------------------

   function Create_Wstring_Tc
     (Bound : CORBA.Unsigned_Long)
      return CORBA.TypeCode.Object
   is
      Result : CORBA.TypeCode.Object := CORBA.TC_Wide_String;
   begin
      CORBA.TypeCode.Internals.Add_Parameter (Result, CORBA.To_Any (Bound));

      return Result;
   end Create_Wstring_Tc;

   -------------------------
   -- Get_Default_Context --
   -------------------------

   function Get_Default_Context
      return CORBA.Context.Ref is
   begin
      raise Program_Error;
      pragma Warnings (Off);
      return Get_Default_Context;
      --  "Possible infinite recursion".
      pragma Warnings (On);
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
      use PolyORB.CORBA_P.Initial_References;
      use PolyORB.Utils.Strings.Lists;

      Services_List : List := List_Initial_Services;

      Result : ObjectIdList;

      It : Iterator := First (Services_List);

   begin
      pragma Debug (O ("List_Initial_Services: enter"));

      while not Last (It) loop
         pragma Debug (O ("Service name: " & Value (It).all));
         IDL_SEQUENCE_ObjectId.Append
           (IDL_SEQUENCE_ObjectId.Sequence (Result),
            To_CORBA_String (Value (It).all));
         Next (It);
      end loop;

      Deallocate (Services_List);

      pragma Debug (O ("List_Initial_Services: end"));
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
      use PolyORB.CORBA_P.Initial_References;

      Id : constant Standard.String := To_Standard_String (Identifier);

   begin
      pragma Debug (O ("Register_Initial_Reference: " & Id));

      --  If string id is empty or id is already registered,
      --  then raise InvalidName.

      if Id = ""
        or else not Is_Nil (Resolve_Initial_References (Id))
      then
         Raise_InvalidName (InvalidName_Members'(null record));
      end if;

      --  If Ref is null, then raise Bad_Param with minor code 27

      if Is_Nil (Ref) then
         Raise_Bad_Param (
           System_Exception_Members'(Minor     => 27,
                                     Completed => Completed_No));
      end if;

      Register_Initial_Reference (Id, Ref);
   end Register_Initial_Reference;

   procedure Register_Initial_Reference
     (Identifier : ObjectId;
      IOR        : String)
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
      use PolyORB.CORBA_P.Initial_References;

      Id : constant Standard.String := To_Standard_String (Identifier);

      Result : CORBA.Object.Ref := Resolve_Initial_References (Id);
   begin
      pragma Debug (O ("Resolve_Initial_References: " & Id));

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
      PolyORB.ORB.Run (The_ORB, May_Poll => True);
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
     (Obj : CORBA.Object.Ref'Class)
     return CORBA.String
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
         (CORBA.Object.Internals.To_PolyORB_Ref
          (CORBA.Object.Ref (Obj))));
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

   ----------------------
   -- Create_Reference --
   ----------------------

   function Create_Reference
     (Object : CORBA.Object.Ref;
      Typ    : Standard.String)
     return PolyORB.References.Ref is
   begin
      if The_ORB = null then
         Raise_Internal (CORBA.Default_Sys_Member);
      end if;

      declare
         Result : PolyORB.References.Ref;

         Oid : constant PolyORB.Objects.Object_Id_Access :=
           new PolyORB.Objects.Object_Id'
           (CORBA.Object.Internals.To_PolyORB_Object (Object));
      begin
         PolyORB.ORB.Create_Reference (The_ORB, Oid, Typ, Result);

         return Result;
      end;
   end Create_Reference;

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
         Register_Initial_Reference
           (To_CORBA_String ("NamingService"),
            To_CORBA_String (Naming_IOR));
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
       Depends   => +"orb"
       & "corba.initial_references",
       Provides  => Empty,
       Implicit  => False,
       Init      => Initialize'Access));
end CORBA.ORB;

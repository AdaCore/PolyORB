------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                            C O R B A . O R B                             --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2001-2003 Free Software Foundation, Inc.           --
--                                                                          --
-- PolyORB is free software; you  can  redistribute  it and/or modify it    --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. PolyORB is distributed  in the hope that it will be  useful,    --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with PolyORB; see file COPYING. If    --
-- not, write to the Free Software Foundation, 59 Temple Place - Suite 330, --
-- Boston, MA 02111-1307, USA.                                              --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
--                PolyORB is maintained by ACT Europe.                      --
--                    (email: sales@act-europe.fr)                          --
--                                                                          --
------------------------------------------------------------------------------

--  The following subprograms still have to be implemented :
--
--  Get_Default_Context --
--  Create_Fixed_Tc --
--  Create_Native_Tc --
--  Create_Recursive_Sequence_Tc --

--  $Id$

with Ada.Exceptions;

with PolyORB.CORBA_P.Initial_References;
with PolyORB.CORBA_P.Policy;

with PolyORB.Configuration;
with PolyORB.Initialization;
pragma Elaborate_All (PolyORB.Initialization); --  WAG:3.15

with PolyORB.Log;
with PolyORB.ORB;
with PolyORB.Objects;
with PolyORB.References.IOR;
with PolyORB.References.Corbaloc;
with PolyORB.Setup;
with PolyORB.Smart_Pointers;
with PolyORB.Utils.Strings.Lists;

package body CORBA.ORB is

   use PolyORB.Log;
   use PolyORB.ORB;
   use PolyORB.Setup;

   package L is new PolyORB.Log.Facility_Log ("corba.orb");
   procedure O (Message : in Standard.String; Level : Log_Level := Debug)
     renames L.Output;

   procedure Register_Initial_Reference
     (Identifier : ObjectId;
      IOR        : String);
   --  Register an initial reference from an IOR given
   --  through the configuration subsystem.

   ---------------------
   -- Create_Alias_TC --
   ---------------------

   function Create_Alias_Tc
     (Id            : in CORBA.RepositoryId;
      Name          : in CORBA.Identifier;
      Original_Type : in CORBA.TypeCode.Object)
      return CORBA.TypeCode.Object
   is
      Result : CORBA.TypeCode.Object := TypeCode.TC_Alias;
   begin
      CORBA.TypeCode.Add_Parameter
        (Result, CORBA.To_Any (CORBA.String (Name)));
      CORBA.TypeCode.Add_Parameter
        (Result, CORBA.To_Any (CORBA.String (Id)));
      CORBA.TypeCode.Add_Parameter (Result, CORBA.To_Any (Original_Type));

      return Result;
   end Create_Alias_Tc;

   ---------------------
   -- Create_Array_Tc --
   ---------------------

   function Create_Array_Tc
     (Length       : in CORBA.Unsigned_Long;
      Element_Type : in CORBA.TypeCode.Object)
      return CORBA.TypeCode.Object
   is
      Result : CORBA.TypeCode.Object := TypeCode.TC_Array;
   begin
      CORBA.TypeCode.Add_Parameter (Result, CORBA.To_Any (Length));
      CORBA.TypeCode.Add_Parameter (Result, CORBA.To_Any (Element_Type));

      return Result;
   end Create_Array_Tc;

   ---------------------
   -- Create_Fixed_Tc --
   ---------------------

   function Create_Fixed_Tc
     (IDL_Digits : in CORBA.Unsigned_Short;
      scale      : in CORBA.Short)
      return CORBA.TypeCode.Object is
   begin
      raise PolyORB.Not_Implemented;
      pragma Warnings (Off);
      return Create_Fixed_Tc (IDL_Digits, scale);
      --  "Possible infinite recursion".
      pragma Warnings (On);
   end Create_Fixed_Tc;

   -------------------------
   -- Create_Interface_Tc --
   -------------------------

   function Create_Interface_Tc
     (Id   : in CORBA.RepositoryId;
      Name : in CORBA.Identifier)
      return CORBA.TypeCode.Object
   is
      Result : TypeCode.Object := TypeCode.TC_Object;
   begin
      CORBA.TypeCode.Add_Parameter
        (Result, CORBA.To_Any (CORBA.String (Name)));
      CORBA.TypeCode.Add_Parameter
        (Result, CORBA.To_Any (CORBA.String (Id)));
      return Result;
   end Create_Interface_Tc;

   -----------------
   -- Create_List --
   -----------------

   procedure Create_List
     (Count    : in     CORBA.Long;
      New_List :    out CORBA.NVList.Ref)
   is
      pragma Warnings (Off);
      --  Parameter 'Count' below is only a hint.
      --  In this implementation, it is ignored.
      pragma Unreferenced (Count);
      pragma Warnings (On);

   begin
      CORBA.NVList.Create (New_List);
   end Create_List;

   procedure Create_List (New_List : out CORBA.ExceptionList.Ref)
     renames CORBA.ExceptionList.Create_List;

   ----------------------
   -- Create_Native_Tc --
   ----------------------

   function Create_Native_Tc
     (Id   : in RepositoryId;
      Name : in Identifier)
      return CORBA.TypeCode.Object is
   begin
      raise PolyORB.Not_Implemented;
      pragma Warnings (Off);
      return Create_Native_Tc (Id, Name);
      --  "Possible infinite recursion".
      pragma Warnings (On);
   end Create_Native_Tc;

   ----------------------------------
   -- Create_Recursive_Sequence_Tc --
   ----------------------------------

   function Create_Recursive_Sequence_Tc
     (Bound  : in CORBA.Unsigned_Long;
      Offset : in CORBA.Unsigned_Long)
      return CORBA.TypeCode.Object is
   begin
      raise PolyORB.Not_Implemented;
      pragma Warnings (Off);
      return Create_Recursive_Sequence_Tc (Bound, Offset);
      --  "Possible infinite recursion".
      pragma Warnings (On);
   end Create_Recursive_Sequence_Tc;

   ------------------------
   -- Create_Sequence_Tc --
   ------------------------

   function Create_Sequence_Tc
     (Bound        : in CORBA.Unsigned_Long;
      Element_Type : in CORBA.TypeCode.Object)
      return CORBA.TypeCode.Object
   is
      Result : CORBA.TypeCode.Object := TypeCode.TC_Sequence;
   begin
      CORBA.TypeCode.Add_Parameter (Result, CORBA.To_Any (Bound));
      CORBA.TypeCode.Add_Parameter (Result, CORBA.To_Any (Element_Type));

      return Result;
   end Create_Sequence_Tc;

   ----------------------
   -- Create_String_Tc --
   ----------------------

   function Create_String_Tc
     (Bound : in CORBA.Unsigned_Long)
      return CORBA.TypeCode.Object
   is
      Result : CORBA.TypeCode.Object := TypeCode.TC_String;
   begin
      CORBA.TypeCode.Add_Parameter (Result, CORBA.To_Any (Bound));

      return Result;
   end Create_String_Tc;

   -----------------------
   -- Create_Wstring_Tc --
   -----------------------

   function Create_Wstring_Tc
     (Bound : in CORBA.Unsigned_Long)
      return CORBA.TypeCode.Object
   is
      Result : CORBA.TypeCode.Object := TypeCode.TC_Wide_String;
   begin
      CORBA.TypeCode.Add_Parameter (Result, CORBA.To_Any (Bound));

      return Result;
   end Create_Wstring_Tc;

   -------------------------
   -- Get_Default_Context --
   -------------------------

   function Get_Default_Context
      return CORBA.Context.Ref is
   begin
      raise PolyORB.Not_Implemented;
      pragma Warnings (Off);
      return Get_Default_Context;
      --  "Possible infinite recursion".
      pragma Warnings (On);
   end Get_Default_Context;

   -----------------------------
   -- Get_Service_Information --
   -----------------------------

   procedure Get_Service_Information
     (Service_Type        : in     CORBA.ServiceType;
      Service_Information :    out ServiceInformation;
      Returns             :    out CORBA.Boolean)
   is
      pragma Warnings (Off); --  WAg:3.15
      pragma Unreferenced (Service_Type);
      pragma Warnings (On); --  WAg:3.15

      Null_Service_Information : constant ServiceInformation :=
        ServiceInformation'(IDL_Sequence_ServiceOption.Null_Sequence,
                            IDL_Sequence_ServiceDetail.Null_Sequence);

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
         IDL_Sequence_ObjectId.Append
           (IDL_Sequence_ObjectId.Sequence (Result),
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
        or else not Is_Nil (Resolve_Initial_References (Id)) then
         declare
            Excp_Memb : InvalidName_Members := (null record);
         begin
            Raise_InvalidName (Excp_Memb);
         end;
      end if;

      --  If Ref is null, then raise Bad_Param with minor code 27

      if Is_Nil (Ref) then
         declare
            Excp_Memb : System_Exception_Members :=
              System_Exception_Members'(Minor     => 27,
                                        Completed => Completed_No);
         begin
            Raise_Bad_Param (Excp_Memb);
         end;
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
         declare
            Excp_Memb : InvalidName_Members := (null record);
         begin
            Raise_InvalidName (Excp_Memb);
         end;
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

   procedure Shutdown (Wait_For_Completion : in Boolean) is
   begin
      Shutdown (The_ORB, Wait_For_Completion);
   end Shutdown;

   ----------------------
   -- Object_To_String --
   ----------------------

   function Object_To_String
     (Obj : in CORBA.Object.Ref'Class)
     return CORBA.String
   is
      use PolyORB.References.IOR;
   begin
      return To_CORBA_String
        (Object_To_String
         (CORBA.Object.To_PolyORB_Ref (CORBA.Object.Ref (Obj))));
   end Object_To_String;

   ------------------------
   -- Object_To_Corbaloc --
   ------------------------

   function Object_To_Corbaloc
     (Obj : in CORBA.Object.Ref'Class)
     return CORBA.String
   is
      use PolyORB.References.Corbaloc;
   begin
      return CORBA.String
        (Object_To_String
         (CORBA.Object.To_PolyORB_Ref (CORBA.Object.Ref (Obj))));
   end Object_To_Corbaloc;

   ----------------------
   -- String_To_Object --
   ----------------------

   procedure String_To_Object
     (From : in     CORBA.String;
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
      when Constraint_Error =>
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

   procedure Initialize (ORB_Name : in Standard.String)
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
     (Object : in CORBA.Object.Ref;
      Typ    : in Standard.String)
     return PolyORB.References.Ref is
   begin
      if The_ORB = null then
         Raise_Internal (CORBA.Default_Sys_Member);
      end if;

      declare
         Result : PolyORB.References.Ref;

         Oid : constant PolyORB.Objects.Object_Id_Access :=
           new PolyORB.Objects.Object_Id'
           (CORBA.Object.To_PolyORB_Object (Object));
      begin
         PolyORB.ORB.Create_Reference (The_ORB, Oid, Typ, Result);

         return Result;
      end;
   end Create_Reference;

   -------------------
   -- Create_Policy --
   -------------------

   function Create_Policy
     (The_Type : in PolicyType;
      Val      :    Any)
     return CORBA.Policy.Ref
   is
      use PolyORB.CORBA_P.Policy;

      Result : CORBA.Policy.Ref;

      Entity : constant PolyORB.Smart_Pointers.Entity_Ptr :=
        new Policy_Object_Type;

   begin
      Set_Policy_Type (Policy_Object_Type (Entity.all), The_Type);
      Set_Policy_Value (Policy_Object_Type (Entity.all), Val);

      CORBA.Policy.Set (Result, Entity);

      return Result;
   end Create_Policy;

   -----------------
   -- Get_Members --
   -----------------

   procedure Get_Members
     (From : in  Ada.Exceptions.Exception_Occurrence;
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
     (Excp_Memb : in InvalidName_Members)
   is
      pragma Warnings (Off); --  WAG:3.15
      pragma Unreferenced (Excp_Memb);
      pragma Warnings (On); --  WAG:3.15
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
        PolyORB.Configuration.Get_Conf
        (Section => "corba", Key => "naming_ior", Default => "");

   begin

      --  Register initial reference for NamingService

      if Naming_IOR /= "" then
         Register_Initial_Reference
           (To_CORBA_String ("NamingService"),
            To_CORBA_String (Naming_IOR));
      end if;

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
       Init      => Initialize'Access));

end CORBA.ORB;

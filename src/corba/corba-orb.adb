------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                            C O R B A . O R B                             --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--             Copyright (C) 1999-2002 Free Software Fundation              --
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
--              PolyORB is maintained by ENST Paris University.             --
--                                                                          --
------------------------------------------------------------------------------

--  The following subprograms still have to be implemented :
--
-- Get_Default_Context --
-- Get_Service_Information --
-- List_Initial_Services --
-- Perform_Work --
-- Shutdown --
-- Work_Pending --

--  $Id$

with Ada.Exceptions;

with PolyORB.Configuration;
with PolyORB.Exceptions;
with PolyORB.Dynamic_Dict;
with PolyORB.Initialization;
with PolyORB.Log;
with PolyORB.ORB;
with PolyORB.Objects;
with PolyORB.References.IOR;
with PolyORB.Setup;
with PolyORB.Smart_Pointers;
with PolyORB.Utils.Strings;
with PolyORB.Utils.Strings.Lists;

package body CORBA.ORB is

   use PolyORB.Log;
   use PolyORB.ORB;
   use PolyORB.Setup;

   package L is new PolyORB.Log.Facility_Log ("corba.orb");
   procedure O (Message : in Standard.String; Level : Log_Level := Debug)
     renames L.Output;

   Nil_Ref : CORBA.Object.Ref;
   package Referenced_Objects is new PolyORB.Dynamic_Dict
     (Value => CORBA.Object.Ref, No_Value => Nil_Ref);
   --  For initial references.

   procedure Initialize;
   --  Perform internal initialization: setup initial references.
   --  Called from within the initialization framework.

   ---------------------
   -- Create_Alias_TC --
   ---------------------

   function Create_Alias_Tc
     (Id            : in CORBA.RepositoryId;
      Name          : in CORBA.Identifier;
      Original_Type : in CORBA.TypeCode.Object)
      return CORBA.TypeCode.Object
   is
   begin
      raise PolyORB.Not_Implemented;
      pragma Warnings (Off);
      return Create_Alias_Tc (Id, Name, Original_Type);
      --  "Possible infinite recursion".
      pragma Warnings (On);
   end Create_Alias_Tc;

   ---------------------
   -- Create_Array_Tc --
   ---------------------

   function Create_Array_Tc
     (Length       : in CORBA.Unsigned_Long;
      Element_Type : in CORBA.TypeCode.Object)
      return CORBA.TypeCode.Object
   is
   begin
      raise PolyORB.Not_Implemented;
      pragma Warnings (Off);
      return Create_Array_Tc (Length, Element_Type);
      --  "Possible infinite recursion".
      pragma Warnings (On);
   end Create_Array_Tc;

   ---------------------
   -- Create_Fixed_Tc --
   ---------------------

   function Create_Fixed_Tc
     (IDL_Digits : in CORBA.Unsigned_Short;
      scale      : in CORBA.Short)
      return CORBA.TypeCode.Object
   is
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
   begin
      raise PolyORB.Not_Implemented;
      pragma Warnings (Off);
      return Create_Interface_Tc (Id, Name);
      --  "Possible infinite recursion".
      pragma Warnings (On);
   end Create_Interface_Tc;

   -----------------
   -- Create_List --
   -----------------

   procedure Create_List
     (Count    : in     CORBA.Long;
      New_List :    out CORBA.NVList.Ref)
   is
   begin
      pragma Warnings (Off);
      --  Parameter 'Count' below is only a hint.
      --  In this implementation, it is ignored.
      pragma Unreferenced (Count);
      pragma Warnings (On);

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
      return CORBA.TypeCode.Object
   is
   begin
      raise PolyORB.Not_Implemented;
      pragma Warnings (Off);
      return Create_Native_Tc (Id, Name);
      --  "Possible infinite recursion".
      pragma Warnings (On);
   end Create_Native_Tc;

   -------------------
   -- Create_Policy --
   -------------------

   procedure Create_Policy
     (The_Type : in PolicyType;
      Val      : Any)
   is
   begin
      raise PolyORB.Not_Implemented;
   end Create_Policy;

   ----------------------------------
   -- Create_Recursive_Sequence_Tc --
   ----------------------------------

   function Create_Recursive_Sequence_Tc
     (Bound  : in CORBA.Unsigned_Long;
      Offset : in CORBA.Unsigned_Long)
      return CORBA.TypeCode.Object
   is
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
   begin
      raise PolyORB.Not_Implemented;
      pragma Warnings (Off);
      return Create_Sequence_Tc (Bound, Element_Type);
      --  "Possible infinite recursion".
      pragma Warnings (On);
   end Create_Sequence_Tc;

   ----------------------
   -- Create_String_Tc --
   ----------------------

   function Create_String_Tc
     (Bound : in CORBA.Unsigned_Long)
      return CORBA.TypeCode.Object
   is
   begin
      raise PolyORB.Not_Implemented;
      pragma Warnings (Off);
      return Create_String_Tc (Bound);
      --  "Possible infinite recursion".
      pragma Warnings (On);
   end Create_String_Tc;

   -----------------------
   -- Create_Wstring_Tc --
   -----------------------

   function Create_Wstring_Tc
     (Bound : in CORBA.Unsigned_Long)
      return CORBA.TypeCode.Object
   is
   begin
      raise PolyORB.Not_Implemented;
      pragma Warnings (Off);
      return Create_Wstring_Tc (Bound);
      --  "Possible infinite recursion".
      pragma Warnings (On);
   end Create_Wstring_Tc;

   -------------------------
   -- Get_Default_Context --
   -------------------------

   function Get_Default_Context
      return CORBA.Context.Ref
   is
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
   begin
      raise PolyORB.Not_Implemented;
   end Get_Service_Information;

   ---------------------------
   -- List_Initial_Services --
   ---------------------------

   function List_Initial_Services return ObjectIdList is
   begin
      raise PolyORB.Not_Implemented;
      pragma Warnings (Off);
      return List_Initial_Services;
      --  "Possible infinite recursion".
      pragma Warnings (On);
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
   begin
      Referenced_Objects.Register
        (To_Standard_String (Identifier), Ref);
   end Register_Initial_Reference;

   procedure Register_Initial_Reference
     (Identifier : ObjectId;
      IOR        : String);
   --  Register an initial reference from an IOR given
   --  through the configuration subsystem.

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
      Id : constant Standard.String
        := To_Standard_String (Identifier);
   begin
      return Referenced_Objects.Lookup (Id);
   exception
      when E : others =>
         pragma Debug
           (O ("Got exception while looking up " & Id & ":"));
         pragma Debug (O (Ada.Exceptions.Exception_Information (E)));
         raise CORBA.InvalidName;
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
      return CORBA.String
        (Object_To_String
         (CORBA.Object.To_PolyORB_Ref (CORBA.Object.Ref (Obj))));
   end Object_To_String;

   ----------------------
   -- String_To_Object --
   ----------------------

   procedure String_To_Object
     (From : in     CORBA.String;
      To   : in out CORBA.Object.Ref'Class)
   is
   begin
      declare
         use PolyORB.References;
         use PolyORB.References.IOR;
         IOR : constant IOR_Type
           := String_To_Object (PolyORB.Types.String (From));
      begin
         CORBA.Object.Set (To, Entity_Of (IOR));
      end;
   exception
      when Constraint_Error =>
         PolyORB.Exceptions.Raise_Bad_Param;
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

   Initialized : Boolean := False;

   procedure Initialize is
      RootPOA : CORBA.Object.Ref;
   begin
      if Initialized then
         return;
      end if;

      CORBA.Object.Set
        (RootPOA, PolyORB.Smart_Pointers.Entity_Ptr
           (Object_Adapter (The_ORB)));
      Register_Initial_Reference
        (To_CORBA_String ("RootPOA"), RootPOA);

      declare
         Naming_IOR : constant Standard.String
           := PolyORB.Configuration.Get_Conf
           (Section => "corba", Key => "naming_ior", Default => "");
      begin
         if Naming_IOR /= "" then
            Register_Initial_Reference
              (To_CORBA_String ("NamingService"),
               To_CORBA_String (Naming_IOR));
         end if;
      end;
      Initialized := True;
   end Initialize;

   procedure Initialize (ORB_Name : in Standard.String)
   is
      pragma Warnings (Off);
      pragma Unreferenced (ORB_Name);
      pragma Warnings (On);
   begin
      null;
   end Initialize;

   ----------------------
   -- Create_Reference --
   ----------------------

   function Create_Reference
     (Object : in CORBA.Object.Ref;
      Typ : in Standard.String)
     return PolyORB.References.Ref
   is
      Result : PolyORB.References.Ref;

      Oid    : constant PolyORB.Objects.Object_Id_Access
        := new PolyORB.Objects.Object_Id'
        (CORBA.Object.To_PolyORB_Object (Object));
   begin
      if The_ORB = null then
         raise Internal;
      end if;

      PolyORB.ORB.Create_Reference
        (The_ORB, Oid, Typ, Result);

      return Result;
   end Create_Reference;

   use PolyORB.Initialization;
   use PolyORB.Utils.Strings;
   use PolyORB.Utils.Strings.Lists;

begin
   Register_Module
     (Module_Info'
      (Name => +"corba.orb",
       Conflicts => Empty,
       Depends => +"orb",
       Provides => +"corba.initial_references",
       Init => Initialize'Access));
end CORBA.ORB;

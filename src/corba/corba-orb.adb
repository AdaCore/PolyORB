------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                            C O R B A . O R B                             --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                Copyright (C) 2001 Free Software Fundation                --
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
-- create_alias_tc --
-- create_array_tc --
-- create_fixed_tc --
-- create_interface_tc --
-- Create_List --
-- create_native_tc --
-- Create_Policy --
-- create_recursive_sequence_tc --
-- create_sequence_tc --
-- create_string_tc --
-- create_wstring_tc --
-- Get_Default_Context --
-- Get_Service_Information --
-- List_Initial_Services --
-- Perform_Work --
-- Resolve_Initial_References --
-- Shutdown --
-- Work_Pending --

--  $Id$

with Ada.Exceptions;

with Sequences.Unbounded;

with PolyORB.Configurator;

with PolyORB.Dynamic_Dict;
pragma Elaborate_All (PolyORB.Dynamic_Dict);
with PolyORB.Log;
pragma Elaborate_All (PolyORB.Log);

with PolyORB.ORB;
with PolyORB.Objects;
with PolyORB.References.IOR;
with PolyORB.Setup;
with PolyORB.Smart_Pointers;

package body CORBA.ORB is

   use PolyORB.Log;
   use PolyORB.ORB;
   use PolyORB.Setup;

   package L is new PolyORB.Log.Facility_Log ("corba.orb");
   procedure O (Message : in Standard.String; Level : Log_Level := Debug)
     renames L.Output;

   package Referenced_Objects is
      new PolyORB.Dynamic_Dict (CORBA.Object.Ref);
   --  For initial references.

   ---------------------
   -- create_alias_tc --
   ---------------------

   function Create_Alias_Tc
     (Id            : in CORBA.RepositoryId;
      Name          : in CORBA.Identifier;
      Original_Type : in CORBA.TypeCode.Object)
      return CORBA.TypeCode.Object
   is
   begin
      raise PolyORB.Not_Implemented;
      return Create_Alias_Tc (Id, Name, Original_Type);
   end Create_Alias_Tc;

   ---------------------
   -- create_array_tc --
   ---------------------

   function Create_Array_Tc
     (Length       : in CORBA.Unsigned_Long;
      Element_Type : in CORBA.TypeCode.Object)
      return CORBA.TypeCode.Object
   is
   begin
      raise PolyORB.Not_Implemented;
      return Create_Array_Tc (Length, Element_Type);
   end Create_Array_Tc;

   ---------------------
   -- create_fixed_tc --
   ---------------------

   function Create_Fixed_Tc
     (IDL_Digits : in CORBA.Unsigned_Short;
      scale      : in CORBA.Short)
      return CORBA.TypeCode.Object
   is
   begin
      raise PolyORB.Not_Implemented;
      return Create_Fixed_Tc (IDL_Digits, scale);
   end Create_Fixed_Tc;

   -------------------------
   -- create_interface_tc --
   -------------------------

   function Create_Interface_Tc
     (Id   : in CORBA.RepositoryId;
      Name : in CORBA.Identifier)
      return CORBA.TypeCode.Object
   is
   begin
      raise PolyORB.Not_Implemented;
      return Create_Interface_Tc (Id, Name);
   end Create_Interface_Tc;

   -----------------
   -- Create_List --
   -----------------

   pragma Warnings (Off);
   --  Parameter 'Count' below is only a hint.
   --  In this implementation, it is ignored.
   procedure Create_List
     (Count    : in     CORBA.Long;
      New_List :    out CORBA.NVList.Ref)
   is
   begin
      CORBA.NVList.Create (New_List);
   end Create_List;
   pragma Warnings (On);

   procedure Create_List (New_List : out CORBA.ExceptionList.Ref)
     renames CORBA.ExceptionList.Create_List;

   ----------------------
   -- create_native_tc --
   ----------------------

   function Create_Native_Tc
     (Id   : in RepositoryId;
      Name : in Identifier)
      return CORBA.TypeCode.Object
   is
   begin
      raise PolyORB.Not_Implemented;
      return Create_Native_Tc (Id, Name);
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
   -- create_recursive_sequence_tc --
   ----------------------------------

   function Create_Recursive_Sequence_Tc
     (Bound  : in CORBA.Unsigned_Long;
      Offset : in CORBA.Unsigned_Long)
      return CORBA.TypeCode.Object
   is
   begin
      raise PolyORB.Not_Implemented;
      return Create_Recursive_Sequence_Tc (Bound, Offset);
   end Create_Recursive_Sequence_Tc;

   ------------------------
   -- create_sequence_tc --
   ------------------------

   function Create_Sequence_Tc
     (Bound        : in CORBA.Unsigned_Long;
      Element_Type : in CORBA.TypeCode.Object)
      return CORBA.TypeCode.Object
   is
   begin
      raise PolyORB.Not_Implemented;
      return Create_Sequence_Tc (Bound, Element_Type);
   end Create_Sequence_Tc;

   ----------------------
   -- create_string_tc --
   ----------------------

   function Create_String_Tc
     (Bound : in CORBA.Unsigned_Long)
      return CORBA.TypeCode.Object
   is
   begin
      raise PolyORB.Not_Implemented;
      return Create_String_Tc (Bound);
   end Create_String_Tc;

   -----------------------
   -- create_wstring_tc --
   -----------------------

   function Create_Wstring_Tc
     (Bound : in CORBA.Unsigned_Long)
      return CORBA.TypeCode.Object
   is
   begin
      raise PolyORB.Not_Implemented;
      return Create_Wstring_Tc (Bound);
   end Create_Wstring_Tc;

   -------------------------
   -- Get_Default_Context --
   -------------------------

   function Get_Default_Context
      return CORBA.Context.Ref
   is
   begin
      raise PolyORB.Not_Implemented;
      return Get_Default_Context;
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
      return List_Initial_Services;
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
      return Object_To_String
        (IOR_Type'(CORBA.Object.To_PolyORB_Ref (CORBA.Object.Ref (Obj))
                   with null record));
   end Object_To_String;

   ----------------------
   -- String_To_Object --
   ----------------------

   procedure String_To_Object
     (From : in     CORBA.String;
      To   : in out CORBA.Object.Ref'Class)
   is
      use PolyORB.References.IOR;
      IOR : constant IOR_Type := String_To_Object (From);
   begin
      CORBA.Object.Set (To, Entity_Of (IOR));
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

   procedure Initialize (ORB_Name : in Standard.String) is
      RootPOA : CORBA.Object.Ref;
   begin
      PolyORB.Configurator.Initialize_World;
      CORBA.Object.Set
        (RootPOA, PolyORB.Smart_Pointers.Entity_Ptr
         (Object_Adapter (The_ORB)));
      Referenced_Objects.Register ("RootPOA", RootPOA);
   exception
      when PolyORB.Configurator.Already_Initialized =>
         raise Initialization_Failure;
      when others =>
         raise;
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

      Oid    : PolyORB.Objects.Object_Id_Access
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

end CORBA.ORB;

------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--           P O L Y O R B . C O R B A _ P . E X C E P T I O N S            --
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

--  $Id: //droopi/main/src/corba/polyorb-corba_p-exceptions.adb#6 $

with Ada.Exceptions; use Ada.Exceptions;
with Ada.Strings.Unbounded;

with CORBA; use CORBA;

with PolyORB.CORBA_P.Exceptions.Stack;
with PolyORB.CORBA_P.Names; use PolyORB.CORBA_P.Names;
with PolyORB.Log;
pragma Elaborate_All (PolyORB.Log);
with PolyORB.Soft_Links;
with PolyORB.Types;
with PolyORB.Utils.Chained_Lists;
pragma Elaborate_All (PolyORB.Utils.Chained_Lists);

package body PolyORB.CORBA_P.Exceptions is

   use PolyORB.Log;

   package L is new PolyORB.Log.Facility_Log ("polyorb.corba_p.exceptions");
   procedure O (Message : in Standard.String; Level : Log_Level := Debug)
     renames L.Output;

   -------------------------------
   -- System exceptions mapping --
   -------------------------------

   type String_Access is access constant String;

   type Exception_Mapping is record
      Exc  : Ada.Exceptions.Exception_Id;
      Name : String_Access;
   end record;

   --  Explicit bounds are required in the nominal subtype
   --  in order to comply with Ravenscar restriction
   --  No_Implicit_Heap_Allocation.

   Mapping : constant array (Positive range 1 .. 37)
     of Exception_Mapping
     := ((Exc  => CORBA.Unknown'Identity,
          Name => new String'("CORBA/UNKNOWN")),
         (Exc  => CORBA.Bad_Param'Identity,
          Name => new String'("CORBA/BAD_PARAM")),
         (Exc  => CORBA.No_Memory'Identity,
          Name => new String'("CORBA/NO_MEMORY")),
         (Exc  => CORBA.Imp_Limit'Identity,
          Name => new String'("CORBA/IMP_LIMIT")),
         (Exc  => CORBA.Comm_Failure'Identity,
          Name => new String'("CORBA/COMM_FAILURE")),
         (Exc  => CORBA.Inv_Objref'Identity,
          Name => new String'("CORBA/INV_OBJREF")),
         (Exc  => CORBA.No_Permission'Identity,
          Name => new String'("CORBA/NO_PERMISSION")),
         (Exc  => CORBA.Internal'Identity,
          Name => new String'("CORBA/INTERNAL")),
         (Exc  => CORBA.Marshal'Identity,
          Name => new String'("CORBA/MARSHAL")),
         (Exc  => CORBA.Initialization_Failure'Identity,
          Name => new String'("CORBA/INITIALIZATION_FAILURE")),
         (Exc  => CORBA.No_Implement'Identity,
          Name => new String'("CORBA/NO_IMPLEMENT")),
         (Exc  => CORBA.Bad_TypeCode'Identity,
          Name => new String'("CORBA/BAD_TYPECODE")),
         (Exc  => CORBA.Bad_Operation'Identity,
          Name => new String'("CORBA/BAD_OPERATION")),
         (Exc  => CORBA.No_Resources'Identity,
          Name => new String'("CORBA/NO_RESOURCES")),
         (Exc  => CORBA.No_Response'Identity,
          Name => new String'("CORBA/NO_RESPONSE")),
         (Exc  => CORBA.Persist_Store'Identity,
          Name => new String'("CORBA/PERSIST_STORE")),
         (Exc  => CORBA.Bad_Inv_Order'Identity,
          Name => new String'("CORBA/BAD_INV_ORDER")),
         (Exc  => CORBA.Transient'Identity,
          Name => new String'("CORBA/TRANSIENT")),
         (Exc  => CORBA.Free_Mem'Identity,
          Name => new String'("CORBA/FREE_MEM")),
         (Exc  => CORBA.Inv_Ident'Identity,
          Name => new String'("CORBA/INV_IDENT")),
         (Exc  => CORBA.Inv_Flag'Identity,
          Name => new String'("CORBA/INV_FLAG")),
         (Exc  => CORBA.Intf_Repos'Identity,
          Name => new String'("CORBA/INTF_REPOS")),
         (Exc  => CORBA.Bad_Context'Identity,
          Name => new String'("CORBA/BAD_CONTEXT")),
         (Exc  => CORBA.Obj_Adapter'Identity,
          Name => new String'("CORBA/OBJ_ADAPTER")),
         (Exc  => CORBA.Data_Conversion'Identity,
          Name => new String'("CORBA/DATA_CONVERSION")),
         (Exc  => CORBA.Object_Not_Exist'Identity,
          Name => new String'("CORBA/OBJECT_NOT_EXIST")),
         (Exc  => CORBA.Transaction_Required'Identity,
          Name => new String'("CORBA/TRANSACTION_REQUIRED")),
         (Exc  => CORBA.Transaction_Rolledback'Identity,
          Name => new String'("CORBA/TRANSACTION_ROLLEDBACK")),
         (Exc  => CORBA.Invalid_Transaction'Identity,
          Name => new String'("CORBA/INVALID_TRANSACTION")),
         (Exc  => CORBA.Adapter_Already_Exists'Identity,
          Name => new String'("CORBA/ADAPTER_ALREADY_EXISTS")),
         (Exc  => CORBA.Invalid_Policy'Identity,
          Name => new String'("CORBA/INVALID_POLICY")),
         (Exc  => CORBA.Wrong_Policy'Identity,
          Name => new String'("CORBA/WRONG_POLICY")),
         (Exc  => CORBA.Servant_Already_Active'Identity,
          Name => new String'("CORBA/SERVANT_ALREADY_ACTIVE")),
         (Exc  => CORBA.Object_Already_Active'Identity,
          Name => new String'("CORBA/OBJECT_ALREADY_ACTIVE")),
         (Exc  => CORBA.Servant_Not_Active'Identity,
          Name => new String'("CORBA/SERVANT_NOT_ACTIVE")),
         (Exc  => CORBA.Servant_Not_Active'Identity,
          Name => new String'("CORBA/OBJECT_NOT_ACTIVE")),
         (Exc  => CORBA.Adapter_Inactive'Identity,
          Name => new String'("CORBA/ADAPTER_INACTIVE")));

   ----------------------
   -- User_Get_Members --
   ----------------------

   procedure User_Get_Members
     (Occurrence : CORBA.Exception_Occurrence;
      Members : out CORBA.IDL_Exception_Members'Class)
     renames PolyORB.CORBA_P.Exceptions.Stack.Get_Members;
   --  Extract members from a user exception occurence

   ------------------------
   -- User_Purge_Members --
   ------------------------

   procedure User_Purge_Members
     (Occurrence : CORBA.Exception_Occurrence)
     renames PolyORB.CORBA_P.Exceptions.Stack.Purge_Members;

   --------------------------
   -- User_Raise_Exception --
   --------------------------

   procedure User_Raise_Exception
     (Id : Ada.Exceptions.Exception_Id;
      Members : IDL_Exception_Members'Class)
     renames PolyORB.CORBA_P.Exceptions.Stack.Raise_Exception;
   --  Raise a user exception with the specified members.

   -------------------------------
   -- System exception handling --
   -------------------------------

   function Get_ExcepId_By_RepositoryId
     (RepoId : in Standard.String)
     return Ada.Exceptions.Exception_Id is
   begin
      for I in Mapping'Range loop
         if RepoId = OMG_RepositoryId (Mapping (I) .Name.all) then
            return Mapping (I).Exc;
         end if;
      end loop;

      return Ada.Exceptions.Null_Id;
   end Get_ExcepId_By_RepositoryId;

   ---------------------
   -- Raise_Exception --
   ---------------------

   procedure Raise_System_Exception
     (Excp      : in Exception_Id;
      Excp_Memb : in System_Exception_Members);
   pragma No_Return (Raise_System_Exception);
   --  Raise the corresponding CORBA exception, and store its
   --  members for later retrieval by Get_Members.

   procedure Raise_System_Exception
     (Excp      : in Exception_Id;
      Excp_Memb : in System_Exception_Members)
   is
      Str : String (1 .. 5);
      Val : CORBA.Unsigned_Long;
   begin
      --  Marshall Minor and Completed fields of EXCP_MEMB into a string.
      --  A trivial marshalling is used:

      --  Str (1 .. 4)   Minor (MSB first)
      --  Str (5)        Completed

      Str (5) := Character'Val (Completion_Status'Pos (Excp_Memb.Completed));
      Val := Excp_Memb.Minor;
      for I in 1 .. 4 loop
         Str (I) := Character'Val (Val / 2 ** 24);
         Val := (Val mod 2 ** 24) * 256;
      end loop;

      --  Raise the exception.
      Ada.Exceptions.Raise_Exception (Excp, Str);

      --  Huh, excp can't be null_id
      raise Program_Error;
   end Raise_System_Exception;

   -----------------
   -- Get_Members --
   -----------------

   procedure Get_Members
     (From : in CORBA.Exception_Occurrence;
      To   : out System_Exception_Members)
   is
      Str : constant String := Ada.Exceptions.Exception_Message (From);
      Val : Unsigned_Long;
   begin
      --  Check length.
      if Str'Length /= 5 then
         Raise_Bad_Param;
      end if;

      --  Unmarshall completion status.
      --  This can raise constraint_error.
      To.Completed := Completion_Status'Val (Character'Pos (Str (Str'Last)));

      --  Unmarshall minor.
      Val := 0;
      for I in Str'First .. Str'Last - 1 loop
         Val := Val * 256 + Character'Pos (Str (I));
      end loop;
      To.Minor := Val;
   exception
      when Constraint_Error =>
         Raise_Bad_Param;
   end Get_Members;

   -------------------------------------
   -- Raise standard CORBA exceptions --
   -------------------------------------

   procedure Raise_Unknown
     (Minor  : CORBA.Unsigned_Long := 0;
      Status : Completion_Status := Completed_No) is
   begin
      Raise_System_Exception
        (Unknown'Identity,
         System_Exception_Members'(Minor => Minor, Completed => Status));
   end Raise_Unknown;

   procedure Raise_Bad_Param
     (Minor  : CORBA.Unsigned_Long := 0;
      Status : Completion_Status := Completed_No) is
   begin
      Raise_System_Exception
        (Bad_Param'Identity,
         System_Exception_Members'(Minor => Minor, Completed => Status));
   end Raise_Bad_Param;

   procedure Raise_Marshal
     (Minor  : CORBA.Unsigned_Long := 0;
      Status : Completion_Status := Completed_No) is
   begin
      Raise_System_Exception
        (Marshal'Identity,
         System_Exception_Members'(Minor => Minor, Completed => Status));
   end Raise_Marshal;

   procedure Raise_Comm_Failure
     (Minor  : CORBA.Unsigned_Long := 0;
      Status : Completion_Status := Completed_No) is
   begin
      Raise_System_Exception
        (Comm_Failure'Identity,
         System_Exception_Members'(Minor => Minor, Completed => Status));
   end Raise_Comm_Failure;

   procedure Raise_Inv_Objref
     (Minor  : CORBA.Unsigned_Long := 0;
      Status : Completion_Status := Completed_No) is
   begin
      Raise_System_Exception
        (Inv_Objref'Identity,
         System_Exception_Members'(Minor => Minor, Completed => Status));
   end Raise_Inv_Objref;

   procedure Raise_Object_Not_Exist
     (Minor  : CORBA.Unsigned_Long := 0;
      Status : Completion_Status := Completed_No) is
   begin
      Raise_System_Exception
        (Object_Not_Exist'Identity,
         System_Exception_Members'(Minor => Minor, Completed => Status));
   end Raise_Object_Not_Exist;

   procedure Raise_Bad_Operation
     (Minor  : CORBA.Unsigned_Long := 0;
      Status : Completion_Status := Completed_No) is
   begin
      Raise_System_Exception
        (Bad_Operation'Identity,
         System_Exception_Members'(Minor => Minor, Completed => Status));
   end Raise_Bad_Operation;

   procedure Raise_Transient
     (Minor  : CORBA.Unsigned_Long := 0;
      Status : Completion_Status := Completed_No) is
   begin
      Raise_System_Exception
        (Transient'Identity,
         System_Exception_Members'(Minor => Minor, Completed => Status));
   end Raise_Transient;

   procedure Raise_Internal
     (Minor  : Unsigned_Long := 0;
      Status : Completion_Status := Completed_No) is
   begin
      Raise_System_Exception
        (Internal'Identity,
         System_Exception_Members'(Minor => Minor, Completed => Status));
   end Raise_Internal;

   procedure Raise_Obj_Adapter
     (Minor  : CORBA.Unsigned_Long := 0;
      Status : Completion_Status := Completed_No) is
   begin
      Raise_System_Exception
        (Obj_Adapter'Identity,
         System_Exception_Members'(Minor => Minor, Completed => Status));
   end Raise_Obj_Adapter;

   procedure Raise_No_Implement
     (Minor  : CORBA.Unsigned_Long := 0;
      Status : Completion_Status := Completed_No) is
   begin
      Raise_System_Exception
        (No_Implement'Identity,
         System_Exception_Members'(Minor => Minor, Completed => Status));
   end Raise_No_Implement;

   procedure Raise_Imp_Limit
     (Minor  : Unsigned_Long := 0;
      Status : Completion_Status := Completed_No) is
   begin
      Raise_System_Exception
        (Imp_Limit'Identity,
         System_Exception_Members'(Minor => Minor, Completed => Status));
   end Raise_Imp_Limit;

   procedure Raise_Bad_Inv_Order
     (Minor  : CORBA.Unsigned_Long := 0;
      Status : Completion_Status := Completed_No) is
   begin
      Raise_System_Exception
        (Bad_Inv_Order'Identity,
         System_Exception_Members'(Minor => Minor, Completed => Status));
   end Raise_Bad_Inv_Order;

   procedure Raise_Bad_TypeCode
     (Minor  : CORBA.Unsigned_Long := 0;
      Status : Completion_Status := Completed_No) is
   begin
      Raise_System_Exception
        (Bad_TypeCode'Identity,
         System_Exception_Members'(Minor => Minor, Completed => Status));
   end Raise_Bad_TypeCode;

   procedure Raise_Adapter_Already_Exists
     (Minor  : CORBA.Unsigned_Long := 0;
      Status : Completion_Status := Completed_No) is
   begin
      Raise_System_Exception
        (Adapter_Already_Exists'Identity,
         System_Exception_Members'(Minor => Minor, Completed => Status));
   end Raise_Adapter_Already_Exists;

   procedure Raise_Invalid_Policy
     (Minor  : CORBA.Unsigned_Long := 0;
      Status : Completion_Status := Completed_No) is
   begin
      Raise_System_Exception
        (Invalid_Policy'Identity,
         System_Exception_Members'(Minor => Minor, Completed => Status));
   end Raise_Invalid_Policy;

   procedure Raise_Wrong_Policy
     (Minor  : CORBA.Unsigned_Long := 0;
      Status : Completion_Status := Completed_No) is
   begin
      Raise_System_Exception
        (Wrong_Policy'Identity,
         System_Exception_Members'(Minor => Minor, Completed => Status));
   end Raise_Wrong_Policy;

   procedure Raise_Servant_Already_Active
     (Minor  : CORBA.Unsigned_Long := 0;
      Status : Completion_Status := Completed_No) is
   begin
      Raise_System_Exception
        (Servant_Already_Active'Identity,
         System_Exception_Members'(Minor => Minor, Completed => Status));
   end Raise_Servant_Already_Active;

   procedure Raise_Object_Already_Active
     (Minor  : CORBA.Unsigned_Long := 0;
      Status : Completion_Status := Completed_No) is
   begin
      Raise_System_Exception
        (Object_Already_Active'Identity,
         System_Exception_Members'(Minor => Minor, Completed => Status));
   end Raise_Object_Already_Active;

   procedure Raise_Servant_Not_Active
     (Minor  : CORBA.Unsigned_Long := 0;
      Status : Completion_Status := Completed_No) is
   begin
      Raise_System_Exception
        (Servant_Not_Active'Identity,
         System_Exception_Members'(Minor => Minor, Completed => Status));
   end Raise_Servant_Not_Active;

   procedure Raise_Object_Not_Active
     (Minor  : CORBA.Unsigned_Long := 0;
      Status : Completion_Status := Completed_No) is
   begin
      Raise_System_Exception
        (Object_Not_Active'Identity,
         System_Exception_Members'(Minor => Minor, Completed => Status));
   end Raise_Object_Not_Active;

   procedure Raise_Adapter_Inactive
     (Minor  : CORBA.Unsigned_Long := 0;
      Status : Completion_Status := Completed_No) is
   begin
      Raise_System_Exception
        (Adapter_Inactive'Identity,
         System_Exception_Members'(Minor => Minor, Completed => Status));
   end Raise_Adapter_Inactive;

   -----------------------------------------------------
   -- System exceptions                               --
   -- Same as CORBA.To_CORBA_String, but redefined to --
   -- avoid a circular elaboration dependency.        --
   -----------------------------------------------------

   function To_RepositoryId
     (S : in Standard.String) return CORBA.RepositoryId;

   function To_RepositoryId
     (S : in Standard.String)
     return CORBA.RepositoryId is
   begin
      return CORBA.RepositoryId
        (Ada.Strings.Unbounded.To_Unbounded_String (S));
   end To_RepositoryId;

   function Occurrence_To_Name
     (Occurrence : CORBA.Exception_Occurrence)
     return CORBA.RepositoryId
   is
      use Ada.Exceptions;
      Id : constant Exception_Id := Exception_Identity (Occurrence);
   begin
      for I in Mapping'Range loop
         if Id = Mapping (I) .Exc then
            return To_RepositoryId (OMG_RepositoryId (Mapping (I) .Name.all));
         end if;
      end loop;
      raise Program_Error;
   end Occurrence_To_Name;

   -----------------------------
   -- User exceptions mapping --
   -----------------------------

   type Exception_Info is record
      TC     : TypeCode.Object;
      Raiser : Raise_From_Any_Procedure;
   end record;

   function Find_Exception_Info
     (For_Exception : CORBA.RepositoryId)
     return Exception_Info;

   -----------------------------
   -- A list of Exception_Info --
   -----------------------------

   package Exception_Lists is new PolyORB.Utils.Chained_Lists
     (Exception_Info);

   All_Exceptions : Exception_Lists.List;

   ---------------
   -- Find_Info --
   ---------------

   function Find_Exception_Info
     (For_Exception : CORBA.RepositoryId)
     return Exception_Info
   is
      use PolyORB.Soft_Links;
      use PolyORB.Types;
      use Exception_Lists;

      Id : constant Types.RepositoryId
        := Types.RepositoryId (For_Exception);
      It : Iterator;
      Info : Exception_Info;

   begin
      pragma Debug
        (O ("Looking up einfo for " & To_Standard_String (For_Exception)));
      Enter_Critical_Section;
      It := First (All_Exceptions);

      while not Last (It) loop
         exit when CORBA.TypeCode.Id (Value (It).TC) = Id;
         Next (It);
      end loop;

      if Last (It) then
         Leave_Critical_Section;
         --  return Unknown_User_Exception_Info;
         --  XXX actually should raise CORBA::UnknownUserException
         --  with members containing an Any.
         pragma Debug (O ("Unknown exception"));
         Raise_Unknown;
      end if;

      Info := Value (It).all;
      Leave_Critical_Section;

      return Info;
   end Find_Exception_Info;

   --------------------
   -- Raise_From_Any --
   --------------------

   function TC_Completion_Status return TypeCode.Object;
   --  The typecode for standard enumeration type
   --  CORBA::completion_status.

   function From_Any (Item : Any.Any) return Completion_Status;

   procedure Raise_From_Any (Occurrence : Any.Any) is
      Repository_Id : constant CORBA.RepositoryId
        := CORBA.RepositoryId
        (Any.TypeCode.Id (Get_Type (Occurrence)));

      System_Id : constant Ada.Exceptions.Exception_Id
        := Get_ExcepId_By_RepositoryId
        (To_Standard_String (Repository_Id));

   begin
      if System_Id /= Ada.Exceptions.Null_Id then
         --  This is a system exception.

         declare
            Minor : constant CORBA.Unsigned_Long
              := From_Any
              (Get_Aggregate_Element
               (Occurrence, TC_Unsigned_Long, 1));
            Completed : constant CORBA.Completion_Status
              := From_Any
              (Get_Aggregate_Element
               (Occurrence, TC_Completion_Status, 1));
         begin
            Raise_System_Exception
              (System_Id,
               System_Exception_Members'
               (Minor => Minor, Completed => Completed));
         end;
      end if;

      declare
         EInfo : constant Exception_Info := Find_Exception_Info
           (Repository_Id);
      begin
         EInfo.Raiser.all (Occurrence);
      end;

      raise Program_Error;
      --  Never reached (Raiser raises an exception.)
   end Raise_From_Any;

   ------------------------
   -- Register_Exception --
   ------------------------

   procedure Register_Exception
     (TC     : in CORBA.TypeCode.Object;
      Raiser : in Raise_From_Any_Procedure)
   is
      use PolyORB.Soft_Links;
      use Exception_Lists;
   begin
      pragma Debug
        (O ("Registering exception: "
            & Types.To_Standard_String (TypeCode.Id (TC))));
      Enter_Critical_Section;
      Append (All_Exceptions, (TC => TC, Raiser => Raiser));
      Leave_Critical_Section;
   end Register_Exception;

   -------------------------------
   -- System_Exception_TypeCode --
   -------------------------------

   TC_Completion_Status_Cache : TypeCode.Object;

   function TC_Completion_Status return TypeCode.Object is
      use type PolyORB.Types.Unsigned_Long;
      TC : TypeCode.Object renames TC_Completion_Status_Cache;
   begin
      if TypeCode.Parameter_Count (TC) /= 0 then
         return TC_Completion_Status_Cache;
      end if;

      TypeCode.Add_Parameter
        (TC, To_Any (To_CORBA_String ("completion_status")));
      TypeCode.Add_Parameter
        (TC, To_Any (To_CORBA_String ("IDL:CORBA/completion_status:1.0")));

      for C in Completion_Status'Range loop
         TypeCode.Add_Parameter
           (TC, To_Any (To_CORBA_String (Completion_Status'Image (C))));
      end loop;
      return TC;
   end TC_Completion_Status;

   function From_Any (Item : Any.Any) return Completion_Status is
   begin
      return Completion_Status'Val
        (Unsigned_Long'
         (From_Any (PolyORB.Any.Get_Aggregate_Element
                    (Item, TC_Unsigned_Long, 0))));
   end From_Any;

   function System_Exception_TypeCode
     (Name : PolyORB.Types.RepositoryId)
     return Any.TypeCode.Object
   is
      TC : TypeCode.Object := TypeCode.TC_Except;
   begin
      --  Construct exception typecode

      TypeCode.Add_Parameter (TC, To_Any (CORBA.String (Name)));
      --  Name

      TypeCode.Add_Parameter
        (TC, To_Any
         (To_CORBA_String ("IDL:") & CORBA.String (Name)
          & To_CORBA_String (":1.0")));
      --  RepositoryId

      TypeCode.Add_Parameter
        (TC, To_Any (TC_Unsigned_Long));
      TypeCode.Add_Parameter
        (TC, To_Any (To_CORBA_String ("minor")));
      --  Component 'minor'

      TypeCode.Add_Parameter
        (TC, To_Any (TC_Completion_Status));
      TypeCode.Add_Parameter
        (TC, To_Any (To_CORBA_String ("completed")));
      --  Component 'completed'

      return TC;
   end System_Exception_TypeCode;

end PolyORB.CORBA_P.Exceptions;

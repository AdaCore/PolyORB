------------------------------------------------------------------------------
--                                                                          --
--                          ADABROKER COMPONENTS                            --
--                                                                          --
--                 A D A B R O K E R . E X C E P T I O N S                  --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.8 $
--                                                                          --
--         Copyright (C) 1999-2000 ENST Paris University, France.           --
--                                                                          --
-- AdaBroker is free software; you  can  redistribute  it and/or modify it  --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. AdaBroker  is distributed  in the hope that it will be  useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with AdaBroker; see file COPYING. If  --
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
--             AdaBroker is maintained by ENST Paris University.            --
--                     (email: broker@inf.enst.fr)                          --
--                                                                          --
------------------------------------------------------------------------------

--  This unit deals with the raising of C exceptions in Ada and ada
--  ones in C.  It is both a C and a Ada class (see Ada_Exceptions.hh)
--  and provides 2 mains methods : raise_C_Exception and
--  raise_Ada_Exception. The first one is called by Ada code and
--  implemented in C. The second is called by C code and implemented
--  in Ada. Both translate exceptions in the other language.

with AdaBroker.Constants;
with AdaBroker.Debug;
pragma Elaborate_All (AdaBroker.Debug);

with Ada.Exceptions; use Ada.Exceptions;

with GNAT.Table;

package body AdaBroker.Exceptions is

   use type AdaBroker.Constants.Exception_Id;

   Flag : constant Natural
     := AdaBroker.Debug.Is_Active ("adabroker.exceptions");
   procedure O is new AdaBroker.Debug.Output (Flag);

   type Stamp is new Natural;
   First_Stamp : constant Stamp := Stamp'First;
   Last_Stamp  : Stamp := First_Stamp;
   Obsolete    : constant Stamp := 256;

   type Member_Record is
      record
         S : Stamp                     := First_Stamp;
         M : IDL_Exception_Members_Ptr := null;
      end record;

   type Occurrence_Id is new Natural;
   Occurrence_Low_Bound : constant Occurrence_Id := 1;
   Occurrence_Initial   : constant Natural       := 8;
   Occurrence_Increment : constant Natural       := 64;

   package Occurrences is new GNAT.Table
     (Member_Record,
      Occurrence_Id,
      Occurrence_Low_Bound,
      Occurrence_Initial,
      Occurrence_Increment);

   Header : constant String := "CORBA::MEMBER";

   procedure C_Raise_Ada_System_Exception
     (Pd_Minor  : in C.unsigned_long;
      Pd_Status : in C.int;
      Ex_Id     : in Ada.Exceptions.Exception_Id;
      Ex_Member : in out CORBA.System_Exception_Members'Class);

   procedure Output_Ex_Member (Member : in IDL_Exception_Members_Ptr);

   procedure Lock_Occurrence_Table;
   pragma Import (CPP, Lock_Occurrence_Table, "Lock_Occurrence_Table__Fv");

   procedure Unlock_Occurrence_Table;
   pragma Import (CPP, Unlock_Occurrence_Table, "Unlock_Occurrence_Table__Fv");

   -----------------
   -- Get_Members --
   -----------------

   function Get_Members
     (From : in Ada.Exceptions.Exception_Occurrence)
     return IDL_Exception_Members'Class
   is
      Occurrence : Occurrence_Id;
      Message    : String  := Exception_Message (From);
      First      : Natural := Message'First;
      Member     : IDL_Exception_Members_Ptr;
   begin
      pragma Debug (O ("get_members: enter"));

      Lock_Occurrence_Table;

      if Message'Length > Header'Length
        and then Message (First .. First + Header'Length - 1) = Header
      then
         Occurrence := Occurrence_Id'Value
           (Message (First + Header'Length .. Message'Last));
      else
         Unlock_Occurrence_Table;
         pragma Debug (O ("incorrect exception message: " & Message));
         raise Constraint_Error;
      end if;

      Member := Occurrences.Table (Occurrence).M;
      Occurrences.Table (Occurrence).S := First_Stamp;
      if Member = null then
         Unlock_Occurrence_Table;
         pragma Debug (O ("null exception member"));
         raise Constraint_Error;
      end if;

      Occurrences.Table (Occurrence).M := null;
      declare
         Result : IDL_Exception_Members'Class := Member.all;
      begin
         Free (Member);
         Unlock_Occurrence_Table;
         pragma Debug (O ("get_members: leave"));
         return Result;
      end;
   end Get_Members;

   -----------------------------------
   -- C_Raise_Ada_System_Exception --
   -----------------------------------

   procedure C_Raise_Ada_System_Exception
     (Pd_Minor  : in C.unsigned_long;
      Pd_Status : in C.int;
      Ex_Id     : in Ada.Exceptions.Exception_Id;
      Ex_Member : in out CORBA.System_Exception_Members'Class)
   is
   begin
      Ex_Member.Minor     := CORBA.Unsigned_Long (Pd_Minor);
      Ex_Member.Completed := To_Status (Pd_Status);

      Raise_CORBA_Exception (Ex_Id, Ex_Member);
   end C_Raise_Ada_System_Exception;

   ----------------------------
   -- Raise_CORBA_Exception --
   ----------------------------

   procedure Raise_CORBA_Exception
     (Ex_Id     : in Ada.Exceptions.Exception_Id;
      Ex_Member : in IDL_Exception_Members'Class)
   is
      Member     : IDL_Exception_Members_Ptr;
      Full       : Boolean := True;
      Occurrence : Occurrence_Id;
   begin
      Lock_Occurrence_Table;

      for Occ in 1 .. Occurrences.Last loop
         if Occurrences.Table (Occ).S = First_Stamp then
            Occurrence := Occ;
            Full       := False;
            exit;
         else
            pragma Debug (O ("occurrence" & Occ'Img & " in use"));
            null;
         end if;
      end loop;

      if Full then
         --  Try to collect garbage before incrementing table.
         for Occ in 1 .. Occurrences.Last loop
            if Last_Stamp - Occurrences.Table (Occ).S > Obsolete then
               pragma Debug (O ("collect occurrence" & Occ'Img));

               Occurrences.Table (Occ).S := First_Stamp;
               if Occurrences.Table (Occ).M /= null then
                  Free (Occurrences.Table (Occ).M);
               end if;

               if Full then
                  Occurrence := Occ;
               end if;
               Full := False;
            end if;
         end loop;


         if Full then
            pragma Debug (O ("no room in occurrence table anymore"));
            Occurrences.Increment_Last;
            Occurrence := Occurrences.Last;

            pragma Debug (O ("create new occurrence" & Occurrence'Img));
         end if;
      end if;

      pragma Debug (O ("occurrence" & Occurrence'Img & " is empty"));
      Member := new IDL_Exception_Members'Class'(Ex_Member);
      Output_Ex_Member (Member);

      Last_Stamp := Last_Stamp + 1;
      Occurrences.Table (Occurrence).M := Member;
      Occurrences.Table (Occurrence).S := Last_Stamp;

      Unlock_Occurrence_Table;

      --  Raise Ada exception with occurrence index as message.
      Ada.Exceptions.Raise_Exception (Ex_Id, Header & Occurrence'Img);
   end Raise_CORBA_Exception;

   ---------------------------------
   -- C_Raise_Ada_Fatal_Exception --
   ---------------------------------

   procedure C_Raise_Ada_Fatal_Exception
     (File    : in C.Strings.chars_ptr;
      Line    : in C.int;
      Err_Msg : in C.Strings.chars_ptr)
   is
   begin
      Ada.Exceptions.Raise_Exception
        (OmniORB_Fatal_Error'Identity,
         Interfaces.C.Strings.Value (File) &
         ":" & Line'Img &
         ": " & Interfaces.C.Strings.Value (Err_Msg));
   end C_Raise_Ada_Fatal_Exception;

   -----------------------------------
   -- C_Raise_Ada_UNKNOWN_Exception --
   -----------------------------------

   procedure C_Raise_Ada_UNKNOWN_Exception
     (Pd_Minor  : in C.unsigned_long;
      Pd_Status : in C.int)
   is
      Ex_Member : CORBA.Unknown_Members;
   begin
      C_Raise_Ada_System_Exception
        (Pd_Minor,
         Pd_Status,
         CORBA.Unknown'Identity,
         Ex_Member);
   end C_Raise_Ada_UNKNOWN_Exception;

   -------------------------------------
   -- C_Raise_Ada_BAD_PARAM_Exception --
   -------------------------------------

   procedure C_Raise_Ada_BAD_PARAM_Exception
     (Pd_Minor  : in C.unsigned_long;
      Pd_Status : in C.int)
   is
      Ex_Member : CORBA.Bad_Param_Members;
   begin
      C_Raise_Ada_System_Exception
        (Pd_Minor,
         Pd_Status,
         CORBA.Bad_Param'Identity,
         Ex_Member);
   end C_Raise_Ada_BAD_PARAM_Exception;

   -------------------------------------
   -- C_Raise_Ada_NO_MEMORY_Exception --
   -------------------------------------

   procedure C_Raise_Ada_NO_MEMORY_Exception
     (Pd_Minor  : in C.unsigned_long;
      Pd_Status : in C.int)
   is
      Ex_Member : CORBA.No_Memory_Members;
   begin
      C_Raise_Ada_System_Exception
        (Pd_Minor,
         Pd_Status,
         CORBA.No_Memory'Identity,
         Ex_Member);
   end C_Raise_Ada_NO_MEMORY_Exception;

   -------------------------------------
   -- C_Raise_Ada_IMP_LIMIT_Exception --
   -------------------------------------

   procedure C_Raise_Ada_IMP_LIMIT_Exception
     (Pd_Minor  : in C.unsigned_long;
      Pd_Status : in C.int)
   is
      Ex_Member : CORBA.Imp_Limit_Members;
   begin
      C_Raise_Ada_System_Exception
        (Pd_Minor,
         Pd_Status,
         CORBA.Imp_Limit'Identity,
         Ex_Member);
   end C_Raise_Ada_IMP_LIMIT_Exception;

   ----------------------------------------
   -- C_Raise_Ada_COMM_FAILURE_Exception --
   ----------------------------------------

   procedure C_Raise_Ada_COMM_FAILURE_Exception
     (Pd_Minor  : in C.unsigned_long;
      Pd_Status : in C.int)
   is
      Ex_Member : CORBA.Comm_Failure_Members;
   begin
      C_Raise_Ada_System_Exception
        (Pd_Minor,
         Pd_Status,
         CORBA.Comm_Failure'Identity,
         Ex_Member);
   end C_Raise_Ada_COMM_FAILURE_Exception;

   --------------------------------------
   -- C_Raise_Ada_INV_OBJREF_Exception --
   --------------------------------------

   procedure C_Raise_Ada_INV_OBJREF_Exception
     (Pd_Minor  : in C.unsigned_long;
      Pd_Status : in C.int)
   is
      Ex_Member : CORBA.Inv_Objref_Members;
   begin
      C_Raise_Ada_System_Exception
        (Pd_Minor,
         Pd_Status,
         CORBA.Inv_Objref'Identity,
         Ex_Member);
   end C_Raise_Ada_INV_OBJREF_Exception;

   --------------------------------------------
   -- C_Raise_Ada_OBJECT_NOT_EXIST_Exception --
   --------------------------------------------

   procedure C_Raise_Ada_OBJECT_NOT_EXIST_Exception
     (Pd_Minor  : in C.unsigned_long;
      Pd_Status : in C.int)
   is
      Ex_Member : CORBA.Object_Not_Exist_Members;
   begin
      C_Raise_Ada_System_Exception
        (Pd_Minor,
         Pd_Status,
         CORBA.Object_Not_Exist'Identity,
         Ex_Member);
   end C_Raise_Ada_OBJECT_NOT_EXIST_Exception;

   -----------------------------------------
   -- C_Raise_Ada_NO_PERMISSION_Exception --
   -----------------------------------------

   procedure C_Raise_Ada_NO_PERMISSION_Exception
     (Pd_Minor  : in C.unsigned_long;
      Pd_Status : in C.int)
   is
      Ex_Member : CORBA.No_Permission_Members;
   begin
      C_Raise_Ada_System_Exception
        (Pd_Minor,
         Pd_Status,
         CORBA.No_Permission'Identity,
         Ex_Member);
   end C_Raise_Ada_NO_PERMISSION_Exception;

   ------------------------------------
   -- C_Raise_Ada_INTERNAL_Exception --
   ------------------------------------

   procedure C_Raise_Ada_INTERNAL_Exception
     (Pd_Minor  : in C.unsigned_long;
      Pd_Status : in C.int)
   is
      Ex_Member : CORBA.Internal_Members;
   begin
      C_Raise_Ada_System_Exception
        (Pd_Minor,
         Pd_Status,
         CORBA.Internal'Identity,
         Ex_Member);
   end C_Raise_Ada_INTERNAL_Exception;

   -----------------------------------
   -- C_Raise_Ada_MARSHAL_Exception --
   -----------------------------------

   procedure C_Raise_Ada_MARSHAL_Exception
     (Pd_Minor  : in C.unsigned_long;
      Pd_Status : in C.int)
   is
      Ex_Member : CORBA.Marshal_Members;
   begin
      C_Raise_Ada_System_Exception
        (Pd_Minor,
         Pd_Status,
         CORBA.Marshal'Identity,
         Ex_Member);
   end C_Raise_Ada_MARSHAL_Exception;

   ---------------------------------------------------
   --  C_Raise_Ada_INITIALIZATION_FAILURE_Exception --
   ---------------------------------------------------

   procedure C_Raise_Ada_INITIALIZATION_FAILURE_Exception
     (Pd_Minor  : in C.unsigned_long;
      Pd_Status : in C.int)
   is
      Ex_Member : CORBA.Initialization_Failure_Members;
   begin
      C_Raise_Ada_System_Exception
        (Pd_Minor,
         Pd_Status,
         CORBA.Initialization_Failure'Identity,
         Ex_Member);
   end C_Raise_Ada_INITIALIZATION_FAILURE_Exception;

   ----------------------------------------
   -- C_Raise_Ada_NO_IMPLEMENT_Exception --
   ----------------------------------------

   procedure C_Raise_Ada_NO_IMPLEMENT_Exception
     (Pd_Minor  : in C.unsigned_long;
      Pd_Status : in C.int)
   is
      Ex_Member : CORBA.No_Implement_Members;
   begin
      C_Raise_Ada_System_Exception
        (Pd_Minor,
         Pd_Status,
         CORBA.No_Implement'Identity,
         Ex_Member);
   end C_Raise_Ada_NO_IMPLEMENT_Exception;

   ----------------------------------------
   -- C_Raise_Ada_BAD_TYPECODE_Exception --
   ----------------------------------------

   procedure C_Raise_Ada_BAD_TYPECODE_Exception
     (Pd_Minor  : in C.unsigned_long;
      Pd_Status : in C.int)
   is
      Ex_Member : CORBA.Bad_Typecode_Members;
   begin
      C_Raise_Ada_System_Exception
        (Pd_Minor,
         Pd_Status,
         CORBA.Bad_Typecode'Identity,
         Ex_Member);
   end C_Raise_Ada_BAD_TYPECODE_Exception;

   -----------------------------------------
   -- C_Raise_Ada_BAD_OPERATION_Exception --
   -----------------------------------------

   procedure C_Raise_Ada_BAD_OPERATION_Exception
     (Pd_Minor  : in C.unsigned_long;
      Pd_Status : in C.int)
   is
      Ex_Member : CORBA.Bad_Operation_Members;
   begin
      C_Raise_Ada_System_Exception
        (Pd_Minor,
         Pd_Status,
         CORBA.Bad_Operation'Identity,
         Ex_Member);
   end C_Raise_Ada_BAD_OPERATION_Exception;

   ----------------------------------------
   -- C_Raise_Ada_NO_RESOURCES_Exception --
   ----------------------------------------

   procedure C_Raise_Ada_NO_RESOURCES_Exception
     (Pd_Minor  : in C.unsigned_long;
      Pd_Status : in C.int)
   is
      Ex_Member : CORBA.No_Resources_Members;
   begin
      C_Raise_Ada_System_Exception
        (Pd_Minor,
         Pd_Status,
         CORBA.No_Resources'Identity,
         Ex_Member);
   end C_Raise_Ada_NO_RESOURCES_Exception;

   ---------------------------------------
   -- C_Raise_Ada_NO_RESPONSE_Exception --
   ---------------------------------------

   procedure C_Raise_Ada_NO_RESPONSE_Exception
     (Pd_Minor  : in C.unsigned_long;
      Pd_Status : in C.int)
   is
      Ex_Member : CORBA.No_Response_Members;
   begin
      C_Raise_Ada_System_Exception
        (Pd_Minor,
         Pd_Status,
         CORBA.No_Response'Identity,
         Ex_Member);
   end C_Raise_Ada_NO_RESPONSE_Exception;

   -----------------------------------------
   -- C_Raise_Ada_PERSIST_STORE_Exception --
   -----------------------------------------

   procedure C_Raise_Ada_PERSIST_STORE_Exception
     (Pd_Minor  : in C.unsigned_long;
      Pd_Status : in C.int)
   is
      Ex_Member : CORBA.Persist_Store_Members;
   begin
      C_Raise_Ada_System_Exception
        (Pd_Minor,
         Pd_Status,
         CORBA.Persist_Store'Identity,
         Ex_Member);
   end C_Raise_Ada_PERSIST_STORE_Exception;

   -----------------------------------------
   -- C_Raise_Ada_BAD_INV_ORDER_Exception --
   -----------------------------------------

   procedure C_Raise_Ada_BAD_INV_ORDER_Exception
     (Pd_Minor  : in C.unsigned_long;
      Pd_Status : in C.int)
   is
      Ex_Member : CORBA.Bad_Inv_Order_Members;
   begin
      C_Raise_Ada_System_Exception
        (Pd_Minor,
         Pd_Status,
         CORBA.Bad_Inv_Order'Identity,
         Ex_Member);
   end C_Raise_Ada_BAD_INV_ORDER_Exception;

   -------------------------------------
   -- C_Raise_Ada_TRANSIENT_Exception --
   -------------------------------------

   procedure C_Raise_Ada_TRANSIENT_Exception
     (Pd_Minor  : in C.unsigned_long;
      Pd_Status : in C.int)
   is
      Ex_Member : CORBA.Transient_Members;
   begin
      C_Raise_Ada_System_Exception
        (Pd_Minor,
         Pd_Status,
         CORBA.Transient'Identity,
         Ex_Member);
   end C_Raise_Ada_TRANSIENT_Exception;

   ------------------------------------
   -- C_Raise_Ada_FREE_MEM_Exception --
   ------------------------------------

   procedure C_Raise_Ada_FREE_MEM_Exception
     (Pd_Minor  : in C.unsigned_long;
      Pd_Status : in C.int)
   is
      Ex_Member : CORBA.Free_Mem_Members;
   begin
      C_Raise_Ada_System_Exception
        (Pd_Minor,
         Pd_Status,
         CORBA.Free_Mem'Identity,
         Ex_Member);
   end C_Raise_Ada_FREE_MEM_Exception;

   -------------------------------------
   -- C_Raise_Ada_INV_IDENT_Exception --
   -------------------------------------

   procedure C_Raise_Ada_INV_IDENT_Exception
     (Pd_Minor  : in C.unsigned_long;
      Pd_Status : in C.int)
   is
      Ex_Member : CORBA.Inv_Ident_Members;
   begin
      C_Raise_Ada_System_Exception
        (Pd_Minor,
         Pd_Status,
         CORBA.Inv_Ident'Identity,
         Ex_Member);
   end C_Raise_Ada_INV_IDENT_Exception;

   ------------------------------------
   -- C_Raise_Ada_INV_FLAG_Exception --
   ------------------------------------

   procedure C_Raise_Ada_INV_FLAG_Exception
     (Pd_Minor  : in C.unsigned_long;
      Pd_Status : in C.int)
   is
      Ex_Member : CORBA.Inv_Flag_Members;
   begin
      C_Raise_Ada_System_Exception
        (Pd_Minor,
         Pd_Status,
         CORBA.Inv_Flag'Identity,
         Ex_Member);
   end C_Raise_Ada_INV_FLAG_Exception;

   --------------------------------------
   -- C_Raise_Ada_INTF_REPOS_Exception --
   --------------------------------------

   procedure C_Raise_Ada_INTF_REPOS_Exception
     (Pd_Minor  : in C.unsigned_long;
      Pd_Status : in C.int)
   is
      Ex_Member : CORBA.Intf_Repos_Members;
   begin
      C_Raise_Ada_System_Exception
        (Pd_Minor,
         Pd_Status,
         CORBA.Intf_Repos'Identity,
         Ex_Member);
   end C_Raise_Ada_INTF_REPOS_Exception;

   ---------------------------------------
   -- C_Raise_Ada_BAD_CONTEXT_Exception --
   ---------------------------------------

   procedure C_Raise_Ada_BAD_CONTEXT_Exception
     (Pd_Minor  : in C.unsigned_long;
      Pd_Status : in C.int)
   is
      Ex_Member : CORBA.Bad_Context_Members;
   begin
      C_Raise_Ada_System_Exception
        (Pd_Minor,
         Pd_Status,
         CORBA.Bad_Context'Identity,
         Ex_Member);
   end C_Raise_Ada_BAD_CONTEXT_Exception;

   ---------------------------------------
   -- C_Raise_Ada_OBJ_ADAPTER_Exception --
   ---------------------------------------

   procedure C_Raise_Ada_OBJ_ADAPTER_Exception
     (Pd_Minor  : in C.unsigned_long;
      Pd_Status : in C.int)
   is
      Ex_Member : CORBA.Obj_Adapter_Members;
   begin
      C_Raise_Ada_System_Exception
        (Pd_Minor,
         Pd_Status,
         CORBA.Obj_Adapter'Identity,
         Ex_Member);
   end C_Raise_Ada_OBJ_ADAPTER_Exception;

   -------------------------------------------
   -- C_Raise_Ada_DATA_CONVERSION_Exception --
   -------------------------------------------

   procedure C_Raise_Ada_DATA_CONVERSION_Exception
     (Pd_Minor  : in C.unsigned_long;
      Pd_Status : in C.int)
   is
      Ex_Member : CORBA.Data_Conversion_Members;
   begin
      C_Raise_Ada_System_Exception
        (Pd_Minor,
         Pd_Status,
         CORBA.Data_Conversion'Identity,
         Ex_Member);
   end C_Raise_Ada_DATA_CONVERSION_Exception;

   ------------------------------------------------
   -- C_Raise_Ada_TRANSACTION_REQUIRED_Exception --
   ------------------------------------------------

   procedure C_Raise_Ada_TRANSACTION_REQUIRED_Exception
     (Pd_Minor  : in C.unsigned_long;
      Pd_Status : in C.int)
   is
      Ex_Member : CORBA.Transaction_Required_Members;
   begin
      C_Raise_Ada_System_Exception
        (Pd_Minor,
         Pd_Status,
         CORBA.Transaction_Required'Identity,
         Ex_Member);
   end C_Raise_Ada_TRANSACTION_REQUIRED_Exception;

   --------------------------------------------------
   -- C_Raise_Ada_TRANSACTION_ROLLEDBACK_Exception --
   --------------------------------------------------

   procedure C_Raise_Ada_TRANSACTION_ROLLEDBACK_Exception
     (Pd_Minor  : in C.unsigned_long;
      Pd_Status : in C.int)
   is
      Ex_Member : CORBA.Transaction_Rolledback_Members;
   begin
      C_Raise_Ada_System_Exception
        (Pd_Minor,
         Pd_Status,
         CORBA.Transaction_Rolledback'Identity,
         Ex_Member);
   end C_Raise_Ada_TRANSACTION_ROLLEDBACK_Exception;

   -----------------------------------------------
   -- C_Raise_Ada_INVALID_TRANSACTION_Exception --
   -----------------------------------------------

   procedure C_Raise_Ada_INVALID_TRANSACTION_Exception
     (Pd_Minor  : in C.unsigned_long;
      Pd_Status : in C.int)
   is
      Ex_Member : CORBA.Invalid_Transaction_Members;
   begin
      C_Raise_Ada_System_Exception
        (Pd_Minor,
         Pd_Status,
         CORBA.Invalid_Transaction'Identity,
         Ex_Member);
   end C_Raise_Ada_INVALID_TRANSACTION_Exception;

   ---------------------------------------------
   -- C_Raise_Ada_WRONG_TRANSACTION_Exception --
   ---------------------------------------------

   procedure C_Raise_Ada_WRONG_TRANSACTION_Exception
     (Pd_Minor  : in C.unsigned_long;
      Pd_Status : in C.int)
   is
      Ex_Member : CORBA.Wrong_Transaction_Members;
   begin
      C_Raise_Ada_System_Exception
        (Pd_Minor,
         Pd_Status,
         CORBA.Wrong_Transaction'Identity,
         Ex_Member);
   end C_Raise_Ada_WRONG_TRANSACTION_Exception;

   ---------------------------------
   -- C_Raise_Ada_Fatal_Exception --
   ---------------------------------

   procedure C_Raise_Ada_Fatal_Exception
     (Pd_Minor  : in C.unsigned_long;
      Pd_Status : in C.int)
   is
      Ex_Member : CORBA.AdaBroker_Fatal_Error_Members;
   begin
      C_Raise_Ada_System_Exception
        (Pd_Minor,
         Pd_Status,
         CORBA.AdaBroker_Fatal_Error'Identity,
         Ex_Member);
   end C_Raise_Ada_Fatal_Exception;

   ----------------------
   -- Output_Ex_Member --
   ----------------------

   procedure Output_Ex_Member (Member : in IDL_Exception_Members_Ptr) is
   begin
      if Member.all in System_Exception_Members'Class then
         declare
            M : System_Exception_Members'Class
              := System_Exception_Members'Class (Member.all);
         begin
            pragma Debug
              (O ("exception member: minor  =" & M.Minor'Img));
            pragma Debug
              (O ("exception member: status = " & M.Completed'Img));
            null;
         end;
      else
         pragma Debug
           (O ("exception member not in System_Exception_Members"));
         null;
      end if;
   end Output_Ex_Member;

begin
   Occurrences.Init;
end AdaBroker.Exceptions;

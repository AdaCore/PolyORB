------------------------------------------------------------------------------
--                                                                          --
--                          ADABROKER COMPONENTS                            --
--                                                                          --
--                 A D A B R O K E R . E X C E P T I O N S                  --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.5 $
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

--  This package deals with the raising of C exceptions in Ada and ada ones
--  in C.  It is both a C and a Ada class (see Ada_Exceptions.hh) and
--  provides 2 mains methods : raise_C_Exception and
--  raise_Ada_Exception. The first one is called by Ada code and
--  implemented in C. The second is called by C code and implemented in
--  Ada. Both translate exceptions in the other language.

with Interfaces.C; use Interfaces.C;

package body AdaBroker.Exceptions is

   procedure C_Raise_Ada_CORBA_Exception
     (Pd_Minor    : in unsigned_long;
      Pd_Status   : in int;
      Ex_Identity : in Ada.Exceptions.Exception_Id;
      Ex_Member   : in out CORBA.Ex_Body'Class);

   function Int_To_Status
     (N : in int)
      return CORBA.Completion_Status;

   function Status_To_Int
     (Status : in CORBA.Completion_Status)
      return int;

   -------------------
   -- Int_To_Status --
   -------------------

   function Int_To_Status
     (N : in int)
      return CORBA.Completion_Status
   is
      Ada_N : Integer;
   begin
      Ada_N := Integer (N);
      return CORBA.Completion_Status'Val (Ada_N);
   end Int_To_Status;

   -------------------
   -- Status_To_Int --
   -------------------

   function Status_To_Int
     (Status : in CORBA.Completion_Status)
      return int
   is
      Ada_Result : Integer;
   begin
      Ada_Result := CORBA.Completion_Status'Pos (Status);
      return int (Ada_Result);
   end Status_To_Int;

   ---------------------------------
   -- C_Raise_Ada_Fatal_Exception --
   ---------------------------------

   procedure C_Raise_Ada_Fatal_Exception
     (File    : in Strings.chars_ptr;
      Line    : in int;
      Err_Msg : in Strings.chars_ptr)
   is
   begin
      Ada.Exceptions.Raise_Exception
        (CORBA.OmniORB_Fatal_Error'Identity,
         Strings.Value (File) &
         ":" & Line'Img &
         ": " & Strings.Value (Err_Msg));
   end C_Raise_Ada_Fatal_Exception;

   -----------------------------------
   -- C_Raise_Ada_CORBA_Exception --
   -----------------------------------

   procedure C_Raise_Ada_CORBA_Exception
     (Pd_Minor    : in unsigned_long;
      Pd_Status   : in int;
      Ex_Identity : in Ada.Exceptions.Exception_Id;
      Ex_Member   : in out CORBA.Ex_Body'Class)
   is
   begin
      Ex_Member.Minor     := CORBA.Unsigned_Long (Pd_Minor);
      Ex_Member.Completed := Int_To_Status (Pd_Status);

      CORBA.Raise_CORBA_Exception (Ex_Identity, Ex_Member);
   end C_Raise_Ada_CORBA_Exception;

   -----------------------------------
   -- C_Raise_Ada_UNKNOWN_Exception --
   -----------------------------------

   procedure C_Raise_Ada_UNKNOWN_Exception
     (Pd_Minor  : in unsigned_long;
      Pd_Status : in int)
   is
      Ex_Member : CORBA.Unknown_Members;
   begin
      C_Raise_Ada_CORBA_Exception
        (Pd_Minor,
         Pd_Status,
         CORBA.Unknown'Identity,
         Ex_Member);
   end C_Raise_Ada_UNKNOWN_Exception;

   -------------------------------------
   -- C_Raise_Ada_BAD_PARAM_Exception --
   -------------------------------------

   procedure C_Raise_Ada_BAD_PARAM_Exception
     (Pd_Minor  : in unsigned_long;
      Pd_Status : in int)
   is
      Ex_Member : CORBA.Bad_Param_Members;
   begin
      C_Raise_Ada_CORBA_Exception
        (Pd_Minor,
         Pd_Status,
         CORBA.Bad_Param'Identity,
         Ex_Member);
   end C_Raise_Ada_BAD_PARAM_Exception;

   -------------------------------------
   -- C_Raise_Ada_NO_MEMORY_Exception --
   -------------------------------------

   procedure C_Raise_Ada_NO_MEMORY_Exception
     (Pd_Minor  : in unsigned_long;
      Pd_Status : in int)
   is
      Ex_Member : CORBA.No_Memory_Members;
   begin
      C_Raise_Ada_CORBA_Exception
        (Pd_Minor,
         Pd_Status,
         CORBA.No_Memory'Identity,
         Ex_Member);
   end C_Raise_Ada_NO_MEMORY_Exception;

   -------------------------------------
   -- C_Raise_Ada_IMP_LIMIT_Exception --
   -------------------------------------

   procedure C_Raise_Ada_IMP_LIMIT_Exception
     (Pd_Minor  : in unsigned_long;
      Pd_Status : in int)
   is
      Ex_Member : CORBA.Imp_Limit_Members;
   begin
      C_Raise_Ada_CORBA_Exception
        (Pd_Minor,
         Pd_Status,
         CORBA.Imp_Limit'Identity,
         Ex_Member);
   end C_Raise_Ada_IMP_LIMIT_Exception;

   ----------------------------------------
   -- C_Raise_Ada_COMM_FAILURE_Exception --
   ----------------------------------------

   procedure C_Raise_Ada_COMM_FAILURE_Exception
     (Pd_Minor  : in unsigned_long;
      Pd_Status : in int)
   is
      Ex_Member : CORBA.Comm_Failure_Members;
   begin
      C_Raise_Ada_CORBA_Exception
        (Pd_Minor,
         Pd_Status,
         CORBA.Comm_Failure'Identity,
         Ex_Member);
   end C_Raise_Ada_COMM_FAILURE_Exception;

   --------------------------------------
   -- C_Raise_Ada_INV_OBJREF_Exception --
   --------------------------------------

   procedure C_Raise_Ada_INV_OBJREF_Exception
     (Pd_Minor  : in unsigned_long;
      Pd_Status : in int)
   is
      Ex_Member : CORBA.Inv_Objref_Members;
   begin
      C_Raise_Ada_CORBA_Exception
        (Pd_Minor,
         Pd_Status,
         CORBA.Inv_Objref'Identity,
         Ex_Member);
   end C_Raise_Ada_INV_OBJREF_Exception;

   --------------------------------------------
   -- C_Raise_Ada_OBJECT_NOT_EXIST_Exception --
   --------------------------------------------

   procedure C_Raise_Ada_OBJECT_NOT_EXIST_Exception
     (Pd_Minor  : in unsigned_long;
      Pd_Status : in int)
   is
      Ex_Member : CORBA.Object_Not_Exist_Members;
   begin
      C_Raise_Ada_CORBA_Exception
        (Pd_Minor,
         Pd_Status,
         CORBA.Object_Not_Exist'Identity,
         Ex_Member);
   end C_Raise_Ada_OBJECT_NOT_EXIST_Exception;

   -----------------------------------------
   -- C_Raise_Ada_NO_PERMISSION_Exception --
   -----------------------------------------

   procedure C_Raise_Ada_NO_PERMISSION_Exception
     (Pd_Minor  : in unsigned_long;
      Pd_Status : in int)
   is
      Ex_Member : CORBA.No_Permission_Members;
   begin
      C_Raise_Ada_CORBA_Exception
        (Pd_Minor,
         Pd_Status,
         CORBA.No_Permission'Identity,
         Ex_Member);
   end C_Raise_Ada_NO_PERMISSION_Exception;

   ------------------------------------
   -- C_Raise_Ada_INTERNAL_Exception --
   ------------------------------------

   procedure C_Raise_Ada_INTERNAL_Exception
     (Pd_Minor  : in unsigned_long;
      Pd_Status : in int)
   is
      Ex_Member : CORBA.Internal_Members;
   begin
      C_Raise_Ada_CORBA_Exception
        (Pd_Minor,
         Pd_Status,
         CORBA.Internal'Identity,
         Ex_Member);
   end C_Raise_Ada_INTERNAL_Exception;

   -----------------------------------
   -- C_Raise_Ada_MARSHAL_Exception --
   -----------------------------------

   procedure C_Raise_Ada_MARSHAL_Exception
     (Pd_Minor  : in unsigned_long;
      Pd_Status : in int)
   is
      Ex_Member : CORBA.Marshal_Members;
   begin
      C_Raise_Ada_CORBA_Exception
        (Pd_Minor,
         Pd_Status,
         CORBA.Marshal'Identity,
         Ex_Member);
   end C_Raise_Ada_MARSHAL_Exception;

   ---------------------------------------------------
   --  C_Raise_Ada_INITIALIZATION_FAILURE_Exception --
   ---------------------------------------------------

   procedure C_Raise_Ada_INITIALIZATION_FAILURE_Exception
     (Pd_Minor  : in unsigned_long;
      Pd_Status : in int)
   is
      Ex_Member : CORBA.Initialization_Failure_Members;
   begin
      C_Raise_Ada_CORBA_Exception
        (Pd_Minor,
         Pd_Status,
         CORBA.Initialization_Failure'Identity,
         Ex_Member);
   end C_Raise_Ada_INITIALIZATION_FAILURE_Exception;

   ----------------------------------------
   -- C_Raise_Ada_NO_IMPLEMENT_Exception --
   ----------------------------------------

   procedure C_Raise_Ada_NO_IMPLEMENT_Exception
     (Pd_Minor  : in unsigned_long;
      Pd_Status : in int)
   is
      Ex_Member : CORBA.No_Implement_Members;
   begin
      C_Raise_Ada_CORBA_Exception
        (Pd_Minor,
         Pd_Status,
         CORBA.No_Implement'Identity,
         Ex_Member);
   end C_Raise_Ada_NO_IMPLEMENT_Exception;

   ----------------------------------------
   -- C_Raise_Ada_BAD_TYPECODE_Exception --
   ----------------------------------------

   procedure C_Raise_Ada_BAD_TYPECODE_Exception
     (Pd_Minor  : in unsigned_long;
      Pd_Status : in int)
   is
      Ex_Member : CORBA.Bad_Typecode_Members;
   begin
      C_Raise_Ada_CORBA_Exception
        (Pd_Minor,
         Pd_Status,
         CORBA.Bad_Typecode'Identity,
         Ex_Member);
   end C_Raise_Ada_BAD_TYPECODE_Exception;

   -----------------------------------------
   -- C_Raise_Ada_BAD_OPERATION_Exception --
   -----------------------------------------

   procedure C_Raise_Ada_BAD_OPERATION_Exception
     (Pd_Minor  : in unsigned_long;
      Pd_Status : in int)
   is
      Ex_Member : CORBA.Bad_Operation_Members;
   begin
      C_Raise_Ada_CORBA_Exception
        (Pd_Minor,
         Pd_Status,
         CORBA.Bad_Operation'Identity,
         Ex_Member);
   end C_Raise_Ada_BAD_OPERATION_Exception;

   ----------------------------------------
   -- C_Raise_Ada_NO_RESOURCES_Exception --
   ----------------------------------------

   procedure C_Raise_Ada_NO_RESOURCES_Exception
     (Pd_Minor  : in unsigned_long;
      Pd_Status : in int)
   is
      Ex_Member : CORBA.No_Resources_Members;
   begin
      C_Raise_Ada_CORBA_Exception
        (Pd_Minor,
         Pd_Status,
         CORBA.No_Resources'Identity,
         Ex_Member);
   end C_Raise_Ada_NO_RESOURCES_Exception;

   ---------------------------------------
   -- C_Raise_Ada_NO_RESPONSE_Exception --
   ---------------------------------------

   procedure C_Raise_Ada_NO_RESPONSE_Exception
     (Pd_Minor  : in unsigned_long;
      Pd_Status : in int)
   is
      Ex_Member : CORBA.No_Response_Members;
   begin
      C_Raise_Ada_CORBA_Exception
        (Pd_Minor,
         Pd_Status,
         CORBA.No_Response'Identity,
         Ex_Member);
   end C_Raise_Ada_NO_RESPONSE_Exception;

   -----------------------------------------
   -- C_Raise_Ada_PERSIST_STORE_Exception --
   -----------------------------------------

   procedure C_Raise_Ada_PERSIST_STORE_Exception
     (Pd_Minor  : in unsigned_long;
      Pd_Status : in int)
   is
      Ex_Member : CORBA.Persist_Store_Members;
   begin
      C_Raise_Ada_CORBA_Exception
        (Pd_Minor,
         Pd_Status,
         CORBA.Persist_Store'Identity,
         Ex_Member);
   end C_Raise_Ada_PERSIST_STORE_Exception;

   -----------------------------------------
   -- C_Raise_Ada_BAD_INV_ORDER_Exception --
   -----------------------------------------

   procedure C_Raise_Ada_BAD_INV_ORDER_Exception
     (Pd_Minor  : in unsigned_long;
      Pd_Status : in int)
   is
      Ex_Member : CORBA.Bad_Inv_Order_Members;
   begin
      C_Raise_Ada_CORBA_Exception
        (Pd_Minor,
         Pd_Status,
         CORBA.Bad_Inv_Order'Identity,
         Ex_Member);
   end C_Raise_Ada_BAD_INV_ORDER_Exception;

   -------------------------------------
   -- C_Raise_Ada_TRANSIENT_Exception --
   -------------------------------------

   procedure C_Raise_Ada_TRANSIENT_Exception
     (Pd_Minor  : in unsigned_long;
      Pd_Status : in int)
   is
      Ex_Member : CORBA.Transient_Members;
   begin
      C_Raise_Ada_CORBA_Exception
        (Pd_Minor,
         Pd_Status,
         CORBA.Transient'Identity,
         Ex_Member);
   end C_Raise_Ada_TRANSIENT_Exception;

   ------------------------------------
   -- C_Raise_Ada_FREE_MEM_Exception --
   ------------------------------------

   procedure C_Raise_Ada_FREE_MEM_Exception
     (Pd_Minor  : in unsigned_long;
      Pd_Status : in int)
   is
      Ex_Member : CORBA.Free_Mem_Members;
   begin
      C_Raise_Ada_CORBA_Exception
        (Pd_Minor,
         Pd_Status,
         CORBA.Free_Mem'Identity,
         Ex_Member);
   end C_Raise_Ada_FREE_MEM_Exception;

   -------------------------------------
   -- C_Raise_Ada_INV_IDENT_Exception --
   -------------------------------------

   procedure C_Raise_Ada_INV_IDENT_Exception
     (Pd_Minor  : in unsigned_long;
      Pd_Status : in int)
   is
      Ex_Member : CORBA.Inv_Ident_Members;
   begin
      C_Raise_Ada_CORBA_Exception
        (Pd_Minor,
         Pd_Status,
         CORBA.Inv_Ident'Identity,
         Ex_Member);
   end C_Raise_Ada_INV_IDENT_Exception;

   ------------------------------------
   -- C_Raise_Ada_INV_FLAG_Exception --
   ------------------------------------

   procedure C_Raise_Ada_INV_FLAG_Exception
     (Pd_Minor  : in unsigned_long;
      Pd_Status : in int)
   is
      Ex_Member : CORBA.Inv_Flag_Members;
   begin
      C_Raise_Ada_CORBA_Exception
        (Pd_Minor,
         Pd_Status,
         CORBA.Inv_Flag'Identity,
         Ex_Member);
   end C_Raise_Ada_INV_FLAG_Exception;

   --------------------------------------
   -- C_Raise_Ada_INTF_REPOS_Exception --
   --------------------------------------

   procedure C_Raise_Ada_INTF_REPOS_Exception
     (Pd_Minor  : in unsigned_long;
      Pd_Status : in int)
   is
      Ex_Member : CORBA.Intf_Repos_Members;
   begin
      C_Raise_Ada_CORBA_Exception
        (Pd_Minor,
         Pd_Status,
         CORBA.Intf_Repos'Identity,
         Ex_Member);
   end C_Raise_Ada_INTF_REPOS_Exception;

   ---------------------------------------
   -- C_Raise_Ada_BAD_CONTEXT_Exception --
   ---------------------------------------

   procedure C_Raise_Ada_BAD_CONTEXT_Exception
     (Pd_Minor  : in unsigned_long;
      Pd_Status : in int)
   is
      Ex_Member : CORBA.Bad_Context_Members;
   begin
      C_Raise_Ada_CORBA_Exception
        (Pd_Minor,
         Pd_Status,
         CORBA.Bad_Context'Identity,
         Ex_Member);
   end C_Raise_Ada_BAD_CONTEXT_Exception;

   ---------------------------------------
   -- C_Raise_Ada_OBJ_ADAPTER_Exception --
   ---------------------------------------

   procedure C_Raise_Ada_OBJ_ADAPTER_Exception
     (Pd_Minor  : in unsigned_long;
      Pd_Status : in int)
   is
      Ex_Member : CORBA.Obj_Adapter_Members;
   begin
      C_Raise_Ada_CORBA_Exception
        (Pd_Minor,
         Pd_Status,
         CORBA.Obj_Adapter'Identity,
         Ex_Member);
   end C_Raise_Ada_OBJ_ADAPTER_Exception;

   -------------------------------------------
   -- C_Raise_Ada_DATA_CONVERSION_Exception --
   -------------------------------------------

   procedure C_Raise_Ada_DATA_CONVERSION_Exception
     (Pd_Minor  : in unsigned_long;
      Pd_Status : in int)
   is
      Ex_Member : CORBA.Data_Conversion_Members;
   begin
      C_Raise_Ada_CORBA_Exception
        (Pd_Minor,
         Pd_Status,
         CORBA.Data_Conversion'Identity,
         Ex_Member);
   end C_Raise_Ada_DATA_CONVERSION_Exception;

   ------------------------------------------------
   -- C_Raise_Ada_TRANSACTION_REQUIRED_Exception --
   ------------------------------------------------

   procedure C_Raise_Ada_TRANSACTION_REQUIRED_Exception
     (Pd_Minor  : in unsigned_long;
      Pd_Status : in int)
   is
      Ex_Member : CORBA.Transaction_Required_Members;
   begin
      C_Raise_Ada_CORBA_Exception
        (Pd_Minor,
         Pd_Status,
         CORBA.Transaction_Required'Identity,
         Ex_Member);
   end C_Raise_Ada_TRANSACTION_REQUIRED_Exception;

   --------------------------------------------------
   -- C_Raise_Ada_TRANSACTION_ROLLEDBACK_Exception --
   --------------------------------------------------

   procedure C_Raise_Ada_TRANSACTION_ROLLEDBACK_Exception
     (Pd_Minor  : in unsigned_long;
      Pd_Status : in int)
   is
      Ex_Member : CORBA.Transaction_Rolledback_Members;
   begin
      C_Raise_Ada_CORBA_Exception
        (Pd_Minor,
         Pd_Status,
         CORBA.Transaction_Rolledback'Identity,
         Ex_Member);
   end C_Raise_Ada_TRANSACTION_ROLLEDBACK_Exception;

   -----------------------------------------------
   -- C_Raise_Ada_INVALID_TRANSACTION_Exception --
   -----------------------------------------------

   procedure C_Raise_Ada_INVALID_TRANSACTION_Exception
     (Pd_Minor  : in unsigned_long;
      Pd_Status : in int)
   is
      Ex_Member : CORBA.Invalid_Transaction_Members;
   begin
      C_Raise_Ada_CORBA_Exception
        (Pd_Minor,
         Pd_Status,
         CORBA.Invalid_Transaction'Identity,
         Ex_Member);
   end C_Raise_Ada_INVALID_TRANSACTION_Exception;

   ---------------------------------------------
   -- C_Raise_Ada_WRONG_TRANSACTION_Exception --
   ---------------------------------------------

   procedure C_Raise_Ada_WRONG_TRANSACTION_Exception
     (Pd_Minor  : in unsigned_long;
      Pd_Status : in int)
   is
      Ex_Member : CORBA.Wrong_Transaction_Members;
   begin
      C_Raise_Ada_CORBA_Exception
        (Pd_Minor,
         Pd_Status,
         CORBA.Wrong_Transaction'Identity,
         Ex_Member);
   end C_Raise_Ada_WRONG_TRANSACTION_Exception;

   ---------------------------------
   -- C_Raise_Ada_Fatal_Exception --
   ---------------------------------

   procedure C_Raise_Ada_Fatal_Exception
     (Pd_Minor  : in unsigned_long;
      Pd_Status : in int)
   is
      Ex_Member : CORBA.Wrong_Transaction_Members;
   begin
      C_Raise_Ada_CORBA_Exception
        (Pd_Minor,
         Pd_Status,
         CORBA.AdaBroker_Fatal_Error'Identity,
         Ex_Member);
   end C_Raise_Ada_Fatal_Exception;

end AdaBroker.Exceptions;

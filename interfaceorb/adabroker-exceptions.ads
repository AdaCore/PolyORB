------------------------------------------------------------------------------
--                                                                          --
--                          ADABROKER COMPONENTS                            --
--                                                                          --
--                 A D A B R O K E R . E X C E P T I O N S                  --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                            $Revision: 1.6 $
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

--  This unit deals with the raising of C exceptions in Ada and ada ones
--  in C.  It is both a C and a Ada class (see Ada_Exceptions.hh) and
--  provides 2 mains methods : raise_C_Exception and
--  raise_Ada_Exception. The first one is called by Ada code and
--  implemented in C. The second is called by C code and implemented in
--  Ada. Both translate exceptions in the other language.

with Ada.Exceptions;
with Ada.Unchecked_Deallocation;

with Interfaces;   use Interfaces;
with Interfaces.C;
with Interfaces.C.Strings;

with CORBA;        use CORBA;

package AdaBroker.Exceptions is

   To_Status : constant array (C.int range 0 .. 2) of Completion_Status
     := (0 => Completed_Yes,
         1 => Completed_No,
         2 => Completed_Maybe);

   To_Int    : constant array (Completion_Status) of C.int
     := (Completed_Yes   => 0,
         Completed_No    => 1,
         Completed_Maybe => 2);

   type IDL_Exception_Members_Ptr is access all IDL_Exception_Members'Class;

   procedure Raise_CORBA_Exception
     (Ex_Id     : in Ada.Exceptions.Exception_Id;
      Ex_Member : in IDL_Exception_Members'Class);

   --     procedure Get_Members
   --       (From : in Ada.Exceptions.Exception_Occurrence;
   --        To   : out IDL_Exception_Members'Class);

   function Get_Members
     (From : in Ada.Exceptions.Exception_Occurrence)
     return IDL_Exception_Members'Class;

   procedure Free is
     new Ada.Unchecked_Deallocation
     (IDL_Exception_Members'Class,
      IDL_Exception_Members_Ptr);

   procedure C_Raise_Ada_Fatal_Exception
     (File    : in C.Strings.chars_ptr;
      Line    : in C.int;
      Err_Msg : in C.Strings.chars_ptr);
   pragma Export
     (Cpp, C_Raise_Ada_Fatal_Exception,
      "Raise_Ada_Fatal_Exception__FPCciT0");

   procedure C_Raise_Ada_UNKNOWN_Exception
     (Pd_Minor  : in C.unsigned_long;
      Pd_Status : in C.int);
   pragma Export
     (Cpp, C_Raise_Ada_UNKNOWN_Exception,
      "Raise_Ada_UNKNOWN_Exception__FUlQ25CORBA16CompletionStatus");

   procedure C_Raise_Ada_BAD_PARAM_Exception
     (Pd_Minor  : in C.unsigned_long;
      Pd_Status : in C.int);
   pragma Export
     (Cpp, C_Raise_Ada_BAD_PARAM_Exception,
      "Raise_Ada_BAD_PARAM_Exception__FUlQ25CORBA16CompletionStatus");

   procedure C_Raise_Ada_NO_MEMORY_Exception
     (Pd_Minor  : in C.unsigned_long;
      Pd_Status : in C.int);
   pragma Export
     (Cpp, C_Raise_Ada_NO_MEMORY_Exception,
      "Raise_Ada_NO_MEMORY_Exception__FUlQ25CORBA16CompletionStatus");

   procedure C_Raise_Ada_IMP_LIMIT_Exception
     (Pd_Minor  : in C.unsigned_long;
      Pd_Status : in C.int);
   pragma Export
     (Cpp, C_Raise_Ada_IMP_LIMIT_Exception,
      "Raise_Ada_IMP_LIMIT_Exception__FUlQ25CORBA16CompletionStatus");

   procedure C_Raise_Ada_COMM_FAILURE_Exception
     (Pd_Minor  : in C.unsigned_long;
      Pd_Status : in C.int);
   pragma Export
     (Cpp, C_Raise_Ada_COMM_FAILURE_Exception,
      "Raise_Ada_COMM_FAILURE_Exception__FUlQ25CORBA16CompletionStatus");

   procedure C_Raise_Ada_INV_OBJREF_Exception
     (Pd_Minor  : in C.unsigned_long;
      Pd_Status : in C.int);
   pragma Export
     (Cpp, C_Raise_Ada_INV_OBJREF_Exception,
      "Raise_Ada_INV_OBJREF_Exception__FUlQ25CORBA16CompletionStatus");

   procedure C_Raise_Ada_OBJECT_NOT_EXIST_Exception
     (Pd_Minor  : in C.unsigned_long;
      Pd_Status : in C.int);
   pragma Export
     (Cpp, C_Raise_Ada_OBJECT_NOT_EXIST_Exception,
      "Raise_Ada_OBJECT_NOT_EXIST_Exception__FUlQ25CORBA16CompletionStatus");

   procedure C_Raise_Ada_NO_PERMISSION_Exception
     (Pd_Minor  : in C.unsigned_long;
      Pd_Status : in C.int);
   pragma Export
     (Cpp, C_Raise_Ada_NO_PERMISSION_Exception,
      "Raise_Ada_NO_PERMISSION_Exception__FUlQ25CORBA16CompletionStatus");

   procedure C_Raise_Ada_INTERNAL_Exception
     (Pd_Minor  : in C.unsigned_long;
      Pd_Status : in C.int);
   pragma Export
     (Cpp, C_Raise_Ada_INTERNAL_Exception,
      "Raise_Ada_INTERNAL_Exception__FUlQ25CORBA16CompletionStatus");

   procedure C_Raise_Ada_MARSHAL_Exception
     (Pd_Minor  : in C.unsigned_long;
      Pd_Status : in C.int);
   pragma Export
     (Cpp, C_Raise_Ada_MARSHAL_Exception,
      "Raise_Ada_MARSHAL_Exception__FUlQ25CORBA16CompletionStatus");

   procedure C_Raise_Ada_INITIALIZATION_FAILURE_Exception
     (Pd_Minor  : in C.unsigned_long;
      Pd_Status : in C.int);
   pragma Export
     (Cpp, C_Raise_Ada_INITIALIZATION_FAILURE_Exception,
      "Raise_Ada_INITIALIZE_Exception__FUlQ25CORBA16CompletionStatus");

   procedure C_Raise_Ada_NO_IMPLEMENT_Exception
     (Pd_Minor  : in C.unsigned_long;
      Pd_Status : in C.int);
   pragma Export
     (Cpp, C_Raise_Ada_NO_IMPLEMENT_Exception,
      "Raise_Ada_NO_IMPLEMENT_Exception__FUlQ25CORBA16CompletionStatus");

   procedure C_Raise_Ada_BAD_TYPECODE_Exception
     (Pd_Minor  : in C.unsigned_long;
      Pd_Status : in C.int);
   pragma Export
     (Cpp, C_Raise_Ada_BAD_TYPECODE_Exception,
      "Raise_Ada_BAD_TYPECODE_Exception__FUlQ25CORBA16CompletionStatus");

   procedure C_Raise_Ada_BAD_OPERATION_Exception
     (Pd_Minor  : in C.unsigned_long;
      Pd_Status : in C.int);
   pragma Export
     (Cpp, C_Raise_Ada_BAD_OPERATION_Exception,
      "Raise_Ada_BAD_OPERATION_Exception__FUlQ25CORBA16CompletionStatus");

   procedure C_Raise_Ada_NO_RESOURCES_Exception
     (Pd_Minor  : in C.unsigned_long;
      Pd_Status : in C.int);
   pragma Export
     (Cpp, C_Raise_Ada_NO_RESOURCES_Exception,
      "Raise_Ada_NO_RESOURCES_Exception__FUlQ25CORBA16CompletionStatus");

   procedure C_Raise_Ada_NO_RESPONSE_Exception
     (Pd_Minor  : in C.unsigned_long;
      Pd_Status : in C.int);
   pragma Export
     (Cpp, C_Raise_Ada_NO_RESPONSE_Exception,
      "Raise_Ada_NO_RESPONSE_Exception__FUlQ25CORBA16CompletionStatus");

   procedure C_Raise_Ada_PERSIST_STORE_Exception
     (Pd_Minor  : in C.unsigned_long;
      Pd_Status : in C.int);
   pragma Export
     (Cpp, C_Raise_Ada_PERSIST_STORE_Exception,
      "Raise_Ada_PERSIST_STORE_Exception__FUlQ25CORBA16CompletionStatus");

   procedure C_Raise_Ada_BAD_INV_ORDER_Exception
     (Pd_Minor  : in C.unsigned_long;
      Pd_Status : in C.int);
   pragma Export
     (Cpp, C_Raise_Ada_BAD_INV_ORDER_Exception,
      "Raise_Ada_BAD_INV_ORDER_Exception__FUlQ25CORBA16CompletionStatus");

   procedure C_Raise_Ada_TRANSIENT_Exception
     (Pd_Minor  : in C.unsigned_long;
      Pd_Status : in C.int);
   pragma Export
     (Cpp, C_Raise_Ada_TRANSIENT_Exception,
      "Raise_Ada_TRANSIENT_Exception__FUlQ25CORBA16CompletionStatus");

   procedure C_Raise_Ada_FREE_MEM_Exception
     (Pd_Minor  : in C.unsigned_long;
      Pd_Status : in C.int);
   pragma Export
     (Cpp, C_Raise_Ada_FREE_MEM_Exception,
      "Raise_Ada_FREE_MEM_Exception__FUlQ25CORBA16CompletionStatus");

   procedure C_Raise_Ada_INV_IDENT_Exception
     (Pd_Minor  : in C.unsigned_long;
      Pd_Status : in C.int);
   pragma Export
     (Cpp, C_Raise_Ada_INV_IDENT_Exception,
      "Raise_Ada_INV_IDENT_Exception__FUlQ25CORBA16CompletionStatus");

   procedure C_Raise_Ada_INV_FLAG_Exception
     (Pd_Minor  : in C.unsigned_long;
      Pd_Status : in C.int);
   pragma Export
     (Cpp, C_Raise_Ada_INV_FLAG_Exception,
      "Raise_Ada_INV_FLAG_Exception__FUlQ25CORBA16CompletionStatus");

   procedure C_Raise_Ada_INTF_REPOS_Exception
     (Pd_Minor  : in C.unsigned_long;
      Pd_Status : in C.int);
   pragma Export
     (Cpp, C_Raise_Ada_INTF_REPOS_Exception,
      "Raise_Ada_INTF_REPOS_Exception__FUlQ25CORBA16CompletionStatus");

   procedure C_Raise_Ada_BAD_CONTEXT_Exception
     (Pd_Minor  : in C.unsigned_long;
      Pd_Status : in C.int);
   pragma Export
     (Cpp, C_Raise_Ada_BAD_CONTEXT_Exception,
      "Raise_Ada_BAD_CONTEXT_Exception__FUlQ25CORBA16CompletionStatus");

   procedure C_Raise_Ada_OBJ_ADAPTER_Exception
     (Pd_Minor  : in C.unsigned_long;
      Pd_Status : in C.int);
   pragma Export
     (Cpp, C_Raise_Ada_OBJ_ADAPTER_Exception,
      "Raise_Ada_OBJ_ADAPTER_Exception__FUlQ25CORBA16CompletionStatus");

   procedure C_Raise_Ada_DATA_CONVERSION_Exception
     (Pd_Minor  : in C.unsigned_long;
      Pd_Status : in C.int);
   pragma Export
     (Cpp, C_Raise_Ada_DATA_CONVERSION_Exception,
      "Raise_Ada_DATA_CONVERSION_Exception__FUlQ25CORBA16CompletionStatus");

   procedure C_Raise_Ada_TRANSACTION_REQUIRED_Exception
     (Pd_Minor  : in C.unsigned_long;
      Pd_Status : in C.int);
   pragma Export
     (Cpp, C_Raise_Ada_TRANSACTION_REQUIRED_Exception,
      "Raise_Ada_TRANSACTION_REQUIRED_Exception_" &
      "_FUlQ25CORBA16CompletionStatus");

   procedure C_Raise_Ada_TRANSACTION_ROLLEDBACK_Exception
     (Pd_Minor  : in C.unsigned_long;
      Pd_Status : in C.int);
   pragma Export
     (Cpp, C_Raise_Ada_TRANSACTION_ROLLEDBACK_Exception,
      "Raise_Ada_TRANSACTION_ROLLEDBACK_Exception_"  &
      "_FUlQ25CORBA16CompletionStatus");

   procedure C_Raise_Ada_INVALID_TRANSACTION_Exception
     (Pd_Minor  : in C.unsigned_long;
      Pd_Status : in C.int);
   pragma Export
     (Cpp, C_Raise_Ada_INVALID_TRANSACTION_Exception,
      "Raise_Ada_INVALID_TRANSACTION_Exception_" &
      "_FUlQ25CORBA16CompletionStatus");

   procedure C_Raise_Ada_WRONG_TRANSACTION_Exception
     (Pd_Minor : in C.unsigned_long;
      Pd_Status : in C.int);
   pragma Export
     (Cpp, C_Raise_Ada_WRONG_TRANSACTION_Exception,
      "Raise_Ada_WRONG_TRANSACTION_Exception__FUlQ25CORBA16CompletionStatus");

   procedure C_Raise_Ada_Fatal_Exception
     (Pd_Minor  : in C.unsigned_long;
      Pd_Status : in C.int);
   pragma Export
     (Cpp, C_Raise_Ada_Fatal_Exception,
      "Raise_Ada_Fatal_Exception__FUlQ25CORBA16CompletionStatus");

end AdaBroker.Exceptions;

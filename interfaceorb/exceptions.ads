
--  This package deals with the raising of C exceptions in Ada and ada ones
--  in C.  It is both a C and a Ada class (see Ada_Exceptions.hh) and
--  provides 2 mains methods : raise_C_Exception and
--  raise_Ada_Exception. The first one is called by Ada code and
--  implemented in C. The second is called by C code and implemented in
--  Ada. Both translate exceptions in the other language.

with Ada.Exceptions;

with Interfaces.C;
with Interfaces.C.Strings;

with CORBA;

with Adabroker_Debug;
pragma Elaborate (Adabroker_Debug);

package Exceptions is

   procedure C_Raise_Ada_Fatal_Exception
     (File    : in Interfaces.C.Strings.chars_ptr;
      Line    : in Interfaces.C.int;
      Err_Msg : in Interfaces.C.Strings.chars_ptr);
   pragma Export
     (Cpp, C_Raise_Ada_Fatal_Exception,
      "Raise_Ada_FatalException__FPCciT0");

   procedure C_Raise_Ada_UNKNOWN_Exception
     (Pd_Minor  : in Interfaces.C.unsigned_long;
      Pd_Status : in Interfaces.C.int);
   pragma Export
     (Cpp, C_Raise_Ada_UNKNOWN_Exception,
      "Raise_Ada_UNKNOWN_Exception__FUlQ25CORBA16CompletionStatus");
   --  Wrapped around C function Raise_Ada_UNKNOWN_Exception declared in
   --  Ada_exceptions.hh Called by C code.  Handles in Ada a Corba
   --  exception that was raised in C.


   procedure C_Raise_Ada_BAD_PARAM_Exception
     (Pd_Minor  : in Interfaces.C.unsigned_long;
      Pd_Status : in Interfaces.C.int);
   pragma Export
     (Cpp, C_Raise_Ada_BAD_PARAM_Exception,
      "Raise_Ada_BAD_PARAM_Exception__FUlQ25CORBA16CompletionStatus");

   procedure C_Raise_Ada_NO_MEMORY_Exception
     (Pd_Minor  : in Interfaces.C.unsigned_long;
      Pd_Status : in Interfaces.C.int);
   pragma Export
     (Cpp, C_Raise_Ada_NO_MEMORY_Exception,
      "Raise_Ada_NO_MEMORY_Exception__FUlQ25CORBA16CompletionStatus");

   procedure C_Raise_Ada_IMP_LIMIT_Exception
     (Pd_Minor  : in Interfaces.C.unsigned_long;
      Pd_Status : in Interfaces.C.int);
   pragma Export
     (Cpp, C_Raise_Ada_IMP_LIMIT_Exception,
      "Raise_Ada_IMP_LIMIT_Exception__FUlQ25CORBA16CompletionStatus");

   procedure C_Raise_Ada_COMM_FAILURE_Exception
     (Pd_Minor  : in Interfaces.C.unsigned_long;
      Pd_Status : in Interfaces.C.int);
   pragma Export
     (Cpp, C_Raise_Ada_COMM_FAILURE_Exception,
      "Raise_Ada_COMM_FAILURE_Exception__FUlQ25CORBA16CompletionStatus");

   procedure C_Raise_Ada_INV_OBJREF_Exception
     (Pd_Minor  : in Interfaces.C.unsigned_long;
      Pd_Status : in Interfaces.C.int);
   pragma Export
     (Cpp, C_Raise_Ada_INV_OBJREF_Exception,
      "Raise_Ada_INV_OBJREF_Exception__FUlQ25CORBA16CompletionStatus");

   procedure C_Raise_Ada_OBJECT_NOT_EXIST_Exception
     (Pd_Minor  : in Interfaces.C.unsigned_long;
      Pd_Status : in Interfaces.C.int);
   pragma Export
     (Cpp, C_Raise_Ada_OBJECT_NOT_EXIST_Exception,
      "Raise_Ada_OBJECT_NOT_EXIST_Exception__FUlQ25CORBA16CompletionStatus");

   procedure C_Raise_Ada_NO_PERMISSION_Exception
     (Pd_Minor  : in Interfaces.C.unsigned_long;
      Pd_Status : in Interfaces.C.int);
   pragma Export
     (Cpp, C_Raise_Ada_NO_PERMISSION_Exception,
      "Raise_Ada_NO_PERMISSION_Exception__FUlQ25CORBA16CompletionStatus");

   procedure C_Raise_Ada_INTERNAL_Exception
     (Pd_Minor  : in Interfaces.C.unsigned_long;
      Pd_Status : in Interfaces.C.int);
   pragma Export
     (Cpp, C_Raise_Ada_INTERNAL_Exception,
      "Raise_Ada_INTERNAL_Exception__FUlQ25CORBA16CompletionStatus");

   procedure C_Raise_Ada_MARSHAL_Exception
     (Pd_Minor  : in Interfaces.C.unsigned_long;
      Pd_Status : in Interfaces.C.int);
   pragma Export
     (Cpp, C_Raise_Ada_MARSHAL_Exception,
      "Raise_Ada_MARSHAL_Exception__FUlQ25CORBA16CompletionStatus");

   procedure C_Raise_Ada_INITIALIZATION_FAILURE_Exception
     (Pd_Minor  : in Interfaces.C.unsigned_long;
      Pd_Status : in Interfaces.C.int);
   pragma Export
     (Cpp, C_Raise_Ada_INITIALIZATION_FAILURE_Exception,
      "Raise_Ada_INITIALIZE_Exception__FUlQ25CORBA16CompletionStatus");

   procedure C_Raise_Ada_NO_IMPLEMENT_Exception
     (Pd_Minor  : in Interfaces.C.unsigned_long;
      Pd_Status : in Interfaces.C.int);
   pragma Export
     (Cpp, C_Raise_Ada_NO_IMPLEMENT_Exception,
      "Raise_Ada_NO_IMPLEMENT_Exception__FUlQ25CORBA16CompletionStatus");

   procedure C_Raise_Ada_BAD_TYPECODE_Exception
     (Pd_Minor  : in Interfaces.C.unsigned_long;
      Pd_Status : in Interfaces.C.int);
   pragma Export
     (Cpp, C_Raise_Ada_BAD_TYPECODE_Exception,
      "Raise_Ada_BAD_TYPECODE_Exception__FUlQ25CORBA16CompletionStatus");

   procedure C_Raise_Ada_BAD_OPERATION_Exception
     (Pd_Minor  : in Interfaces.C.unsigned_long;
      Pd_Status : in Interfaces.C.int);
   pragma Export
     (Cpp, C_Raise_Ada_BAD_OPERATION_Exception,
      "Raise_Ada_BAD_OPERATION_Exception__FUlQ25CORBA16CompletionStatus");

   procedure C_Raise_Ada_NO_RESOURCES_Exception
     (Pd_Minor  : in Interfaces.C.unsigned_long;
      Pd_Status : in Interfaces.C.int);
   pragma Export
     (Cpp, C_Raise_Ada_NO_RESOURCES_Exception,
      "Raise_Ada_NO_RESOURCES_Exception__FUlQ25CORBA16CompletionStatus");

   procedure C_Raise_Ada_NO_RESPONSE_Exception
     (Pd_Minor  : in Interfaces.C.unsigned_long;
      Pd_Status : in Interfaces.C.int);
   pragma Export
     (Cpp, C_Raise_Ada_NO_RESPONSE_Exception,
      "Raise_Ada_NO_RESPONSE_Exception__FUlQ25CORBA16CompletionStatus");

   procedure C_Raise_Ada_PERSIST_STORE_Exception
     (Pd_Minor  : in Interfaces.C.unsigned_long;
      Pd_Status : in Interfaces.C.int);
   pragma Export
     (Cpp, C_Raise_Ada_PERSIST_STORE_Exception,
      "Raise_Ada_PERSIST_STORE_Exception__FUlQ25CORBA16CompletionStatus");

   procedure C_Raise_Ada_BAD_INV_ORDER_Exception
     (Pd_Minor  : in Interfaces.C.unsigned_long;
      Pd_Status : in Interfaces.C.int);
   pragma Export
     (Cpp, C_Raise_Ada_BAD_INV_ORDER_Exception,
      "Raise_Ada_BAD_INV_ORDER_Exception__FUlQ25CORBA16CompletionStatus");

   procedure C_Raise_Ada_TRANSIENT_Exception
     (Pd_Minor  : in Interfaces.C.unsigned_long;
      Pd_Status : in Interfaces.C.int);
   pragma Export
     (Cpp, C_Raise_Ada_TRANSIENT_Exception,
      "Raise_Ada_TRANSIENT_Exception__FUlQ25CORBA16CompletionStatus");

   procedure C_Raise_Ada_FREE_MEM_Exception
     (Pd_Minor  : in Interfaces.C.unsigned_long;
      Pd_Status : in Interfaces.C.int);
   pragma Export
     (Cpp, C_Raise_Ada_FREE_MEM_Exception,
      "Raise_Ada_FREE_MEM_Exception__FUlQ25CORBA16CompletionStatus");

   procedure C_Raise_Ada_INV_IDENT_Exception
     (Pd_Minor  : in Interfaces.C.unsigned_long;
      Pd_Status : in Interfaces.C.int);
   pragma Export
     (Cpp, C_Raise_Ada_INV_IDENT_Exception,
      "Raise_Ada_INV_IDENT_Exception__FUlQ25CORBA16CompletionStatus");

   procedure C_Raise_Ada_INV_FLAG_Exception
     (Pd_Minor  : in Interfaces.C.unsigned_long;
      Pd_Status : in Interfaces.C.int);
   pragma Export
     (Cpp, C_Raise_Ada_INV_FLAG_Exception,
      "Raise_Ada_INV_FLAG_Exception__FUlQ25CORBA16CompletionStatus");

   procedure C_Raise_Ada_INTF_REPOS_Exception
     (Pd_Minor  : in Interfaces.C.unsigned_long;
      Pd_Status : in Interfaces.C.int);
   pragma Export
     (Cpp, C_Raise_Ada_INTF_REPOS_Exception,
      "Raise_Ada_INTF_REPOS_Exception__FUlQ25CORBA16CompletionStatus");

   procedure C_Raise_Ada_BAD_CONTEXT_Exception
     (Pd_Minor  : in Interfaces.C.unsigned_long;
      Pd_Status : in Interfaces.C.int);
   pragma Export
     (Cpp, C_Raise_Ada_BAD_CONTEXT_Exception,
      "Raise_Ada_BAD_CONTEXT_Exception__FUlQ25CORBA16CompletionStatus");

   procedure C_Raise_Ada_OBJ_ADAPTER_Exception
     (Pd_Minor  : in Interfaces.C.unsigned_long;
      Pd_Status : in Interfaces.C.int);
   pragma Export
     (Cpp, C_Raise_Ada_OBJ_ADAPTER_Exception,
      "Raise_Ada_OBJ_ADAPTER_Exception__FUlQ25CORBA16CompletionStatus");

   procedure C_Raise_Ada_DATA_CONVERSION_Exception
     (Pd_Minor  : in Interfaces.C.unsigned_long;
      Pd_Status : in Interfaces.C.int);
   pragma Export
     (Cpp, C_Raise_Ada_DATA_CONVERSION_Exception,
      "Raise_Ada_DATA_CONVERSION_Exception__FUlQ25CORBA16CompletionStatus");

   procedure C_Raise_Ada_TRANSACTION_REQUIRED_Exception
     (Pd_Minor  : in Interfaces.C.unsigned_long;
      Pd_Status : in Interfaces.C.int);
   pragma Export
     (Cpp, C_Raise_Ada_TRANSACTION_REQUIRED_Exception,
      "Raise_Ada_TRANSACTION_REQUIRED_Exception_" &
      "_FUlQ25CORBA16CompletionStatus");

   procedure C_Raise_Ada_TRANSACTION_ROLLEDBACK_Exception
     (Pd_Minor  : in Interfaces.C.unsigned_long;
      Pd_Status : in Interfaces.C.int);
   pragma Export
     (Cpp, C_Raise_Ada_TRANSACTION_ROLLEDBACK_Exception,
      "Raise_Ada_TRANSACTION_ROLLEDBACK_Exception_"  &
      "_FUlQ25CORBA16CompletionStatus");

   procedure C_Raise_Ada_INVALID_TRANSACTION_Exception
     (Pd_Minor  : in Interfaces.C.unsigned_long;
      Pd_Status : in Interfaces.C.int);
   pragma Export
     (Cpp, C_Raise_Ada_INVALID_TRANSACTION_Exception,
      "Raise_Ada_INVALID_TRANSACTION_Exception_" &
      "_FUlQ25CORBA16CompletionStatus");

   procedure C_Raise_Ada_WRONG_TRANSACTION_Exception
     (Pd_Minor : in Interfaces.C.unsigned_long;
      Pd_Status : in Interfaces.C.int);
   pragma Export
     (Cpp, C_Raise_Ada_WRONG_TRANSACTION_Exception,
      "Raise_Ada_WRONG_TRANSACTION_Exception__FUlQ25CORBA16CompletionStatus");

   procedure C_Raise_Ada_Fatal_Exception
     (Pd_Minor  : in Interfaces.C.unsigned_long;
      Pd_Status : in Interfaces.C.int);
   pragma Export
     (Cpp, C_Raise_Ada_Fatal_Exception,
      "Raise_Ada_Fatal_Exception__FUlQ25CORBA16CompletionStatus");

end Exceptions;

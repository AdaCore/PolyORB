
--  This package deals with the raising of C exceptions in Ada and ada ones
--  in C.  It is both a C and a Ada class (see Ada_Exceptions.hh) and
--  provides 2 mains methods : raise_C_Exception and
--  raise_Ada_Exception. The first one is called by Ada code and
--  implemented in C. The second is called by C code and implemented in
--  Ada. Both translate exceptions in the other language.

with Adabroker_Debug; use Adabroker_Debug;

package body Exceptions is

   Debug : constant Boolean := Adabroker_Debug.Is_Active ("exceptions");

   function Int_To_Status
     (N : in Interfaces.C.int)
      return CORBA.Completion_Status;

   function Status_To_Int
     (Status : in CORBA.Completion_Status)
      return Interfaces.C.int;

   -------------------
   -- Int_To_Status --
   -------------------

   function Int_To_Status
     (N : in Interfaces.C.int)
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
      return Interfaces.C.int
   is
      Ada_Result : Integer;
   begin
      Ada_Result := CORBA.Completion_Status'Pos (Status);
      return Interfaces.C.int (Ada_Result);
   end Status_To_Int;

   ---------------------------------
   -- C_Raise_Ada_Fatal_Exception --
   ---------------------------------

   procedure C_Raise_Ada_Fatal_Exception
     (File    : in Interfaces.C.Strings.chars_ptr;
      Line    : in Interfaces.C.int;
      Err_Msg : in Interfaces.C.Strings.chars_ptr)
   is
   begin
      Ada.Exceptions.Raise_Exception
        (CORBA.OmniORB_Fatal_Error'Identity,
         "In " & Interfaces.C.Strings.Value (File) &
         ", line " & Interfaces.C.int'Image (Line) &
         " : " & CORBA.CRLF &
         Interfaces.C.Strings.Value (Err_Msg));
   end C_Raise_Ada_Fatal_Exception;

   -----------------------------------
   -- C_Raise_Ada_UNKNOWN_Exception --
   -----------------------------------

   procedure C_Raise_Ada_UNKNOWN_Exception
     (Pd_Minor  : in Interfaces.C.unsigned_long;
      Pd_Status : in Interfaces.C.int)
   is
      Ada_Pd_Minor  : CORBA.Unsigned_Long;
      Ada_Pd_Status : CORBA.Completion_Status;
   begin
      pragma Debug
        (Output (Debug, "exceptions.C_Raise_Ada_UNKNOWN_Exception"));

      --  Transform the arguments in a Ada type ...
      Ada_Pd_Minor  := CORBA.Unsigned_Long (Pd_Minor);
      Ada_Pd_Status := Int_To_Status (Pd_Status);

      --  Raise the exception
      CORBA.Raise_CORBA_Exception
        (CORBA.Unknown'Identity,
         CORBA.Unknown_Members'(Ada_Pd_Minor, Ada_Pd_Status));
   end C_Raise_Ada_UNKNOWN_Exception;

   -------------------------------------
   -- C_Raise_Ada_BAD_PARAM_Exception --
   -------------------------------------

   procedure C_Raise_Ada_BAD_PARAM_Exception
     (Pd_Minor  : in Interfaces.C.unsigned_long;
      Pd_Status : in Interfaces.C.int)
   is
      Ada_Pd_Minor  : CORBA.Unsigned_Long;
      Ada_Pd_Status : CORBA.Completion_Status;
   begin
      --  Transform the arguments in a Ada type ...
      Ada_Pd_Minor := CORBA.Unsigned_Long (Pd_Minor);
      Ada_Pd_Status := Int_To_Status (Pd_Status);

      --  Raise the exception
      CORBA.Raise_CORBA_Exception
        (CORBA.Bad_Param'Identity,
         CORBA.Bad_Param_Members'(Ada_Pd_Minor, Ada_Pd_Status));
   end C_Raise_Ada_BAD_PARAM_Exception;

   -------------------------------------
   -- C_Raise_Ada_NO_MEMORY_Exception --
   -------------------------------------

   procedure C_Raise_Ada_NO_MEMORY_Exception
     (Pd_Minor  : in Interfaces.C.unsigned_long;
      Pd_Status : in Interfaces.C.int)
   is
      Ada_Pd_Minor  : CORBA.Unsigned_Long;
      Ada_Pd_Status : CORBA.Completion_Status;
   begin
      --  Transform the arguments in a Ada type ...
      Ada_Pd_Minor := CORBA.Unsigned_Long (Pd_Minor);
      Ada_Pd_Status := Int_To_Status (Pd_Status);

      --  Raise the exception
      CORBA.Raise_CORBA_Exception
        (CORBA.No_Memory'Identity,
         CORBA.No_Memory_Members'(Ada_Pd_Minor, Ada_Pd_Status));
   end C_Raise_Ada_NO_MEMORY_Exception;

   -------------------------------------
   -- C_Raise_Ada_IMP_LIMIT_Exception --
   -------------------------------------

   procedure C_Raise_Ada_IMP_LIMIT_Exception
     (Pd_Minor  : in Interfaces.C.unsigned_long;
      Pd_Status : in Interfaces.C.int)
   is
      Ada_Pd_Minor  : CORBA.Unsigned_Long;
      Ada_Pd_Status : CORBA.Completion_Status;
   begin
      --  Transform the arguments in a Ada type ...
      Ada_Pd_Minor  := CORBA.Unsigned_Long (Pd_Minor);
      Ada_Pd_Status := Int_To_Status (Pd_Status);

      --  Raises the exception
      CORBA.Raise_CORBA_Exception
        (CORBA.Imp_Limit'Identity,
         CORBA.Imp_Limit_Members'(Ada_Pd_Minor, Ada_Pd_Status));
   end C_Raise_Ada_IMP_LIMIT_Exception;

   ----------------------------------------
   -- C_Raise_Ada_COMM_FAILURE_Exception --
   ----------------------------------------

   procedure C_Raise_Ada_COMM_FAILURE_Exception
     (Pd_Minor  : in Interfaces.C.unsigned_long;
      Pd_Status : in Interfaces.C.int)
   is
      Ada_Pd_Minor : CORBA.Unsigned_Long;
      Ada_Pd_Status : CORBA.Completion_Status;
   begin
      --  Transform the arguments in a Ada type ...
      Ada_Pd_Minor  := CORBA.Unsigned_Long (Pd_Minor);
      Ada_Pd_Status := Int_To_Status (Pd_Status);

      --  Raise the exception
      CORBA.Raise_CORBA_Exception
        (CORBA.Comm_Failure'Identity,
         CORBA.Comm_Failure_Members'(Ada_Pd_Minor, Ada_Pd_Status));
   end C_Raise_Ada_COMM_FAILURE_Exception;

   --------------------------------------
   -- C_Raise_Ada_INV_OBJREF_Exception --
   --------------------------------------

   procedure C_Raise_Ada_INV_OBJREF_Exception
     (Pd_Minor  : in Interfaces.C.unsigned_long;
      Pd_Status : in Interfaces.C.int)
   is
      Ada_Pd_Minor  : CORBA.Unsigned_Long;
      Ada_Pd_Status : CORBA.Completion_Status;
   begin
      --  Transform the arguments in a Ada type ...
      Ada_Pd_Minor := CORBA.Unsigned_Long (Pd_Minor);
      Ada_Pd_Status := Int_To_Status (Pd_Status);
      --  Raise the exception
      CORBA.Raise_CORBA_Exception
        (CORBA.Inv_Objref'Identity,
         CORBA.Inv_Objref_Members'(Ada_Pd_Minor, Ada_Pd_Status));
   end C_Raise_Ada_INV_OBJREF_Exception;

   --------------------------------------------
   -- C_Raise_Ada_OBJECT_NOT_EXIST_Exception --
   --------------------------------------------

   procedure C_Raise_Ada_OBJECT_NOT_EXIST_Exception
     (Pd_Minor  : in Interfaces.C.unsigned_long;
      Pd_Status : in Interfaces.C.int)
   is
      Ada_Pd_Minor  : CORBA.Unsigned_Long;
      Ada_Pd_Status : CORBA.Completion_Status;
   begin
      --  Transform the arguments in a Ada type ...
      Ada_Pd_Minor  := CORBA.Unsigned_Long (Pd_Minor);
      Ada_Pd_Status := Int_To_Status (Pd_Status);

      --  Raise the exception
      CORBA.Raise_CORBA_Exception
        (CORBA.Object_Not_Exist'Identity,
         CORBA.Object_Not_Exist_Members'(Ada_Pd_Minor, Ada_Pd_Status));
   end C_Raise_Ada_OBJECT_NOT_EXIST_Exception;

   -----------------------------------------
   -- C_Raise_Ada_NO_PERMISSION_Exception --
   -----------------------------------------

   procedure C_Raise_Ada_NO_PERMISSION_Exception
     (Pd_Minor  : in Interfaces.C.unsigned_long;
      Pd_Status : in Interfaces.C.int)
   is
      Ada_Pd_Minor  : CORBA.Unsigned_Long;
      Ada_Pd_Status : CORBA.Completion_Status;
   begin
      --  Transform the arguments in a Ada type ...
      Ada_Pd_Minor  := CORBA.Unsigned_Long (Pd_Minor);
      Ada_Pd_Status := Int_To_Status (Pd_Status);
      --  Raise the exception
      CORBA.Raise_CORBA_Exception
        (CORBA.No_Permission'Identity,
         CORBA.No_Permission_Members'(Ada_Pd_Minor, Ada_Pd_Status));
   end C_Raise_Ada_NO_PERMISSION_Exception;

   ------------------------------------
   -- C_Raise_Ada_INTERNAL_Exception --
   ------------------------------------

   procedure C_Raise_Ada_INTERNAL_Exception
     (Pd_Minor  : in Interfaces.C.unsigned_long;
      Pd_Status : in Interfaces.C.int)
   is
      Ada_Pd_Minor  : CORBA.Unsigned_Long;
      Ada_Pd_Status : CORBA.Completion_Status;
   begin
      --  Transform the arguments in a Ada type ...
      Ada_Pd_Minor := CORBA.Unsigned_Long (Pd_Minor);
      Ada_Pd_Status := Int_To_Status (Pd_Status);

      --  Raise the exception
      CORBA.Raise_CORBA_Exception
        (CORBA.Internal'Identity,
         CORBA.Internal_Members'(Ada_Pd_Minor, Ada_Pd_Status));
   end C_Raise_Ada_INTERNAL_Exception;

   -----------------------------------
   -- C_Raise_Ada_MARSHAL_Exception --
   -----------------------------------

   procedure C_Raise_Ada_MARSHAL_Exception
     (Pd_Minor  : in Interfaces.C.unsigned_long;
      Pd_Status : in Interfaces.C.int)
   is
      Ada_Pd_Minor  : CORBA.Unsigned_Long;
      Ada_Pd_Status : CORBA.Completion_Status;
   begin
      --  Transform the arguments in a Ada type ...
      Ada_Pd_Minor := CORBA.Unsigned_Long (Pd_Minor);
      Ada_Pd_Status := Int_To_Status (Pd_Status);

      --  Raise the exception
      CORBA.Raise_CORBA_Exception
        (CORBA.Marshal'Identity,
         CORBA.Marshal_Members'(Ada_Pd_Minor, Ada_Pd_Status));
   end C_Raise_Ada_MARSHAL_Exception;

   ---------------------------------------------------
   --  C_Raise_Ada_INITIALIZATION_FAILURE_Exception --
   ---------------------------------------------------

   procedure C_Raise_Ada_INITIALIZATION_FAILURE_Exception
     (Pd_Minor  : in Interfaces.C.unsigned_long;
      Pd_Status : in Interfaces.C.int)
   is
      Ada_Pd_Minor  : CORBA.Unsigned_Long;
      Ada_Pd_Status : CORBA.Completion_Status;
   begin
      --  Transform the arguments in a Ada type ...
      Ada_Pd_Minor := CORBA.Unsigned_Long (Pd_Minor);
      Ada_Pd_Status := Int_To_Status (Pd_Status);

      --  Raise the exception
      CORBA.Raise_CORBA_Exception
        (CORBA.Initialization_Failure'Identity,
         CORBA.Initialization_Failure_Members'(Ada_Pd_Minor, Ada_Pd_Status));
   end C_Raise_Ada_INITIALIZATION_FAILURE_Exception;

   ----------------------------------------
   -- C_Raise_Ada_NO_IMPLEMENT_Exception --
   ----------------------------------------

   procedure C_Raise_Ada_NO_IMPLEMENT_Exception
     (Pd_Minor  : in Interfaces.C.unsigned_long;
      Pd_Status : in Interfaces.C.int)
   is
      Ada_Pd_Minor  : CORBA.Unsigned_Long;
      Ada_Pd_Status : CORBA.Completion_Status;
   begin
      --  Transform the arguments in a Ada type ...
      Ada_Pd_Minor := CORBA.Unsigned_Long (Pd_Minor);
      Ada_Pd_Status := Int_To_Status (Pd_Status);

      --  Raise the exception
      CORBA.Raise_CORBA_Exception
        (CORBA.No_Implement'Identity,
         CORBA.No_Implement_Members'(Ada_Pd_Minor, Ada_Pd_Status));
   end C_Raise_Ada_NO_IMPLEMENT_Exception;

   ----------------------------------------
   -- C_Raise_Ada_BAD_TYPECODE_Exception --
   ----------------------------------------

   procedure C_Raise_Ada_BAD_TYPECODE_Exception
     (Pd_Minor  : in Interfaces.C.unsigned_long;
      Pd_Status : in Interfaces.C.int)
   is
      Ada_Pd_Minor  : CORBA.Unsigned_Long;
      Ada_Pd_Status : CORBA.Completion_Status;
   begin
      --  Transform the arguments in a Ada type ...
      Ada_Pd_Minor := CORBA.Unsigned_Long (Pd_Minor);
      Ada_Pd_Status := Int_To_Status (Pd_Status);

      --  Raise the exception
      CORBA.Raise_CORBA_Exception
        (CORBA.Bad_Typecode'Identity,
         CORBA.Bad_Typecode_Members'(Ada_Pd_Minor, Ada_Pd_Status));
   end C_Raise_Ada_BAD_TYPECODE_Exception;

   -----------------------------------------
   -- C_Raise_Ada_BAD_OPERATION_Exception --
   -----------------------------------------

   procedure C_Raise_Ada_BAD_OPERATION_Exception
     (Pd_Minor  : in Interfaces.C.unsigned_long;
      Pd_Status : in Interfaces.C.int)
   is
      Ada_Pd_Minor  : CORBA.Unsigned_Long;
      Ada_Pd_Status : CORBA.Completion_Status;
   begin
      --  Transform the arguments in a Ada type ...
      Ada_Pd_Minor := CORBA.Unsigned_Long (Pd_Minor);
      Ada_Pd_Status := Int_To_Status (Pd_Status);

      --  Raise the exception
      CORBA.Raise_CORBA_Exception
        (CORBA.Bad_Operation'Identity,
         CORBA.Bad_Operation_Members'(Ada_Pd_Minor, Ada_Pd_Status));
   end C_Raise_Ada_BAD_OPERATION_Exception;

   ----------------------------------------
   -- C_Raise_Ada_NO_RESOURCES_Exception --
   ----------------------------------------

   procedure C_Raise_Ada_NO_RESOURCES_Exception
     (Pd_Minor  : in Interfaces.C.unsigned_long;
      Pd_Status : in Interfaces.C.int)
   is
      Ada_Pd_Minor  : CORBA.Unsigned_Long;
      Ada_Pd_Status : CORBA.Completion_Status;
   begin
      --  Transform the arguments in a Ada type ...
      Ada_Pd_Minor := CORBA.Unsigned_Long (Pd_Minor);
      Ada_Pd_Status := Int_To_Status (Pd_Status);

      --  Raise the exception
      CORBA.Raise_CORBA_Exception
        (CORBA.No_Resources'Identity,
         CORBA.No_Resources_Members'(Ada_Pd_Minor, Ada_Pd_Status));
   end C_Raise_Ada_NO_RESOURCES_Exception;

   ---------------------------------------
   -- C_Raise_Ada_NO_RESPONSE_Exception --
   ---------------------------------------

   procedure C_Raise_Ada_NO_RESPONSE_Exception
     (Pd_Minor  : in Interfaces.C.unsigned_long;
      Pd_Status : in Interfaces.C.int)
   is
      Ada_Pd_Minor  : CORBA.Unsigned_Long;
      Ada_Pd_Status : CORBA.Completion_Status;
   begin
      --  Transform the arguments in a Ada type ...
      Ada_Pd_Minor  := CORBA.Unsigned_Long (Pd_Minor);
      Ada_Pd_Status := Int_To_Status (Pd_Status);

      --  Raise the exception
      CORBA.Raise_CORBA_Exception
        (CORBA.No_Response'Identity,
         CORBA.No_Response_Members'(Ada_Pd_Minor, Ada_Pd_Status));
   end C_Raise_Ada_NO_RESPONSE_Exception;

   -----------------------------------------
   -- C_Raise_Ada_PERSIST_STORE_Exception --
   -----------------------------------------

   procedure C_Raise_Ada_PERSIST_STORE_Exception
     (Pd_Minor  : in Interfaces.C.unsigned_long;
      Pd_Status : in Interfaces.C.int)
   is
      Ada_Pd_Minor  : CORBA.Unsigned_Long;
      Ada_Pd_Status : CORBA.Completion_Status;
   begin
      --  Transform the arguments in a Ada type ...
      Ada_Pd_Minor  := CORBA.Unsigned_Long (Pd_Minor);
      Ada_Pd_Status := Int_To_Status (Pd_Status);

      --  Raise the exception
      CORBA.Raise_CORBA_Exception
        (CORBA.Persist_Store'Identity,
         CORBA.Persist_Store_Members'(Ada_Pd_Minor, Ada_Pd_Status));
   end C_Raise_Ada_PERSIST_STORE_Exception;

   -----------------------------------------
   -- C_Raise_Ada_BAD_INV_ORDER_Exception --
   -----------------------------------------

   procedure C_Raise_Ada_BAD_INV_ORDER_Exception
     (Pd_Minor  : in Interfaces.C.unsigned_long;
      Pd_Status : in Interfaces.C.int)
   is
      Ada_Pd_Minor  : CORBA.Unsigned_Long;
      Ada_Pd_Status : CORBA.Completion_Status;
   begin
      --  Transform the arguments in a Ada type ...
      Ada_Pd_Minor  := CORBA.Unsigned_Long (Pd_Minor);
      Ada_Pd_Status := Int_To_Status (Pd_Status);

      --  Raise the exception
      CORBA.Raise_CORBA_Exception
        (CORBA.Bad_Inv_Order'Identity,
         CORBA.Bad_Inv_Order_Members'(Ada_Pd_Minor, Ada_Pd_Status));
   end C_Raise_Ada_BAD_INV_ORDER_Exception;

   -------------------------------------
   -- C_Raise_Ada_TRANSIENT_Exception --
   -------------------------------------

   procedure C_Raise_Ada_TRANSIENT_Exception
     (Pd_Minor  : in Interfaces.C.unsigned_long;
      Pd_Status : in Interfaces.C.int)
   is
      Ada_Pd_Minor  : CORBA.Unsigned_Long;
      Ada_Pd_Status : CORBA.Completion_Status;
   begin
      --  Transform the arguments in a Ada type ...
      Ada_Pd_Minor  := CORBA.Unsigned_Long (Pd_Minor);
      Ada_Pd_Status := Int_To_Status (Pd_Status);

      --  Raise the exception
      CORBA.Raise_CORBA_Exception
        (CORBA.Transient'Identity,
         CORBA.Transient_Members'(Ada_Pd_Minor, Ada_Pd_Status));
   end C_Raise_Ada_TRANSIENT_Exception;

   ------------------------------------
   -- C_Raise_Ada_FREE_MEM_Exception --
   ------------------------------------

   procedure C_Raise_Ada_FREE_MEM_Exception
     (Pd_Minor  : in Interfaces.C.unsigned_long;
      Pd_Status : in Interfaces.C.int)
   is
      Ada_Pd_Minor  : CORBA.Unsigned_Long;
      Ada_Pd_Status : CORBA.Completion_Status;
   begin
      --  Transform the arguments in a Ada type ...
      Ada_Pd_Minor  := CORBA.Unsigned_Long (Pd_Minor);
      Ada_Pd_Status := Int_To_Status (Pd_Status);

      --  Raise the exception
      CORBA.Raise_CORBA_Exception
        (CORBA.Free_Mem'Identity,
         CORBA.Free_Mem_Members'(Ada_Pd_Minor, Ada_Pd_Status));
   end C_Raise_Ada_FREE_MEM_Exception;

   -------------------------------------
   -- C_Raise_Ada_INV_IDENT_Exception --
   -------------------------------------

   procedure C_Raise_Ada_INV_IDENT_Exception
     (Pd_Minor  : in Interfaces.C.unsigned_long;
      Pd_Status : in Interfaces.C.int)
   is
      Ada_Pd_Minor  : CORBA.Unsigned_Long;
      Ada_Pd_Status : CORBA.Completion_Status;
   begin
      --  Transform the arguments in a Ada type ...
      Ada_Pd_Minor  := CORBA.Unsigned_Long (Pd_Minor);
      Ada_Pd_Status := Int_To_Status (Pd_Status);

      --  Raise the exception
      CORBA.Raise_CORBA_Exception
        (CORBA.Inv_Ident'Identity,
         CORBA.Inv_Ident_Members'(Ada_Pd_Minor, Ada_Pd_Status));
   end C_Raise_Ada_INV_IDENT_Exception;

   ------------------------------------
   -- C_Raise_Ada_INV_FLAG_Exception --
   ------------------------------------

   procedure C_Raise_Ada_INV_FLAG_Exception
     (Pd_Minor  : in Interfaces.C.unsigned_long;
      Pd_Status : in Interfaces.C.int)
   is
      Ada_Pd_Minor  : CORBA.Unsigned_Long;
      Ada_Pd_Status : CORBA.Completion_Status;
   begin
      --  Transform the arguments in a Ada type ...
      Ada_Pd_Minor  := CORBA.Unsigned_Long (Pd_Minor);
      Ada_Pd_Status := Int_To_Status (Pd_Status);

      --  Raise the exception
      CORBA.Raise_CORBA_Exception
        (CORBA.Inv_Flag'Identity,
         CORBA.Inv_Flag_Members'(Ada_Pd_Minor, Ada_Pd_Status));
   end C_Raise_Ada_INV_FLAG_Exception;

   --------------------------------------
   -- C_Raise_Ada_INTF_REPOS_Exception --
   --------------------------------------

   procedure C_Raise_Ada_INTF_REPOS_Exception
     (Pd_Minor  : in Interfaces.C.unsigned_long;
      Pd_Status : in Interfaces.C.int)
   is
      Ada_Pd_Minor  : CORBA.Unsigned_Long;
      Ada_Pd_Status : CORBA.Completion_Status;
   begin
      --  Transform the arguments in a Ada type ...
      Ada_Pd_Minor  := CORBA.Unsigned_Long (Pd_Minor);
      Ada_Pd_Status := Int_To_Status (Pd_Status);

      --  Raise the exception
      CORBA.Raise_CORBA_Exception
        (CORBA.Intf_Repos'Identity,
         CORBA.Intf_Repos_Members'(Ada_Pd_Minor, Ada_Pd_Status));
   end C_Raise_Ada_INTF_REPOS_Exception;

   ---------------------------------------
   -- C_Raise_Ada_BAD_CONTEXT_Exception --
   ---------------------------------------

   procedure C_Raise_Ada_BAD_CONTEXT_Exception
     (Pd_Minor  : in Interfaces.C.unsigned_long;
      Pd_Status : in Interfaces.C.int)
   is
      Ada_Pd_Minor  : CORBA.Unsigned_Long;
      Ada_Pd_Status : CORBA.Completion_Status;
   begin
      --  Transform the arguments in a Ada type ...
      Ada_Pd_Minor  := CORBA.Unsigned_Long (Pd_Minor);
      Ada_Pd_Status := Int_To_Status (Pd_Status);

      --  Raise the exception
      CORBA.Raise_CORBA_Exception
        (CORBA.Bad_Context'Identity,
         CORBA.Bad_Context_Members'(Ada_Pd_Minor, Ada_Pd_Status));
   end C_Raise_Ada_BAD_CONTEXT_Exception;

   ---------------------------------------
   -- C_Raise_Ada_OBJ_ADAPTER_Exception --
   ---------------------------------------

   procedure C_Raise_Ada_OBJ_ADAPTER_Exception
     (Pd_Minor  : in Interfaces.C.unsigned_long;
      Pd_Status : in Interfaces.C.int)
   is
      Ada_Pd_Minor  : CORBA.Unsigned_Long;
      Ada_Pd_Status : CORBA.Completion_Status;
   begin
      --  Transform the arguments in a Ada type ...
      Ada_Pd_Minor  := CORBA.Unsigned_Long (Pd_Minor);
      Ada_Pd_Status := Int_To_Status (Pd_Status);

      --  Raise the exception
      CORBA.Raise_CORBA_Exception
        (CORBA.Obj_Adapter'Identity,
         CORBA.Obj_Adapter_Members'(Ada_Pd_Minor, Ada_Pd_Status));
   end C_Raise_Ada_OBJ_ADAPTER_Exception;

   -------------------------------------------
   -- C_Raise_Ada_DATA_CONVERSION_Exception --
   -------------------------------------------

   procedure C_Raise_Ada_DATA_CONVERSION_Exception
     (Pd_Minor  : in Interfaces.C.unsigned_long;
      Pd_Status : in Interfaces.C.int)
   is
      Ada_Pd_Minor  : CORBA.Unsigned_Long;
      Ada_Pd_Status : CORBA.Completion_Status;
   begin
      --  Transform the arguments in a Ada type ...
      Ada_Pd_Minor  := CORBA.Unsigned_Long (Pd_Minor);
      Ada_Pd_Status := Int_To_Status (Pd_Status);

      --  Raise the exception
      CORBA.Raise_CORBA_Exception
        (CORBA.Data_Conversion'Identity,
         CORBA.Data_Conversion_Members'(Ada_Pd_Minor, Ada_Pd_Status));
   end C_Raise_Ada_DATA_CONVERSION_Exception;

   ------------------------------------------------
   -- C_Raise_Ada_TRANSACTION_REQUIRED_Exception --
   ------------------------------------------------

   procedure C_Raise_Ada_TRANSACTION_REQUIRED_Exception
     (Pd_Minor  : in Interfaces.C.unsigned_long;
      Pd_Status : in Interfaces.C.int)
   is
      Ada_Pd_Minor  : CORBA.Unsigned_Long;
      Ada_Pd_Status : CORBA.Completion_Status;
   begin
      --  Transform the arguments in a Ada type ...
      Ada_Pd_Minor  := CORBA.Unsigned_Long (Pd_Minor);
      Ada_Pd_Status := Int_To_Status (Pd_Status);

      --  Raise the exception
      CORBA.Raise_CORBA_Exception
        (CORBA.Transaction_Required'Identity,
         CORBA.Transaction_Required_Members'(Ada_Pd_Minor, Ada_Pd_Status));
   end C_Raise_Ada_TRANSACTION_REQUIRED_Exception;

   --------------------------------------------------
   -- C_Raise_Ada_TRANSACTION_ROLLEDBACK_Exception --
   --------------------------------------------------

   procedure C_Raise_Ada_TRANSACTION_ROLLEDBACK_Exception
     (Pd_Minor  : in Interfaces.C.unsigned_long;
      Pd_Status : in Interfaces.C.int)
   is
      Ada_Pd_Minor  : CORBA.Unsigned_Long;
      Ada_Pd_Status : CORBA.Completion_Status;
   begin
      --  Transform the arguments in a Ada type ...
      Ada_Pd_Minor  := CORBA.Unsigned_Long (Pd_Minor);
      Ada_Pd_Status := Int_To_Status (Pd_Status);

      --  Raise the exception
      CORBA.Raise_CORBA_Exception
        (CORBA.Transaction_Rolledback'Identity,
         CORBA.Transaction_Rolledback_Members'(Ada_Pd_Minor, Ada_Pd_Status));
   end C_Raise_Ada_TRANSACTION_ROLLEDBACK_Exception;

   -----------------------------------------------
   -- C_Raise_Ada_INVALID_TRANSACTION_Exception --
   -----------------------------------------------

   procedure C_Raise_Ada_INVALID_TRANSACTION_Exception
     (Pd_Minor  : in Interfaces.C.unsigned_long;
      Pd_Status : in Interfaces.C.int)
   is
      Ada_Pd_Minor  : CORBA.Unsigned_Long;
      Ada_Pd_Status : CORBA.Completion_Status;
   begin
      --  Transform the arguments in a Ada type ...
      Ada_Pd_Minor  := CORBA.Unsigned_Long (Pd_Minor);
      Ada_Pd_Status := Int_To_Status (Pd_Status);

      --  Raise the exception
      CORBA.Raise_CORBA_Exception
        (CORBA.Invalid_Transaction'Identity,
         CORBA.Invalid_Transaction_Members'(Ada_Pd_Minor, Ada_Pd_Status));
   end C_Raise_Ada_INVALID_TRANSACTION_Exception;

   ---------------------------------------------
   -- C_Raise_Ada_WRONG_TRANSACTION_Exception --
   ---------------------------------------------

   procedure C_Raise_Ada_WRONG_TRANSACTION_Exception
     (Pd_Minor  : in Interfaces.C.unsigned_long;
      Pd_Status : in Interfaces.C.int)
   is
      Ada_Pd_Minor  : CORBA.Unsigned_Long;
      Ada_Pd_Status : CORBA.Completion_Status;
   begin
      --  Transform the arguments in a Ada type ...
      Ada_Pd_Minor  := CORBA.Unsigned_Long (Pd_Minor);
      Ada_Pd_Status := Int_To_Status (Pd_Status);

      --  Raise the exception
      CORBA.Raise_CORBA_Exception
        (CORBA.Wrong_Transaction'Identity,
         CORBA.Wrong_Transaction_Members'(Ada_Pd_Minor, Ada_Pd_Status));
   end C_Raise_Ada_WRONG_TRANSACTION_Exception;

   ---------------------------------
   -- C_Raise_Ada_Fatal_Exception --
   ---------------------------------

   procedure C_Raise_Ada_Fatal_Exception
     (Pd_Minor  : in Interfaces.C.unsigned_long;
      Pd_Status : in Interfaces.C.int)
   is
      Ada_Pd_Minor  : CORBA.Unsigned_Long;
      Ada_Pd_Status : CORBA.Completion_Status;
   begin
      --  Transform the arguments in a Ada type ...
      Ada_Pd_Minor  := CORBA.Unsigned_Long (Pd_Minor);
      Ada_Pd_Status := Int_To_Status (Pd_Status);

      --  Raise the exception
      CORBA.Raise_CORBA_Exception
        (CORBA.AdaBroker_Fatal_Error'Identity,
         CORBA.Wrong_Transaction_Members'(Ada_Pd_Minor, Ada_Pd_Status));
   end C_Raise_Ada_Fatal_Exception;

end Exceptions;

with CORBA.Object.OmniORB;
with CORBA; use CORBA;
with all_exceptions.Skel;
with AdaBroker.Exceptions; use AdaBroker.Exceptions;
package body all_exceptions.Impl is 

   procedure Unknown_exception_test
     (Self : access Object)
   is
      Member : CORBA.Unknown_Members;
   begin
      Member := (Minor => 100, Completed => CORBA.Completed_No);
      Raise_CORBA_Exception (Unknown'Identity, Member);
   end Unknown_exception_test;

   procedure Bad_Param_exception_test
     (Self : access Object)
   is
      Member : CORBA.Bad_Param_Members;
   begin
      Member := (Minor => 101, Completed => CORBA.Completed_Maybe);
      Raise_CORBA_Exception (Bad_Param'Identity, Member);
   end Bad_Param_exception_test;

   procedure No_Memory_exception_test
     (Self : access Object)
   is
      Member : CORBA.No_Memory_Members;
   begin
      Member := (Minor => 102, Completed => CORBA.Completed_Yes);
      Raise_CORBA_Exception (No_Memory'Identity, Member);
   end No_Memory_exception_test;

   procedure Imp_Limit_exception_test
     (Self : access Object)
   is
      Member : CORBA.Imp_Limit_Members;
   begin
      Member := (Minor => 103, Completed => CORBA.Completed_Yes);
      Raise_CORBA_Exception (Imp_Limit'Identity, Member);
   end Imp_Limit_exception_test;

   procedure Comm_Failure_exception_test
     (Self : access Object)
   is
      Member : CORBA.Comm_Failure_Members;
   begin
      Member := (Minor => 104, Completed => CORBA.Completed_Yes);
      Raise_CORBA_Exception (Comm_Failure'Identity, Member);
   end Comm_Failure_exception_test;

   procedure Inv_Objref_exception_test
     (Self : access Object)
   is
      Member : CORBA.Inv_Objref_Members;
   begin
      Member := (Minor => 105, Completed => CORBA.Completed_Yes);
      Raise_CORBA_Exception (Inv_Objref'Identity, Member);
   end Inv_Objref_exception_test;

   procedure No_Permission_exception_test
     (Self : access Object)
   is
      Member : CORBA.No_Permission_Members;
   begin
      Member := (Minor => 106, Completed => CORBA.Completed_Yes);
      Raise_CORBA_Exception (No_Permission'Identity, Member);
   end No_Permission_exception_test;

   procedure Internal_exception_test
     (Self : access Object)
   is
      Member : CORBA.Internal_Members;
   begin
      Member := (Minor => 107, Completed => CORBA.Completed_Yes);
      Raise_CORBA_Exception (Internal'Identity, Member);
   end Internal_exception_test;

   procedure Marshal_exception_test
     (Self : access Object)
   is
      Member : CORBA.Marshal_Members;
   begin
      Member := (Minor => 108, Completed => CORBA.Completed_Yes);
      Raise_CORBA_Exception (Marshal'Identity, Member);
   end Marshal_exception_test;

   procedure Initialization_Failure_exception_test
     (Self : access Object)
   is
      Member : CORBA.Initialization_Failure_Members;
   begin
      Member := (Minor => 109, Completed => CORBA.Completed_Yes);
      Raise_CORBA_Exception (Initialization_Failure'Identity, Member);
   end Initialization_Failure_exception_test;

   procedure No_Implement_exception_test
     (Self : access Object)
   is
      Member : CORBA.No_Implement_Members;
   begin
      Member := (Minor => 110, Completed => CORBA.Completed_Yes);
      Raise_CORBA_Exception (No_Implement'Identity, Member);
   end No_Implement_exception_test;

   procedure Bad_Typecode_exception_test
     (Self : access Object)
   is
      Member : CORBA.Bad_Typecode_Members;
   begin
      Member := (Minor => 111, Completed => CORBA.Completed_Yes);
      Raise_CORBA_Exception (Bad_Typecode'Identity, Member);
   end Bad_Typecode_exception_test;

   procedure Bad_Operation_exception_test
     (Self : access Object)
   is
      Member : CORBA.Bad_Operation_Members;
   begin
      Member := (Minor => 112, Completed => CORBA.Completed_Yes);
      Raise_CORBA_Exception (Bad_Operation'Identity, Member);
   end Bad_Operation_exception_test;

   procedure No_Resources_exception_test
     (Self : access Object)
   is
      Member : CORBA.No_Resources_Members;
   begin
      Member := (Minor => 113, Completed => CORBA.Completed_Yes);
      Raise_CORBA_Exception (No_Resources'Identity, Member);
   end No_Resources_exception_test;

   procedure No_Response_exception_test
     (Self : access Object)
   is
      Member : CORBA.No_Response_Members;
   begin
      Member := (Minor => 114, Completed => CORBA.Completed_Yes);
      Raise_CORBA_Exception (No_Response'Identity, Member);
   end No_Response_exception_test;

   procedure Persist_Store_exception_test
     (Self : access Object)
   is
      Member : CORBA.Persist_Store_Members;
   begin
      Member := (Minor => 115, Completed => CORBA.Completed_Yes);
      Raise_CORBA_Exception (Persist_Store'Identity, Member);
   end Persist_Store_exception_test;

   procedure Bad_Inv_Order_exception_test
     (Self : access Object)
   is
      Member : CORBA.Bad_Inv_Order_Members;
   begin
      Member := (Minor => 116, Completed => CORBA.Completed_Yes);
      Raise_CORBA_Exception (Bad_Inv_Order'Identity, Member);
   end Bad_Inv_Order_exception_test;

   procedure Transient_exception_test
     (Self : access Object)
   is
      Member : CORBA.Transient_Members;
   begin
      Member := (Minor => 117, Completed => CORBA.Completed_Yes);
      Raise_CORBA_Exception (Transient'Identity, Member);
   end Transient_exception_test;

   procedure Free_Mem_exception_test
     (Self : access Object)
   is
      Member : CORBA.Free_Mem_Members;
   begin
      Member := (Minor => 118, Completed => CORBA.Completed_Yes);
      Raise_CORBA_Exception (Free_Mem'Identity, Member);
   end Free_Mem_exception_test;

   procedure Inv_Ident_exception_test
     (Self : access Object)
   is
      Member : CORBA.Inv_Ident_Members;
   begin
      Member := (Minor => 119, Completed => CORBA.Completed_Yes);
      Raise_CORBA_Exception (Inv_Ident'Identity, Member);
   end Inv_Ident_exception_test;

   procedure Inv_Flag_exception_test
     (Self : access Object)
   is
      Member : CORBA.Inv_Flag_Members;
   begin
      Member := (Minor => 120, Completed => CORBA.Completed_Yes);
      Raise_CORBA_Exception (Inv_Flag'Identity, Member);
   end Inv_Flag_exception_test;

   procedure Intf_Repos_exception_test
     (Self : access Object)
   is
      Member : CORBA.Intf_Repos_Members;
   begin
      Member := (Minor => 121, Completed => CORBA.Completed_Yes);
      Raise_CORBA_Exception (Intf_Repos'Identity, Member);
   end Intf_Repos_exception_test;

   procedure Bad_Context_exception_test
     (Self : access Object)
   is
      Member : CORBA.Bad_Context_Members;
   begin
      Member := (Minor => 122, Completed => CORBA.Completed_Yes);
      Raise_CORBA_Exception (Bad_Context'Identity, Member);
   end Bad_Context_exception_test;

   procedure Obj_Adapter_exception_test
     (Self : access Object)
   is
      Member : CORBA.Obj_Adapter_Members;
   begin
      Member := (Minor => 123, Completed => CORBA.Completed_Yes);
      Raise_CORBA_Exception (Obj_Adapter'Identity, Member);
   end Obj_Adapter_exception_test;

   procedure Data_Conversion_exception_test
     (Self : access Object)
   is
      Member : CORBA.Data_Conversion_Members;
   begin
      Member := (Minor => 124, Completed => CORBA.Completed_Yes);
      Raise_CORBA_Exception (Data_Conversion'Identity, Member);
   end Data_Conversion_exception_test;

   procedure AdaBroker_Fatal_Error_exception_test
     (Self : access Object)
   is
   begin
      raise AdaBroker_Fatal_Error;
   end AdaBroker_Fatal_Error_exception_test;

   procedure Not_Implemented_Yet_exception_test
     (Self : access Object)
   is
   begin
      raise Not_Implemented_Yet;
   end Not_Implemented_Yet_exception_test;

   procedure Not_Initialized_Yet_exception_test
     (Self : access Object)
   is
   begin
      raise Not_Initialized_Yet;
   end Not_Initialized_Yet_exception_test;

   procedure C_Out_Of_Range_exception_test
     (Self : access Object)
   is
   begin 
      raise C_Out_Of_Range;
   end C_Out_Of_Range_exception_test;

   procedure Wrong_Union_Case_exception_test
     (Self : access Object)
   is
   begin 
      raise Wrong_Union_Case;
   end Wrong_Union_Case_exception_test;

   procedure Object_Not_Exist_exception_test
     (Self : access Object)
   is
      Member : CORBA.Object_Not_Exist_Members;
   begin
      Member := (Minor => 125, Completed => CORBA.Completed_Yes);
      Raise_CORBA_Exception (Object_Not_Exist'Identity, Member);
   end Object_Not_Exist_exception_test;

   procedure Transaction_Required_exception_test
     (Self : access Object)
   is
      Member : CORBA.Transaction_Required_Members;
   begin
      Member := (Minor => 126, Completed => CORBA.Completed_Yes);
      Raise_CORBA_Exception (Transaction_Required'Identity, Member);
   end Transaction_Required_exception_test;

   procedure Transaction_Rolledback_exception_test
     (Self : access Object)
   is
      Member : CORBA.Transaction_Rolledback_Members;
   begin
      Member := (Minor => 127, Completed => CORBA.Completed_Yes);
      Raise_CORBA_Exception (Transaction_Rolledback'Identity, Member);
   end Transaction_Rolledback_exception_test;

   procedure Invalid_Transaction_exception_test
     (Self : access Object)
   is
      Member : CORBA.Invalid_Transaction_Members;
   begin
      Member := (Minor => 128, Completed => CORBA.Completed_Yes);
      Raise_CORBA_Exception (Invalid_Transaction'Identity, Member);
   end Invalid_Transaction_exception_test;

   procedure Wrong_Transaction_exception_test
     (Self : access Object)
   is
      Member : CORBA.Wrong_Transaction_Members;
   begin
      Member := (Minor => 129, Completed => CORBA.Completed_Yes);
      Raise_CORBA_Exception (Wrong_Transaction'Identity, Member);
   end Wrong_Transaction_exception_test;

   -----------------------------------------------------------
   --  Implementations objects are controlled, you can add  --
   --  instructions in the following functions as specified --
   -----------------------------------------------------------

   procedure Initialize (Self : in out Object) is
   begin
      AdaBroker.OmniORB.Initialize
        (AdaBroker.OmniORB.ImplObject (Self),
         all_exceptions.Repository_Id);
      -- Add user code *BELOW* this line
   end Initialize;

   procedure Adjust (Self: in out Object) is
   begin
      AdaBroker.OmniORB.Adjust
        (AdaBroker.OmniORB.ImplObject (Self));
      -- Add user code *BELOW* this line
   end Adjust;

   procedure Finalize (Self : in out Object) is
   begin
      -- Add user code *BEFORE* this line
      AdaBroker.OmniORB.Finalize
        (AdaBroker.OmniORB.ImplObject (Self));
   end Finalize;

begin
   CORBA.Object.OmniORB.Register
     (all_exceptions.Repository_Id,
      all_exceptions.Nil_Ref,
      all_exceptions.Skel.Dispatch'Access);
end all_exceptions.Impl;

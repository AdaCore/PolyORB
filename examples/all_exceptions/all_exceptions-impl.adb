with all_exceptions.Skeleton ;
with Corba;
use Corba;

package body all_exceptions.Impl is


   -----------------------
   -- IDL definitions   --
   -----------------------

   --  Unknown_exception_test
   -------------------------------
   procedure Unknown_exception_test(Self : access Object) is
      Member : Corba.Unknown_Members ;
   begin
      Member := ( Minor => 0 , Completed => Corba.Completed_Yes);
      Corba.Raise_Corba_Exception ( Unknown'Identity, Member);
   end;


   --  Bad_Param_exception_test
   -------------------------------
   procedure Bad_Param_exception_test(Self : access Object) is
      Member : Corba.Bad_Param_Members ;
   begin
      Member := ( Minor => 1 , Completed => Corba.Completed_No);
      Corba.Raise_Corba_Exception ( Bad_Param'Identity, Member);
   end;


   --  No_Memory_exception_test
   -------------------------------
   procedure No_Memory_exception_test(Self : access Object) is
      Member : Corba.No_Memory_Members ;
   begin
      Member := ( Minor => 2 , Completed => Corba.Completed_Maybe);
      Corba.Raise_Corba_Exception ( No_Memory'Identity, Member);
   end;


   --  Imp_Limit_exception_test
   -------------------------------
   procedure Imp_Limit_exception_test(Self : access Object) is
      Member : Corba.Imp_Limit_Members ;
   begin
      Member := ( Minor => 3 , Completed => Corba.Completed_Yes);
      Corba.Raise_Corba_Exception ( Imp_Limit'Identity, Member);
   end;


   --  Comm_Failure_exception_test
   -------------------------------
   procedure Comm_Failure_exception_test(Self : access Object) is
      Member : Corba.Comm_Failure_Members ;
   begin
      Member := ( Minor => 4 , Completed => Corba.Completed_Yes);
      Corba.Raise_Corba_Exception ( Comm_Failure'Identity, Member);
   end;


   --  Inv_Objref_exception_test
   -------------------------------
   procedure Inv_Objref_exception_test(Self : access Object) is
      Member : Corba.Inv_Objref_Members ;
   begin
      Member := ( Minor => 5 , Completed => Corba.Completed_Yes);
      Corba.Raise_Corba_Exception ( Inv_Objref'Identity, Member);
   end;


   --  No_Permission_exception_test
   -------------------------------
   procedure No_Permission_exception_test(Self : access Object) is
      Member : Corba.No_Permission_Members ;
   begin
      Member := ( Minor => 6 , Completed => Corba.Completed_Yes);
      Corba.Raise_Corba_Exception (No_Permission'Identity, Member);
   end;


   --  Internal_exception_test
   -------------------------------
   procedure Internal_exception_test(Self : access Object) is
      Member : Corba.Internal_Members ;
   begin
      Member := ( Minor => 7 , Completed => Corba.Completed_Yes);
      Corba.Raise_Corba_Exception ( Internal'Identity, Member);
   end;


   --  Marshal_exception_test
   -------------------------------
   procedure Marshal_exception_test(Self : access Object) is
      Member : Corba.Marshal_Members ;
   begin
      Member := ( Minor => 8 , Completed => Corba.Completed_Yes);
      Corba.Raise_Corba_Exception ( Marshal'Identity, Member);
   end;


   --  Initialization_Failure_exception_test
   -------------------------------
   procedure Initialization_Failure_exception_test(Self : access Object) is
      Member : Corba.Initialization_Failure_Members ;
   begin
      Member := ( Minor => 9 , Completed => Corba.Completed_Yes);
      Corba.Raise_Corba_Exception ( Initialization_Failure'Identity, Member);
   end;


   --  No_Implement_exception_test
   -------------------------------
   procedure No_Implement_exception_test(Self : access Object) is
      Member : Corba.No_Implement_Members ;
   begin
      Member := ( Minor => 10 , Completed => Corba.Completed_Yes);
      Corba.Raise_Corba_Exception ( No_Implement'Identity, Member);
   end;


   --  Bad_Typecode_exception_test
   -------------------------------
   procedure Bad_Typecode_exception_test(Self : access Object) is
      Member : Corba.Bad_Typecode_Members ;
   begin
      Member := ( Minor => 11 , Completed => Corba.Completed_Yes);
      Corba.Raise_Corba_Exception (Bad_Typecode'Identity, Member);
   end;


   --  Bad_Operation_exception_test
   -------------------------------
   procedure Bad_Operation_exception_test(Self : access Object) is
      Member : Corba.Bad_Operation_Members ;
   begin
      Member := ( Minor => 12 , Completed => Corba.Completed_Yes);
      Corba.Raise_Corba_Exception ( Bad_Operation'Identity, Member);
   end;


   --  No_Ressources_exception_test
   -------------------------------
   procedure No_Ressources_exception_test(Self : access Object) is
      Member : Corba.No_Resources_Members  ;
   begin
      Member := ( Minor => 13 , Completed => Corba.Completed_Yes);
      Corba.Raise_Corba_Exception ( No_Resources'Identity, Member);
   end;


   --  No_Response_exception_test
   -------------------------------
   procedure No_Response_exception_test(Self : access Object) is
      Member : Corba.No_Response_Members ;
   begin
      Member := ( Minor => 14 , Completed => Corba.Completed_Yes);
      Corba.Raise_Corba_Exception (  No_Response'Identity, Member);
   end;


   --  Persist_Store_exception_test
   -------------------------------
   procedure Persist_Store_exception_test(Self : access Object) is
      Member : Corba.Persist_Store_Members ;
   begin
      Member := ( Minor => 15 , Completed => Corba.Completed_Yes);
      Corba.Raise_Corba_Exception ( Persist_Store'Identity, Member);
   end;


   --  Bad_Inv_Order_exception_test
   -------------------------------
   procedure Bad_Inv_Order_exception_test(Self : access Object) is
      Member : Corba.Bad_Inv_Order_Members ;
   begin
      Member := ( Minor => 16 , Completed => Corba.Completed_Yes);
      Corba.Raise_Corba_Exception ( Bad_Inv_Order'Identity, Member);
   end;


   --  Transient_simple_exception_test
   -------------------------------
   procedure Transient_simple_exception_test(Self : access Object) is
      Member : Corba.Transient_Members ;
   begin
      Member := ( Minor => 17 , Completed => Corba.Completed_Yes);
      Corba.Raise_Corba_Exception ( Transient'Identity, Member);
   end;


   --  Free_Mem_exception_test
   -------------------------------
   procedure Free_Mem_exception_test(Self : access Object) is
      Member : Corba.Free_Mem_Members ;
   begin
      Member := ( Minor => 18 , Completed => Corba.Completed_Yes);
      Corba.Raise_Corba_Exception ( Free_Mem'Identity, Member);
   end;


   --  Inv_Indent_exception_test
   -------------------------------
   procedure Inv_Indent_exception_test(Self : access Object) is
      Member : Corba.Inv_Ident_Members ;
   begin
      Member := ( Minor => 19 , Completed => Corba.Completed_Yes);
      Corba.Raise_Corba_Exception ( Inv_Ident'Identity, Member);
   end;


   --  Inv_Flag_exception_test
   -------------------------------
   procedure Inv_Flag_exception_test(Self : access Object) is
      Member : Corba.Inv_Flag_Members ;
   begin
      Member := ( Minor => 20 , Completed => Corba.Completed_Yes);
      Corba.Raise_Corba_Exception ( Inv_Flag'Identity, Member);
   end;


   --  Intf_Repos_exception_test
   -------------------------------
   procedure Intf_Repos_exception_test(Self : access Object) is
      Member : Corba.Intf_Repos_Members ;
   begin
      Member := ( Minor => 21 , Completed => Corba.Completed_Yes);
      Corba.Raise_Corba_Exception ( Intf_Repos'Identity, Member);
   end;


   --  Bad_Context_exception_test
   -------------------------------
   procedure Bad_Context_exception_test(Self : access Object) is
      Member : Corba.Bad_Context_Members ;
   begin
      Member := ( Minor => 22 , Completed => Corba.Completed_Yes);
      Corba.Raise_Corba_Exception ( Bad_Context'Identity, Member);
   end;


   --  Obj_Adapter_exception_test
   -------------------------------
   procedure Obj_Adapter_exception_test(Self : access Object) is
      Member : Corba.Obj_Adapter_Members ;
   begin
      Member := ( Minor => 23 , Completed => Corba.Completed_Yes);
      Corba.Raise_Corba_Exception ( Obj_Adapter'Identity, Member);
  end;


   --  Data_Concersion_exception_test
   -------------------------------
   procedure Data_Concersion_exception_test(Self : access Object) is
      Member : Corba.Data_Conversion_Members ;
   begin
      Member := ( Minor => 24 , Completed => Corba.Completed_Yes);
      Corba.Raise_Corba_Exception ( Data_Conversion'Identity, Member);
   end;


   --  AdaBroker_Fatal_Error_exception_test
   -------------------------------
   procedure AdaBroker_Fatal_Error_exception_test(Self : access Object) is
   begin
      raise Corba.AdaBroker_Fatal_Error;
   end;


   --  AdaBroker_Not_Implemented_Yet_exception_test
   -------------------------------
   procedure AdaBroker_Not_Implemented_Yet_exception_test(Self : access Object) is
   begin
      raise Corba.AdaBroker_Not_Implemented_Yet;
   end;


   --  No_Initialisation_Error_exception_test
   -------------------------------
   procedure No_Initialisation_Error_exception_test(Self : access Object) is
   begin
      raise Corba.No_Initialisation_Error;
   end;


   --  C_Out_Of_Range_exception_test
   -------------------------------
   procedure C_Out_Of_Range_exception_test(Self : access Object) is
   begin
      raise Corba.C_Out_Of_Range;
   end;


   --  Dummy_User_exception_test
   -------------------------------
   procedure Dummy_User_exception_test(Self : access Object) is
   begin
      raise Corba.Dummy_User;
   end;

   -- Object_Not_Exist
   -------------------------------
   procedure Object_Not_Exist_exception_test(Self : access Object ) is
      Member : Corba.Object_Not_Exist_Members ;
   begin
      Member := ( Minor => 25 , Completed => Corba.Completed_Yes);
      Corba.Raise_Corba_Exception ( Object_Not_Exist'Identity, Member);
   end;


   --  Transaction_Required exception
   -------------------------------
   procedure Transaction_Required_exception_test(Self : access Object ) is
      Member : Corba.Transaction_Required_Members ;
   begin
      Member := ( Minor => 26 , Completed => Corba.Completed_Yes);
      Corba.Raise_Corba_Exception ( Transaction_Required'Identity, Member);
   end;


   -- Transaction_Rolledback exception
   -------------------------------
   procedure Transaction_Rolledback_exception_test(Self : access Object ) is
      Member : Corba.Transaction_Rolledback_Members ;
   begin
      Member := ( Minor => 27 , Completed => Corba.Completed_Yes);
      Corba.Raise_Corba_Exception ( Transaction_Rolledback'Identity, Member);
   end;


    -- Invalid_Transaction exception
   -------------------------------
  procedure Invalid_Transaction_exception_test(Self : access Object ) is
      Member : Corba.Invalid_Transaction_Members ;
   begin
      Member := ( Minor => 28 , Completed => Corba.Completed_Yes);
      Corba.Raise_Corba_Exception ( Invalid_Transaction'Identity, Member);
   end;


   -- Wrong_Transaction exception
   -------------------------------
   procedure Wrong_Transaction_exception_test(Self : access Object ) is
      Member : Corba.Wrong_Transaction_Members ;
   begin
      Member := ( Minor => 29 , Completed => Corba.Completed_Yes);
      Corba.Raise_Corba_Exception ( Wrong_Transaction'Identity, Member);
   end;






   -----------------------------------------------------------
   --  Implementations objects are controlled, you can add  --
   --  instructions in the following functions as specified --
   -----------------------------------------------------------

   -- Initialize
   -------------
   procedure Initialize(Self : in out Object) is
   begin
      Omniobject.Initialize(Omniobject.Implemented_Object(Self)) ;
      Init_Local_Object(Self,
                        Repository_Id,
                        all_exceptions.Skeleton.Dispatch'Access,
                        all_exceptions.Is_A'Access) ;
      -- You can add things *BELOW* this line

   end Initialize ;


   -- Adjust
   ---------
   procedure Adjust(Self: in out Object) is
   begin
   Omniobject.Adjust(Omniobject.Implemented_Object(Self)) ;
      -- You can add things *BELOW* this line

   end Adjust ;


   -- Finalize
   -----------
   procedure Finalize(Self : in out Object) is
   begin

      -- You can add things *BEFORE* this line
   Omniobject.Finalize(Omniobject.Implemented_Object(Self)) ;
   end Finalize ;


end all_exceptions.Impl ;

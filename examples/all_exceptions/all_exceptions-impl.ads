with AdaBroker.OmniObject ;
package all_exceptions.Impl is

   type Object is new AdaBroker.OmniObject.Implemented_Object with private ;
   type Object_Ptr is access all Object ;


   -----------------------
   -- IDL definitions   --
   -----------------------

   procedure Unknown_exception_test(Self : access Object ) ;
   procedure Bad_Param_exception_test(Self : access Object ) ;
   procedure No_Memory_exception_test(Self : access Object ) ;
   procedure Imp_Limit_exception_test(Self : access Object ) ;
   procedure Comm_Failure_exception_test(Self : access Object ) ;
   procedure Inv_Objref_exception_test(Self : access Object ) ;
   procedure No_Permission_exception_test(Self : access Object ) ;
   procedure Internal_exception_test(Self : access Object ) ;
   procedure Marshal_exception_test(Self : access Object ) ;
   procedure Initialization_Failure_exception_test(Self : access Object ) ;
   procedure No_Implement_exception_test(Self : access Object ) ;
   procedure Bad_Typecode_exception_test(Self : access Object ) ;
   procedure Bad_Operation_exception_test(Self : access Object ) ;
   procedure No_Ressources_exception_test(Self : access Object ) ;
   procedure No_Response_exception_test(Self : access Object ) ;
   procedure Persist_Store_exception_test(Self : access Object ) ;
   procedure Bad_Inv_Order_exception_test(Self : access Object ) ;
   procedure Transient_simple_exception_test(Self : access Object ) ;
   procedure Free_Mem_exception_test(Self : access Object ) ;
   procedure Inv_Indent_exception_test(Self : access Object ) ;
   procedure Inv_Flag_exception_test(Self : access Object ) ;
   procedure Intf_Repos_exception_test(Self : access Object ) ;
   procedure Bad_Context_exception_test(Self : access Object ) ;
   procedure Obj_Adapter_exception_test(Self : access Object ) ;
   procedure Data_Concersion_exception_test(Self : access Object ) ;
   procedure AdaBroker_Fatal_Error_exception_test(Self : access Object ) ;
   procedure AdaBroker_Not_Implemented_Yet_exception_test(Self : access Object ) ;
   procedure No_Initialisation_Error_exception_test(Self : access Object ) ;
   procedure C_Out_Of_Range_exception_test(Self : access Object ) ;
   procedure Dummy_User_exception_test(Self : access Object ) ;
   procedure Object_Not_Exist_exception_test(Self : access Object ) ;
   procedure Transaction_Required_exception_test(Self : access Object ) ;
   procedure Transaction_Rolledback_exception_test(Self : access Object ) ;
   procedure Invalid_Transaction_exception_test(Self : access Object ) ;
   procedure Wrong_Transaction_exception_test(Self : access Object ) ;


private

   -- You may add fields to this record
   type Object is new AdaBroker.OmniObject.Implemented_Object with record
      Null ;
   end record ;

   --------------------------------------------------
   ----          finalization operators          ----
   --------------------------------------------------
   procedure Initialize(Self : in out Object) ;
   procedure Adjust(Self : in out Object) ;
   procedure Finalize(Self : in out Object) ;

end all_exceptions.Impl ;

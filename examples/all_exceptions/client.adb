----------------------------------------------------------------------------
----                                                                    ----
----     This in a hand-written client for All_Exceptions example             ----
----                                                                    ----
----     It provides a declaration of each simple type with the         ----
----     echo function associated.                                      ----
----                                                                    ----
----                                                                    ----
----                server                                              ----
----                                                                    ----
----                authors : Fabien Azavant, Sebastien Ponce           ----
----                                                                    ----
----------------------------------------------------------------------------



with Ada.Command_Line ;
with Text_Io ; use Text_Io ;
with Corba, Corba.Orb, Corba.Boa, Corba.Object ;
with All_Exceptions ; use All_Exceptions ;
use Corba.Object ;
use Corba;

procedure Client is

   Orb : Corba.Orb.Object := Corba.Orb.Orb_Init("omniORB2");
   Boa : Corba.Boa.Object := Corba.Orb.Boa_Init(Orb, "omniORB2_BOA") ;

   IOR : Corba.String ;
   IOR_Arg : Standard.String := Ada.Command_Line.Argument(1) ;
   MyAll_Exceptions : All_Exceptions.Ref ;

begin

   Put_Line("main : Starting client") ;

   if Ada.Command_Line.Argument_Count < 1 then
      Put_Line ("usage : client <IOR_string_from_server>") ;
      return ;
   end if ;

   -- transforms the Ada string into Corba.String
   IOR := Corba.To_Corba_String(IOR_Arg) ;

   -- getting the Corba.Object
   Corba.Orb.String_To_Object(IOR, MyAll_Exceptions) ;
   Put_Line("main : Got the Corba.Object") ;

   -- checking if it worked
   if All_Exceptions.Is_Nil(MyAll_Exceptions) then
      Put_Line("main : cannot invoke on a nil reference") ;
      return ;
   end if ;
   Put_Line("main : Ok : Corba.Object is not nil") ;

   -------------------
   -- Exceptions tests --
   -------------------

   -- Unknown exception
   declare
      Member : Unknown_members ;
   begin
      Put_Line ("####### Test of Unknown #######") ;
      Put_Line ("I call unknown_exception_test") ;
      Unknown_Exception_Test(MyAll_Exceptions);
   exception
      when E : Unknown =>
         Put_Line ("A Unknown exception has just been catched !!!") ;
         Corba.Get_Members (E ,Member) ;
         Put_Line ("It has a member whose value is : " &
                   Corba.Unsigned_Long'Image(Member.Minor)) ;
   end ;

   Put_Line ("") ;
   Put_Line ("") ;

   -- Bad_Param exception
   declare
      Member : Bad_Param_members ;
   begin
      Put_Line ("####### Test of Bad_param #######") ;
      Put_Line ("I call bad_param_exception_test") ;
      Bad_Param_Exception_Test(MyAll_Exceptions);
   exception
      when E : Bad_param =>
         Put_Line ("A bad_param exception has just been catched !!!") ;
         Corba.Get_Members (E ,Member) ;
         Put_Line ("It has a member whose value is : " &
                   Corba.Unsigned_Long'Image(Member.Minor)) ;
   end ;

   Put_Line ("") ;
   Put_Line ("") ;

   -- No_memory exception      **********raise an Unknown exception => bug
   declare
      Member : No_memory_members ;
   begin
      Put_Line ("####### Test of No_memory #######") ;
      Put_Line ("I call no_memory_exception_test") ;
--      No_memory_Exception_Test(MyAll_Exceptions);

   exception
      when E : No_memory =>
         Put_Line ("A No_Memory exception has just been catched !!!") ;
         Corba.Get_Members (E ,Member) ;
         Put_Line ("It has a member whose value is : " &
                   Corba.Unsigned_Long'Image(Member.Minor)) ;
   end ;

   Put_Line ("") ;
   Put_Line ("") ;

   -- Imp_Limit exception
   declare
      Member : Imp_Limit_members ;
   begin
      Put_Line ("####### Test of Imp_Limit #######") ;
      Put_Line ("I call Imp_Limit_exception_test") ;
      Imp_Limit_Exception_Test(MyAll_Exceptions);
   exception
      when E : Imp_Limit =>
         Put_Line ("A Imp_Limit exception has just been catched !!!") ;
         Corba.Get_Members (E ,Member) ;
         Put_Line ("It has a member whose value is : " &
                   Corba.Unsigned_Long'Image(Member.Minor)) ;
   end ;

   Put_Line ("") ;
   Put_Line ("") ;

   -- Comm_Failure exception      **********make a loop in the program
   declare
      Member : Comm_Failure_members ;
   begin
      Put_Line ("####### Test of Comm_Failure #######") ;
      Put_Line ("I call Comm_Failure_exception_test") ;
--      Comm_Failure_Exception_Test(MyAll_Exceptions);
   exception
      when E : Comm_Failure =>
         Put_Line ("A Comm_Failure exception has just been catched !!!") ;
         Corba.Get_Members (E ,Member) ;
         Put_Line ("It has a member whose value is : " &
                   Corba.Unsigned_Long'Image(Member.Minor)) ;
   end ;

   Put_Line ("") ;
   Put_Line ("") ;

   -- Inv_Objref exception
   declare
      Member : Inv_Objref_members ;
   begin
      Put_Line ("####### Test of Inv_Objref #######") ;
      Put_Line ("I call Inv_Objref_exception_test") ;
      Inv_Objref_Exception_Test(MyAll_Exceptions);
   exception
      when E : Inv_Objref =>
         Put_Line ("A Inv_Objref exception has just been catched !!!") ;
         Corba.Get_Members (E ,Member) ;
         Put_Line ("It has a member whose value is : " &
                   Corba.Unsigned_Long'Image(Member.Minor)) ;
   end ;

   Put_Line ("") ;
   Put_Line ("") ;

   -- No_Permission exception
   declare
      Member : No_Permission_members ;
   begin
      Put_Line ("####### Test of No_Permission #######") ;
      Put_Line ("I call No_Permission_exception_test") ;
      No_Permission_Exception_Test(MyAll_Exceptions);
   exception
      when E : No_Permission =>
         Put_Line ("A No_Permission exception has just been catched !!!") ;
         Corba.Get_Members (E ,Member) ;
         Put_Line ("It has a member whose value is : " &
                   Corba.Unsigned_Long'Image(Member.Minor)) ;
   end ;

   Put_Line ("") ;
   Put_Line ("") ;

   -- Internal exception
   declare
      Member : Internal_members ;
   begin
      Put_Line ("####### Test of Internal #######") ;
      Put_Line ("I call Internal_exception_test") ;
      Internal_Exception_Test(MyAll_Exceptions);
   exception
      when E : Internal =>
         Put_Line ("A Internal exception has just been catched !!!") ;
         Corba.Get_Members (E ,Member) ;
         Put_Line ("It has a member whose value is : " &
                   Corba.Unsigned_Long'Image(Member.Minor)) ;
   end ;

   Put_Line ("") ;
   Put_Line ("") ;

   -- Marshal exception      **********raise an Unknown exception => bug
   declare
      Member : Marshal_members ;
   begin
      Put_Line ("####### Test of Marshal #######") ;
      Put_Line ("I call Marshal_exception_test") ;
--      Marshal_Exception_Test(MyAll_Exceptions);
   exception
      when E : Marshal =>
         Put_Line ("A Marshal exception has just been catched !!!") ;
         Corba.Get_Members (E ,Member) ;
         Put_Line ("It has a member whose value is : " &
                   Corba.Unsigned_Long'Image(Member.Minor)) ;
   end ;

   Put_Line ("") ;
   Put_Line ("") ;

   -- Initialization_Failure exception
   declare
      Member : Initialization_Failure_members ;
   begin
      Put_Line ("####### Test of Initialization_Failure #######") ;
      Put_Line ("I call Initialization_Failure_exception_test") ;
      Initialization_Failure_Exception_Test(MyAll_Exceptions);
   exception
      when E : Initialization_Failure =>
         Put_Line ("A Initialization_Failure exception has just been catched !!!") ;
         Corba.Get_Members (E ,Member) ;
         Put_Line ("It has a member whose value is : " &
                   Corba.Unsigned_Long'Image(Member.Minor)) ;
   end ;

   Put_Line ("") ;
   Put_Line ("") ;

   -- No_Implement exception
   declare
      Member : No_Implement_members ;
   begin
      Put_Line ("####### Test of No_Implement #######") ;
      Put_Line ("I call No_Implement_exception_test") ;
      No_Implement_Exception_Test(MyAll_Exceptions);
   exception
      when E : No_Implement =>
         Put_Line ("A No_Implement exception has just been catched !!!") ;
         Corba.Get_Members (E ,Member) ;
         Put_Line ("It has a member whose value is : " &
                   Corba.Unsigned_Long'Image(Member.Minor)) ;
   end ;

   Put_Line ("") ;
   Put_Line ("") ;

   -- Bad_Typecode exception
   declare
      Member : Bad_Typecode_members ;
   begin
      Put_Line ("####### Test of Bad_Typecode #######") ;
      Put_Line ("I call Bad_Typecode_exception_test") ;
      Bad_Typecode_Exception_Test(MyAll_Exceptions);
   exception
      when E : Bad_Typecode =>
         Put_Line ("A Bad_Typecode exception has just been catched !!!") ;
         Corba.Get_Members (E ,Member) ;
         Put_Line ("It has a member whose value is : " &
                   Corba.Unsigned_Long'Image(Member.Minor)) ;
   end ;

   Put_Line ("") ;
   Put_Line ("") ;

   -- Bad_Operation exception
   declare
      Member : Bad_Operation_members ;
   begin
      Put_Line ("####### Test of Bad_Operation #######") ;
      Put_Line ("I call Bad_Operation_exception_test") ;
      Bad_Operation_Exception_Test(MyAll_Exceptions);
   exception
      when E : Bad_Operation =>
         Put_Line ("A Bad_Operation exception has just been catched !!!") ;
         Corba.Get_Members (E ,Member) ;
         Put_Line ("It has a member whose value is : " &
                   Corba.Unsigned_Long'Image(Member.Minor)) ;
   end ;

   Put_Line ("") ;
   Put_Line ("") ;

   -- No_Resources exception       **********raise an Unknown exception => bug
   declare
      Member : No_Resources_members ;
   begin
      Put_Line ("####### Test of No_Resources #######") ;
      Put_Line ("I call No_Resources_exception_test") ;
--      No_Ressources_Exception_Test(MyAll_Exceptions);
   exception
      when E : No_Resources =>
         Put_Line ("A No_Resources exception has just been catched !!!") ;
         Corba.Get_Members (E ,Member) ;
         Put_Line ("It has a member whose value is : " &
                   Corba.Unsigned_Long'Image(Member.Minor)) ;
   end ;

   Put_Line ("") ;
   Put_Line ("") ;

   -- No_Response exception        **********raise an Unknown exception => bug
   declare
      Member : No_Response_members ;
   begin
      Put_Line ("####### Test of No_Response #######") ;
      Put_Line ("I call No_Response_exception_test") ;
--      No_Response_Exception_Test(MyAll_Exceptions);
   exception
      when E : No_Response =>
         Put_Line ("A No_Response exception has just been catched !!!") ;
         Corba.Get_Members (E ,Member) ;
         Put_Line ("It has a member whose value is : " &
                   Corba.Unsigned_Long'Image(Member.Minor)) ;
   end ;

   Put_Line ("") ;
   Put_Line ("") ;

   -- Persist_Store exception
   declare
      Member : Persist_Store_members ;
   begin
      Put_Line ("####### Test of Persist_Store #######") ;
      Put_Line ("I call Persist_Store_exception_test") ;
      Persist_Store_Exception_Test(MyAll_Exceptions);
   exception
      when E : Persist_Store =>
         Put_Line ("A Persist_Store exception has just been catched !!!") ;
         Corba.Get_Members (E ,Member) ;
         Put_Line ("It has a member whose value is : " &
                   Corba.Unsigned_Long'Image(Member.Minor)) ;
   end ;

   Put_Line ("") ;
   Put_Line ("") ;

   -- Bad_Inv_Order exception
   declare
      Member : Bad_Inv_Order_members ;
   begin
      Put_Line ("####### Test of Bad_Inv_Order #######") ;
      Put_Line ("I call Bad_Inv_Order_exception_test") ;
      Bad_Inv_Order_Exception_Test(MyAll_Exceptions);
   exception
      when E : Bad_Inv_Order =>
         Put_Line ("A Bad_Inv_Order exception has just been catched !!!") ;
         Corba.Get_Members (E ,Member) ;
         Put_Line ("It has a member whose value is : " &
                   Corba.Unsigned_Long'Image(Member.Minor)) ;
   end ;

   Put_Line ("") ;
   Put_Line ("") ;

   -- Transient exception         ********make a loop in the program
   declare
      Member : Transient_members ;
   begin
      Put_Line ("####### Test of Transient #######") ;
      Put_Line ("I call Transient_exception_test") ;
--      Transient_Simple_Exception_Test(MyAll_Exceptions);
   exception
      when E : Transient =>
         Put_Line ("A Transient exception has just been catched !!!") ;
         Corba.Get_Members (E ,Member) ;
         Put_Line ("It has a member whose value is : " &
                   Corba.Unsigned_Long'Image(Member.Minor)) ;
   end ;

   Put_Line ("") ;
   Put_Line ("") ;

   -- Free_Mem exception
   declare
      Member : Free_Mem_members ;
   begin
      Put_Line ("####### Test of Free_Mem #######") ;
      Put_Line ("I call Free_Mem_exception_test") ;
      Free_Mem_Exception_Test(MyAll_Exceptions);
   exception
      when E : Free_Mem =>
         Put_Line ("A Free_Mem exception has just been catched !!!") ;
         Corba.Get_Members (E ,Member) ;
         Put_Line ("It has a member whose value is : " &
                   Corba.Unsigned_Long'Image(Member.Minor)) ;
   end ;

   Put_Line ("") ;
   Put_Line ("") ;

   -- Inv_Ident exception
   declare
      Member : Inv_Ident_members ;
   begin
      Put_Line ("####### Test of Inv_Ident #######") ;
      Put_Line ("I call Inv_Ident_exception_test") ;
      Inv_Indent_Exception_Test(MyAll_Exceptions);
   exception
      when E : Inv_Ident =>
         Put_Line ("A Inv_Ident exception has just been catched !!!") ;
         Corba.Get_Members (E ,Member) ;
         Put_Line ("It has a member whose value is : " &
                   Corba.Unsigned_Long'Image(Member.Minor)) ;
   end ;

   Put_Line ("") ;
   Put_Line ("") ;


   -- Inv_Flag exception
   declare
      Member : Inv_Flag_members ;
   begin
      Put_Line ("####### Test of Inv_Flag #######") ;
      Put_Line ("I call Inv_Flag_exception_test") ;
      Inv_Flag_Exception_Test(MyAll_Exceptions);
   exception
      when E : Inv_Flag =>
         Put_Line ("A Inv_Flag exception has just been catched !!!") ;
         Corba.Get_Members (E ,Member) ;
         Put_Line ("It has a member whose value is : " &
                   Corba.Unsigned_Long'Image(Member.Minor)) ;
   end ;

   Put_Line ("") ;
   Put_Line ("") ;

   -- Intf_Repos exception
   declare
      Member : Intf_Repos_members ;
   begin
      Put_Line ("####### Test of Intf_Repos #######") ;
      Put_Line ("I call Intf_Repos_exception_test") ;
      Intf_Repos_Exception_Test(MyAll_Exceptions);
   exception
      when E : Intf_Repos =>
         Put_Line ("A Intf_Repos exception has just been catched !!!") ;
         Corba.Get_Members (E ,Member) ;
         Put_Line ("It has a member whose value is : " &
                   Corba.Unsigned_Long'Image(Member.Minor)) ;
   end ;

   Put_Line ("") ;
   Put_Line ("") ;

   -- Bad_Context exception
   declare
      Member : Bad_Context_members ;
   begin
      Put_Line ("####### Test of Bad_Context #######") ;
      Put_Line ("I call Bad_Context_exception_test") ;
      Bad_Context_Exception_Test(MyAll_Exceptions);
   exception
      when E : Bad_Context =>
         Put_Line ("A Bad_Context exception has just been catched !!!") ;
         Corba.Get_Members (E ,Member) ;
         Put_Line ("It has a member whose value is : " &
                   Corba.Unsigned_Long'Image(Member.Minor)) ;
   end ;

   Put_Line ("") ;
   Put_Line ("") ;

   -- Obj_Adapter exception
   declare
      Member : Obj_Adapter_members ;
   begin
      Put_Line ("####### Test of Obj_Adapter #######") ;
      Put_Line ("I call Obj_Adapter_exception_test") ;
      Obj_Adapter_Exception_Test(MyAll_Exceptions);
   exception
      when E : Obj_Adapter =>
         Put_Line ("A Obj_Adapter exception has just been catched !!!") ;
         Corba.Get_Members (E ,Member) ;
         Put_Line ("It has a member whose value is : " &
                   Corba.Unsigned_Long'Image(Member.Minor)) ;
   end ;

   Put_Line ("") ;
   Put_Line ("") ;

   -- Data_Conversion exception
   declare
      Member : Data_Conversion_members ;
   begin
      Put_Line ("####### Test of Data_Conversion #######") ;
      Put_Line ("I call Data_Conversion_exception_test") ;
      Data_Concersion_Exception_Test(MyAll_Exceptions);
   exception
      when E : Data_Conversion =>
         Put_Line ("A Data_Conversion exception has just been catched !!!") ;
         Corba.Get_Members (E ,Member) ;
         Put_Line ("It has a member whose value is : " &
                   Corba.Unsigned_Long'Image(Member.Minor)) ;
   end ;

   Put_Line ("") ;
   Put_Line ("") ;

   --  AdaBroker_Fatal_Error_exception_test : shut down the server : normal
   declare
   begin
      Put_Line ("####### Test of AdaBroker_Fatal_Error #######") ;
      Put_Line ("I call AdaBroker_Fatal_Error_exception_test") ;
--      AdaBroker_Fatal_Error_Exception_Test(MyAll_Exceptions);
   exception
      when E : AdaBroker_Fatal_Error =>
         Put_Line ("A AdaBroker_Fatal_Error exception has just been catched !!!") ;
   end ;

   Put_Line ("") ;
   Put_Line ("") ;

   --  AdaBroker_Not_Implemented_Yet_exception_test : shut down the server : normal
   declare
   begin
      Put_Line ("####### Test of AdaBroker_Not_Implemented_Yet #######") ;
      Put_Line ("I call AdaBroker_Not_Implemented_Yet_exception_test") ;
--      AdaBroker_Not_Implemented_Yet_Exception_Test(MyAll_Exceptions);
   exception
      when E : AdaBroker_Not_Implemented_Yet =>
         Put_Line ("A AdaBroker_Not_Implemented_Yet exception has just been catched !!!") ;
   end ;

   Put_Line ("") ;
   Put_Line ("") ;

   --  No_Initialisation_Error_exception_test : shut down the server : normal
   declare
   begin
      Put_Line ("####### Test of No_Initialisation_Error #######") ;
      Put_Line ("I call No_Initialisation_Error_exception_test") ;
--      No_Initialisation_Error_Exception_Test(MyAll_Exceptions);
   exception
      when E : No_Initialisation_Error =>
         Put_Line ("A No_Initialisation_Error exception has just been catched !!!") ;
   end ;

   Put_Line ("") ;
   Put_Line ("") ;

   --  C_Out_Of_Range_exception_test : shut down the server : normal
   declare
   begin
      Put_Line ("####### Test of C_Out_Of_Range #######") ;
      Put_Line ("I call C_Out_Of_Range_exception_test") ;
--      C_Out_Of_Range_Exception_Test(MyAll_Exceptions);
   exception
      when E : C_Out_Of_Range =>
         Put_Line ("A C_Out_Of_Range  exception has just been catched !!!") ;
   end ;

   Put_Line ("") ;
   Put_Line ("") ;

   --  Dummy_User_exception_test : shut down the server : normal
   declare
   begin
      Put_Line ("####### Test of Dummy_User #######") ;
      Put_Line ("I call Dummy_User_exception_test") ;
--      Dummy_User_Exception_Test(MyAll_Exceptions);
   exception
      when E : Dummy_User =>
         Put_Line ("A Dummy_User  exception has just been catched !!!") ;
   end ;

end Client ;




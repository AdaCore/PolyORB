with Ada.Command_Line;
with Ada.Text_IO;
with CORBA; use CORBA;
with CORBA.ORB;
with CORBA.Object;
with All_Exceptions; use All_Exceptions;
with Report;         use Report;

procedure Client is
   IOR : CORBA.String;
   MyAll_Exceptions : All_Exceptions.Ref;
   Ok : Boolean;
begin

   if Ada.Command_Line.Argument_Count < 1 then
      Ada.Text_IO.Put_Line ("usage : client <IOR_string_from_server>");
      return;
   end if;

   ORB.Init ("omniORB2");
   IOR := CORBA.To_CORBA_String (Ada.Command_Line.Argument (1));
   ORB.String_To_Object (IOR, MyAll_Exceptions);

   Output ("test not nil reference", not Is_Nil (MyAll_Exceptions));

   declare
      Member : Unknown_members;
   begin
      Ok := False;
      Unknown_Exception_Test (MyAll_Exceptions);
   exception
      when E : Unknown =>
         CORBA.Get_Members (E, Member);
         Ok := (Member.Completed = Completed_No) and then (Member.Minor = 100);
      when others =>
         null;
   end;
   Output ("test Unknown exception", Ok);
   declare
      Member : Bad_Param_members;
   begin
      Ok := False;
      Bad_Param_Exception_Test (MyAll_Exceptions);
   exception
      when E : Bad_Param =>
         CORBA.Get_Members (E, Member);
         Ok := (Member.Completed = Completed_Maybe) and then (Member.Minor = 101);
      when others =>
         null;
   end;
   Output ("test Bad_Param exception", Ok);
   declare
      Member : No_Memory_members;
   begin
      Ok := False;
      No_Memory_Exception_Test (MyAll_Exceptions);
   exception
      when E : No_Memory =>
         CORBA.Get_Members (E, Member);
         Ok := (Member.Completed = Completed_Yes) and then (Member.Minor = 102);
      when others =>
         null;
   end;
   Output ("test No_Memory exception", Ok);
   declare
      Member : Imp_Limit_members;
   begin
      Ok := False;
      Imp_Limit_Exception_Test (MyAll_Exceptions);
   exception
      when E : Imp_Limit =>
         CORBA.Get_Members (E, Member);
         Ok := (Member.Completed = Completed_Yes) and then (Member.Minor = 103);
      when others =>
         null;
   end;
   Output ("test Imp_Limit exception", Ok);
   declare
      Member : Inv_Objref_members;
   begin
      Ok := False;
      Inv_Objref_Exception_Test (MyAll_Exceptions);
   exception
      when E : Inv_Objref =>
         CORBA.Get_Members (E, Member);
         Ok := (Member.Completed = Completed_Yes) and then (Member.Minor = 105);
      when others =>
         null;
   end;
   Output ("test Inv_Objref exception", Ok);
   declare
      Member : No_Permission_members;
   begin
      Ok := False;
      No_Permission_Exception_Test (MyAll_Exceptions);
   exception
      when E : No_Permission =>
         CORBA.Get_Members (E, Member);
         Ok := (Member.Completed = Completed_Yes) and then (Member.Minor = 106);
      when others =>
         null;
   end;
   Output ("test No_Permission exception", Ok);
   declare
      Member : Internal_members;
   begin
      Ok := False;
      Internal_Exception_Test (MyAll_Exceptions);
   exception
      when E : Internal =>
         CORBA.Get_Members (E, Member);
         Ok := (Member.Completed = Completed_Yes) and then (Member.Minor = 107);
      when others =>
         null;
   end;
   Output ("test Internal exception", Ok);
   declare
      Member  : Marshal_members;
   begin
      Ok := False;
      Marshal_Exception_Test (MyAll_Exceptions);
   exception
      when E : Unknown =>
         Ok := True;
      when E : Marshal =>
         CORBA.Get_Members (E, Member);
         Ok := (Member.Completed = Completed_Yes) and then (Member.Minor = 108);
      when others =>
         null;
   end;
   Output ("test Marshal (or Unknown) exception", Ok);
   declare
      Member : Initialization_Failure_members;
   begin
      Ok := False;
      Initialization_Failure_Exception_Test (MyAll_Exceptions);
   exception
      when E : Initialization_Failure =>
         CORBA.Get_Members (E, Member);
         Ok := (Member.Completed = Completed_Yes) and then (Member.Minor = 109);
      when others =>
         null;
   end;
   Output ("test Initialization_Failure exception", Ok);
   declare
      Member : No_Implement_members;
   begin
      Ok := False;
      No_Implement_Exception_Test (MyAll_Exceptions);
   exception
      when E : No_Implement =>
         CORBA.Get_Members (E, Member);
         Ok := (Member.Completed = Completed_Yes) and then (Member.Minor = 110);
      when others =>
         null;
   end;
   Output ("test No_Implement exception", Ok);
   declare
      Member : Bad_Typecode_members;
   begin
      Ok := False;
      Bad_Typecode_Exception_Test (MyAll_Exceptions);
   exception
      when E : Bad_Typecode =>
         CORBA.Get_Members (E, Member);
         Ok := (Member.Completed = Completed_Yes) and then (Member.Minor = 111);
      when others =>
         null;
   end;
   Output ("test Bad_Typecode exception", Ok);
   declare
      Member : Bad_Operation_members;
   begin
      Ok := False;
      Bad_Operation_Exception_Test (MyAll_Exceptions);
   exception
      when E : Bad_Operation =>
         CORBA.Get_Members (E, Member);
         Ok := (Member.Completed = Completed_Yes) and then (Member.Minor = 112);
      when others =>
         null;
   end;
   Output ("test Bad_Operation exception", Ok);
   declare
      Member  : No_Resources_members;
   begin
      Ok := False;
      No_Resources_Exception_Test (MyAll_Exceptions);
   exception
      when E : Unknown =>
         Ok := True;
      when E : No_Resources =>
         CORBA.Get_Members (E, Member);
         Ok := (Member.Completed = Completed_Yes) and then (Member.Minor = 113);
      when others =>
         null;
   end;
   Output ("test No_Resources (or Unknown) exception", Ok);
   declare
      Member  : No_Response_members;
   begin
      Ok := False;
      No_Response_Exception_Test (MyAll_Exceptions);
   exception
      when E : Unknown =>
         Ok := True;
      when E : No_Response =>
         CORBA.Get_Members (E, Member);
         Ok := (Member.Completed = Completed_Yes) and then (Member.Minor = 114);
      when others =>
         null;
   end;
   Output ("test No_Response (or Unknown) exception", Ok);
   declare
      Member : Persist_Store_members;
   begin
      Ok := False;
      Persist_Store_Exception_Test (MyAll_Exceptions);
   exception
      when E : Persist_Store =>
         CORBA.Get_Members (E, Member);
         Ok := (Member.Completed = Completed_Yes) and then (Member.Minor = 115);
      when others =>
         null;
   end;
   Output ("test Persist_Store exception", Ok);
   declare
      Member : Bad_Inv_Order_members;
   begin
      Ok := False;
      Bad_Inv_Order_Exception_Test (MyAll_Exceptions);
   exception
      when E : Bad_Inv_Order =>
         CORBA.Get_Members (E, Member);
         Ok := (Member.Completed = Completed_Yes) and then (Member.Minor = 116);
      when others =>
         null;
   end;
   Output ("test Bad_Inv_Order exception", Ok);
   declare
      Member : Free_Mem_members;
   begin
      Ok := False;
      Free_Mem_Exception_Test (MyAll_Exceptions);
   exception
      when E : Free_Mem =>
         CORBA.Get_Members (E, Member);
         Ok := (Member.Completed = Completed_Yes) and then (Member.Minor = 118);
      when others =>
         null;
   end;
   Output ("test Free_Mem exception", Ok);
   declare
      Member : Inv_Ident_members;
   begin
      Ok := False;
      Inv_Ident_Exception_Test (MyAll_Exceptions);
   exception
      when E : Inv_Ident =>
         CORBA.Get_Members (E, Member);
         Ok := (Member.Completed = Completed_Yes) and then (Member.Minor = 119);
      when others =>
         null;
   end;
   Output ("test Inv_Ident exception", Ok);
   declare
      Member : Inv_Flag_members;
   begin
      Ok := False;
      Inv_Flag_Exception_Test (MyAll_Exceptions);
   exception
      when E : Inv_Flag =>
         CORBA.Get_Members (E, Member);
         Ok := (Member.Completed = Completed_Yes) and then (Member.Minor = 120);
      when others =>
         null;
   end;
   Output ("test Inv_Flag exception", Ok);
   declare
      Member : Intf_Repos_members;
   begin
      Ok := False;
      Intf_Repos_Exception_Test (MyAll_Exceptions);
   exception
      when E : Intf_Repos =>
         CORBA.Get_Members (E, Member);
         Ok := (Member.Completed = Completed_Yes) and then (Member.Minor = 121);
      when others =>
         null;
   end;
   Output ("test Intf_Repos exception", Ok);
   declare
      Member : Bad_Context_members;
   begin
      Ok := False;
      Bad_Context_Exception_Test (MyAll_Exceptions);
   exception
      when E : Bad_Context =>
         CORBA.Get_Members (E, Member);
         Ok := (Member.Completed = Completed_Yes) and then (Member.Minor = 122);
      when others =>
         null;
   end;
   Output ("test Bad_Context exception", Ok);
   declare
      Member : Obj_Adapter_members;
   begin
      Ok := False;
      Obj_Adapter_Exception_Test (MyAll_Exceptions);
   exception
      when E : Obj_Adapter =>
         CORBA.Get_Members (E, Member);
         Ok := (Member.Completed = Completed_Yes) and then (Member.Minor = 123);
      when others =>
         null;
   end;
   Output ("test Obj_Adapter exception", Ok);
   declare
      Member : Data_Conversion_members;
   begin
      Ok := False;
      Data_Conversion_Exception_Test (MyAll_Exceptions);
   exception
      when E : Data_Conversion =>
         CORBA.Get_Members (E, Member);
         Ok := (Member.Completed = Completed_Yes) and then (Member.Minor = 124);
      when others =>
         null;
   end;
   Output ("test Data_Conversion exception", Ok);
   declare
      Member : Object_Not_Exist_members;
   begin
      Ok := False;
      Object_Not_Exist_Exception_Test (MyAll_Exceptions);
   exception
      when E : Object_Not_Exist =>
         CORBA.Get_Members (E, Member);
         Ok := (Member.Completed = Completed_Yes) and then (Member.Minor = 125);
      when others =>
         null;
   end;
   Output ("test Object_Not_Exist exception", Ok);
   declare
      Member : Transaction_Required_members;
   begin
      Ok := False;
      Transaction_Required_Exception_Test (MyAll_Exceptions);
   exception
      when E : Transaction_Required =>
         CORBA.Get_Members (E, Member);
         Ok := (Member.Completed = Completed_Yes) and then (Member.Minor = 126);
      when others =>
         null;
   end;
   Output ("test Transaction_Required exception", Ok);
   declare
      Member : Transaction_Rolledback_members;
   begin
      Ok := False;
      Transaction_Rolledback_Exception_Test (MyAll_Exceptions);
   exception
      when E : Transaction_Rolledback =>
         CORBA.Get_Members (E, Member);
         Ok := (Member.Completed = Completed_Yes) and then (Member.Minor = 127);
      when others =>
         null;
   end;
   Output ("test Transaction_Rolledback exception", Ok);
   declare
      Member  : Invalid_Transaction_members;
   begin
      Ok := False;
      Invalid_Transaction_Exception_Test (MyAll_Exceptions);
   exception
      when E : Unknown =>
         Ok := True;
      when E : Invalid_Transaction =>
         CORBA.Get_Members (E, Member);
         Ok := (Member.Completed = Completed_Yes) and then (Member.Minor = 128);
      when others =>
         null;
   end;
   Output ("test Invalid_Transaction (or Unknown) exception", Ok);
   declare
      Member : Wrong_Transaction_members;
   begin
      Ok := False;
      Wrong_Transaction_Exception_Test (MyAll_Exceptions);
   exception
      when E : Wrong_Transaction =>
         CORBA.Get_Members (E, Member);
         Ok := (Member.Completed = Completed_Yes) and then (Member.Minor = 129);
      when others =>
         null;
   end;
   Output ("test Wrong_Transaction exception", Ok);
end Client;

------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                               C L I E N T                                --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2004-2012, Free Software Foundation, Inc.          --
--                                                                          --
-- This is free software;  you can redistribute it  and/or modify it  under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for  more details.                                               --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
--                  PolyORB is maintained by AdaCore                        --
--                     (email: sales@adacore.com)                           --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Command_Line;
with Ada.Text_IO;

with CORBA.ORB;

with PolyORB.Utils.Report;

with all_exceptions; use all_exceptions;

with PolyORB.Setup.Client;
pragma Warnings (Off, PolyORB.Setup.Client);

procedure Client is
   use CORBA;

   use PolyORB.Utils.Report;

   IOR : CORBA.String;
   MyAll_Exceptions : all_exceptions.Ref;
   Ok : Boolean;

begin
   New_Test ("CORBA Exceptions");

   CORBA.ORB.Initialize ("ORB");

   if Ada.Command_Line.Argument_Count < 1 then
      Ada.Text_IO.Put_Line ("usage : client <IOR_string_from_server>");
      return;
   end if;

   IOR := CORBA.To_CORBA_String (Ada.Command_Line.Argument (1));
   ORB.String_To_Object (IOR, MyAll_Exceptions);

   Output ("test not nil reference", not Is_Nil (MyAll_Exceptions));

   begin
      Ok := False;
      Unknown_exception_test (MyAll_Exceptions);
   exception
      when Unknown =>
         Ok := True;

      when others =>
         null;
   end;
   Output ("test Unknown exception", Ok);

   declare
      Member : Bad_Param_Members;
   begin
      Ok := False;
      Bad_Param_exception_test (MyAll_Exceptions);
   exception
      when E : Bad_Param =>
         CORBA.Get_Members (E, Member);
         Ok := (Member.Completed = Completed_Maybe)
           and then (Member.Minor = (1 or CORBA.OMGVMCID));

      when others =>
         null;
   end;
   Output ("test Bad_Param exception", Ok);

   declare
      Member : No_Memory_Members;
   begin
      Ok := False;
      No_Memory_exception_test (MyAll_Exceptions);
   exception
      when E : No_Memory =>
         CORBA.Get_Members (E, Member);
         Ok := (Member.Completed = Completed_Yes)
           and then (Member.Minor = 102);

      when others =>
         null;
   end;
   Output ("test No_Memory exception", Ok);

   declare
      Member : Imp_Limit_Members;
   begin
      Ok := False;
      Imp_Limit_exception_test (MyAll_Exceptions);
   exception
      when E : Imp_Limit =>
         CORBA.Get_Members (E, Member);
         Ok := (Member.Completed = Completed_Yes)
           and then (Member.Minor = 103);

      when others =>
         null;
   end;
   Output ("test Imp_Limit exception", Ok);

   declare
      Member : Inv_Objref_Members;
   begin
      Ok := False;
      Inv_Objref_exception_test (MyAll_Exceptions);
   exception
      when E : Inv_Objref =>
         CORBA.Get_Members (E, Member);
         Ok := (Member.Completed = Completed_Yes)
           and then (Member.Minor = 105);

      when others =>
         null;
   end;
   Output ("test Inv_Objref exception", Ok);

   declare
      Member : No_Permission_Members;
   begin
      Ok := False;
      No_Permission_exception_test (MyAll_Exceptions);
   exception
      when E : No_Permission =>
         CORBA.Get_Members (E, Member);
         Ok := (Member.Completed = Completed_Yes)
           and then (Member.Minor = 106);

      when others =>
         null;
   end;
   Output ("test No_Permission exception", Ok);

   declare
      Member : Internal_Members;
   begin
      Ok := False;
      Internal_exception_test (MyAll_Exceptions);
   exception
      when E : Internal =>
         CORBA.Get_Members (E, Member);
         Ok := (Member.Completed = Completed_Yes)
           and then (Member.Minor = 107);

      when others =>
         null;
   end;
   Output ("test Internal exception", Ok);

   declare
      Member  : Marshal_Members;
   begin
      Ok := False;
      Marshal_exception_test (MyAll_Exceptions);
   exception
      when Unknown =>
         Ok := True;

      when E : Marshal =>
         CORBA.Get_Members (E, Member);
         Ok := (Member.Completed = Completed_Yes)
           and then (Member.Minor = 108);

      when others =>
         null;
   end;
   Output ("test Marshal (or Unknown) exception", Ok);

   declare
      Member : Initialization_Failure_Members;
   begin
      Ok := False;
      Initialization_Failure_exception_test (MyAll_Exceptions);
   exception
      when E : Initialization_Failure =>
         CORBA.Get_Members (E, Member);
         Ok := (Member.Completed = Completed_Yes)
           and then (Member.Minor = 109);

      when others =>
         null;
   end;
   Output ("test Initialization_Failure exception", Ok);

   declare
      Member : No_Implement_Members;
   begin
      Ok := False;
      No_Implement_exception_test (MyAll_Exceptions);
   exception
      when E : No_Implement =>
         CORBA.Get_Members (E, Member);
         Ok := (Member.Completed = Completed_Yes)
           and then (Member.Minor = 110);

      when others =>
         null;
   end;
   Output ("test No_Implement exception", Ok);

   declare
      Member : Bad_Typecode_Members;
   begin
      Ok := False;
      Bad_Typecode_exception_test (MyAll_Exceptions);
   exception
      when E : Bad_TypeCode =>
         CORBA.Get_Members (E, Member);
         Ok := (Member.Completed = Completed_Yes)
           and then (Member.Minor = 111);

      when others =>
         null;
   end;
   Output ("test Bad_Typecode exception", Ok);

   declare
      Member : Bad_Operation_Members;
   begin
      Ok := False;
      Bad_Operation_exception_test (MyAll_Exceptions);
   exception
      when E : Bad_Operation =>
         CORBA.Get_Members (E, Member);
         Ok := (Member.Completed = Completed_Yes)
           and then (Member.Minor = 112);

      when others =>
         null;
   end;
   Output ("test Bad_Operation exception", Ok);

   declare
      Member  : No_Resources_Members;
   begin
      Ok := False;
      No_Resources_exception_test (MyAll_Exceptions);
   exception
      when Unknown =>
         Ok := True;

      when E : No_Resources =>
         CORBA.Get_Members (E, Member);
         Ok := (Member.Completed = Completed_Yes)
           and then (Member.Minor = 113);

      when others =>
         null;
   end;
   Output ("test No_Resources (or Unknown) exception", Ok);

   declare
      Member  : No_Response_Members;
   begin
      Ok := False;
      No_Response_exception_test (MyAll_Exceptions);
   exception
      when Unknown =>
         Ok := True;

      when E : No_Response =>
         CORBA.Get_Members (E, Member);
         Ok := (Member.Completed = Completed_Yes)
           and then (Member.Minor = 114);

      when others =>
         null;
   end;
   Output ("test No_Response (or Unknown) exception", Ok);

   declare
      Member : Persist_Store_Members;
   begin
      Ok := False;
      Persist_Store_exception_test (MyAll_Exceptions);
   exception
      when E : Persist_Store =>
         CORBA.Get_Members (E, Member);
         Ok := (Member.Completed = Completed_Yes)
           and then (Member.Minor = 115);

      when others =>
         null;
   end;
   Output ("test Persist_Store exception", Ok);

   declare
      Member : Bad_Inv_Order_Members;
   begin
      Ok := False;
      Bad_Inv_Order_exception_test (MyAll_Exceptions);
   exception
      when E : Bad_Inv_Order =>
         CORBA.Get_Members (E, Member);
         Ok := (Member.Completed = Completed_Yes)
           and then (Member.Minor = 116);

      when others =>
         null;
   end;
   Output ("test Bad_Inv_Order exception", Ok);

   declare
      Member : Free_Mem_Members;
   begin
      Ok := False;
      Free_Mem_exception_test (MyAll_Exceptions);
   exception
      when E : Free_Mem =>
         CORBA.Get_Members (E, Member);
         Ok := (Member.Completed = Completed_Yes)
           and then (Member.Minor = 118);

      when others =>
         null;
   end;
   Output ("test Free_Mem exception", Ok);

   declare
      Member : Inv_Ident_Members;
   begin
      Ok := False;
      Inv_Ident_exception_test (MyAll_Exceptions);
   exception
      when E : Inv_Ident =>
         CORBA.Get_Members (E, Member);
         Ok := (Member.Completed = Completed_Yes)
           and then (Member.Minor = 119);

      when others =>
         null;
   end;
   Output ("test Inv_Ident exception", Ok);

   declare
      Member : Inv_Flag_Members;
   begin
      Ok := False;
      Inv_Flag_exception_test (MyAll_Exceptions);
   exception
      when E : Inv_Flag =>
         CORBA.Get_Members (E, Member);
         Ok := (Member.Completed = Completed_Yes)
           and then (Member.Minor = 120);

      when others =>
         null;
   end;
   Output ("test Inv_Flag exception", Ok);

   declare
      Member : Intf_Repos_Members;
   begin
      Ok := False;
      Intf_Repos_exception_test (MyAll_Exceptions);
   exception
      when E : Intf_Repos =>
         CORBA.Get_Members (E, Member);
         Ok := (Member.Completed = Completed_Yes)
           and then (Member.Minor = 121);

      when others =>
         null;
   end;
   Output ("test Intf_Repos exception", Ok);

   declare
      Member : Bad_Context_Members;
   begin
      Ok := False;
      Bad_Context_exception_test (MyAll_Exceptions);
   exception
      when E : Bad_Context =>
         CORBA.Get_Members (E, Member);
         Ok := (Member.Completed = Completed_Yes)
           and then (Member.Minor = 122);

      when others =>
         null;
   end;
   Output ("test Bad_Context exception", Ok);

   declare
      Member : Obj_Adapter_Members;
   begin
      Ok := False;
      Obj_Adapter_exception_test (MyAll_Exceptions);
   exception
      when E : Obj_Adapter =>
         CORBA.Get_Members (E, Member);
         Ok := (Member.Completed = Completed_Yes)
           and then (Member.Minor = 123);

      when others =>
         null;
   end;
   Output ("test Obj_Adapter exception", Ok);

   declare
      Member : Data_Conversion_Members;
   begin
      Ok := False;
      Data_Conversion_exception_test (MyAll_Exceptions);
   exception
      when E : Data_Conversion =>
         CORBA.Get_Members (E, Member);
         Ok := (Member.Completed = Completed_Yes)
           and then (Member.Minor = 124);

      when others =>
         null;
   end;
   Output ("test Data_Conversion exception", Ok);

   declare
      Member : Object_Not_Exist_Members;
   begin
      Ok := False;
      Object_Not_Exist_exception_test (MyAll_Exceptions);
   exception
      when E : Object_Not_Exist =>
         CORBA.Get_Members (E, Member);
         Ok := (Member.Completed = Completed_Yes)
           and then (Member.Minor = 125);

      when others =>
         null;
   end;
   Output ("test Object_Not_Exist exception", Ok);

   declare
      Member : Transaction_Required_Members;
   begin
      Ok := False;
      Transaction_Required_exception_test (MyAll_Exceptions);
   exception
      when E : Transaction_Required =>
         CORBA.Get_Members (E, Member);
         Ok := (Member.Completed = Completed_Yes)
           and then (Member.Minor = 126);

      when others =>
         null;
   end;
   Output ("test Transaction_Required exception", Ok);

   declare
      Member : Transaction_Rolledback_Members;
   begin
      Ok := False;
      Transaction_Rolledback_exception_test (MyAll_Exceptions);
   exception
      when E : Transaction_Rolledback =>
         CORBA.Get_Members (E, Member);
         Ok := (Member.Completed = Completed_Yes)
           and then (Member.Minor = 127);

      when others =>
         null;
   end;
   Output ("test Transaction_Rolledback exception", Ok);

   declare
      Member  : Invalid_Transaction_Members;
   begin
      Ok := False;
      Invalid_Transaction_exception_test (MyAll_Exceptions);
   exception
      when Unknown =>
         Ok := True;
      when E : Invalid_Transaction =>
         CORBA.Get_Members (E, Member);
         Ok := (Member.Completed = Completed_Yes)
           and then (Member.Minor = 128);

      when others =>
         null;
   end;
   Output ("test Invalid_Transaction (or Unknown) exception", Ok);

   End_Report;
end Client;

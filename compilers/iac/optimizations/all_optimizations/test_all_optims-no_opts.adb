-------------------------------------------------
--  This file has been generated automatically
--  by IAC (Idl to Ada Compiler)
-------------------------------------------------
--  Do NOT hand-modify this file, as your
--  changes will be lost when you re-run the
--  Idl to Ada Compiler.
-------------------------------------------------
pragma Style_Checks
  ("NM32766");
with test_all_optims.Impl;
with CORBA;
with test_all_optims.Helper;
with CORBA.NVList;
with CORBA.ServerRequest;
with PolyORB.CORBA_P.Implicit_CORBA_Methods;
with PortableServer;
with CORBA.ORB;
with PolyORB.CORBA_P.Exceptions;
with PolyORB.Utils.Strings;
with PolyORB.Utils.Strings.Lists;
with PolyORB.Initialization;

--  Added to test skeleton Optimisation

with Ada.Real_Time;  use Ada.Real_Time;
with Statistics;     use Statistics;

package body test_all_optims.Skel is

   procedure Invoke
     (Self : in PortableServer.Servant;
      Request : in CORBA.ServerRequest.Object_Ptr);

   procedure Invoke
     (Self : in PortableServer.Servant;
      Request : in CORBA.ServerRequest.Object_Ptr)
   is
      T2 : Time;
      T1 : Time;
      Delta1 : Duration;

      Operation_Ü : constant Standard.String :=
        CORBA.To_Standard_String
           (CORBA.ServerRequest.Operation
              (Request.all));
      Argument_List_Ü : CORBA.NVList.Ref;
      String_Id : String := Operation_Ü
        (Operation_Ü'Length - 1 .. Operation_Ü'Length);
      Index_Ü : Natural;
   begin
      Index_Ü := Natural'Value (String_Id);
      T1 := Clock;
      CORBA.ORB.Create_List
        (0,
         Argument_List_Ü);
      begin
         if (Operation_Ü
            = "_is_a")
         then
            begin
               PolyORB.CORBA_P.Implicit_CORBA_Methods.Handle_Is_A
                 (Request,
                  Argument_List_Ü,
                  test_all_optims.Is_A'Access);
            end;
         elsif (Operation_Ü
            = "echoSixteenKb00")
         then
            declare
               data : test_all_optims.sixteenKb;
               Arg_Name_U_data : constant CORBA.Identifier :=
                 CORBA.To_CORBA_String
                    ("data");
               Argument_U_data : CORBA.Any :=
                 CORBA.Internals.Get_Empty_Any
                    (test_all_optims.Helper.TC_sixteenKb);
               Result_Ü : test_all_optims.sixteenKb;
            begin
               CORBA.NVList.Add_Item
                 (Argument_List_Ü,
                  Arg_Name_U_data,
                  Argument_U_data,
                  CORBA.ARG_IN);
               --  Processing request
               CORBA.ServerRequest.Arguments
                 (Request,
                  Argument_List_Ü);
               --  Setting Parameters
               data :=
                 test_all_optims.Helper.From_Any
                    (Argument_U_data);
               --  Call Implementation
               Result_Ü :=
                 test_all_optims.Impl.echoSixteenKb00
                    (test_all_optims.Impl.Object'Class
                       (Self.all)'Access,
                     data);
               --  Setting the result
               CORBA.ServerRequest.Set_Result
                 (Request,
                  test_all_optims.Helper.To_Any
                    (Result_Ü));
            end;
         elsif (Operation_Ü
            = "echoSixteenKb01")
         then
            declare
               data : test_all_optims.sixteenKb;
               Arg_Name_U_data : constant CORBA.Identifier :=
                 CORBA.To_CORBA_String
                    ("data");
               Argument_U_data : CORBA.Any :=
                 CORBA.Internals.Get_Empty_Any
                    (test_all_optims.Helper.TC_sixteenKb);
               Result_Ü : test_all_optims.sixteenKb;
            begin
               CORBA.NVList.Add_Item
                 (Argument_List_Ü,
                  Arg_Name_U_data,
                  Argument_U_data,
                  CORBA.ARG_IN);
               --  Processing request
               CORBA.ServerRequest.Arguments
                 (Request,
                  Argument_List_Ü);
               --  Setting Parameters
               data :=
                 test_all_optims.Helper.From_Any
                    (Argument_U_data);
               --  Call Implementation
               Result_Ü :=
                 test_all_optims.Impl.echoSixteenKb01
                    (test_all_optims.Impl.Object'Class
                       (Self.all)'Access,
                     data);
               --  Setting the result
               CORBA.ServerRequest.Set_Result
                 (Request,
                  test_all_optims.Helper.To_Any
                    (Result_Ü));
            end;
         elsif (Operation_Ü
            = "echoSixteenKb02")
         then
            declare
               data : test_all_optims.sixteenKb;
               Arg_Name_U_data : constant CORBA.Identifier :=
                 CORBA.To_CORBA_String
                    ("data");
               Argument_U_data : CORBA.Any :=
                 CORBA.Internals.Get_Empty_Any
                    (test_all_optims.Helper.TC_sixteenKb);
               Result_Ü : test_all_optims.sixteenKb;
            begin
               CORBA.NVList.Add_Item
                 (Argument_List_Ü,
                  Arg_Name_U_data,
                  Argument_U_data,
                  CORBA.ARG_IN);
               --  Processing request
               CORBA.ServerRequest.Arguments
                 (Request,
                  Argument_List_Ü);
               --  Setting Parameters
               data :=
                 test_all_optims.Helper.From_Any
                    (Argument_U_data);
               --  Call Implementation
               Result_Ü :=
                 test_all_optims.Impl.echoSixteenKb02
                    (test_all_optims.Impl.Object'Class
                       (Self.all)'Access,
                     data);
               --  Setting the result
               CORBA.ServerRequest.Set_Result
                 (Request,
                  test_all_optims.Helper.To_Any
                    (Result_Ü));
            end;
         elsif (Operation_Ü
            = "echoSixteenKb03")
         then
            declare
               data : test_all_optims.sixteenKb;
               Arg_Name_U_data : constant CORBA.Identifier :=
                 CORBA.To_CORBA_String
                    ("data");
               Argument_U_data : CORBA.Any :=
                 CORBA.Internals.Get_Empty_Any
                    (test_all_optims.Helper.TC_sixteenKb);
               Result_Ü : test_all_optims.sixteenKb;
            begin
               CORBA.NVList.Add_Item
                 (Argument_List_Ü,
                  Arg_Name_U_data,
                  Argument_U_data,
                  CORBA.ARG_IN);
               --  Processing request
               CORBA.ServerRequest.Arguments
                 (Request,
                  Argument_List_Ü);
               --  Setting Parameters
               data :=
                 test_all_optims.Helper.From_Any
                    (Argument_U_data);
               --  Call Implementation
               Result_Ü :=
                 test_all_optims.Impl.echoSixteenKb03
                    (test_all_optims.Impl.Object'Class
                       (Self.all)'Access,
                     data);
               --  Setting the result
               CORBA.ServerRequest.Set_Result
                 (Request,
                  test_all_optims.Helper.To_Any
                    (Result_Ü));
            end;
         elsif (Operation_Ü
            = "echoSixteenKb04")
         then
            declare
               data : test_all_optims.sixteenKb;
               Arg_Name_U_data : constant CORBA.Identifier :=
                 CORBA.To_CORBA_String
                    ("data");
               Argument_U_data : CORBA.Any :=
                 CORBA.Internals.Get_Empty_Any
                    (test_all_optims.Helper.TC_sixteenKb);
               Result_Ü : test_all_optims.sixteenKb;
            begin
               CORBA.NVList.Add_Item
                 (Argument_List_Ü,
                  Arg_Name_U_data,
                  Argument_U_data,
                  CORBA.ARG_IN);
               --  Processing request
               CORBA.ServerRequest.Arguments
                 (Request,
                  Argument_List_Ü);
               --  Setting Parameters
               data :=
                 test_all_optims.Helper.From_Any
                    (Argument_U_data);
               --  Call Implementation
               Result_Ü :=
                 test_all_optims.Impl.echoSixteenKb04
                    (test_all_optims.Impl.Object'Class
                       (Self.all)'Access,
                     data);
               --  Setting the result
               CORBA.ServerRequest.Set_Result
                 (Request,
                  test_all_optims.Helper.To_Any
                    (Result_Ü));
            end;
         elsif (Operation_Ü
            = "echoSixteenKb05")
         then
            declare
               data : test_all_optims.sixteenKb;
               Arg_Name_U_data : constant CORBA.Identifier :=
                 CORBA.To_CORBA_String
                    ("data");
               Argument_U_data : CORBA.Any :=
                 CORBA.Internals.Get_Empty_Any
                    (test_all_optims.Helper.TC_sixteenKb);
               Result_Ü : test_all_optims.sixteenKb;
            begin
               CORBA.NVList.Add_Item
                 (Argument_List_Ü,
                  Arg_Name_U_data,
                  Argument_U_data,
                  CORBA.ARG_IN);
               --  Processing request
               CORBA.ServerRequest.Arguments
                 (Request,
                  Argument_List_Ü);
               --  Setting Parameters
               data :=
                 test_all_optims.Helper.From_Any
                    (Argument_U_data);
               --  Call Implementation
               Result_Ü :=
                 test_all_optims.Impl.echoSixteenKb05
                    (test_all_optims.Impl.Object'Class
                       (Self.all)'Access,
                     data);
               --  Setting the result
               CORBA.ServerRequest.Set_Result
                 (Request,
                  test_all_optims.Helper.To_Any
                    (Result_Ü));
            end;
         elsif (Operation_Ü
            = "echoSixteenKb06")
         then
            declare
               data : test_all_optims.sixteenKb;
               Arg_Name_U_data : constant CORBA.Identifier :=
                 CORBA.To_CORBA_String
                    ("data");
               Argument_U_data : CORBA.Any :=
                 CORBA.Internals.Get_Empty_Any
                    (test_all_optims.Helper.TC_sixteenKb);
               Result_Ü : test_all_optims.sixteenKb;
            begin
               CORBA.NVList.Add_Item
                 (Argument_List_Ü,
                  Arg_Name_U_data,
                  Argument_U_data,
                  CORBA.ARG_IN);
               --  Processing request
               CORBA.ServerRequest.Arguments
                 (Request,
                  Argument_List_Ü);
               --  Setting Parameters
               data :=
                 test_all_optims.Helper.From_Any
                    (Argument_U_data);
               --  Call Implementation
               Result_Ü :=
                 test_all_optims.Impl.echoSixteenKb06
                    (test_all_optims.Impl.Object'Class
                       (Self.all)'Access,
                     data);
               --  Setting the result
               CORBA.ServerRequest.Set_Result
                 (Request,
                  test_all_optims.Helper.To_Any
                    (Result_Ü));
            end;
         elsif (Operation_Ü
            = "echoSixteenKb07")
         then
            declare
               data : test_all_optims.sixteenKb;
               Arg_Name_U_data : constant CORBA.Identifier :=
                 CORBA.To_CORBA_String
                    ("data");
               Argument_U_data : CORBA.Any :=
                 CORBA.Internals.Get_Empty_Any
                    (test_all_optims.Helper.TC_sixteenKb);
               Result_Ü : test_all_optims.sixteenKb;
            begin
               CORBA.NVList.Add_Item
                 (Argument_List_Ü,
                  Arg_Name_U_data,
                  Argument_U_data,
                  CORBA.ARG_IN);
               --  Processing request
               CORBA.ServerRequest.Arguments
                 (Request,
                  Argument_List_Ü);
               --  Setting Parameters
               data :=
                 test_all_optims.Helper.From_Any
                    (Argument_U_data);
               --  Call Implementation
               Result_Ü :=
                 test_all_optims.Impl.echoSixteenKb07
                    (test_all_optims.Impl.Object'Class
                       (Self.all)'Access,
                     data);
               --  Setting the result
               CORBA.ServerRequest.Set_Result
                 (Request,
                  test_all_optims.Helper.To_Any
                    (Result_Ü));
            end;
         elsif (Operation_Ü
            = "echoSixteenKb08")
         then
            declare
               data : test_all_optims.sixteenKb;
               Arg_Name_U_data : constant CORBA.Identifier :=
                 CORBA.To_CORBA_String
                    ("data");
               Argument_U_data : CORBA.Any :=
                 CORBA.Internals.Get_Empty_Any
                    (test_all_optims.Helper.TC_sixteenKb);
               Result_Ü : test_all_optims.sixteenKb;
            begin
               CORBA.NVList.Add_Item
                 (Argument_List_Ü,
                  Arg_Name_U_data,
                  Argument_U_data,
                  CORBA.ARG_IN);
               --  Processing request
               CORBA.ServerRequest.Arguments
                 (Request,
                  Argument_List_Ü);
               --  Setting Parameters
               data :=
                 test_all_optims.Helper.From_Any
                    (Argument_U_data);
               --  Call Implementation
               Result_Ü :=
                 test_all_optims.Impl.echoSixteenKb08
                    (test_all_optims.Impl.Object'Class
                       (Self.all)'Access,
                     data);
               --  Setting the result
               CORBA.ServerRequest.Set_Result
                 (Request,
                  test_all_optims.Helper.To_Any
                    (Result_Ü));
            end;
         elsif (Operation_Ü
            = "echoSixteenKb09")
         then
            declare
               data : test_all_optims.sixteenKb;
               Arg_Name_U_data : constant CORBA.Identifier :=
                 CORBA.To_CORBA_String
                    ("data");
               Argument_U_data : CORBA.Any :=
                 CORBA.Internals.Get_Empty_Any
                    (test_all_optims.Helper.TC_sixteenKb);
               Result_Ü : test_all_optims.sixteenKb;
            begin
               CORBA.NVList.Add_Item
                 (Argument_List_Ü,
                  Arg_Name_U_data,
                  Argument_U_data,
                  CORBA.ARG_IN);
               --  Processing request
               CORBA.ServerRequest.Arguments
                 (Request,
                  Argument_List_Ü);
               --  Setting Parameters
               data :=
                 test_all_optims.Helper.From_Any
                    (Argument_U_data);
               --  Call Implementation
               Result_Ü :=
                 test_all_optims.Impl.echoSixteenKb09
                    (test_all_optims.Impl.Object'Class
                       (Self.all)'Access,
                     data);
               --  Setting the result
               CORBA.ServerRequest.Set_Result
                 (Request,
                  test_all_optims.Helper.To_Any
                    (Result_Ü));
            end;
         elsif (Operation_Ü
            = "echoSixteenKb10")
         then
            declare
               data : test_all_optims.sixteenKb;
               Arg_Name_U_data : constant CORBA.Identifier :=
                 CORBA.To_CORBA_String
                    ("data");
               Argument_U_data : CORBA.Any :=
                 CORBA.Internals.Get_Empty_Any
                    (test_all_optims.Helper.TC_sixteenKb);
               Result_Ü : test_all_optims.sixteenKb;
            begin
               CORBA.NVList.Add_Item
                 (Argument_List_Ü,
                  Arg_Name_U_data,
                  Argument_U_data,
                  CORBA.ARG_IN);
               --  Processing request
               CORBA.ServerRequest.Arguments
                 (Request,
                  Argument_List_Ü);
               --  Setting Parameters
               data :=
                 test_all_optims.Helper.From_Any
                    (Argument_U_data);
               --  Call Implementation
               Result_Ü :=
                 test_all_optims.Impl.echoSixteenKb10
                    (test_all_optims.Impl.Object'Class
                       (Self.all)'Access,
                     data);
               --  Setting the result
               CORBA.ServerRequest.Set_Result
                 (Request,
                  test_all_optims.Helper.To_Any
                    (Result_Ü));
            end;
         elsif (Operation_Ü
            = "echoSixteenKb11")
         then
            declare
               data : test_all_optims.sixteenKb;
               Arg_Name_U_data : constant CORBA.Identifier :=
                 CORBA.To_CORBA_String
                    ("data");
               Argument_U_data : CORBA.Any :=
                 CORBA.Internals.Get_Empty_Any
                    (test_all_optims.Helper.TC_sixteenKb);
               Result_Ü : test_all_optims.sixteenKb;
            begin
               CORBA.NVList.Add_Item
                 (Argument_List_Ü,
                  Arg_Name_U_data,
                  Argument_U_data,
                  CORBA.ARG_IN);
               --  Processing request
               CORBA.ServerRequest.Arguments
                 (Request,
                  Argument_List_Ü);
               --  Setting Parameters
               data :=
                 test_all_optims.Helper.From_Any
                    (Argument_U_data);
               --  Call Implementation
               Result_Ü :=
                 test_all_optims.Impl.echoSixteenKb11
                    (test_all_optims.Impl.Object'Class
                       (Self.all)'Access,
                     data);
               --  Setting the result
               CORBA.ServerRequest.Set_Result
                 (Request,
                  test_all_optims.Helper.To_Any
                    (Result_Ü));
            end;
         elsif (Operation_Ü
            = "echoSixteenKb12")
         then
            declare
               data : test_all_optims.sixteenKb;
               Arg_Name_U_data : constant CORBA.Identifier :=
                 CORBA.To_CORBA_String
                    ("data");
               Argument_U_data : CORBA.Any :=
                 CORBA.Internals.Get_Empty_Any
                    (test_all_optims.Helper.TC_sixteenKb);
               Result_Ü : test_all_optims.sixteenKb;
            begin
               CORBA.NVList.Add_Item
                 (Argument_List_Ü,
                  Arg_Name_U_data,
                  Argument_U_data,
                  CORBA.ARG_IN);
               --  Processing request
               CORBA.ServerRequest.Arguments
                 (Request,
                  Argument_List_Ü);
               --  Setting Parameters
               data :=
                 test_all_optims.Helper.From_Any
                    (Argument_U_data);
               --  Call Implementation
               Result_Ü :=
                 test_all_optims.Impl.echoSixteenKb12
                    (test_all_optims.Impl.Object'Class
                       (Self.all)'Access,
                     data);
               --  Setting the result
               CORBA.ServerRequest.Set_Result
                 (Request,
                  test_all_optims.Helper.To_Any
                    (Result_Ü));
            end;
         elsif (Operation_Ü
            = "echoSixteenKb13")
         then
            declare
               data : test_all_optims.sixteenKb;
               Arg_Name_U_data : constant CORBA.Identifier :=
                 CORBA.To_CORBA_String
                    ("data");
               Argument_U_data : CORBA.Any :=
                 CORBA.Internals.Get_Empty_Any
                    (test_all_optims.Helper.TC_sixteenKb);
               Result_Ü : test_all_optims.sixteenKb;
            begin
               CORBA.NVList.Add_Item
                 (Argument_List_Ü,
                  Arg_Name_U_data,
                  Argument_U_data,
                  CORBA.ARG_IN);
               --  Processing request
               CORBA.ServerRequest.Arguments
                 (Request,
                  Argument_List_Ü);
               --  Setting Parameters
               data :=
                 test_all_optims.Helper.From_Any
                    (Argument_U_data);
               --  Call Implementation
               Result_Ü :=
                 test_all_optims.Impl.echoSixteenKb13
                    (test_all_optims.Impl.Object'Class
                       (Self.all)'Access,
                     data);
               --  Setting the result
               CORBA.ServerRequest.Set_Result
                 (Request,
                  test_all_optims.Helper.To_Any
                    (Result_Ü));
            end;
         elsif (Operation_Ü
            = "echoSixteenKb14")
         then
            declare
               data : test_all_optims.sixteenKb;
               Arg_Name_U_data : constant CORBA.Identifier :=
                 CORBA.To_CORBA_String
                    ("data");
               Argument_U_data : CORBA.Any :=
                 CORBA.Internals.Get_Empty_Any
                    (test_all_optims.Helper.TC_sixteenKb);
               Result_Ü : test_all_optims.sixteenKb;
            begin
               CORBA.NVList.Add_Item
                 (Argument_List_Ü,
                  Arg_Name_U_data,
                  Argument_U_data,
                  CORBA.ARG_IN);
               --  Processing request
               CORBA.ServerRequest.Arguments
                 (Request,
                  Argument_List_Ü);
               --  Setting Parameters
               data :=
                 test_all_optims.Helper.From_Any
                    (Argument_U_data);
               --  Call Implementation
               Result_Ü :=
                 test_all_optims.Impl.echoSixteenKb14
                    (test_all_optims.Impl.Object'Class
                       (Self.all)'Access,
                     data);
               --  Setting the result
               CORBA.ServerRequest.Set_Result
                 (Request,
                  test_all_optims.Helper.To_Any
                    (Result_Ü));
            end;
         elsif (Operation_Ü
            = "echoSixteenKb15")
         then
            declare
               data : test_all_optims.sixteenKb;
               Arg_Name_U_data : constant CORBA.Identifier :=
                 CORBA.To_CORBA_String
                    ("data");
               Argument_U_data : CORBA.Any :=
                 CORBA.Internals.Get_Empty_Any
                    (test_all_optims.Helper.TC_sixteenKb);
               Result_Ü : test_all_optims.sixteenKb;
            begin
               CORBA.NVList.Add_Item
                 (Argument_List_Ü,
                  Arg_Name_U_data,
                  Argument_U_data,
                  CORBA.ARG_IN);
               --  Processing request
               CORBA.ServerRequest.Arguments
                 (Request,
                  Argument_List_Ü);
               --  Setting Parameters
               data :=
                 test_all_optims.Helper.From_Any
                    (Argument_U_data);
               --  Call Implementation
               Result_Ü :=
                 test_all_optims.Impl.echoSixteenKb15
                    (test_all_optims.Impl.Object'Class
                       (Self.all)'Access,
                     data);
               --  Setting the result
               CORBA.ServerRequest.Set_Result
                 (Request,
                  test_all_optims.Helper.To_Any
                    (Result_Ü));
            end;
         elsif (Operation_Ü
            = "echoSixteenKb16")
         then
            declare
               data : test_all_optims.sixteenKb;
               Arg_Name_U_data : constant CORBA.Identifier :=
                 CORBA.To_CORBA_String
                    ("data");
               Argument_U_data : CORBA.Any :=
                 CORBA.Internals.Get_Empty_Any
                    (test_all_optims.Helper.TC_sixteenKb);
               Result_Ü : test_all_optims.sixteenKb;
            begin
               CORBA.NVList.Add_Item
                 (Argument_List_Ü,
                  Arg_Name_U_data,
                  Argument_U_data,
                  CORBA.ARG_IN);
               --  Processing request
               CORBA.ServerRequest.Arguments
                 (Request,
                  Argument_List_Ü);
               --  Setting Parameters
               data :=
                 test_all_optims.Helper.From_Any
                    (Argument_U_data);
               --  Call Implementation
               Result_Ü :=
                 test_all_optims.Impl.echoSixteenKb16
                    (test_all_optims.Impl.Object'Class
                       (Self.all)'Access,
                     data);
               --  Setting the result
               CORBA.ServerRequest.Set_Result
                 (Request,
                  test_all_optims.Helper.To_Any
                    (Result_Ü));
            end;
         elsif (Operation_Ü
            = "echoSixteenKb17")
         then
            declare
               data : test_all_optims.sixteenKb;
               Arg_Name_U_data : constant CORBA.Identifier :=
                 CORBA.To_CORBA_String
                    ("data");
               Argument_U_data : CORBA.Any :=
                 CORBA.Internals.Get_Empty_Any
                    (test_all_optims.Helper.TC_sixteenKb);
               Result_Ü : test_all_optims.sixteenKb;
            begin
               CORBA.NVList.Add_Item
                 (Argument_List_Ü,
                  Arg_Name_U_data,
                  Argument_U_data,
                  CORBA.ARG_IN);
               --  Processing request
               CORBA.ServerRequest.Arguments
                 (Request,
                  Argument_List_Ü);
               --  Setting Parameters
               data :=
                 test_all_optims.Helper.From_Any
                    (Argument_U_data);
               --  Call Implementation
               Result_Ü :=
                 test_all_optims.Impl.echoSixteenKb17
                    (test_all_optims.Impl.Object'Class
                       (Self.all)'Access,
                     data);
               --  Setting the result
               CORBA.ServerRequest.Set_Result
                 (Request,
                  test_all_optims.Helper.To_Any
                    (Result_Ü));
            end;
         elsif (Operation_Ü
            = "echoSixteenKb18")
         then
            declare
               data : test_all_optims.sixteenKb;
               Arg_Name_U_data : constant CORBA.Identifier :=
                 CORBA.To_CORBA_String
                    ("data");
               Argument_U_data : CORBA.Any :=
                 CORBA.Internals.Get_Empty_Any
                    (test_all_optims.Helper.TC_sixteenKb);
               Result_Ü : test_all_optims.sixteenKb;
            begin
               CORBA.NVList.Add_Item
                 (Argument_List_Ü,
                  Arg_Name_U_data,
                  Argument_U_data,
                  CORBA.ARG_IN);
               --  Processing request
               CORBA.ServerRequest.Arguments
                 (Request,
                  Argument_List_Ü);
               --  Setting Parameters
               data :=
                 test_all_optims.Helper.From_Any
                    (Argument_U_data);
               --  Call Implementation
               Result_Ü :=
                 test_all_optims.Impl.echoSixteenKb18
                    (test_all_optims.Impl.Object'Class
                       (Self.all)'Access,
                     data);
               --  Setting the result
               CORBA.ServerRequest.Set_Result
                 (Request,
                  test_all_optims.Helper.To_Any
                    (Result_Ü));
            end;
         elsif (Operation_Ü
            = "echoSixteenKb19")
         then
            declare
               data : test_all_optims.sixteenKb;
               Arg_Name_U_data : constant CORBA.Identifier :=
                 CORBA.To_CORBA_String
                    ("data");
               Argument_U_data : CORBA.Any :=
                 CORBA.Internals.Get_Empty_Any
                    (test_all_optims.Helper.TC_sixteenKb);
               Result_Ü : test_all_optims.sixteenKb;
            begin
               CORBA.NVList.Add_Item
                 (Argument_List_Ü,
                  Arg_Name_U_data,
                  Argument_U_data,
                  CORBA.ARG_IN);
               --  Processing request
               CORBA.ServerRequest.Arguments
                 (Request,
                  Argument_List_Ü);
               --  Setting Parameters
               data :=
                 test_all_optims.Helper.From_Any
                    (Argument_U_data);
               --  Call Implementation
               Result_Ü :=
                 test_all_optims.Impl.echoSixteenKb19
                    (test_all_optims.Impl.Object'Class
                       (Self.all)'Access,
                     data);
               --  Setting the result
               CORBA.ServerRequest.Set_Result
                 (Request,
                  test_all_optims.Helper.To_Any
                    (Result_Ü));
            end;
         elsif (Operation_Ü
            = "echoSixteenKb20")
         then
            declare
               data : test_all_optims.sixteenKb;
               Arg_Name_U_data : constant CORBA.Identifier :=
                 CORBA.To_CORBA_String
                    ("data");
               Argument_U_data : CORBA.Any :=
                 CORBA.Internals.Get_Empty_Any
                    (test_all_optims.Helper.TC_sixteenKb);
               Result_Ü : test_all_optims.sixteenKb;
            begin
               CORBA.NVList.Add_Item
                 (Argument_List_Ü,
                  Arg_Name_U_data,
                  Argument_U_data,
                  CORBA.ARG_IN);
               --  Processing request
               CORBA.ServerRequest.Arguments
                 (Request,
                  Argument_List_Ü);
               --  Setting Parameters
               data :=
                 test_all_optims.Helper.From_Any
                    (Argument_U_data);
               --  Call Implementation
               Result_Ü :=
                 test_all_optims.Impl.echoSixteenKb20
                    (test_all_optims.Impl.Object'Class
                       (Self.all)'Access,
                     data);
               --  Setting the result
               CORBA.ServerRequest.Set_Result
                 (Request,
                  test_all_optims.Helper.To_Any
                    (Result_Ü));
            end;
         elsif (Operation_Ü
            = "echoSixteenKb21")
         then
            declare
               data : test_all_optims.sixteenKb;
               Arg_Name_U_data : constant CORBA.Identifier :=
                 CORBA.To_CORBA_String
                    ("data");
               Argument_U_data : CORBA.Any :=
                 CORBA.Internals.Get_Empty_Any
                    (test_all_optims.Helper.TC_sixteenKb);
               Result_Ü : test_all_optims.sixteenKb;
            begin
               CORBA.NVList.Add_Item
                 (Argument_List_Ü,
                  Arg_Name_U_data,
                  Argument_U_data,
                  CORBA.ARG_IN);
               --  Processing request
               CORBA.ServerRequest.Arguments
                 (Request,
                  Argument_List_Ü);
               --  Setting Parameters
               data :=
                 test_all_optims.Helper.From_Any
                    (Argument_U_data);
               --  Call Implementation
               Result_Ü :=
                 test_all_optims.Impl.echoSixteenKb21
                    (test_all_optims.Impl.Object'Class
                       (Self.all)'Access,
                     data);
               --  Setting the result
               CORBA.ServerRequest.Set_Result
                 (Request,
                  test_all_optims.Helper.To_Any
                    (Result_Ü));
            end;
         elsif (Operation_Ü
            = "echoSixteenKb22")
         then
            declare
               data : test_all_optims.sixteenKb;
               Arg_Name_U_data : constant CORBA.Identifier :=
                 CORBA.To_CORBA_String
                    ("data");
               Argument_U_data : CORBA.Any :=
                 CORBA.Internals.Get_Empty_Any
                    (test_all_optims.Helper.TC_sixteenKb);
               Result_Ü : test_all_optims.sixteenKb;
            begin
               CORBA.NVList.Add_Item
                 (Argument_List_Ü,
                  Arg_Name_U_data,
                  Argument_U_data,
                  CORBA.ARG_IN);
               --  Processing request
               CORBA.ServerRequest.Arguments
                 (Request,
                  Argument_List_Ü);
               --  Setting Parameters
               data :=
                 test_all_optims.Helper.From_Any
                    (Argument_U_data);
               --  Call Implementation
               Result_Ü :=
                 test_all_optims.Impl.echoSixteenKb22
                    (test_all_optims.Impl.Object'Class
                       (Self.all)'Access,
                     data);
               --  Setting the result
               CORBA.ServerRequest.Set_Result
                 (Request,
                  test_all_optims.Helper.To_Any
                    (Result_Ü));
            end;
         elsif (Operation_Ü
            = "echoSixteenKb23")
         then
            declare
               data : test_all_optims.sixteenKb;
               Arg_Name_U_data : constant CORBA.Identifier :=
                 CORBA.To_CORBA_String
                    ("data");
               Argument_U_data : CORBA.Any :=
                 CORBA.Internals.Get_Empty_Any
                    (test_all_optims.Helper.TC_sixteenKb);
               Result_Ü : test_all_optims.sixteenKb;
            begin
               CORBA.NVList.Add_Item
                 (Argument_List_Ü,
                  Arg_Name_U_data,
                  Argument_U_data,
                  CORBA.ARG_IN);
               --  Processing request
               CORBA.ServerRequest.Arguments
                 (Request,
                  Argument_List_Ü);
               --  Setting Parameters
               data :=
                 test_all_optims.Helper.From_Any
                    (Argument_U_data);
               --  Call Implementation
               Result_Ü :=
                 test_all_optims.Impl.echoSixteenKb23
                    (test_all_optims.Impl.Object'Class
                       (Self.all)'Access,
                     data);
               --  Setting the result
               CORBA.ServerRequest.Set_Result
                 (Request,
                  test_all_optims.Helper.To_Any
                    (Result_Ü));
            end;
         elsif (Operation_Ü
            = "echoSixteenKb24")
         then
            declare
               data : test_all_optims.sixteenKb;
               Arg_Name_U_data : constant CORBA.Identifier :=
                 CORBA.To_CORBA_String
                    ("data");
               Argument_U_data : CORBA.Any :=
                 CORBA.Internals.Get_Empty_Any
                    (test_all_optims.Helper.TC_sixteenKb);
               Result_Ü : test_all_optims.sixteenKb;
            begin
               CORBA.NVList.Add_Item
                 (Argument_List_Ü,
                  Arg_Name_U_data,
                  Argument_U_data,
                  CORBA.ARG_IN);
               --  Processing request
               CORBA.ServerRequest.Arguments
                 (Request,
                  Argument_List_Ü);
               --  Setting Parameters
               data :=
                 test_all_optims.Helper.From_Any
                    (Argument_U_data);
               --  Call Implementation
               Result_Ü :=
                 test_all_optims.Impl.echoSixteenKb24
                    (test_all_optims.Impl.Object'Class
                       (Self.all)'Access,
                     data);
               --  Setting the result
               CORBA.ServerRequest.Set_Result
                 (Request,
                  test_all_optims.Helper.To_Any
                    (Result_Ü));
            end;
         elsif (Operation_Ü
            = "echoSixteenKb25")
         then
            declare
               data : test_all_optims.sixteenKb;
               Arg_Name_U_data : constant CORBA.Identifier :=
                 CORBA.To_CORBA_String
                    ("data");
               Argument_U_data : CORBA.Any :=
                 CORBA.Internals.Get_Empty_Any
                    (test_all_optims.Helper.TC_sixteenKb);
               Result_Ü : test_all_optims.sixteenKb;
            begin
               CORBA.NVList.Add_Item
                 (Argument_List_Ü,
                  Arg_Name_U_data,
                  Argument_U_data,
                  CORBA.ARG_IN);
               --  Processing request
               CORBA.ServerRequest.Arguments
                 (Request,
                  Argument_List_Ü);
               --  Setting Parameters
               data :=
                 test_all_optims.Helper.From_Any
                    (Argument_U_data);
               --  Call Implementation
               Result_Ü :=
                 test_all_optims.Impl.echoSixteenKb25
                    (test_all_optims.Impl.Object'Class
                       (Self.all)'Access,
                     data);
               --  Setting the result
               CORBA.ServerRequest.Set_Result
                 (Request,
                  test_all_optims.Helper.To_Any
                    (Result_Ü));
            end;
         elsif (Operation_Ü
            = "echoSixteenKb26")
         then
            declare
               data : test_all_optims.sixteenKb;
               Arg_Name_U_data : constant CORBA.Identifier :=
                 CORBA.To_CORBA_String
                    ("data");
               Argument_U_data : CORBA.Any :=
                 CORBA.Internals.Get_Empty_Any
                    (test_all_optims.Helper.TC_sixteenKb);
               Result_Ü : test_all_optims.sixteenKb;
            begin
               CORBA.NVList.Add_Item
                 (Argument_List_Ü,
                  Arg_Name_U_data,
                  Argument_U_data,
                  CORBA.ARG_IN);
               --  Processing request
               CORBA.ServerRequest.Arguments
                 (Request,
                  Argument_List_Ü);
               --  Setting Parameters
               data :=
                 test_all_optims.Helper.From_Any
                    (Argument_U_data);
               --  Call Implementation
               Result_Ü :=
                 test_all_optims.Impl.echoSixteenKb26
                    (test_all_optims.Impl.Object'Class
                       (Self.all)'Access,
                     data);
               --  Setting the result
               CORBA.ServerRequest.Set_Result
                 (Request,
                  test_all_optims.Helper.To_Any
                    (Result_Ü));
            end;
         elsif (Operation_Ü
            = "echoSixteenKb27")
         then
            declare
               data : test_all_optims.sixteenKb;
               Arg_Name_U_data : constant CORBA.Identifier :=
                 CORBA.To_CORBA_String
                    ("data");
               Argument_U_data : CORBA.Any :=
                 CORBA.Internals.Get_Empty_Any
                    (test_all_optims.Helper.TC_sixteenKb);
               Result_Ü : test_all_optims.sixteenKb;
            begin
               CORBA.NVList.Add_Item
                 (Argument_List_Ü,
                  Arg_Name_U_data,
                  Argument_U_data,
                  CORBA.ARG_IN);
               --  Processing request
               CORBA.ServerRequest.Arguments
                 (Request,
                  Argument_List_Ü);
               --  Setting Parameters
               data :=
                 test_all_optims.Helper.From_Any
                    (Argument_U_data);
               --  Call Implementation
               Result_Ü :=
                 test_all_optims.Impl.echoSixteenKb27
                    (test_all_optims.Impl.Object'Class
                       (Self.all)'Access,
                     data);
               --  Setting the result
               CORBA.ServerRequest.Set_Result
                 (Request,
                  test_all_optims.Helper.To_Any
                    (Result_Ü));
            end;
         elsif (Operation_Ü
            = "echoSixteenKb28")
         then
            declare
               data : test_all_optims.sixteenKb;
               Arg_Name_U_data : constant CORBA.Identifier :=
                 CORBA.To_CORBA_String
                    ("data");
               Argument_U_data : CORBA.Any :=
                 CORBA.Internals.Get_Empty_Any
                    (test_all_optims.Helper.TC_sixteenKb);
               Result_Ü : test_all_optims.sixteenKb;
            begin
               CORBA.NVList.Add_Item
                 (Argument_List_Ü,
                  Arg_Name_U_data,
                  Argument_U_data,
                  CORBA.ARG_IN);
               --  Processing request
               CORBA.ServerRequest.Arguments
                 (Request,
                  Argument_List_Ü);
               --  Setting Parameters
               data :=
                 test_all_optims.Helper.From_Any
                    (Argument_U_data);
               --  Call Implementation
               Result_Ü :=
                 test_all_optims.Impl.echoSixteenKb28
                    (test_all_optims.Impl.Object'Class
                       (Self.all)'Access,
                     data);
               --  Setting the result
               CORBA.ServerRequest.Set_Result
                 (Request,
                  test_all_optims.Helper.To_Any
                    (Result_Ü));
            end;
         elsif (Operation_Ü
            = "echoSixteenKb29")
         then
            declare
               data : test_all_optims.sixteenKb;
               Arg_Name_U_data : constant CORBA.Identifier :=
                 CORBA.To_CORBA_String
                    ("data");
               Argument_U_data : CORBA.Any :=
                 CORBA.Internals.Get_Empty_Any
                    (test_all_optims.Helper.TC_sixteenKb);
               Result_Ü : test_all_optims.sixteenKb;
            begin
               CORBA.NVList.Add_Item
                 (Argument_List_Ü,
                  Arg_Name_U_data,
                  Argument_U_data,
                  CORBA.ARG_IN);
               --  Processing request
               CORBA.ServerRequest.Arguments
                 (Request,
                  Argument_List_Ü);
               --  Setting Parameters
               data :=
                 test_all_optims.Helper.From_Any
                    (Argument_U_data);
               --  Call Implementation
               Result_Ü :=
                 test_all_optims.Impl.echoSixteenKb29
                    (test_all_optims.Impl.Object'Class
                       (Self.all)'Access,
                     data);
               --  Setting the result
               CORBA.ServerRequest.Set_Result
                 (Request,
                  test_all_optims.Helper.To_Any
                    (Result_Ü));
            end;
         elsif (Operation_Ü
            = "echoSixteenKb30")
         then
            declare
               data : test_all_optims.sixteenKb;
               Arg_Name_U_data : constant CORBA.Identifier :=
                 CORBA.To_CORBA_String
                    ("data");
               Argument_U_data : CORBA.Any :=
                 CORBA.Internals.Get_Empty_Any
                    (test_all_optims.Helper.TC_sixteenKb);
               Result_Ü : test_all_optims.sixteenKb;
            begin
               CORBA.NVList.Add_Item
                 (Argument_List_Ü,
                  Arg_Name_U_data,
                  Argument_U_data,
                  CORBA.ARG_IN);
               --  Processing request
               CORBA.ServerRequest.Arguments
                 (Request,
                  Argument_List_Ü);
               --  Setting Parameters
               data :=
                 test_all_optims.Helper.From_Any
                    (Argument_U_data);
               --  Call Implementation
               Result_Ü :=
                 test_all_optims.Impl.echoSixteenKb30
                    (test_all_optims.Impl.Object'Class
                       (Self.all)'Access,
                     data);
               --  Setting the result
               CORBA.ServerRequest.Set_Result
                 (Request,
                  test_all_optims.Helper.To_Any
                    (Result_Ü));
            end;
         elsif (Operation_Ü
            = "echoSixteenKb31")
         then
            declare
               data : test_all_optims.sixteenKb;
               Arg_Name_U_data : constant CORBA.Identifier :=
                 CORBA.To_CORBA_String
                    ("data");
               Argument_U_data : CORBA.Any :=
                 CORBA.Internals.Get_Empty_Any
                    (test_all_optims.Helper.TC_sixteenKb);
               Result_Ü : test_all_optims.sixteenKb;
            begin
               CORBA.NVList.Add_Item
                 (Argument_List_Ü,
                  Arg_Name_U_data,
                  Argument_U_data,
                  CORBA.ARG_IN);
               --  Processing request
               CORBA.ServerRequest.Arguments
                 (Request,
                  Argument_List_Ü);
               --  Setting Parameters
               data :=
                 test_all_optims.Helper.From_Any
                    (Argument_U_data);
               --  Call Implementation
               Result_Ü :=
                 test_all_optims.Impl.echoSixteenKb31
                    (test_all_optims.Impl.Object'Class
                       (Self.all)'Access,
                     data);
               --  Setting the result
               CORBA.ServerRequest.Set_Result
                 (Request,
                  test_all_optims.Helper.To_Any
                    (Result_Ü));
            end;
         elsif (Operation_Ü
            = "echoSixteenKb32")
         then
            declare
               data : test_all_optims.sixteenKb;
               Arg_Name_U_data : constant CORBA.Identifier :=
                 CORBA.To_CORBA_String
                    ("data");
               Argument_U_data : CORBA.Any :=
                 CORBA.Internals.Get_Empty_Any
                    (test_all_optims.Helper.TC_sixteenKb);
               Result_Ü : test_all_optims.sixteenKb;
            begin
               CORBA.NVList.Add_Item
                 (Argument_List_Ü,
                  Arg_Name_U_data,
                  Argument_U_data,
                  CORBA.ARG_IN);
               --  Processing request
               CORBA.ServerRequest.Arguments
                 (Request,
                  Argument_List_Ü);
               --  Setting Parameters
               data :=
                 test_all_optims.Helper.From_Any
                    (Argument_U_data);
               --  Call Implementation
               Result_Ü :=
                 test_all_optims.Impl.echoSixteenKb32
                    (test_all_optims.Impl.Object'Class
                       (Self.all)'Access,
                     data);
               --  Setting the result
               CORBA.ServerRequest.Set_Result
                 (Request,
                  test_all_optims.Helper.To_Any
                    (Result_Ü));
            end;
         elsif (Operation_Ü
            = "echoSixteenKb33")
         then
            declare
               data : test_all_optims.sixteenKb;
               Arg_Name_U_data : constant CORBA.Identifier :=
                 CORBA.To_CORBA_String
                    ("data");
               Argument_U_data : CORBA.Any :=
                 CORBA.Internals.Get_Empty_Any
                    (test_all_optims.Helper.TC_sixteenKb);
               Result_Ü : test_all_optims.sixteenKb;
            begin
               CORBA.NVList.Add_Item
                 (Argument_List_Ü,
                  Arg_Name_U_data,
                  Argument_U_data,
                  CORBA.ARG_IN);
               --  Processing request
               CORBA.ServerRequest.Arguments
                 (Request,
                  Argument_List_Ü);
               --  Setting Parameters
               data :=
                 test_all_optims.Helper.From_Any
                    (Argument_U_data);
               --  Call Implementation
               Result_Ü :=
                 test_all_optims.Impl.echoSixteenKb33
                    (test_all_optims.Impl.Object'Class
                       (Self.all)'Access,
                     data);
               --  Setting the result
               CORBA.ServerRequest.Set_Result
                 (Request,
                  test_all_optims.Helper.To_Any
                    (Result_Ü));
            end;
         elsif (Operation_Ü
            = "echoSixteenKb34")
         then
            declare
               data : test_all_optims.sixteenKb;
               Arg_Name_U_data : constant CORBA.Identifier :=
                 CORBA.To_CORBA_String
                    ("data");
               Argument_U_data : CORBA.Any :=
                 CORBA.Internals.Get_Empty_Any
                    (test_all_optims.Helper.TC_sixteenKb);
               Result_Ü : test_all_optims.sixteenKb;
            begin
               CORBA.NVList.Add_Item
                 (Argument_List_Ü,
                  Arg_Name_U_data,
                  Argument_U_data,
                  CORBA.ARG_IN);
               --  Processing request
               CORBA.ServerRequest.Arguments
                 (Request,
                  Argument_List_Ü);
               --  Setting Parameters
               data :=
                 test_all_optims.Helper.From_Any
                    (Argument_U_data);
               --  Call Implementation
               Result_Ü :=
                 test_all_optims.Impl.echoSixteenKb34
                    (test_all_optims.Impl.Object'Class
                       (Self.all)'Access,
                     data);
               --  Setting the result
               CORBA.ServerRequest.Set_Result
                 (Request,
                  test_all_optims.Helper.To_Any
                    (Result_Ü));
            end;
         elsif (Operation_Ü
            = "echoSixteenKb35")
         then
            declare
               data : test_all_optims.sixteenKb;
               Arg_Name_U_data : constant CORBA.Identifier :=
                 CORBA.To_CORBA_String
                    ("data");
               Argument_U_data : CORBA.Any :=
                 CORBA.Internals.Get_Empty_Any
                    (test_all_optims.Helper.TC_sixteenKb);
               Result_Ü : test_all_optims.sixteenKb;
            begin
               CORBA.NVList.Add_Item
                 (Argument_List_Ü,
                  Arg_Name_U_data,
                  Argument_U_data,
                  CORBA.ARG_IN);
               --  Processing request
               CORBA.ServerRequest.Arguments
                 (Request,
                  Argument_List_Ü);
               --  Setting Parameters
               data :=
                 test_all_optims.Helper.From_Any
                    (Argument_U_data);
               --  Call Implementation
               Result_Ü :=
                 test_all_optims.Impl.echoSixteenKb35
                    (test_all_optims.Impl.Object'Class
                       (Self.all)'Access,
                     data);
               --  Setting the result
               CORBA.ServerRequest.Set_Result
                 (Request,
                  test_all_optims.Helper.To_Any
                    (Result_Ü));
            end;
         elsif (Operation_Ü
            = "echoSixteenKb36")
         then
            declare
               data : test_all_optims.sixteenKb;
               Arg_Name_U_data : constant CORBA.Identifier :=
                 CORBA.To_CORBA_String
                    ("data");
               Argument_U_data : CORBA.Any :=
                 CORBA.Internals.Get_Empty_Any
                    (test_all_optims.Helper.TC_sixteenKb);
               Result_Ü : test_all_optims.sixteenKb;
            begin
               CORBA.NVList.Add_Item
                 (Argument_List_Ü,
                  Arg_Name_U_data,
                  Argument_U_data,
                  CORBA.ARG_IN);
               --  Processing request
               CORBA.ServerRequest.Arguments
                 (Request,
                  Argument_List_Ü);
               --  Setting Parameters
               data :=
                 test_all_optims.Helper.From_Any
                    (Argument_U_data);
               --  Call Implementation
               Result_Ü :=
                 test_all_optims.Impl.echoSixteenKb36
                    (test_all_optims.Impl.Object'Class
                       (Self.all)'Access,
                     data);
               --  Setting the result
               CORBA.ServerRequest.Set_Result
                 (Request,
                  test_all_optims.Helper.To_Any
                    (Result_Ü));
            end;
         elsif (Operation_Ü
            = "echoSixteenKb37")
         then
            declare
               data : test_all_optims.sixteenKb;
               Arg_Name_U_data : constant CORBA.Identifier :=
                 CORBA.To_CORBA_String
                    ("data");
               Argument_U_data : CORBA.Any :=
                 CORBA.Internals.Get_Empty_Any
                    (test_all_optims.Helper.TC_sixteenKb);
               Result_Ü : test_all_optims.sixteenKb;
            begin
               CORBA.NVList.Add_Item
                 (Argument_List_Ü,
                  Arg_Name_U_data,
                  Argument_U_data,
                  CORBA.ARG_IN);
               --  Processing request
               CORBA.ServerRequest.Arguments
                 (Request,
                  Argument_List_Ü);
               --  Setting Parameters
               data :=
                 test_all_optims.Helper.From_Any
                    (Argument_U_data);
               --  Call Implementation
               Result_Ü :=
                 test_all_optims.Impl.echoSixteenKb37
                    (test_all_optims.Impl.Object'Class
                       (Self.all)'Access,
                     data);
               --  Setting the result
               CORBA.ServerRequest.Set_Result
                 (Request,
                  test_all_optims.Helper.To_Any
                    (Result_Ü));
            end;
         elsif (Operation_Ü
            = "echoSixteenKb38")
         then
            declare
               data : test_all_optims.sixteenKb;
               Arg_Name_U_data : constant CORBA.Identifier :=
                 CORBA.To_CORBA_String
                    ("data");
               Argument_U_data : CORBA.Any :=
                 CORBA.Internals.Get_Empty_Any
                    (test_all_optims.Helper.TC_sixteenKb);
               Result_Ü : test_all_optims.sixteenKb;
            begin
               CORBA.NVList.Add_Item
                 (Argument_List_Ü,
                  Arg_Name_U_data,
                  Argument_U_data,
                  CORBA.ARG_IN);
               --  Processing request
               CORBA.ServerRequest.Arguments
                 (Request,
                  Argument_List_Ü);
               --  Setting Parameters
               data :=
                 test_all_optims.Helper.From_Any
                    (Argument_U_data);
               --  Call Implementation
               Result_Ü :=
                 test_all_optims.Impl.echoSixteenKb38
                    (test_all_optims.Impl.Object'Class
                       (Self.all)'Access,
                     data);
               --  Setting the result
               CORBA.ServerRequest.Set_Result
                 (Request,
                  test_all_optims.Helper.To_Any
                    (Result_Ü));
            end;
         elsif (Operation_Ü
            = "echoSixteenKb39")
         then
            declare
               data : test_all_optims.sixteenKb;
               Arg_Name_U_data : constant CORBA.Identifier :=
                 CORBA.To_CORBA_String
                    ("data");
               Argument_U_data : CORBA.Any :=
                 CORBA.Internals.Get_Empty_Any
                    (test_all_optims.Helper.TC_sixteenKb);
               Result_Ü : test_all_optims.sixteenKb;
            begin
               CORBA.NVList.Add_Item
                 (Argument_List_Ü,
                  Arg_Name_U_data,
                  Argument_U_data,
                  CORBA.ARG_IN);
               --  Processing request
               CORBA.ServerRequest.Arguments
                 (Request,
                  Argument_List_Ü);
               --  Setting Parameters
               data :=
                 test_all_optims.Helper.From_Any
                    (Argument_U_data);
               --  Call Implementation
               Result_Ü :=
                 test_all_optims.Impl.echoSixteenKb39
                    (test_all_optims.Impl.Object'Class
                       (Self.all)'Access,
                     data);
               --  Setting the result
               CORBA.ServerRequest.Set_Result
                 (Request,
                  test_all_optims.Helper.To_Any
                    (Result_Ü));
            end;
         elsif (Operation_Ü
            = "echoSixteenKb40")
         then
            declare
               data : test_all_optims.sixteenKb;
               Arg_Name_U_data : constant CORBA.Identifier :=
                 CORBA.To_CORBA_String
                    ("data");
               Argument_U_data : CORBA.Any :=
                 CORBA.Internals.Get_Empty_Any
                    (test_all_optims.Helper.TC_sixteenKb);
               Result_Ü : test_all_optims.sixteenKb;
            begin
               CORBA.NVList.Add_Item
                 (Argument_List_Ü,
                  Arg_Name_U_data,
                  Argument_U_data,
                  CORBA.ARG_IN);
               --  Processing request
               CORBA.ServerRequest.Arguments
                 (Request,
                  Argument_List_Ü);
               --  Setting Parameters
               data :=
                 test_all_optims.Helper.From_Any
                    (Argument_U_data);
               --  Call Implementation
               Result_Ü :=
                 test_all_optims.Impl.echoSixteenKb40
                    (test_all_optims.Impl.Object'Class
                       (Self.all)'Access,
                     data);
               --  Setting the result
               CORBA.ServerRequest.Set_Result
                 (Request,
                  test_all_optims.Helper.To_Any
                    (Result_Ü));
            end;
         elsif (Operation_Ü
            = "echoSixteenKb41")
         then
            declare
               data : test_all_optims.sixteenKb;
               Arg_Name_U_data : constant CORBA.Identifier :=
                 CORBA.To_CORBA_String
                    ("data");
               Argument_U_data : CORBA.Any :=
                 CORBA.Internals.Get_Empty_Any
                    (test_all_optims.Helper.TC_sixteenKb);
               Result_Ü : test_all_optims.sixteenKb;
            begin
               CORBA.NVList.Add_Item
                 (Argument_List_Ü,
                  Arg_Name_U_data,
                  Argument_U_data,
                  CORBA.ARG_IN);
               --  Processing request
               CORBA.ServerRequest.Arguments
                 (Request,
                  Argument_List_Ü);
               --  Setting Parameters
               data :=
                 test_all_optims.Helper.From_Any
                    (Argument_U_data);
               --  Call Implementation
               Result_Ü :=
                 test_all_optims.Impl.echoSixteenKb41
                    (test_all_optims.Impl.Object'Class
                       (Self.all)'Access,
                     data);
               --  Setting the result
               CORBA.ServerRequest.Set_Result
                 (Request,
                  test_all_optims.Helper.To_Any
                    (Result_Ü));
            end;
         elsif (Operation_Ü
            = "echoSixteenKb42")
         then
            declare
               data : test_all_optims.sixteenKb;
               Arg_Name_U_data : constant CORBA.Identifier :=
                 CORBA.To_CORBA_String
                    ("data");
               Argument_U_data : CORBA.Any :=
                 CORBA.Internals.Get_Empty_Any
                    (test_all_optims.Helper.TC_sixteenKb);
               Result_Ü : test_all_optims.sixteenKb;
            begin
               CORBA.NVList.Add_Item
                 (Argument_List_Ü,
                  Arg_Name_U_data,
                  Argument_U_data,
                  CORBA.ARG_IN);
               --  Processing request
               CORBA.ServerRequest.Arguments
                 (Request,
                  Argument_List_Ü);
               --  Setting Parameters
               data :=
                 test_all_optims.Helper.From_Any
                    (Argument_U_data);
               --  Call Implementation
               Result_Ü :=
                 test_all_optims.Impl.echoSixteenKb42
                    (test_all_optims.Impl.Object'Class
                       (Self.all)'Access,
                     data);
               --  Setting the result
               CORBA.ServerRequest.Set_Result
                 (Request,
                  test_all_optims.Helper.To_Any
                    (Result_Ü));
            end;
         elsif (Operation_Ü
            = "echoSixteenKb43")
         then
            declare
               data : test_all_optims.sixteenKb;
               Arg_Name_U_data : constant CORBA.Identifier :=
                 CORBA.To_CORBA_String
                    ("data");
               Argument_U_data : CORBA.Any :=
                 CORBA.Internals.Get_Empty_Any
                    (test_all_optims.Helper.TC_sixteenKb);
               Result_Ü : test_all_optims.sixteenKb;
            begin
               CORBA.NVList.Add_Item
                 (Argument_List_Ü,
                  Arg_Name_U_data,
                  Argument_U_data,
                  CORBA.ARG_IN);
               --  Processing request
               CORBA.ServerRequest.Arguments
                 (Request,
                  Argument_List_Ü);
               --  Setting Parameters
               data :=
                 test_all_optims.Helper.From_Any
                    (Argument_U_data);
               --  Call Implementation
               Result_Ü :=
                 test_all_optims.Impl.echoSixteenKb43
                    (test_all_optims.Impl.Object'Class
                       (Self.all)'Access,
                     data);
               --  Setting the result
               CORBA.ServerRequest.Set_Result
                 (Request,
                  test_all_optims.Helper.To_Any
                    (Result_Ü));
            end;
         elsif (Operation_Ü
            = "echoSixteenKb44")
         then
            declare
               data : test_all_optims.sixteenKb;
               Arg_Name_U_data : constant CORBA.Identifier :=
                 CORBA.To_CORBA_String
                    ("data");
               Argument_U_data : CORBA.Any :=
                 CORBA.Internals.Get_Empty_Any
                    (test_all_optims.Helper.TC_sixteenKb);
               Result_Ü : test_all_optims.sixteenKb;
            begin
               CORBA.NVList.Add_Item
                 (Argument_List_Ü,
                  Arg_Name_U_data,
                  Argument_U_data,
                  CORBA.ARG_IN);
               --  Processing request
               CORBA.ServerRequest.Arguments
                 (Request,
                  Argument_List_Ü);
               --  Setting Parameters
               data :=
                 test_all_optims.Helper.From_Any
                    (Argument_U_data);
               --  Call Implementation
               Result_Ü :=
                 test_all_optims.Impl.echoSixteenKb44
                    (test_all_optims.Impl.Object'Class
                       (Self.all)'Access,
                     data);
               --  Setting the result
               CORBA.ServerRequest.Set_Result
                 (Request,
                  test_all_optims.Helper.To_Any
                    (Result_Ü));
            end;
         elsif (Operation_Ü
            = "echoSixteenKb45")
         then
            declare
               data : test_all_optims.sixteenKb;
               Arg_Name_U_data : constant CORBA.Identifier :=
                 CORBA.To_CORBA_String
                    ("data");
               Argument_U_data : CORBA.Any :=
                 CORBA.Internals.Get_Empty_Any
                    (test_all_optims.Helper.TC_sixteenKb);
               Result_Ü : test_all_optims.sixteenKb;
            begin
               CORBA.NVList.Add_Item
                 (Argument_List_Ü,
                  Arg_Name_U_data,
                  Argument_U_data,
                  CORBA.ARG_IN);
               --  Processing request
               CORBA.ServerRequest.Arguments
                 (Request,
                  Argument_List_Ü);
               --  Setting Parameters
               data :=
                 test_all_optims.Helper.From_Any
                    (Argument_U_data);
               --  Call Implementation
               Result_Ü :=
                 test_all_optims.Impl.echoSixteenKb45
                    (test_all_optims.Impl.Object'Class
                       (Self.all)'Access,
                     data);
               --  Setting the result
               CORBA.ServerRequest.Set_Result
                 (Request,
                  test_all_optims.Helper.To_Any
                    (Result_Ü));
            end;
         elsif (Operation_Ü
            = "echoSixteenKb46")
         then
            declare
               data : test_all_optims.sixteenKb;
               Arg_Name_U_data : constant CORBA.Identifier :=
                 CORBA.To_CORBA_String
                    ("data");
               Argument_U_data : CORBA.Any :=
                 CORBA.Internals.Get_Empty_Any
                    (test_all_optims.Helper.TC_sixteenKb);
               Result_Ü : test_all_optims.sixteenKb;
            begin
               CORBA.NVList.Add_Item
                 (Argument_List_Ü,
                  Arg_Name_U_data,
                  Argument_U_data,
                  CORBA.ARG_IN);
               --  Processing request
               CORBA.ServerRequest.Arguments
                 (Request,
                  Argument_List_Ü);
               --  Setting Parameters
               data :=
                 test_all_optims.Helper.From_Any
                    (Argument_U_data);
               --  Call Implementation
               Result_Ü :=
                 test_all_optims.Impl.echoSixteenKb46
                    (test_all_optims.Impl.Object'Class
                       (Self.all)'Access,
                     data);
               --  Setting the result
               CORBA.ServerRequest.Set_Result
                 (Request,
                  test_all_optims.Helper.To_Any
                    (Result_Ü));
            end;
         elsif (Operation_Ü
            = "echoSixteenKb47")
         then
            declare
               data : test_all_optims.sixteenKb;
               Arg_Name_U_data : constant CORBA.Identifier :=
                 CORBA.To_CORBA_String
                    ("data");
               Argument_U_data : CORBA.Any :=
                 CORBA.Internals.Get_Empty_Any
                    (test_all_optims.Helper.TC_sixteenKb);
               Result_Ü : test_all_optims.sixteenKb;
            begin
               CORBA.NVList.Add_Item
                 (Argument_List_Ü,
                  Arg_Name_U_data,
                  Argument_U_data,
                  CORBA.ARG_IN);
               --  Processing request
               CORBA.ServerRequest.Arguments
                 (Request,
                  Argument_List_Ü);
               --  Setting Parameters
               data :=
                 test_all_optims.Helper.From_Any
                    (Argument_U_data);
               --  Call Implementation
               Result_Ü :=
                 test_all_optims.Impl.echoSixteenKb47
                    (test_all_optims.Impl.Object'Class
                       (Self.all)'Access,
                     data);
               --  Setting the result
               CORBA.ServerRequest.Set_Result
                 (Request,
                  test_all_optims.Helper.To_Any
                    (Result_Ü));
            end;
         elsif (Operation_Ü
            = "echoSixteenKb48")
         then
            declare
               data : test_all_optims.sixteenKb;
               Arg_Name_U_data : constant CORBA.Identifier :=
                 CORBA.To_CORBA_String
                    ("data");
               Argument_U_data : CORBA.Any :=
                 CORBA.Internals.Get_Empty_Any
                    (test_all_optims.Helper.TC_sixteenKb);
               Result_Ü : test_all_optims.sixteenKb;
            begin
               CORBA.NVList.Add_Item
                 (Argument_List_Ü,
                  Arg_Name_U_data,
                  Argument_U_data,
                  CORBA.ARG_IN);
               --  Processing request
               CORBA.ServerRequest.Arguments
                 (Request,
                  Argument_List_Ü);
               --  Setting Parameters
               data :=
                 test_all_optims.Helper.From_Any
                    (Argument_U_data);
               --  Call Implementation
               Result_Ü :=
                 test_all_optims.Impl.echoSixteenKb48
                    (test_all_optims.Impl.Object'Class
                       (Self.all)'Access,
                     data);
               --  Setting the result
               CORBA.ServerRequest.Set_Result
                 (Request,
                  test_all_optims.Helper.To_Any
                    (Result_Ü));
            end;
         elsif (Operation_Ü
            = "echoSixteenKb49")
         then
            declare
               data : test_all_optims.sixteenKb;
               Arg_Name_U_data : constant CORBA.Identifier :=
                 CORBA.To_CORBA_String
                    ("data");
               Argument_U_data : CORBA.Any :=
                 CORBA.Internals.Get_Empty_Any
                    (test_all_optims.Helper.TC_sixteenKb);
               Result_Ü : test_all_optims.sixteenKb;
            begin
               CORBA.NVList.Add_Item
                 (Argument_List_Ü,
                  Arg_Name_U_data,
                  Argument_U_data,
                  CORBA.ARG_IN);
               --  Processing request
               CORBA.ServerRequest.Arguments
                 (Request,
                  Argument_List_Ü);
               --  Setting Parameters
               data :=
                 test_all_optims.Helper.From_Any
                    (Argument_U_data);
               --  Call Implementation
               Result_Ü :=
                 test_all_optims.Impl.echoSixteenKb49
                    (test_all_optims.Impl.Object'Class
                       (Self.all)'Access,
                     data);
               --  Setting the result
               CORBA.ServerRequest.Set_Result
                 (Request,
                  test_all_optims.Helper.To_Any
                    (Result_Ü));
            end;
         elsif (Operation_Ü
            = "echoSixteenKb50")
         then
            declare
               data : test_all_optims.sixteenKb;
               Arg_Name_U_data : constant CORBA.Identifier :=
                 CORBA.To_CORBA_String
                    ("data");
               Argument_U_data : CORBA.Any :=
                 CORBA.Internals.Get_Empty_Any
                    (test_all_optims.Helper.TC_sixteenKb);
               Result_Ü : test_all_optims.sixteenKb;
            begin
               CORBA.NVList.Add_Item
                 (Argument_List_Ü,
                  Arg_Name_U_data,
                  Argument_U_data,
                  CORBA.ARG_IN);
               --  Processing request
               CORBA.ServerRequest.Arguments
                 (Request,
                  Argument_List_Ü);
               --  Setting Parameters
               data :=
                 test_all_optims.Helper.From_Any
                    (Argument_U_data);
               --  Call Implementation
               Result_Ü :=
                 test_all_optims.Impl.echoSixteenKb50
                    (test_all_optims.Impl.Object'Class
                       (Self.all)'Access,
                     data);
               --  Setting the result
               CORBA.ServerRequest.Set_Result
                 (Request,
                  test_all_optims.Helper.To_Any
                    (Result_Ü));
            end;
         elsif (Operation_Ü
            = "echoSixteenKb51")
         then
            declare
               data : test_all_optims.sixteenKb;
               Arg_Name_U_data : constant CORBA.Identifier :=
                 CORBA.To_CORBA_String
                    ("data");
               Argument_U_data : CORBA.Any :=
                 CORBA.Internals.Get_Empty_Any
                    (test_all_optims.Helper.TC_sixteenKb);
               Result_Ü : test_all_optims.sixteenKb;
            begin
               CORBA.NVList.Add_Item
                 (Argument_List_Ü,
                  Arg_Name_U_data,
                  Argument_U_data,
                  CORBA.ARG_IN);
               --  Processing request
               CORBA.ServerRequest.Arguments
                 (Request,
                  Argument_List_Ü);
               --  Setting Parameters
               data :=
                 test_all_optims.Helper.From_Any
                    (Argument_U_data);
               --  Call Implementation
               Result_Ü :=
                 test_all_optims.Impl.echoSixteenKb51
                    (test_all_optims.Impl.Object'Class
                       (Self.all)'Access,
                     data);
               --  Setting the result
               CORBA.ServerRequest.Set_Result
                 (Request,
                  test_all_optims.Helper.To_Any
                    (Result_Ü));
            end;
         elsif (Operation_Ü
            = "echoSixteenKb52")
         then
            declare
               data : test_all_optims.sixteenKb;
               Arg_Name_U_data : constant CORBA.Identifier :=
                 CORBA.To_CORBA_String
                    ("data");
               Argument_U_data : CORBA.Any :=
                 CORBA.Internals.Get_Empty_Any
                    (test_all_optims.Helper.TC_sixteenKb);
               Result_Ü : test_all_optims.sixteenKb;
            begin
               CORBA.NVList.Add_Item
                 (Argument_List_Ü,
                  Arg_Name_U_data,
                  Argument_U_data,
                  CORBA.ARG_IN);
               --  Processing request
               CORBA.ServerRequest.Arguments
                 (Request,
                  Argument_List_Ü);
               --  Setting Parameters
               data :=
                 test_all_optims.Helper.From_Any
                    (Argument_U_data);
               --  Call Implementation
               Result_Ü :=
                 test_all_optims.Impl.echoSixteenKb52
                    (test_all_optims.Impl.Object'Class
                       (Self.all)'Access,
                     data);
               --  Setting the result
               CORBA.ServerRequest.Set_Result
                 (Request,
                  test_all_optims.Helper.To_Any
                    (Result_Ü));
            end;
         elsif (Operation_Ü
            = "echoSixteenKb53")
         then
            declare
               data : test_all_optims.sixteenKb;
               Arg_Name_U_data : constant CORBA.Identifier :=
                 CORBA.To_CORBA_String
                    ("data");
               Argument_U_data : CORBA.Any :=
                 CORBA.Internals.Get_Empty_Any
                    (test_all_optims.Helper.TC_sixteenKb);
               Result_Ü : test_all_optims.sixteenKb;
            begin
               CORBA.NVList.Add_Item
                 (Argument_List_Ü,
                  Arg_Name_U_data,
                  Argument_U_data,
                  CORBA.ARG_IN);
               --  Processing request
               CORBA.ServerRequest.Arguments
                 (Request,
                  Argument_List_Ü);
               --  Setting Parameters
               data :=
                 test_all_optims.Helper.From_Any
                    (Argument_U_data);
               --  Call Implementation
               Result_Ü :=
                 test_all_optims.Impl.echoSixteenKb53
                    (test_all_optims.Impl.Object'Class
                       (Self.all)'Access,
                     data);
               --  Setting the result
               CORBA.ServerRequest.Set_Result
                 (Request,
                  test_all_optims.Helper.To_Any
                    (Result_Ü));
            end;
         elsif (Operation_Ü
            = "echoSixteenKb54")
         then
            declare
               data : test_all_optims.sixteenKb;
               Arg_Name_U_data : constant CORBA.Identifier :=
                 CORBA.To_CORBA_String
                    ("data");
               Argument_U_data : CORBA.Any :=
                 CORBA.Internals.Get_Empty_Any
                    (test_all_optims.Helper.TC_sixteenKb);
               Result_Ü : test_all_optims.sixteenKb;
            begin
               CORBA.NVList.Add_Item
                 (Argument_List_Ü,
                  Arg_Name_U_data,
                  Argument_U_data,
                  CORBA.ARG_IN);
               --  Processing request
               CORBA.ServerRequest.Arguments
                 (Request,
                  Argument_List_Ü);
               --  Setting Parameters
               data :=
                 test_all_optims.Helper.From_Any
                    (Argument_U_data);
               --  Call Implementation
               Result_Ü :=
                 test_all_optims.Impl.echoSixteenKb54
                    (test_all_optims.Impl.Object'Class
                       (Self.all)'Access,
                     data);
               --  Setting the result
               CORBA.ServerRequest.Set_Result
                 (Request,
                  test_all_optims.Helper.To_Any
                    (Result_Ü));
            end;
         elsif (Operation_Ü
            = "echoSixteenKb55")
         then
            declare
               data : test_all_optims.sixteenKb;
               Arg_Name_U_data : constant CORBA.Identifier :=
                 CORBA.To_CORBA_String
                    ("data");
               Argument_U_data : CORBA.Any :=
                 CORBA.Internals.Get_Empty_Any
                    (test_all_optims.Helper.TC_sixteenKb);
               Result_Ü : test_all_optims.sixteenKb;
            begin
               CORBA.NVList.Add_Item
                 (Argument_List_Ü,
                  Arg_Name_U_data,
                  Argument_U_data,
                  CORBA.ARG_IN);
               --  Processing request
               CORBA.ServerRequest.Arguments
                 (Request,
                  Argument_List_Ü);
               --  Setting Parameters
               data :=
                 test_all_optims.Helper.From_Any
                    (Argument_U_data);
               --  Call Implementation
               Result_Ü :=
                 test_all_optims.Impl.echoSixteenKb55
                    (test_all_optims.Impl.Object'Class
                       (Self.all)'Access,
                     data);
               --  Setting the result
               CORBA.ServerRequest.Set_Result
                 (Request,
                  test_all_optims.Helper.To_Any
                    (Result_Ü));
            end;
         elsif (Operation_Ü
            = "echoSixteenKb56")
         then
            declare
               data : test_all_optims.sixteenKb;
               Arg_Name_U_data : constant CORBA.Identifier :=
                 CORBA.To_CORBA_String
                    ("data");
               Argument_U_data : CORBA.Any :=
                 CORBA.Internals.Get_Empty_Any
                    (test_all_optims.Helper.TC_sixteenKb);
               Result_Ü : test_all_optims.sixteenKb;
            begin
               CORBA.NVList.Add_Item
                 (Argument_List_Ü,
                  Arg_Name_U_data,
                  Argument_U_data,
                  CORBA.ARG_IN);
               --  Processing request
               CORBA.ServerRequest.Arguments
                 (Request,
                  Argument_List_Ü);
               --  Setting Parameters
               data :=
                 test_all_optims.Helper.From_Any
                    (Argument_U_data);
               --  Call Implementation
               Result_Ü :=
                 test_all_optims.Impl.echoSixteenKb56
                    (test_all_optims.Impl.Object'Class
                       (Self.all)'Access,
                     data);
               --  Setting the result
               CORBA.ServerRequest.Set_Result
                 (Request,
                  test_all_optims.Helper.To_Any
                    (Result_Ü));
            end;
         elsif (Operation_Ü
            = "echoSixteenKb57")
         then
            declare
               data : test_all_optims.sixteenKb;
               Arg_Name_U_data : constant CORBA.Identifier :=
                 CORBA.To_CORBA_String
                    ("data");
               Argument_U_data : CORBA.Any :=
                 CORBA.Internals.Get_Empty_Any
                    (test_all_optims.Helper.TC_sixteenKb);
               Result_Ü : test_all_optims.sixteenKb;
            begin
               CORBA.NVList.Add_Item
                 (Argument_List_Ü,
                  Arg_Name_U_data,
                  Argument_U_data,
                  CORBA.ARG_IN);
               --  Processing request
               CORBA.ServerRequest.Arguments
                 (Request,
                  Argument_List_Ü);
               --  Setting Parameters
               data :=
                 test_all_optims.Helper.From_Any
                    (Argument_U_data);
               --  Call Implementation
               Result_Ü :=
                 test_all_optims.Impl.echoSixteenKb57
                    (test_all_optims.Impl.Object'Class
                       (Self.all)'Access,
                     data);
               --  Setting the result
               CORBA.ServerRequest.Set_Result
                 (Request,
                  test_all_optims.Helper.To_Any
                    (Result_Ü));
            end;
         elsif (Operation_Ü
            = "echoSixteenKb58")
         then
            declare
               data : test_all_optims.sixteenKb;
               Arg_Name_U_data : constant CORBA.Identifier :=
                 CORBA.To_CORBA_String
                    ("data");
               Argument_U_data : CORBA.Any :=
                 CORBA.Internals.Get_Empty_Any
                    (test_all_optims.Helper.TC_sixteenKb);
               Result_Ü : test_all_optims.sixteenKb;
            begin
               CORBA.NVList.Add_Item
                 (Argument_List_Ü,
                  Arg_Name_U_data,
                  Argument_U_data,
                  CORBA.ARG_IN);
               --  Processing request
               CORBA.ServerRequest.Arguments
                 (Request,
                  Argument_List_Ü);
               --  Setting Parameters
               data :=
                 test_all_optims.Helper.From_Any
                    (Argument_U_data);
               --  Call Implementation
               Result_Ü :=
                 test_all_optims.Impl.echoSixteenKb58
                    (test_all_optims.Impl.Object'Class
                       (Self.all)'Access,
                     data);
               --  Setting the result
               CORBA.ServerRequest.Set_Result
                 (Request,
                  test_all_optims.Helper.To_Any
                    (Result_Ü));
            end;
         elsif (Operation_Ü
            = "echoSixteenKb59")
         then
            declare
               data : test_all_optims.sixteenKb;
               Arg_Name_U_data : constant CORBA.Identifier :=
                 CORBA.To_CORBA_String
                    ("data");
               Argument_U_data : CORBA.Any :=
                 CORBA.Internals.Get_Empty_Any
                    (test_all_optims.Helper.TC_sixteenKb);
               Result_Ü : test_all_optims.sixteenKb;
            begin
               CORBA.NVList.Add_Item
                 (Argument_List_Ü,
                  Arg_Name_U_data,
                  Argument_U_data,
                  CORBA.ARG_IN);
               --  Processing request
               CORBA.ServerRequest.Arguments
                 (Request,
                  Argument_List_Ü);
               --  Setting Parameters
               data :=
                 test_all_optims.Helper.From_Any
                    (Argument_U_data);
               --  Call Implementation
               Result_Ü :=
                 test_all_optims.Impl.echoSixteenKb59
                    (test_all_optims.Impl.Object'Class
                       (Self.all)'Access,
                     data);
               --  Setting the result
               CORBA.ServerRequest.Set_Result
                 (Request,
                  test_all_optims.Helper.To_Any
                    (Result_Ü));
            end;
         elsif (Operation_Ü
            = "echoSixteenKb60")
         then
            declare
               data : test_all_optims.sixteenKb;
               Arg_Name_U_data : constant CORBA.Identifier :=
                 CORBA.To_CORBA_String
                    ("data");
               Argument_U_data : CORBA.Any :=
                 CORBA.Internals.Get_Empty_Any
                    (test_all_optims.Helper.TC_sixteenKb);
               Result_Ü : test_all_optims.sixteenKb;
            begin
               CORBA.NVList.Add_Item
                 (Argument_List_Ü,
                  Arg_Name_U_data,
                  Argument_U_data,
                  CORBA.ARG_IN);
               --  Processing request
               CORBA.ServerRequest.Arguments
                 (Request,
                  Argument_List_Ü);
               --  Setting Parameters
               data :=
                 test_all_optims.Helper.From_Any
                    (Argument_U_data);
               --  Call Implementation
               Result_Ü :=
                 test_all_optims.Impl.echoSixteenKb60
                    (test_all_optims.Impl.Object'Class
                       (Self.all)'Access,
                     data);
               --  Setting the result
               CORBA.ServerRequest.Set_Result
                 (Request,
                  test_all_optims.Helper.To_Any
                    (Result_Ü));
            end;
         elsif (Operation_Ü
            = "echoSixteenKb61")
         then
            declare
               data : test_all_optims.sixteenKb;
               Arg_Name_U_data : constant CORBA.Identifier :=
                 CORBA.To_CORBA_String
                    ("data");
               Argument_U_data : CORBA.Any :=
                 CORBA.Internals.Get_Empty_Any
                    (test_all_optims.Helper.TC_sixteenKb);
               Result_Ü : test_all_optims.sixteenKb;
            begin
               CORBA.NVList.Add_Item
                 (Argument_List_Ü,
                  Arg_Name_U_data,
                  Argument_U_data,
                  CORBA.ARG_IN);
               --  Processing request
               CORBA.ServerRequest.Arguments
                 (Request,
                  Argument_List_Ü);
               --  Setting Parameters
               data :=
                 test_all_optims.Helper.From_Any
                    (Argument_U_data);
               --  Call Implementation
               Result_Ü :=
                 test_all_optims.Impl.echoSixteenKb61
                    (test_all_optims.Impl.Object'Class
                       (Self.all)'Access,
                     data);
               --  Setting the result
               CORBA.ServerRequest.Set_Result
                 (Request,
                  test_all_optims.Helper.To_Any
                    (Result_Ü));
            end;
         elsif (Operation_Ü
            = "echoSixteenKb62")
         then
            declare
               data : test_all_optims.sixteenKb;
               Arg_Name_U_data : constant CORBA.Identifier :=
                 CORBA.To_CORBA_String
                    ("data");
               Argument_U_data : CORBA.Any :=
                 CORBA.Internals.Get_Empty_Any
                    (test_all_optims.Helper.TC_sixteenKb);
               Result_Ü : test_all_optims.sixteenKb;
            begin
               CORBA.NVList.Add_Item
                 (Argument_List_Ü,
                  Arg_Name_U_data,
                  Argument_U_data,
                  CORBA.ARG_IN);
               --  Processing request
               CORBA.ServerRequest.Arguments
                 (Request,
                  Argument_List_Ü);
               --  Setting Parameters
               data :=
                 test_all_optims.Helper.From_Any
                    (Argument_U_data);
               --  Call Implementation
               Result_Ü :=
                 test_all_optims.Impl.echoSixteenKb62
                    (test_all_optims.Impl.Object'Class
                       (Self.all)'Access,
                     data);
               --  Setting the result
               CORBA.ServerRequest.Set_Result
                 (Request,
                  test_all_optims.Helper.To_Any
                    (Result_Ü));
            end;
         elsif (Operation_Ü
            = "echoSixteenKb63")
         then
            declare
               data : test_all_optims.sixteenKb;
               Arg_Name_U_data : constant CORBA.Identifier :=
                 CORBA.To_CORBA_String
                    ("data");
               Argument_U_data : CORBA.Any :=
                 CORBA.Internals.Get_Empty_Any
                    (test_all_optims.Helper.TC_sixteenKb);
               Result_Ü : test_all_optims.sixteenKb;
            begin
               CORBA.NVList.Add_Item
                 (Argument_List_Ü,
                  Arg_Name_U_data,
                  Argument_U_data,
                  CORBA.ARG_IN);
               --  Processing request
               CORBA.ServerRequest.Arguments
                 (Request,
                  Argument_List_Ü);
               --  Setting Parameters
               data :=
                 test_all_optims.Helper.From_Any
                    (Argument_U_data);
               --  Call Implementation
               Result_Ü :=
                 test_all_optims.Impl.echoSixteenKb63
                    (test_all_optims.Impl.Object'Class
                       (Self.all)'Access,
                     data);
               --  Setting the result
               CORBA.ServerRequest.Set_Result
                 (Request,
                  test_all_optims.Helper.To_Any
                    (Result_Ü));
            end;
         elsif (Operation_Ü
            = "echoSixteenKb64")
         then
            declare
               data : test_all_optims.sixteenKb;
               Arg_Name_U_data : constant CORBA.Identifier :=
                 CORBA.To_CORBA_String
                    ("data");
               Argument_U_data : CORBA.Any :=
                 CORBA.Internals.Get_Empty_Any
                    (test_all_optims.Helper.TC_sixteenKb);
               Result_Ü : test_all_optims.sixteenKb;
            begin
               CORBA.NVList.Add_Item
                 (Argument_List_Ü,
                  Arg_Name_U_data,
                  Argument_U_data,
                  CORBA.ARG_IN);
               --  Processing request
               CORBA.ServerRequest.Arguments
                 (Request,
                  Argument_List_Ü);
               --  Setting Parameters
               data :=
                 test_all_optims.Helper.From_Any
                    (Argument_U_data);
               --  Call Implementation
               Result_Ü :=
                 test_all_optims.Impl.echoSixteenKb64
                    (test_all_optims.Impl.Object'Class
                       (Self.all)'Access,
                     data);
               --  Setting the result
               CORBA.ServerRequest.Set_Result
                 (Request,
                  test_all_optims.Helper.To_Any
                    (Result_Ü));
            end;
         elsif (Operation_Ü
            = "echoSixteenKb65")
         then
            declare
               data : test_all_optims.sixteenKb;
               Arg_Name_U_data : constant CORBA.Identifier :=
                 CORBA.To_CORBA_String
                    ("data");
               Argument_U_data : CORBA.Any :=
                 CORBA.Internals.Get_Empty_Any
                    (test_all_optims.Helper.TC_sixteenKb);
               Result_Ü : test_all_optims.sixteenKb;
            begin
               CORBA.NVList.Add_Item
                 (Argument_List_Ü,
                  Arg_Name_U_data,
                  Argument_U_data,
                  CORBA.ARG_IN);
               --  Processing request
               CORBA.ServerRequest.Arguments
                 (Request,
                  Argument_List_Ü);
               --  Setting Parameters
               data :=
                 test_all_optims.Helper.From_Any
                    (Argument_U_data);
               --  Call Implementation
               Result_Ü :=
                 test_all_optims.Impl.echoSixteenKb65
                    (test_all_optims.Impl.Object'Class
                       (Self.all)'Access,
                     data);
               --  Setting the result
               CORBA.ServerRequest.Set_Result
                 (Request,
                  test_all_optims.Helper.To_Any
                    (Result_Ü));
            end;
         elsif (Operation_Ü
            = "echoSixteenKb66")
         then
            declare
               data : test_all_optims.sixteenKb;
               Arg_Name_U_data : constant CORBA.Identifier :=
                 CORBA.To_CORBA_String
                    ("data");
               Argument_U_data : CORBA.Any :=
                 CORBA.Internals.Get_Empty_Any
                    (test_all_optims.Helper.TC_sixteenKb);
               Result_Ü : test_all_optims.sixteenKb;
            begin
               CORBA.NVList.Add_Item
                 (Argument_List_Ü,
                  Arg_Name_U_data,
                  Argument_U_data,
                  CORBA.ARG_IN);
               --  Processing request
               CORBA.ServerRequest.Arguments
                 (Request,
                  Argument_List_Ü);
               --  Setting Parameters
               data :=
                 test_all_optims.Helper.From_Any
                    (Argument_U_data);
               --  Call Implementation
               Result_Ü :=
                 test_all_optims.Impl.echoSixteenKb66
                    (test_all_optims.Impl.Object'Class
                       (Self.all)'Access,
                     data);
               --  Setting the result
               CORBA.ServerRequest.Set_Result
                 (Request,
                  test_all_optims.Helper.To_Any
                    (Result_Ü));
            end;
         elsif (Operation_Ü
            = "echoSixteenKb67")
         then
            declare
               data : test_all_optims.sixteenKb;
               Arg_Name_U_data : constant CORBA.Identifier :=
                 CORBA.To_CORBA_String
                    ("data");
               Argument_U_data : CORBA.Any :=
                 CORBA.Internals.Get_Empty_Any
                    (test_all_optims.Helper.TC_sixteenKb);
               Result_Ü : test_all_optims.sixteenKb;
            begin
               CORBA.NVList.Add_Item
                 (Argument_List_Ü,
                  Arg_Name_U_data,
                  Argument_U_data,
                  CORBA.ARG_IN);
               --  Processing request
               CORBA.ServerRequest.Arguments
                 (Request,
                  Argument_List_Ü);
               --  Setting Parameters
               data :=
                 test_all_optims.Helper.From_Any
                    (Argument_U_data);
               --  Call Implementation
               Result_Ü :=
                 test_all_optims.Impl.echoSixteenKb67
                    (test_all_optims.Impl.Object'Class
                       (Self.all)'Access,
                     data);
               --  Setting the result
               CORBA.ServerRequest.Set_Result
                 (Request,
                  test_all_optims.Helper.To_Any
                    (Result_Ü));
            end;
         elsif (Operation_Ü
            = "echoSixteenKb68")
         then
            declare
               data : test_all_optims.sixteenKb;
               Arg_Name_U_data : constant CORBA.Identifier :=
                 CORBA.To_CORBA_String
                    ("data");
               Argument_U_data : CORBA.Any :=
                 CORBA.Internals.Get_Empty_Any
                    (test_all_optims.Helper.TC_sixteenKb);
               Result_Ü : test_all_optims.sixteenKb;
            begin
               CORBA.NVList.Add_Item
                 (Argument_List_Ü,
                  Arg_Name_U_data,
                  Argument_U_data,
                  CORBA.ARG_IN);
               --  Processing request
               CORBA.ServerRequest.Arguments
                 (Request,
                  Argument_List_Ü);
               --  Setting Parameters
               data :=
                 test_all_optims.Helper.From_Any
                    (Argument_U_data);
               --  Call Implementation
               Result_Ü :=
                 test_all_optims.Impl.echoSixteenKb68
                    (test_all_optims.Impl.Object'Class
                       (Self.all)'Access,
                     data);
               --  Setting the result
               CORBA.ServerRequest.Set_Result
                 (Request,
                  test_all_optims.Helper.To_Any
                    (Result_Ü));
            end;
         elsif (Operation_Ü
            = "echoSixteenKb69")
         then
            declare
               data : test_all_optims.sixteenKb;
               Arg_Name_U_data : constant CORBA.Identifier :=
                 CORBA.To_CORBA_String
                    ("data");
               Argument_U_data : CORBA.Any :=
                 CORBA.Internals.Get_Empty_Any
                    (test_all_optims.Helper.TC_sixteenKb);
               Result_Ü : test_all_optims.sixteenKb;
            begin
               CORBA.NVList.Add_Item
                 (Argument_List_Ü,
                  Arg_Name_U_data,
                  Argument_U_data,
                  CORBA.ARG_IN);
               --  Processing request
               CORBA.ServerRequest.Arguments
                 (Request,
                  Argument_List_Ü);
               --  Setting Parameters
               data :=
                 test_all_optims.Helper.From_Any
                    (Argument_U_data);
               --  Call Implementation
               Result_Ü :=
                 test_all_optims.Impl.echoSixteenKb69
                    (test_all_optims.Impl.Object'Class
                       (Self.all)'Access,
                     data);
               --  Setting the result
               CORBA.ServerRequest.Set_Result
                 (Request,
                  test_all_optims.Helper.To_Any
                    (Result_Ü));
            end;
         elsif (Operation_Ü
            = "echoSixteenKb70")
         then
            declare
               data : test_all_optims.sixteenKb;
               Arg_Name_U_data : constant CORBA.Identifier :=
                 CORBA.To_CORBA_String
                    ("data");
               Argument_U_data : CORBA.Any :=
                 CORBA.Internals.Get_Empty_Any
                    (test_all_optims.Helper.TC_sixteenKb);
               Result_Ü : test_all_optims.sixteenKb;
            begin
               CORBA.NVList.Add_Item
                 (Argument_List_Ü,
                  Arg_Name_U_data,
                  Argument_U_data,
                  CORBA.ARG_IN);
               --  Processing request
               CORBA.ServerRequest.Arguments
                 (Request,
                  Argument_List_Ü);
               --  Setting Parameters
               data :=
                 test_all_optims.Helper.From_Any
                    (Argument_U_data);
               --  Call Implementation
               Result_Ü :=
                 test_all_optims.Impl.echoSixteenKb70
                    (test_all_optims.Impl.Object'Class
                       (Self.all)'Access,
                     data);
               --  Setting the result
               CORBA.ServerRequest.Set_Result
                 (Request,
                  test_all_optims.Helper.To_Any
                    (Result_Ü));
            end;
         elsif (Operation_Ü
            = "echoSixteenKb71")
         then
            declare
               data : test_all_optims.sixteenKb;
               Arg_Name_U_data : constant CORBA.Identifier :=
                 CORBA.To_CORBA_String
                    ("data");
               Argument_U_data : CORBA.Any :=
                 CORBA.Internals.Get_Empty_Any
                    (test_all_optims.Helper.TC_sixteenKb);
               Result_Ü : test_all_optims.sixteenKb;
            begin
               CORBA.NVList.Add_Item
                 (Argument_List_Ü,
                  Arg_Name_U_data,
                  Argument_U_data,
                  CORBA.ARG_IN);
               --  Processing request
               CORBA.ServerRequest.Arguments
                 (Request,
                  Argument_List_Ü);
               --  Setting Parameters
               data :=
                 test_all_optims.Helper.From_Any
                    (Argument_U_data);
               --  Call Implementation
               Result_Ü :=
                 test_all_optims.Impl.echoSixteenKb71
                    (test_all_optims.Impl.Object'Class
                       (Self.all)'Access,
                     data);
               --  Setting the result
               CORBA.ServerRequest.Set_Result
                 (Request,
                  test_all_optims.Helper.To_Any
                    (Result_Ü));
            end;
         elsif (Operation_Ü
            = "echoSixteenKb72")
         then
            declare
               data : test_all_optims.sixteenKb;
               Arg_Name_U_data : constant CORBA.Identifier :=
                 CORBA.To_CORBA_String
                    ("data");
               Argument_U_data : CORBA.Any :=
                 CORBA.Internals.Get_Empty_Any
                    (test_all_optims.Helper.TC_sixteenKb);
               Result_Ü : test_all_optims.sixteenKb;
            begin
               CORBA.NVList.Add_Item
                 (Argument_List_Ü,
                  Arg_Name_U_data,
                  Argument_U_data,
                  CORBA.ARG_IN);
               --  Processing request
               CORBA.ServerRequest.Arguments
                 (Request,
                  Argument_List_Ü);
               --  Setting Parameters
               data :=
                 test_all_optims.Helper.From_Any
                    (Argument_U_data);
               --  Call Implementation
               Result_Ü :=
                 test_all_optims.Impl.echoSixteenKb72
                    (test_all_optims.Impl.Object'Class
                       (Self.all)'Access,
                     data);
               --  Setting the result
               CORBA.ServerRequest.Set_Result
                 (Request,
                  test_all_optims.Helper.To_Any
                    (Result_Ü));
            end;
         elsif (Operation_Ü
            = "echoSixteenKb73")
         then
            declare
               data : test_all_optims.sixteenKb;
               Arg_Name_U_data : constant CORBA.Identifier :=
                 CORBA.To_CORBA_String
                    ("data");
               Argument_U_data : CORBA.Any :=
                 CORBA.Internals.Get_Empty_Any
                    (test_all_optims.Helper.TC_sixteenKb);
               Result_Ü : test_all_optims.sixteenKb;
            begin
               CORBA.NVList.Add_Item
                 (Argument_List_Ü,
                  Arg_Name_U_data,
                  Argument_U_data,
                  CORBA.ARG_IN);
               --  Processing request
               CORBA.ServerRequest.Arguments
                 (Request,
                  Argument_List_Ü);
               --  Setting Parameters
               data :=
                 test_all_optims.Helper.From_Any
                    (Argument_U_data);
               --  Call Implementation
               Result_Ü :=
                 test_all_optims.Impl.echoSixteenKb73
                    (test_all_optims.Impl.Object'Class
                       (Self.all)'Access,
                     data);
               --  Setting the result
               CORBA.ServerRequest.Set_Result
                 (Request,
                  test_all_optims.Helper.To_Any
                    (Result_Ü));
            end;
         elsif (Operation_Ü
            = "echoSixteenKb74")
         then
            declare
               data : test_all_optims.sixteenKb;
               Arg_Name_U_data : constant CORBA.Identifier :=
                 CORBA.To_CORBA_String
                    ("data");
               Argument_U_data : CORBA.Any :=
                 CORBA.Internals.Get_Empty_Any
                    (test_all_optims.Helper.TC_sixteenKb);
               Result_Ü : test_all_optims.sixteenKb;
            begin
               CORBA.NVList.Add_Item
                 (Argument_List_Ü,
                  Arg_Name_U_data,
                  Argument_U_data,
                  CORBA.ARG_IN);
               --  Processing request
               CORBA.ServerRequest.Arguments
                 (Request,
                  Argument_List_Ü);
               --  Setting Parameters
               data :=
                 test_all_optims.Helper.From_Any
                    (Argument_U_data);
               --  Call Implementation
               Result_Ü :=
                 test_all_optims.Impl.echoSixteenKb74
                    (test_all_optims.Impl.Object'Class
                       (Self.all)'Access,
                     data);
               --  Setting the result
               CORBA.ServerRequest.Set_Result
                 (Request,
                  test_all_optims.Helper.To_Any
                    (Result_Ü));
            end;
         elsif (Operation_Ü
            = "echoSixteenKb75")
         then
            declare
               data : test_all_optims.sixteenKb;
               Arg_Name_U_data : constant CORBA.Identifier :=
                 CORBA.To_CORBA_String
                    ("data");
               Argument_U_data : CORBA.Any :=
                 CORBA.Internals.Get_Empty_Any
                    (test_all_optims.Helper.TC_sixteenKb);
               Result_Ü : test_all_optims.sixteenKb;
            begin
               CORBA.NVList.Add_Item
                 (Argument_List_Ü,
                  Arg_Name_U_data,
                  Argument_U_data,
                  CORBA.ARG_IN);
               --  Processing request
               CORBA.ServerRequest.Arguments
                 (Request,
                  Argument_List_Ü);
               --  Setting Parameters
               data :=
                 test_all_optims.Helper.From_Any
                    (Argument_U_data);
               --  Call Implementation
               Result_Ü :=
                 test_all_optims.Impl.echoSixteenKb75
                    (test_all_optims.Impl.Object'Class
                       (Self.all)'Access,
                     data);
               --  Setting the result
               CORBA.ServerRequest.Set_Result
                 (Request,
                  test_all_optims.Helper.To_Any
                    (Result_Ü));
            end;
         elsif (Operation_Ü
            = "echoSixteenKb76")
         then
            declare
               data : test_all_optims.sixteenKb;
               Arg_Name_U_data : constant CORBA.Identifier :=
                 CORBA.To_CORBA_String
                    ("data");
               Argument_U_data : CORBA.Any :=
                 CORBA.Internals.Get_Empty_Any
                    (test_all_optims.Helper.TC_sixteenKb);
               Result_Ü : test_all_optims.sixteenKb;
            begin
               CORBA.NVList.Add_Item
                 (Argument_List_Ü,
                  Arg_Name_U_data,
                  Argument_U_data,
                  CORBA.ARG_IN);
               --  Processing request
               CORBA.ServerRequest.Arguments
                 (Request,
                  Argument_List_Ü);
               --  Setting Parameters
               data :=
                 test_all_optims.Helper.From_Any
                    (Argument_U_data);
               --  Call Implementation
               Result_Ü :=
                 test_all_optims.Impl.echoSixteenKb76
                    (test_all_optims.Impl.Object'Class
                       (Self.all)'Access,
                     data);
               --  Setting the result
               CORBA.ServerRequest.Set_Result
                 (Request,
                  test_all_optims.Helper.To_Any
                    (Result_Ü));
            end;
         elsif (Operation_Ü
            = "echoSixteenKb77")
         then
            declare
               data : test_all_optims.sixteenKb;
               Arg_Name_U_data : constant CORBA.Identifier :=
                 CORBA.To_CORBA_String
                    ("data");
               Argument_U_data : CORBA.Any :=
                 CORBA.Internals.Get_Empty_Any
                    (test_all_optims.Helper.TC_sixteenKb);
               Result_Ü : test_all_optims.sixteenKb;
            begin
               CORBA.NVList.Add_Item
                 (Argument_List_Ü,
                  Arg_Name_U_data,
                  Argument_U_data,
                  CORBA.ARG_IN);
               --  Processing request
               CORBA.ServerRequest.Arguments
                 (Request,
                  Argument_List_Ü);
               --  Setting Parameters
               data :=
                 test_all_optims.Helper.From_Any
                    (Argument_U_data);
               --  Call Implementation
               Result_Ü :=
                 test_all_optims.Impl.echoSixteenKb77
                    (test_all_optims.Impl.Object'Class
                       (Self.all)'Access,
                     data);
               --  Setting the result
               CORBA.ServerRequest.Set_Result
                 (Request,
                  test_all_optims.Helper.To_Any
                    (Result_Ü));
            end;
         elsif (Operation_Ü
            = "echoSixteenKb78")
         then
            declare
               data : test_all_optims.sixteenKb;
               Arg_Name_U_data : constant CORBA.Identifier :=
                 CORBA.To_CORBA_String
                    ("data");
               Argument_U_data : CORBA.Any :=
                 CORBA.Internals.Get_Empty_Any
                    (test_all_optims.Helper.TC_sixteenKb);
               Result_Ü : test_all_optims.sixteenKb;
            begin
               CORBA.NVList.Add_Item
                 (Argument_List_Ü,
                  Arg_Name_U_data,
                  Argument_U_data,
                  CORBA.ARG_IN);
               --  Processing request
               CORBA.ServerRequest.Arguments
                 (Request,
                  Argument_List_Ü);
               --  Setting Parameters
               data :=
                 test_all_optims.Helper.From_Any
                    (Argument_U_data);
               --  Call Implementation
               Result_Ü :=
                 test_all_optims.Impl.echoSixteenKb78
                    (test_all_optims.Impl.Object'Class
                       (Self.all)'Access,
                     data);
               --  Setting the result
               CORBA.ServerRequest.Set_Result
                 (Request,
                  test_all_optims.Helper.To_Any
                    (Result_Ü));
            end;
         elsif (Operation_Ü
            = "echoSixteenKb79")
         then
            declare
               data : test_all_optims.sixteenKb;
               Arg_Name_U_data : constant CORBA.Identifier :=
                 CORBA.To_CORBA_String
                    ("data");
               Argument_U_data : CORBA.Any :=
                 CORBA.Internals.Get_Empty_Any
                    (test_all_optims.Helper.TC_sixteenKb);
               Result_Ü : test_all_optims.sixteenKb;
            begin
               CORBA.NVList.Add_Item
                 (Argument_List_Ü,
                  Arg_Name_U_data,
                  Argument_U_data,
                  CORBA.ARG_IN);
               --  Processing request
               CORBA.ServerRequest.Arguments
                 (Request,
                  Argument_List_Ü);
               --  Setting Parameters
               data :=
                 test_all_optims.Helper.From_Any
                    (Argument_U_data);
               --  Call Implementation
               Result_Ü :=
                 test_all_optims.Impl.echoSixteenKb79
                    (test_all_optims.Impl.Object'Class
                       (Self.all)'Access,
                     data);
               --  Setting the result
               CORBA.ServerRequest.Set_Result
                 (Request,
                  test_all_optims.Helper.To_Any
                    (Result_Ü));
            end;
         elsif (Operation_Ü
            = "echoSixteenKb80")
         then
            declare
               data : test_all_optims.sixteenKb;
               Arg_Name_U_data : constant CORBA.Identifier :=
                 CORBA.To_CORBA_String
                    ("data");
               Argument_U_data : CORBA.Any :=
                 CORBA.Internals.Get_Empty_Any
                    (test_all_optims.Helper.TC_sixteenKb);
               Result_Ü : test_all_optims.sixteenKb;
            begin
               CORBA.NVList.Add_Item
                 (Argument_List_Ü,
                  Arg_Name_U_data,
                  Argument_U_data,
                  CORBA.ARG_IN);
               --  Processing request
               CORBA.ServerRequest.Arguments
                 (Request,
                  Argument_List_Ü);
               --  Setting Parameters
               data :=
                 test_all_optims.Helper.From_Any
                    (Argument_U_data);
               --  Call Implementation
               Result_Ü :=
                 test_all_optims.Impl.echoSixteenKb80
                    (test_all_optims.Impl.Object'Class
                       (Self.all)'Access,
                     data);
               --  Setting the result
               CORBA.ServerRequest.Set_Result
                 (Request,
                  test_all_optims.Helper.To_Any
                    (Result_Ü));
            end;
         elsif (Operation_Ü
            = "echoSixteenKb81")
         then
            declare
               data : test_all_optims.sixteenKb;
               Arg_Name_U_data : constant CORBA.Identifier :=
                 CORBA.To_CORBA_String
                    ("data");
               Argument_U_data : CORBA.Any :=
                 CORBA.Internals.Get_Empty_Any
                    (test_all_optims.Helper.TC_sixteenKb);
               Result_Ü : test_all_optims.sixteenKb;
            begin
               CORBA.NVList.Add_Item
                 (Argument_List_Ü,
                  Arg_Name_U_data,
                  Argument_U_data,
                  CORBA.ARG_IN);
               --  Processing request
               CORBA.ServerRequest.Arguments
                 (Request,
                  Argument_List_Ü);
               --  Setting Parameters
               data :=
                 test_all_optims.Helper.From_Any
                    (Argument_U_data);
               --  Call Implementation
               Result_Ü :=
                 test_all_optims.Impl.echoSixteenKb81
                    (test_all_optims.Impl.Object'Class
                       (Self.all)'Access,
                     data);
               --  Setting the result
               CORBA.ServerRequest.Set_Result
                 (Request,
                  test_all_optims.Helper.To_Any
                    (Result_Ü));
            end;
         elsif (Operation_Ü
            = "echoSixteenKb82")
         then
            declare
               data : test_all_optims.sixteenKb;
               Arg_Name_U_data : constant CORBA.Identifier :=
                 CORBA.To_CORBA_String
                    ("data");
               Argument_U_data : CORBA.Any :=
                 CORBA.Internals.Get_Empty_Any
                    (test_all_optims.Helper.TC_sixteenKb);
               Result_Ü : test_all_optims.sixteenKb;
            begin
               CORBA.NVList.Add_Item
                 (Argument_List_Ü,
                  Arg_Name_U_data,
                  Argument_U_data,
                  CORBA.ARG_IN);
               --  Processing request
               CORBA.ServerRequest.Arguments
                 (Request,
                  Argument_List_Ü);
               --  Setting Parameters
               data :=
                 test_all_optims.Helper.From_Any
                    (Argument_U_data);
               --  Call Implementation
               Result_Ü :=
                 test_all_optims.Impl.echoSixteenKb82
                    (test_all_optims.Impl.Object'Class
                       (Self.all)'Access,
                     data);
               --  Setting the result
               CORBA.ServerRequest.Set_Result
                 (Request,
                  test_all_optims.Helper.To_Any
                    (Result_Ü));
            end;
         elsif (Operation_Ü
            = "echoSixteenKb83")
         then
            declare
               data : test_all_optims.sixteenKb;
               Arg_Name_U_data : constant CORBA.Identifier :=
                 CORBA.To_CORBA_String
                    ("data");
               Argument_U_data : CORBA.Any :=
                 CORBA.Internals.Get_Empty_Any
                    (test_all_optims.Helper.TC_sixteenKb);
               Result_Ü : test_all_optims.sixteenKb;
            begin
               CORBA.NVList.Add_Item
                 (Argument_List_Ü,
                  Arg_Name_U_data,
                  Argument_U_data,
                  CORBA.ARG_IN);
               --  Processing request
               CORBA.ServerRequest.Arguments
                 (Request,
                  Argument_List_Ü);
               --  Setting Parameters
               data :=
                 test_all_optims.Helper.From_Any
                    (Argument_U_data);
               --  Call Implementation
               Result_Ü :=
                 test_all_optims.Impl.echoSixteenKb83
                    (test_all_optims.Impl.Object'Class
                       (Self.all)'Access,
                     data);
               --  Setting the result
               CORBA.ServerRequest.Set_Result
                 (Request,
                  test_all_optims.Helper.To_Any
                    (Result_Ü));
            end;
         elsif (Operation_Ü
            = "echoSixteenKb84")
         then
            declare
               data : test_all_optims.sixteenKb;
               Arg_Name_U_data : constant CORBA.Identifier :=
                 CORBA.To_CORBA_String
                    ("data");
               Argument_U_data : CORBA.Any :=
                 CORBA.Internals.Get_Empty_Any
                    (test_all_optims.Helper.TC_sixteenKb);
               Result_Ü : test_all_optims.sixteenKb;
            begin
               CORBA.NVList.Add_Item
                 (Argument_List_Ü,
                  Arg_Name_U_data,
                  Argument_U_data,
                  CORBA.ARG_IN);
               --  Processing request
               CORBA.ServerRequest.Arguments
                 (Request,
                  Argument_List_Ü);
               --  Setting Parameters
               data :=
                 test_all_optims.Helper.From_Any
                    (Argument_U_data);
               --  Call Implementation
               Result_Ü :=
                 test_all_optims.Impl.echoSixteenKb84
                    (test_all_optims.Impl.Object'Class
                       (Self.all)'Access,
                     data);
               --  Setting the result
               CORBA.ServerRequest.Set_Result
                 (Request,
                  test_all_optims.Helper.To_Any
                    (Result_Ü));
            end;
         elsif (Operation_Ü
            = "echoSixteenKb85")
         then
            declare
               data : test_all_optims.sixteenKb;
               Arg_Name_U_data : constant CORBA.Identifier :=
                 CORBA.To_CORBA_String
                    ("data");
               Argument_U_data : CORBA.Any :=
                 CORBA.Internals.Get_Empty_Any
                    (test_all_optims.Helper.TC_sixteenKb);
               Result_Ü : test_all_optims.sixteenKb;
            begin
               CORBA.NVList.Add_Item
                 (Argument_List_Ü,
                  Arg_Name_U_data,
                  Argument_U_data,
                  CORBA.ARG_IN);
               --  Processing request
               CORBA.ServerRequest.Arguments
                 (Request,
                  Argument_List_Ü);
               --  Setting Parameters
               data :=
                 test_all_optims.Helper.From_Any
                    (Argument_U_data);
               --  Call Implementation
               Result_Ü :=
                 test_all_optims.Impl.echoSixteenKb85
                    (test_all_optims.Impl.Object'Class
                       (Self.all)'Access,
                     data);
               --  Setting the result
               CORBA.ServerRequest.Set_Result
                 (Request,
                  test_all_optims.Helper.To_Any
                    (Result_Ü));
            end;
         elsif (Operation_Ü
            = "echoSixteenKb86")
         then
            declare
               data : test_all_optims.sixteenKb;
               Arg_Name_U_data : constant CORBA.Identifier :=
                 CORBA.To_CORBA_String
                    ("data");
               Argument_U_data : CORBA.Any :=
                 CORBA.Internals.Get_Empty_Any
                    (test_all_optims.Helper.TC_sixteenKb);
               Result_Ü : test_all_optims.sixteenKb;
            begin
               CORBA.NVList.Add_Item
                 (Argument_List_Ü,
                  Arg_Name_U_data,
                  Argument_U_data,
                  CORBA.ARG_IN);
               --  Processing request
               CORBA.ServerRequest.Arguments
                 (Request,
                  Argument_List_Ü);
               --  Setting Parameters
               data :=
                 test_all_optims.Helper.From_Any
                    (Argument_U_data);
               --  Call Implementation
               Result_Ü :=
                 test_all_optims.Impl.echoSixteenKb86
                    (test_all_optims.Impl.Object'Class
                       (Self.all)'Access,
                     data);
               --  Setting the result
               CORBA.ServerRequest.Set_Result
                 (Request,
                  test_all_optims.Helper.To_Any
                    (Result_Ü));
            end;
         elsif (Operation_Ü
            = "echoSixteenKb87")
         then
            declare
               data : test_all_optims.sixteenKb;
               Arg_Name_U_data : constant CORBA.Identifier :=
                 CORBA.To_CORBA_String
                    ("data");
               Argument_U_data : CORBA.Any :=
                 CORBA.Internals.Get_Empty_Any
                    (test_all_optims.Helper.TC_sixteenKb);
               Result_Ü : test_all_optims.sixteenKb;
            begin
               CORBA.NVList.Add_Item
                 (Argument_List_Ü,
                  Arg_Name_U_data,
                  Argument_U_data,
                  CORBA.ARG_IN);
               --  Processing request
               CORBA.ServerRequest.Arguments
                 (Request,
                  Argument_List_Ü);
               --  Setting Parameters
               data :=
                 test_all_optims.Helper.From_Any
                    (Argument_U_data);
               --  Call Implementation
               Result_Ü :=
                 test_all_optims.Impl.echoSixteenKb87
                    (test_all_optims.Impl.Object'Class
                       (Self.all)'Access,
                     data);
               --  Setting the result
               CORBA.ServerRequest.Set_Result
                 (Request,
                  test_all_optims.Helper.To_Any
                    (Result_Ü));
            end;
         elsif (Operation_Ü
            = "echoSixteenKb88")
         then
            declare
               data : test_all_optims.sixteenKb;
               Arg_Name_U_data : constant CORBA.Identifier :=
                 CORBA.To_CORBA_String
                    ("data");
               Argument_U_data : CORBA.Any :=
                 CORBA.Internals.Get_Empty_Any
                    (test_all_optims.Helper.TC_sixteenKb);
               Result_Ü : test_all_optims.sixteenKb;
            begin
               CORBA.NVList.Add_Item
                 (Argument_List_Ü,
                  Arg_Name_U_data,
                  Argument_U_data,
                  CORBA.ARG_IN);
               --  Processing request
               CORBA.ServerRequest.Arguments
                 (Request,
                  Argument_List_Ü);
               --  Setting Parameters
               data :=
                 test_all_optims.Helper.From_Any
                    (Argument_U_data);
               --  Call Implementation
               Result_Ü :=
                 test_all_optims.Impl.echoSixteenKb88
                    (test_all_optims.Impl.Object'Class
                       (Self.all)'Access,
                     data);
               --  Setting the result
               CORBA.ServerRequest.Set_Result
                 (Request,
                  test_all_optims.Helper.To_Any
                    (Result_Ü));
            end;
         elsif (Operation_Ü
            = "echoSixteenKb89")
         then
            declare
               data : test_all_optims.sixteenKb;
               Arg_Name_U_data : constant CORBA.Identifier :=
                 CORBA.To_CORBA_String
                    ("data");
               Argument_U_data : CORBA.Any :=
                 CORBA.Internals.Get_Empty_Any
                    (test_all_optims.Helper.TC_sixteenKb);
               Result_Ü : test_all_optims.sixteenKb;
            begin
               CORBA.NVList.Add_Item
                 (Argument_List_Ü,
                  Arg_Name_U_data,
                  Argument_U_data,
                  CORBA.ARG_IN);
               --  Processing request
               CORBA.ServerRequest.Arguments
                 (Request,
                  Argument_List_Ü);
               --  Setting Parameters
               data :=
                 test_all_optims.Helper.From_Any
                    (Argument_U_data);
               --  Call Implementation
               Result_Ü :=
                 test_all_optims.Impl.echoSixteenKb89
                    (test_all_optims.Impl.Object'Class
                       (Self.all)'Access,
                     data);
               --  Setting the result
               CORBA.ServerRequest.Set_Result
                 (Request,
                  test_all_optims.Helper.To_Any
                    (Result_Ü));
            end;
         elsif (Operation_Ü
            = "echoSixteenKb90")
         then
            declare
               data : test_all_optims.sixteenKb;
               Arg_Name_U_data : constant CORBA.Identifier :=
                 CORBA.To_CORBA_String
                    ("data");
               Argument_U_data : CORBA.Any :=
                 CORBA.Internals.Get_Empty_Any
                    (test_all_optims.Helper.TC_sixteenKb);
               Result_Ü : test_all_optims.sixteenKb;
            begin
               CORBA.NVList.Add_Item
                 (Argument_List_Ü,
                  Arg_Name_U_data,
                  Argument_U_data,
                  CORBA.ARG_IN);
               --  Processing request
               CORBA.ServerRequest.Arguments
                 (Request,
                  Argument_List_Ü);
               --  Setting Parameters
               data :=
                 test_all_optims.Helper.From_Any
                    (Argument_U_data);
               --  Call Implementation
               Result_Ü :=
                 test_all_optims.Impl.echoSixteenKb90
                    (test_all_optims.Impl.Object'Class
                       (Self.all)'Access,
                     data);
               --  Setting the result
               CORBA.ServerRequest.Set_Result
                 (Request,
                  test_all_optims.Helper.To_Any
                    (Result_Ü));
            end;
         elsif (Operation_Ü
            = "echoSixteenKb91")
         then
            declare
               data : test_all_optims.sixteenKb;
               Arg_Name_U_data : constant CORBA.Identifier :=
                 CORBA.To_CORBA_String
                    ("data");
               Argument_U_data : CORBA.Any :=
                 CORBA.Internals.Get_Empty_Any
                    (test_all_optims.Helper.TC_sixteenKb);
               Result_Ü : test_all_optims.sixteenKb;
            begin
               CORBA.NVList.Add_Item
                 (Argument_List_Ü,
                  Arg_Name_U_data,
                  Argument_U_data,
                  CORBA.ARG_IN);
               --  Processing request
               CORBA.ServerRequest.Arguments
                 (Request,
                  Argument_List_Ü);
               --  Setting Parameters
               data :=
                 test_all_optims.Helper.From_Any
                    (Argument_U_data);
               --  Call Implementation
               Result_Ü :=
                 test_all_optims.Impl.echoSixteenKb91
                    (test_all_optims.Impl.Object'Class
                       (Self.all)'Access,
                     data);
               --  Setting the result
               CORBA.ServerRequest.Set_Result
                 (Request,
                  test_all_optims.Helper.To_Any
                    (Result_Ü));
            end;
         elsif (Operation_Ü
            = "echoSixteenKb92")
         then
            declare
               data : test_all_optims.sixteenKb;
               Arg_Name_U_data : constant CORBA.Identifier :=
                 CORBA.To_CORBA_String
                    ("data");
               Argument_U_data : CORBA.Any :=
                 CORBA.Internals.Get_Empty_Any
                    (test_all_optims.Helper.TC_sixteenKb);
               Result_Ü : test_all_optims.sixteenKb;
            begin
               CORBA.NVList.Add_Item
                 (Argument_List_Ü,
                  Arg_Name_U_data,
                  Argument_U_data,
                  CORBA.ARG_IN);
               --  Processing request
               CORBA.ServerRequest.Arguments
                 (Request,
                  Argument_List_Ü);
               --  Setting Parameters
               data :=
                 test_all_optims.Helper.From_Any
                    (Argument_U_data);
               --  Call Implementation
               Result_Ü :=
                 test_all_optims.Impl.echoSixteenKb92
                    (test_all_optims.Impl.Object'Class
                       (Self.all)'Access,
                     data);
               --  Setting the result
               CORBA.ServerRequest.Set_Result
                 (Request,
                  test_all_optims.Helper.To_Any
                    (Result_Ü));
            end;
         elsif (Operation_Ü
            = "echoSixteenKb93")
         then
            declare
               data : test_all_optims.sixteenKb;
               Arg_Name_U_data : constant CORBA.Identifier :=
                 CORBA.To_CORBA_String
                    ("data");
               Argument_U_data : CORBA.Any :=
                 CORBA.Internals.Get_Empty_Any
                    (test_all_optims.Helper.TC_sixteenKb);
               Result_Ü : test_all_optims.sixteenKb;
            begin
               CORBA.NVList.Add_Item
                 (Argument_List_Ü,
                  Arg_Name_U_data,
                  Argument_U_data,
                  CORBA.ARG_IN);
               --  Processing request
               CORBA.ServerRequest.Arguments
                 (Request,
                  Argument_List_Ü);
               --  Setting Parameters
               data :=
                 test_all_optims.Helper.From_Any
                    (Argument_U_data);
               --  Call Implementation
               Result_Ü :=
                 test_all_optims.Impl.echoSixteenKb93
                    (test_all_optims.Impl.Object'Class
                       (Self.all)'Access,
                     data);
               --  Setting the result
               CORBA.ServerRequest.Set_Result
                 (Request,
                  test_all_optims.Helper.To_Any
                    (Result_Ü));
            end;
         elsif (Operation_Ü
            = "echoSixteenKb94")
         then
            declare
               data : test_all_optims.sixteenKb;
               Arg_Name_U_data : constant CORBA.Identifier :=
                 CORBA.To_CORBA_String
                    ("data");
               Argument_U_data : CORBA.Any :=
                 CORBA.Internals.Get_Empty_Any
                    (test_all_optims.Helper.TC_sixteenKb);
               Result_Ü : test_all_optims.sixteenKb;
            begin
               CORBA.NVList.Add_Item
                 (Argument_List_Ü,
                  Arg_Name_U_data,
                  Argument_U_data,
                  CORBA.ARG_IN);
               --  Processing request
               CORBA.ServerRequest.Arguments
                 (Request,
                  Argument_List_Ü);
               --  Setting Parameters
               data :=
                 test_all_optims.Helper.From_Any
                    (Argument_U_data);
               --  Call Implementation
               Result_Ü :=
                 test_all_optims.Impl.echoSixteenKb94
                    (test_all_optims.Impl.Object'Class
                       (Self.all)'Access,
                     data);
               --  Setting the result
               CORBA.ServerRequest.Set_Result
                 (Request,
                  test_all_optims.Helper.To_Any
                    (Result_Ü));
            end;
         elsif (Operation_Ü
            = "echoSixteenKb95")
         then
            declare
               data : test_all_optims.sixteenKb;
               Arg_Name_U_data : constant CORBA.Identifier :=
                 CORBA.To_CORBA_String
                    ("data");
               Argument_U_data : CORBA.Any :=
                 CORBA.Internals.Get_Empty_Any
                    (test_all_optims.Helper.TC_sixteenKb);
               Result_Ü : test_all_optims.sixteenKb;
            begin
               CORBA.NVList.Add_Item
                 (Argument_List_Ü,
                  Arg_Name_U_data,
                  Argument_U_data,
                  CORBA.ARG_IN);
               --  Processing request
               CORBA.ServerRequest.Arguments
                 (Request,
                  Argument_List_Ü);
               --  Setting Parameters
               data :=
                 test_all_optims.Helper.From_Any
                    (Argument_U_data);
               --  Call Implementation
               Result_Ü :=
                 test_all_optims.Impl.echoSixteenKb95
                    (test_all_optims.Impl.Object'Class
                       (Self.all)'Access,
                     data);
               --  Setting the result
               CORBA.ServerRequest.Set_Result
                 (Request,
                  test_all_optims.Helper.To_Any
                    (Result_Ü));
            end;
         elsif (Operation_Ü
            = "echoSixteenKb96")
         then
            declare
               data : test_all_optims.sixteenKb;
               Arg_Name_U_data : constant CORBA.Identifier :=
                 CORBA.To_CORBA_String
                    ("data");
               Argument_U_data : CORBA.Any :=
                 CORBA.Internals.Get_Empty_Any
                    (test_all_optims.Helper.TC_sixteenKb);
               Result_Ü : test_all_optims.sixteenKb;
            begin
               CORBA.NVList.Add_Item
                 (Argument_List_Ü,
                  Arg_Name_U_data,
                  Argument_U_data,
                  CORBA.ARG_IN);
               --  Processing request
               CORBA.ServerRequest.Arguments
                 (Request,
                  Argument_List_Ü);
               --  Setting Parameters
               data :=
                 test_all_optims.Helper.From_Any
                    (Argument_U_data);
               --  Call Implementation
               Result_Ü :=
                 test_all_optims.Impl.echoSixteenKb96
                    (test_all_optims.Impl.Object'Class
                       (Self.all)'Access,
                     data);
               --  Setting the result
               CORBA.ServerRequest.Set_Result
                 (Request,
                  test_all_optims.Helper.To_Any
                    (Result_Ü));
            end;
         elsif (Operation_Ü
            = "echoSixteenKb97")
         then
            declare
               data : test_all_optims.sixteenKb;
               Arg_Name_U_data : constant CORBA.Identifier :=
                 CORBA.To_CORBA_String
                    ("data");
               Argument_U_data : CORBA.Any :=
                 CORBA.Internals.Get_Empty_Any
                    (test_all_optims.Helper.TC_sixteenKb);
               Result_Ü : test_all_optims.sixteenKb;
            begin
               CORBA.NVList.Add_Item
                 (Argument_List_Ü,
                  Arg_Name_U_data,
                  Argument_U_data,
                  CORBA.ARG_IN);
               --  Processing request
               CORBA.ServerRequest.Arguments
                 (Request,
                  Argument_List_Ü);
               --  Setting Parameters
               data :=
                 test_all_optims.Helper.From_Any
                    (Argument_U_data);
               --  Call Implementation
               Result_Ü :=
                 test_all_optims.Impl.echoSixteenKb97
                    (test_all_optims.Impl.Object'Class
                       (Self.all)'Access,
                     data);
               --  Setting the result
               CORBA.ServerRequest.Set_Result
                 (Request,
                  test_all_optims.Helper.To_Any
                    (Result_Ü));
            end;
         elsif (Operation_Ü
            = "echoSixteenKb98")
         then
            declare
               data : test_all_optims.sixteenKb;
               Arg_Name_U_data : constant CORBA.Identifier :=
                 CORBA.To_CORBA_String
                    ("data");
               Argument_U_data : CORBA.Any :=
                 CORBA.Internals.Get_Empty_Any
                    (test_all_optims.Helper.TC_sixteenKb);
               Result_Ü : test_all_optims.sixteenKb;
            begin
               CORBA.NVList.Add_Item
                 (Argument_List_Ü,
                  Arg_Name_U_data,
                  Argument_U_data,
                  CORBA.ARG_IN);
               --  Processing request
               CORBA.ServerRequest.Arguments
                 (Request,
                  Argument_List_Ü);
               --  Setting Parameters
               data :=
                 test_all_optims.Helper.From_Any
                    (Argument_U_data);
               --  Call Implementation
               Result_Ü :=
                 test_all_optims.Impl.echoSixteenKb98
                    (test_all_optims.Impl.Object'Class
                       (Self.all)'Access,
                     data);
               --  Setting the result
               CORBA.ServerRequest.Set_Result
                 (Request,
                  test_all_optims.Helper.To_Any
                    (Result_Ü));
            end;
         elsif (Operation_Ü
            = "echoSixteenKb99")
         then
            declare
               data : test_all_optims.sixteenKb;
               Arg_Name_U_data : constant CORBA.Identifier :=
                 CORBA.To_CORBA_String
                    ("data");
               Argument_U_data : CORBA.Any :=
                 CORBA.Internals.Get_Empty_Any
                    (test_all_optims.Helper.TC_sixteenKb);
               Result_Ü : test_all_optims.sixteenKb;
            begin
               CORBA.NVList.Add_Item
                 (Argument_List_Ü,
                  Arg_Name_U_data,
                  Argument_U_data,
                  CORBA.ARG_IN);
               --  Processing request
               CORBA.ServerRequest.Arguments
                 (Request,
                  Argument_List_Ü);
               --  Setting Parameters
               data :=
                 test_all_optims.Helper.From_Any
                    (Argument_U_data);
               --  Call Implementation
               Result_Ü :=
                 test_all_optims.Impl.echoSixteenKb99
                    (test_all_optims.Impl.Object'Class
                       (Self.all)'Access,
                     data);
               --  Setting the result
               CORBA.ServerRequest.Set_Result
                 (Request,
                  test_all_optims.Helper.To_Any
                    (Result_Ü));
            end;
         elsif (Operation_Ü
            = "_interface")
         then
            begin
               PolyORB.CORBA_P.Implicit_CORBA_Methods.Handle_Interface
                 (Request,
                  Argument_List_Ü,
                  Repository_Id);
            end;
         elsif (Operation_Ü
            = "_domain_managers")
         then
            begin
               PolyORB.CORBA_P.Implicit_CORBA_Methods.Handle_Domain_Managers
                 (Self,
                  Request,
                  Argument_List_Ü);
            end;
         elsif (Operation_Ü
            = "_non_existent")
         then
            begin
               PolyORB.CORBA_P.Implicit_CORBA_Methods.Handle_Non_Existent
                 (Request,
                  Argument_List_Ü);
            end;
         elsif (Operation_Ü
            = "_not_existent")
         then
            begin
               PolyORB.CORBA_P.Implicit_CORBA_Methods.Handle_Non_Existent
                 (Request,
                  Argument_List_Ü);
            end;
         else
            CORBA.Raise_Bad_Operation
              (CORBA.Default_Sys_Member);
         end if;
      exception
         when E : others =>
            begin
               CORBA.ServerRequest.Set_Exception
                 (Request,
                  CORBA.Internals.To_CORBA_Any
                    (PolyORB.CORBA_P.Exceptions.System_Exception_To_Any
                       (E)));
               return;
            end;
      end;
      T2 := Clock;
      Delta1 := Ada.Real_Time.To_Duration (T2 - T1);
      Insert_Duration (Index_Ü, Delta1);
   end Invoke;

   function Servant_Is_A
     (Obj : in PortableServer.Servant)
     return Boolean;

   function Servant_Is_A
     (Obj : in PortableServer.Servant)
     return Boolean
   is
   begin
      return (Obj.all
         in test_all_optims.Impl.Object'Class);
   end Servant_Is_A;

   procedure Deferred_Initialization

   is
   begin
      PortableServer.Internals.Register_Skeleton
        (CORBA.To_CORBA_String
           (test_all_optims.Repository_Id),
         Servant_Is_A'Access,
         Is_A'Access,
         Invoke'Access);

      --  Initialize the statistics module

      Initialise_Stats (100);
   end Deferred_Initialization;

begin
   PolyORB.Initialization.Register_Module
     (PolyORB.Initialization.Module_Info'
        (Name => PolyORB.Utils.Strings."+"
           ("test_all_optims.Skel"),
         Conflicts => PolyORB.Utils.Strings.Lists.Empty,
         Depends => PolyORB.Utils.Strings.Lists.Empty,
         Provides => PolyORB.Utils.Strings.Lists.Empty,
         Implicit => False,
         Init => Deferred_Initialization'Access));
end test_all_optims.Skel;

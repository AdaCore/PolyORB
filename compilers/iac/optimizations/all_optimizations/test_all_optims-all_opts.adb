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
with PolyORB.Errors;
with test_all_optims.CDR;
with PolyORB.Requests;
with PolyORB.CORBA_P.Exceptions;
with PolyORB.CORBA_P.Implicit_CORBA_Methods;
with test_all_optims_Hash;
with PortableServer;
with CORBA.ServerRequest;
with CORBA;
with CORBA.NVList;
with PolyORB.Utils.Strings;
with PolyORB.Utils.Strings.Lists;
with PolyORB.Initialization;

--  Added to test skeleton Optimisation

with Ada.Real_Time;  use Ada.Real_Time;
with Statistics;     use Statistics;

package body test_all_optims.Skel is

   N_Operations : constant Standard.Natural :=
     105;

   type String_Ptr is
     access Standard.String;

   Invoke_Db : array (0 .. (N_Operations
      - 1)) of String_Ptr :=
     (others => null);

   procedure Register_Procedure
     (Operation_Name : in Standard.String);

   procedure Register_Procedure
     (Operation_Name : in Standard.String)
   is
      Index_Ü : Standard.Natural;
      Invoke_Name_Access : String_Ptr;
   begin
      Index_Ü :=
        test_all_optims_Hash.Hash
           (Operation_Name);
      if (Invoke_Db
        (Index_Ü)
         /= null)
      then
         raise Program_Error;
      end if;
      Invoke_Name_Access :=
        new Standard.String'
           (Operation_Name);
      Invoke_Db
        (Index_Ü) :=
        Invoke_Name_Access;
   end Register_Procedure;

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
      Index_Ü : Standard.Natural;
      Invoke_Name_Access : String_Ptr;
   begin
      T1 := Clock;
      Index_Ü :=
        test_all_optims_Hash.Hash
           (Operation_Ü);
      Invoke_Name_Access :=
        Invoke_Db
           (Index_Ü);
      begin
         if (Operation_Ü
            = Invoke_Name_Access.all)
         then
            case Index_Ü is
               when 0 =>
                  declare
                     data : test_all_optims.sixteenKb;
                     Error : PolyORB.Errors.Error_Container;
                     Result_Ü : test_all_optims.sixteenKb;
                  begin
                     --  Setting the request Payload
                     test_all_optims.CDR.echoSixteenKb00_Set_Args
                       (Request,
                        new test_all_optims.CDR.echoSixteenKb00_Args_Type);
                     --  Processing request
                     PolyORB.Requests.Arguments
                       (PolyORB.Requests.Request_Access
                          (Request),
                        Request.Payload,
                        Error);
                     if PolyORB.Errors.Found
                       (Error)
                     then
                        PolyORB.CORBA_P.Exceptions.Raise_From_Error
                          (Error);
                     end if;
                     --  Setting Parameters
                     data :=
                       test_all_optims.CDR.echoSixteenKb00_Args_Type
                          (Request.Payload.Args.all).data;
                     --  Call Implementation
                     Result_Ü :=
                       test_all_optims.Impl.echoSixteenKb00
                          (test_all_optims.Impl.Object'Class
                             (Self.all)'Access,
                           data);
                     --  Setting the result
                     test_all_optims.CDR.echoSixteenKb00_Args_Type
                       (Request.Payload.Args.all).Returns :=
                       Result_Ü;
                  end;
               when 1 =>
                  declare
                     data : test_all_optims.sixteenKb;
                     Error : PolyORB.Errors.Error_Container;
                     Result_Ü : test_all_optims.sixteenKb;
                  begin
                     --  Setting the request Payload
                     test_all_optims.CDR.echoSixteenKb01_Set_Args
                       (Request,
                        new test_all_optims.CDR.echoSixteenKb01_Args_Type);
                     --  Processing request
                     PolyORB.Requests.Arguments
                       (PolyORB.Requests.Request_Access
                          (Request),
                        Request.Payload,
                        Error);
                     if PolyORB.Errors.Found
                       (Error)
                     then
                        PolyORB.CORBA_P.Exceptions.Raise_From_Error
                          (Error);
                     end if;
                     --  Setting Parameters
                     data :=
                       test_all_optims.CDR.echoSixteenKb01_Args_Type
                          (Request.Payload.Args.all).data;
                     --  Call Implementation
                     Result_Ü :=
                       test_all_optims.Impl.echoSixteenKb01
                          (test_all_optims.Impl.Object'Class
                             (Self.all)'Access,
                           data);
                     --  Setting the result
                     test_all_optims.CDR.echoSixteenKb01_Args_Type
                       (Request.Payload.Args.all).Returns :=
                       Result_Ü;
                  end;
               when 2 =>
                  declare
                     data : test_all_optims.sixteenKb;
                     Error : PolyORB.Errors.Error_Container;
                     Result_Ü : test_all_optims.sixteenKb;
                  begin
                     --  Setting the request Payload
                     test_all_optims.CDR.echoSixteenKb02_Set_Args
                       (Request,
                        new test_all_optims.CDR.echoSixteenKb02_Args_Type);
                     --  Processing request
                     PolyORB.Requests.Arguments
                       (PolyORB.Requests.Request_Access
                          (Request),
                        Request.Payload,
                        Error);
                     if PolyORB.Errors.Found
                       (Error)
                     then
                        PolyORB.CORBA_P.Exceptions.Raise_From_Error
                          (Error);
                     end if;
                     --  Setting Parameters
                     data :=
                       test_all_optims.CDR.echoSixteenKb02_Args_Type
                          (Request.Payload.Args.all).data;
                     --  Call Implementation
                     Result_Ü :=
                       test_all_optims.Impl.echoSixteenKb02
                          (test_all_optims.Impl.Object'Class
                             (Self.all)'Access,
                           data);
                     --  Setting the result
                     test_all_optims.CDR.echoSixteenKb02_Args_Type
                       (Request.Payload.Args.all).Returns :=
                       Result_Ü;
                  end;
               when 3 =>
                  declare
                     data : test_all_optims.sixteenKb;
                     Error : PolyORB.Errors.Error_Container;
                     Result_Ü : test_all_optims.sixteenKb;
                  begin
                     --  Setting the request Payload
                     test_all_optims.CDR.echoSixteenKb03_Set_Args
                       (Request,
                        new test_all_optims.CDR.echoSixteenKb03_Args_Type);
                     --  Processing request
                     PolyORB.Requests.Arguments
                       (PolyORB.Requests.Request_Access
                          (Request),
                        Request.Payload,
                        Error);
                     if PolyORB.Errors.Found
                       (Error)
                     then
                        PolyORB.CORBA_P.Exceptions.Raise_From_Error
                          (Error);
                     end if;
                     --  Setting Parameters
                     data :=
                       test_all_optims.CDR.echoSixteenKb03_Args_Type
                          (Request.Payload.Args.all).data;
                     --  Call Implementation
                     Result_Ü :=
                       test_all_optims.Impl.echoSixteenKb03
                          (test_all_optims.Impl.Object'Class
                             (Self.all)'Access,
                           data);
                     --  Setting the result
                     test_all_optims.CDR.echoSixteenKb03_Args_Type
                       (Request.Payload.Args.all).Returns :=
                       Result_Ü;
                  end;
               when 4 =>
                  declare
                     data : test_all_optims.sixteenKb;
                     Error : PolyORB.Errors.Error_Container;
                     Result_Ü : test_all_optims.sixteenKb;
                  begin
                     --  Setting the request Payload
                     test_all_optims.CDR.echoSixteenKb04_Set_Args
                       (Request,
                        new test_all_optims.CDR.echoSixteenKb04_Args_Type);
                     --  Processing request
                     PolyORB.Requests.Arguments
                       (PolyORB.Requests.Request_Access
                          (Request),
                        Request.Payload,
                        Error);
                     if PolyORB.Errors.Found
                       (Error)
                     then
                        PolyORB.CORBA_P.Exceptions.Raise_From_Error
                          (Error);
                     end if;
                     --  Setting Parameters
                     data :=
                       test_all_optims.CDR.echoSixteenKb04_Args_Type
                          (Request.Payload.Args.all).data;
                     --  Call Implementation
                     Result_Ü :=
                       test_all_optims.Impl.echoSixteenKb04
                          (test_all_optims.Impl.Object'Class
                             (Self.all)'Access,
                           data);
                     --  Setting the result
                     test_all_optims.CDR.echoSixteenKb04_Args_Type
                       (Request.Payload.Args.all).Returns :=
                       Result_Ü;
                  end;
               when 5 =>
                  declare
                     data : test_all_optims.sixteenKb;
                     Error : PolyORB.Errors.Error_Container;
                     Result_Ü : test_all_optims.sixteenKb;
                  begin
                     --  Setting the request Payload
                     test_all_optims.CDR.echoSixteenKb05_Set_Args
                       (Request,
                        new test_all_optims.CDR.echoSixteenKb05_Args_Type);
                     --  Processing request
                     PolyORB.Requests.Arguments
                       (PolyORB.Requests.Request_Access
                          (Request),
                        Request.Payload,
                        Error);
                     if PolyORB.Errors.Found
                       (Error)
                     then
                        PolyORB.CORBA_P.Exceptions.Raise_From_Error
                          (Error);
                     end if;
                     --  Setting Parameters
                     data :=
                       test_all_optims.CDR.echoSixteenKb05_Args_Type
                          (Request.Payload.Args.all).data;
                     --  Call Implementation
                     Result_Ü :=
                       test_all_optims.Impl.echoSixteenKb05
                          (test_all_optims.Impl.Object'Class
                             (Self.all)'Access,
                           data);
                     --  Setting the result
                     test_all_optims.CDR.echoSixteenKb05_Args_Type
                       (Request.Payload.Args.all).Returns :=
                       Result_Ü;
                  end;
               when 6 =>
                  declare
                     data : test_all_optims.sixteenKb;
                     Error : PolyORB.Errors.Error_Container;
                     Result_Ü : test_all_optims.sixteenKb;
                  begin
                     --  Setting the request Payload
                     test_all_optims.CDR.echoSixteenKb06_Set_Args
                       (Request,
                        new test_all_optims.CDR.echoSixteenKb06_Args_Type);
                     --  Processing request
                     PolyORB.Requests.Arguments
                       (PolyORB.Requests.Request_Access
                          (Request),
                        Request.Payload,
                        Error);
                     if PolyORB.Errors.Found
                       (Error)
                     then
                        PolyORB.CORBA_P.Exceptions.Raise_From_Error
                          (Error);
                     end if;
                     --  Setting Parameters
                     data :=
                       test_all_optims.CDR.echoSixteenKb06_Args_Type
                          (Request.Payload.Args.all).data;
                     --  Call Implementation
                     Result_Ü :=
                       test_all_optims.Impl.echoSixteenKb06
                          (test_all_optims.Impl.Object'Class
                             (Self.all)'Access,
                           data);
                     --  Setting the result
                     test_all_optims.CDR.echoSixteenKb06_Args_Type
                       (Request.Payload.Args.all).Returns :=
                       Result_Ü;
                  end;
               when 7 =>
                  declare
                     data : test_all_optims.sixteenKb;
                     Error : PolyORB.Errors.Error_Container;
                     Result_Ü : test_all_optims.sixteenKb;
                  begin
                     --  Setting the request Payload
                     test_all_optims.CDR.echoSixteenKb07_Set_Args
                       (Request,
                        new test_all_optims.CDR.echoSixteenKb07_Args_Type);
                     --  Processing request
                     PolyORB.Requests.Arguments
                       (PolyORB.Requests.Request_Access
                          (Request),
                        Request.Payload,
                        Error);
                     if PolyORB.Errors.Found
                       (Error)
                     then
                        PolyORB.CORBA_P.Exceptions.Raise_From_Error
                          (Error);
                     end if;
                     --  Setting Parameters
                     data :=
                       test_all_optims.CDR.echoSixteenKb07_Args_Type
                          (Request.Payload.Args.all).data;
                     --  Call Implementation
                     Result_Ü :=
                       test_all_optims.Impl.echoSixteenKb07
                          (test_all_optims.Impl.Object'Class
                             (Self.all)'Access,
                           data);
                     --  Setting the result
                     test_all_optims.CDR.echoSixteenKb07_Args_Type
                       (Request.Payload.Args.all).Returns :=
                       Result_Ü;
                  end;
               when 8 =>
                  declare
                     data : test_all_optims.sixteenKb;
                     Error : PolyORB.Errors.Error_Container;
                     Result_Ü : test_all_optims.sixteenKb;
                  begin
                     --  Setting the request Payload
                     test_all_optims.CDR.echoSixteenKb08_Set_Args
                       (Request,
                        new test_all_optims.CDR.echoSixteenKb08_Args_Type);
                     --  Processing request
                     PolyORB.Requests.Arguments
                       (PolyORB.Requests.Request_Access
                          (Request),
                        Request.Payload,
                        Error);
                     if PolyORB.Errors.Found
                       (Error)
                     then
                        PolyORB.CORBA_P.Exceptions.Raise_From_Error
                          (Error);
                     end if;
                     --  Setting Parameters
                     data :=
                       test_all_optims.CDR.echoSixteenKb08_Args_Type
                          (Request.Payload.Args.all).data;
                     --  Call Implementation
                     Result_Ü :=
                       test_all_optims.Impl.echoSixteenKb08
                          (test_all_optims.Impl.Object'Class
                             (Self.all)'Access,
                           data);
                     --  Setting the result
                     test_all_optims.CDR.echoSixteenKb08_Args_Type
                       (Request.Payload.Args.all).Returns :=
                       Result_Ü;
                  end;
               when 9 =>
                  declare
                     data : test_all_optims.sixteenKb;
                     Error : PolyORB.Errors.Error_Container;
                     Result_Ü : test_all_optims.sixteenKb;
                  begin
                     --  Setting the request Payload
                     test_all_optims.CDR.echoSixteenKb09_Set_Args
                       (Request,
                        new test_all_optims.CDR.echoSixteenKb09_Args_Type);
                     --  Processing request
                     PolyORB.Requests.Arguments
                       (PolyORB.Requests.Request_Access
                          (Request),
                        Request.Payload,
                        Error);
                     if PolyORB.Errors.Found
                       (Error)
                     then
                        PolyORB.CORBA_P.Exceptions.Raise_From_Error
                          (Error);
                     end if;
                     --  Setting Parameters
                     data :=
                       test_all_optims.CDR.echoSixteenKb09_Args_Type
                          (Request.Payload.Args.all).data;
                     --  Call Implementation
                     Result_Ü :=
                       test_all_optims.Impl.echoSixteenKb09
                          (test_all_optims.Impl.Object'Class
                             (Self.all)'Access,
                           data);
                     --  Setting the result
                     test_all_optims.CDR.echoSixteenKb09_Args_Type
                       (Request.Payload.Args.all).Returns :=
                       Result_Ü;
                  end;
               when 10 =>
                  declare
                     data : test_all_optims.sixteenKb;
                     Error : PolyORB.Errors.Error_Container;
                     Result_Ü : test_all_optims.sixteenKb;
                  begin
                     --  Setting the request Payload
                     test_all_optims.CDR.echoSixteenKb10_Set_Args
                       (Request,
                        new test_all_optims.CDR.echoSixteenKb10_Args_Type);
                     --  Processing request
                     PolyORB.Requests.Arguments
                       (PolyORB.Requests.Request_Access
                          (Request),
                        Request.Payload,
                        Error);
                     if PolyORB.Errors.Found
                       (Error)
                     then
                        PolyORB.CORBA_P.Exceptions.Raise_From_Error
                          (Error);
                     end if;
                     --  Setting Parameters
                     data :=
                       test_all_optims.CDR.echoSixteenKb10_Args_Type
                          (Request.Payload.Args.all).data;
                     --  Call Implementation
                     Result_Ü :=
                       test_all_optims.Impl.echoSixteenKb10
                          (test_all_optims.Impl.Object'Class
                             (Self.all)'Access,
                           data);
                     --  Setting the result
                     test_all_optims.CDR.echoSixteenKb10_Args_Type
                       (Request.Payload.Args.all).Returns :=
                       Result_Ü;
                  end;
               when 11 =>
                  declare
                     data : test_all_optims.sixteenKb;
                     Error : PolyORB.Errors.Error_Container;
                     Result_Ü : test_all_optims.sixteenKb;
                  begin
                     --  Setting the request Payload
                     test_all_optims.CDR.echoSixteenKb11_Set_Args
                       (Request,
                        new test_all_optims.CDR.echoSixteenKb11_Args_Type);
                     --  Processing request
                     PolyORB.Requests.Arguments
                       (PolyORB.Requests.Request_Access
                          (Request),
                        Request.Payload,
                        Error);
                     if PolyORB.Errors.Found
                       (Error)
                     then
                        PolyORB.CORBA_P.Exceptions.Raise_From_Error
                          (Error);
                     end if;
                     --  Setting Parameters
                     data :=
                       test_all_optims.CDR.echoSixteenKb11_Args_Type
                          (Request.Payload.Args.all).data;
                     --  Call Implementation
                     Result_Ü :=
                       test_all_optims.Impl.echoSixteenKb11
                          (test_all_optims.Impl.Object'Class
                             (Self.all)'Access,
                           data);
                     --  Setting the result
                     test_all_optims.CDR.echoSixteenKb11_Args_Type
                       (Request.Payload.Args.all).Returns :=
                       Result_Ü;
                  end;
               when 12 =>
                  declare
                     data : test_all_optims.sixteenKb;
                     Error : PolyORB.Errors.Error_Container;
                     Result_Ü : test_all_optims.sixteenKb;
                  begin
                     --  Setting the request Payload
                     test_all_optims.CDR.echoSixteenKb12_Set_Args
                       (Request,
                        new test_all_optims.CDR.echoSixteenKb12_Args_Type);
                     --  Processing request
                     PolyORB.Requests.Arguments
                       (PolyORB.Requests.Request_Access
                          (Request),
                        Request.Payload,
                        Error);
                     if PolyORB.Errors.Found
                       (Error)
                     then
                        PolyORB.CORBA_P.Exceptions.Raise_From_Error
                          (Error);
                     end if;
                     --  Setting Parameters
                     data :=
                       test_all_optims.CDR.echoSixteenKb12_Args_Type
                          (Request.Payload.Args.all).data;
                     --  Call Implementation
                     Result_Ü :=
                       test_all_optims.Impl.echoSixteenKb12
                          (test_all_optims.Impl.Object'Class
                             (Self.all)'Access,
                           data);
                     --  Setting the result
                     test_all_optims.CDR.echoSixteenKb12_Args_Type
                       (Request.Payload.Args.all).Returns :=
                       Result_Ü;
                  end;
               when 13 =>
                  declare
                     data : test_all_optims.sixteenKb;
                     Error : PolyORB.Errors.Error_Container;
                     Result_Ü : test_all_optims.sixteenKb;
                  begin
                     --  Setting the request Payload
                     test_all_optims.CDR.echoSixteenKb13_Set_Args
                       (Request,
                        new test_all_optims.CDR.echoSixteenKb13_Args_Type);
                     --  Processing request
                     PolyORB.Requests.Arguments
                       (PolyORB.Requests.Request_Access
                          (Request),
                        Request.Payload,
                        Error);
                     if PolyORB.Errors.Found
                       (Error)
                     then
                        PolyORB.CORBA_P.Exceptions.Raise_From_Error
                          (Error);
                     end if;
                     --  Setting Parameters
                     data :=
                       test_all_optims.CDR.echoSixteenKb13_Args_Type
                          (Request.Payload.Args.all).data;
                     --  Call Implementation
                     Result_Ü :=
                       test_all_optims.Impl.echoSixteenKb13
                          (test_all_optims.Impl.Object'Class
                             (Self.all)'Access,
                           data);
                     --  Setting the result
                     test_all_optims.CDR.echoSixteenKb13_Args_Type
                       (Request.Payload.Args.all).Returns :=
                       Result_Ü;
                  end;
               when 14 =>
                  declare
                     data : test_all_optims.sixteenKb;
                     Error : PolyORB.Errors.Error_Container;
                     Result_Ü : test_all_optims.sixteenKb;
                  begin
                     --  Setting the request Payload
                     test_all_optims.CDR.echoSixteenKb14_Set_Args
                       (Request,
                        new test_all_optims.CDR.echoSixteenKb14_Args_Type);
                     --  Processing request
                     PolyORB.Requests.Arguments
                       (PolyORB.Requests.Request_Access
                          (Request),
                        Request.Payload,
                        Error);
                     if PolyORB.Errors.Found
                       (Error)
                     then
                        PolyORB.CORBA_P.Exceptions.Raise_From_Error
                          (Error);
                     end if;
                     --  Setting Parameters
                     data :=
                       test_all_optims.CDR.echoSixteenKb14_Args_Type
                          (Request.Payload.Args.all).data;
                     --  Call Implementation
                     Result_Ü :=
                       test_all_optims.Impl.echoSixteenKb14
                          (test_all_optims.Impl.Object'Class
                             (Self.all)'Access,
                           data);
                     --  Setting the result
                     test_all_optims.CDR.echoSixteenKb14_Args_Type
                       (Request.Payload.Args.all).Returns :=
                       Result_Ü;
                  end;
               when 15 =>
                  declare
                     data : test_all_optims.sixteenKb;
                     Error : PolyORB.Errors.Error_Container;
                     Result_Ü : test_all_optims.sixteenKb;
                  begin
                     --  Setting the request Payload
                     test_all_optims.CDR.echoSixteenKb15_Set_Args
                       (Request,
                        new test_all_optims.CDR.echoSixteenKb15_Args_Type);
                     --  Processing request
                     PolyORB.Requests.Arguments
                       (PolyORB.Requests.Request_Access
                          (Request),
                        Request.Payload,
                        Error);
                     if PolyORB.Errors.Found
                       (Error)
                     then
                        PolyORB.CORBA_P.Exceptions.Raise_From_Error
                          (Error);
                     end if;
                     --  Setting Parameters
                     data :=
                       test_all_optims.CDR.echoSixteenKb15_Args_Type
                          (Request.Payload.Args.all).data;
                     --  Call Implementation
                     Result_Ü :=
                       test_all_optims.Impl.echoSixteenKb15
                          (test_all_optims.Impl.Object'Class
                             (Self.all)'Access,
                           data);
                     --  Setting the result
                     test_all_optims.CDR.echoSixteenKb15_Args_Type
                       (Request.Payload.Args.all).Returns :=
                       Result_Ü;
                  end;
               when 16 =>
                  declare
                     data : test_all_optims.sixteenKb;
                     Error : PolyORB.Errors.Error_Container;
                     Result_Ü : test_all_optims.sixteenKb;
                  begin
                     --  Setting the request Payload
                     test_all_optims.CDR.echoSixteenKb16_Set_Args
                       (Request,
                        new test_all_optims.CDR.echoSixteenKb16_Args_Type);
                     --  Processing request
                     PolyORB.Requests.Arguments
                       (PolyORB.Requests.Request_Access
                          (Request),
                        Request.Payload,
                        Error);
                     if PolyORB.Errors.Found
                       (Error)
                     then
                        PolyORB.CORBA_P.Exceptions.Raise_From_Error
                          (Error);
                     end if;
                     --  Setting Parameters
                     data :=
                       test_all_optims.CDR.echoSixteenKb16_Args_Type
                          (Request.Payload.Args.all).data;
                     --  Call Implementation
                     Result_Ü :=
                       test_all_optims.Impl.echoSixteenKb16
                          (test_all_optims.Impl.Object'Class
                             (Self.all)'Access,
                           data);
                     --  Setting the result
                     test_all_optims.CDR.echoSixteenKb16_Args_Type
                       (Request.Payload.Args.all).Returns :=
                       Result_Ü;
                  end;
               when 17 =>
                  declare
                     data : test_all_optims.sixteenKb;
                     Error : PolyORB.Errors.Error_Container;
                     Result_Ü : test_all_optims.sixteenKb;
                  begin
                     --  Setting the request Payload
                     test_all_optims.CDR.echoSixteenKb17_Set_Args
                       (Request,
                        new test_all_optims.CDR.echoSixteenKb17_Args_Type);
                     --  Processing request
                     PolyORB.Requests.Arguments
                       (PolyORB.Requests.Request_Access
                          (Request),
                        Request.Payload,
                        Error);
                     if PolyORB.Errors.Found
                       (Error)
                     then
                        PolyORB.CORBA_P.Exceptions.Raise_From_Error
                          (Error);
                     end if;
                     --  Setting Parameters
                     data :=
                       test_all_optims.CDR.echoSixteenKb17_Args_Type
                          (Request.Payload.Args.all).data;
                     --  Call Implementation
                     Result_Ü :=
                       test_all_optims.Impl.echoSixteenKb17
                          (test_all_optims.Impl.Object'Class
                             (Self.all)'Access,
                           data);
                     --  Setting the result
                     test_all_optims.CDR.echoSixteenKb17_Args_Type
                       (Request.Payload.Args.all).Returns :=
                       Result_Ü;
                  end;
               when 18 =>
                  declare
                     data : test_all_optims.sixteenKb;
                     Error : PolyORB.Errors.Error_Container;
                     Result_Ü : test_all_optims.sixteenKb;
                  begin
                     --  Setting the request Payload
                     test_all_optims.CDR.echoSixteenKb18_Set_Args
                       (Request,
                        new test_all_optims.CDR.echoSixteenKb18_Args_Type);
                     --  Processing request
                     PolyORB.Requests.Arguments
                       (PolyORB.Requests.Request_Access
                          (Request),
                        Request.Payload,
                        Error);
                     if PolyORB.Errors.Found
                       (Error)
                     then
                        PolyORB.CORBA_P.Exceptions.Raise_From_Error
                          (Error);
                     end if;
                     --  Setting Parameters
                     data :=
                       test_all_optims.CDR.echoSixteenKb18_Args_Type
                          (Request.Payload.Args.all).data;
                     --  Call Implementation
                     Result_Ü :=
                       test_all_optims.Impl.echoSixteenKb18
                          (test_all_optims.Impl.Object'Class
                             (Self.all)'Access,
                           data);
                     --  Setting the result
                     test_all_optims.CDR.echoSixteenKb18_Args_Type
                       (Request.Payload.Args.all).Returns :=
                       Result_Ü;
                  end;
               when 19 =>
                  declare
                     data : test_all_optims.sixteenKb;
                     Error : PolyORB.Errors.Error_Container;
                     Result_Ü : test_all_optims.sixteenKb;
                  begin
                     --  Setting the request Payload
                     test_all_optims.CDR.echoSixteenKb19_Set_Args
                       (Request,
                        new test_all_optims.CDR.echoSixteenKb19_Args_Type);
                     --  Processing request
                     PolyORB.Requests.Arguments
                       (PolyORB.Requests.Request_Access
                          (Request),
                        Request.Payload,
                        Error);
                     if PolyORB.Errors.Found
                       (Error)
                     then
                        PolyORB.CORBA_P.Exceptions.Raise_From_Error
                          (Error);
                     end if;
                     --  Setting Parameters
                     data :=
                       test_all_optims.CDR.echoSixteenKb19_Args_Type
                          (Request.Payload.Args.all).data;
                     --  Call Implementation
                     Result_Ü :=
                       test_all_optims.Impl.echoSixteenKb19
                          (test_all_optims.Impl.Object'Class
                             (Self.all)'Access,
                           data);
                     --  Setting the result
                     test_all_optims.CDR.echoSixteenKb19_Args_Type
                       (Request.Payload.Args.all).Returns :=
                       Result_Ü;
                  end;
               when 20 =>
                  declare
                     data : test_all_optims.sixteenKb;
                     Error : PolyORB.Errors.Error_Container;
                     Result_Ü : test_all_optims.sixteenKb;
                  begin
                     --  Setting the request Payload
                     test_all_optims.CDR.echoSixteenKb20_Set_Args
                       (Request,
                        new test_all_optims.CDR.echoSixteenKb20_Args_Type);
                     --  Processing request
                     PolyORB.Requests.Arguments
                       (PolyORB.Requests.Request_Access
                          (Request),
                        Request.Payload,
                        Error);
                     if PolyORB.Errors.Found
                       (Error)
                     then
                        PolyORB.CORBA_P.Exceptions.Raise_From_Error
                          (Error);
                     end if;
                     --  Setting Parameters
                     data :=
                       test_all_optims.CDR.echoSixteenKb20_Args_Type
                          (Request.Payload.Args.all).data;
                     --  Call Implementation
                     Result_Ü :=
                       test_all_optims.Impl.echoSixteenKb20
                          (test_all_optims.Impl.Object'Class
                             (Self.all)'Access,
                           data);
                     --  Setting the result
                     test_all_optims.CDR.echoSixteenKb20_Args_Type
                       (Request.Payload.Args.all).Returns :=
                       Result_Ü;
                  end;
               when 21 =>
                  declare
                     data : test_all_optims.sixteenKb;
                     Error : PolyORB.Errors.Error_Container;
                     Result_Ü : test_all_optims.sixteenKb;
                  begin
                     --  Setting the request Payload
                     test_all_optims.CDR.echoSixteenKb21_Set_Args
                       (Request,
                        new test_all_optims.CDR.echoSixteenKb21_Args_Type);
                     --  Processing request
                     PolyORB.Requests.Arguments
                       (PolyORB.Requests.Request_Access
                          (Request),
                        Request.Payload,
                        Error);
                     if PolyORB.Errors.Found
                       (Error)
                     then
                        PolyORB.CORBA_P.Exceptions.Raise_From_Error
                          (Error);
                     end if;
                     --  Setting Parameters
                     data :=
                       test_all_optims.CDR.echoSixteenKb21_Args_Type
                          (Request.Payload.Args.all).data;
                     --  Call Implementation
                     Result_Ü :=
                       test_all_optims.Impl.echoSixteenKb21
                          (test_all_optims.Impl.Object'Class
                             (Self.all)'Access,
                           data);
                     --  Setting the result
                     test_all_optims.CDR.echoSixteenKb21_Args_Type
                       (Request.Payload.Args.all).Returns :=
                       Result_Ü;
                  end;
               when 22 =>
                  declare
                     data : test_all_optims.sixteenKb;
                     Error : PolyORB.Errors.Error_Container;
                     Result_Ü : test_all_optims.sixteenKb;
                  begin
                     --  Setting the request Payload
                     test_all_optims.CDR.echoSixteenKb22_Set_Args
                       (Request,
                        new test_all_optims.CDR.echoSixteenKb22_Args_Type);
                     --  Processing request
                     PolyORB.Requests.Arguments
                       (PolyORB.Requests.Request_Access
                          (Request),
                        Request.Payload,
                        Error);
                     if PolyORB.Errors.Found
                       (Error)
                     then
                        PolyORB.CORBA_P.Exceptions.Raise_From_Error
                          (Error);
                     end if;
                     --  Setting Parameters
                     data :=
                       test_all_optims.CDR.echoSixteenKb22_Args_Type
                          (Request.Payload.Args.all).data;
                     --  Call Implementation
                     Result_Ü :=
                       test_all_optims.Impl.echoSixteenKb22
                          (test_all_optims.Impl.Object'Class
                             (Self.all)'Access,
                           data);
                     --  Setting the result
                     test_all_optims.CDR.echoSixteenKb22_Args_Type
                       (Request.Payload.Args.all).Returns :=
                       Result_Ü;
                  end;
               when 23 =>
                  declare
                     data : test_all_optims.sixteenKb;
                     Error : PolyORB.Errors.Error_Container;
                     Result_Ü : test_all_optims.sixteenKb;
                  begin
                     --  Setting the request Payload
                     test_all_optims.CDR.echoSixteenKb23_Set_Args
                       (Request,
                        new test_all_optims.CDR.echoSixteenKb23_Args_Type);
                     --  Processing request
                     PolyORB.Requests.Arguments
                       (PolyORB.Requests.Request_Access
                          (Request),
                        Request.Payload,
                        Error);
                     if PolyORB.Errors.Found
                       (Error)
                     then
                        PolyORB.CORBA_P.Exceptions.Raise_From_Error
                          (Error);
                     end if;
                     --  Setting Parameters
                     data :=
                       test_all_optims.CDR.echoSixteenKb23_Args_Type
                          (Request.Payload.Args.all).data;
                     --  Call Implementation
                     Result_Ü :=
                       test_all_optims.Impl.echoSixteenKb23
                          (test_all_optims.Impl.Object'Class
                             (Self.all)'Access,
                           data);
                     --  Setting the result
                     test_all_optims.CDR.echoSixteenKb23_Args_Type
                       (Request.Payload.Args.all).Returns :=
                       Result_Ü;
                  end;
               when 24 =>
                  declare
                     data : test_all_optims.sixteenKb;
                     Error : PolyORB.Errors.Error_Container;
                     Result_Ü : test_all_optims.sixteenKb;
                  begin
                     --  Setting the request Payload
                     test_all_optims.CDR.echoSixteenKb24_Set_Args
                       (Request,
                        new test_all_optims.CDR.echoSixteenKb24_Args_Type);
                     --  Processing request
                     PolyORB.Requests.Arguments
                       (PolyORB.Requests.Request_Access
                          (Request),
                        Request.Payload,
                        Error);
                     if PolyORB.Errors.Found
                       (Error)
                     then
                        PolyORB.CORBA_P.Exceptions.Raise_From_Error
                          (Error);
                     end if;
                     --  Setting Parameters
                     data :=
                       test_all_optims.CDR.echoSixteenKb24_Args_Type
                          (Request.Payload.Args.all).data;
                     --  Call Implementation
                     Result_Ü :=
                       test_all_optims.Impl.echoSixteenKb24
                          (test_all_optims.Impl.Object'Class
                             (Self.all)'Access,
                           data);
                     --  Setting the result
                     test_all_optims.CDR.echoSixteenKb24_Args_Type
                       (Request.Payload.Args.all).Returns :=
                       Result_Ü;
                  end;
               when 25 =>
                  declare
                     data : test_all_optims.sixteenKb;
                     Error : PolyORB.Errors.Error_Container;
                     Result_Ü : test_all_optims.sixteenKb;
                  begin
                     --  Setting the request Payload
                     test_all_optims.CDR.echoSixteenKb25_Set_Args
                       (Request,
                        new test_all_optims.CDR.echoSixteenKb25_Args_Type);
                     --  Processing request
                     PolyORB.Requests.Arguments
                       (PolyORB.Requests.Request_Access
                          (Request),
                        Request.Payload,
                        Error);
                     if PolyORB.Errors.Found
                       (Error)
                     then
                        PolyORB.CORBA_P.Exceptions.Raise_From_Error
                          (Error);
                     end if;
                     --  Setting Parameters
                     data :=
                       test_all_optims.CDR.echoSixteenKb25_Args_Type
                          (Request.Payload.Args.all).data;
                     --  Call Implementation
                     Result_Ü :=
                       test_all_optims.Impl.echoSixteenKb25
                          (test_all_optims.Impl.Object'Class
                             (Self.all)'Access,
                           data);
                     --  Setting the result
                     test_all_optims.CDR.echoSixteenKb25_Args_Type
                       (Request.Payload.Args.all).Returns :=
                       Result_Ü;
                  end;
               when 26 =>
                  declare
                     data : test_all_optims.sixteenKb;
                     Error : PolyORB.Errors.Error_Container;
                     Result_Ü : test_all_optims.sixteenKb;
                  begin
                     --  Setting the request Payload
                     test_all_optims.CDR.echoSixteenKb26_Set_Args
                       (Request,
                        new test_all_optims.CDR.echoSixteenKb26_Args_Type);
                     --  Processing request
                     PolyORB.Requests.Arguments
                       (PolyORB.Requests.Request_Access
                          (Request),
                        Request.Payload,
                        Error);
                     if PolyORB.Errors.Found
                       (Error)
                     then
                        PolyORB.CORBA_P.Exceptions.Raise_From_Error
                          (Error);
                     end if;
                     --  Setting Parameters
                     data :=
                       test_all_optims.CDR.echoSixteenKb26_Args_Type
                          (Request.Payload.Args.all).data;
                     --  Call Implementation
                     Result_Ü :=
                       test_all_optims.Impl.echoSixteenKb26
                          (test_all_optims.Impl.Object'Class
                             (Self.all)'Access,
                           data);
                     --  Setting the result
                     test_all_optims.CDR.echoSixteenKb26_Args_Type
                       (Request.Payload.Args.all).Returns :=
                       Result_Ü;
                  end;
               when 27 =>
                  declare
                     data : test_all_optims.sixteenKb;
                     Error : PolyORB.Errors.Error_Container;
                     Result_Ü : test_all_optims.sixteenKb;
                  begin
                     --  Setting the request Payload
                     test_all_optims.CDR.echoSixteenKb27_Set_Args
                       (Request,
                        new test_all_optims.CDR.echoSixteenKb27_Args_Type);
                     --  Processing request
                     PolyORB.Requests.Arguments
                       (PolyORB.Requests.Request_Access
                          (Request),
                        Request.Payload,
                        Error);
                     if PolyORB.Errors.Found
                       (Error)
                     then
                        PolyORB.CORBA_P.Exceptions.Raise_From_Error
                          (Error);
                     end if;
                     --  Setting Parameters
                     data :=
                       test_all_optims.CDR.echoSixteenKb27_Args_Type
                          (Request.Payload.Args.all).data;
                     --  Call Implementation
                     Result_Ü :=
                       test_all_optims.Impl.echoSixteenKb27
                          (test_all_optims.Impl.Object'Class
                             (Self.all)'Access,
                           data);
                     --  Setting the result
                     test_all_optims.CDR.echoSixteenKb27_Args_Type
                       (Request.Payload.Args.all).Returns :=
                       Result_Ü;
                  end;
               when 28 =>
                  declare
                     data : test_all_optims.sixteenKb;
                     Error : PolyORB.Errors.Error_Container;
                     Result_Ü : test_all_optims.sixteenKb;
                  begin
                     --  Setting the request Payload
                     test_all_optims.CDR.echoSixteenKb28_Set_Args
                       (Request,
                        new test_all_optims.CDR.echoSixteenKb28_Args_Type);
                     --  Processing request
                     PolyORB.Requests.Arguments
                       (PolyORB.Requests.Request_Access
                          (Request),
                        Request.Payload,
                        Error);
                     if PolyORB.Errors.Found
                       (Error)
                     then
                        PolyORB.CORBA_P.Exceptions.Raise_From_Error
                          (Error);
                     end if;
                     --  Setting Parameters
                     data :=
                       test_all_optims.CDR.echoSixteenKb28_Args_Type
                          (Request.Payload.Args.all).data;
                     --  Call Implementation
                     Result_Ü :=
                       test_all_optims.Impl.echoSixteenKb28
                          (test_all_optims.Impl.Object'Class
                             (Self.all)'Access,
                           data);
                     --  Setting the result
                     test_all_optims.CDR.echoSixteenKb28_Args_Type
                       (Request.Payload.Args.all).Returns :=
                       Result_Ü;
                  end;
               when 29 =>
                  declare
                     data : test_all_optims.sixteenKb;
                     Error : PolyORB.Errors.Error_Container;
                     Result_Ü : test_all_optims.sixteenKb;
                  begin
                     --  Setting the request Payload
                     test_all_optims.CDR.echoSixteenKb29_Set_Args
                       (Request,
                        new test_all_optims.CDR.echoSixteenKb29_Args_Type);
                     --  Processing request
                     PolyORB.Requests.Arguments
                       (PolyORB.Requests.Request_Access
                          (Request),
                        Request.Payload,
                        Error);
                     if PolyORB.Errors.Found
                       (Error)
                     then
                        PolyORB.CORBA_P.Exceptions.Raise_From_Error
                          (Error);
                     end if;
                     --  Setting Parameters
                     data :=
                       test_all_optims.CDR.echoSixteenKb29_Args_Type
                          (Request.Payload.Args.all).data;
                     --  Call Implementation
                     Result_Ü :=
                       test_all_optims.Impl.echoSixteenKb29
                          (test_all_optims.Impl.Object'Class
                             (Self.all)'Access,
                           data);
                     --  Setting the result
                     test_all_optims.CDR.echoSixteenKb29_Args_Type
                       (Request.Payload.Args.all).Returns :=
                       Result_Ü;
                  end;
               when 30 =>
                  declare
                     data : test_all_optims.sixteenKb;
                     Error : PolyORB.Errors.Error_Container;
                     Result_Ü : test_all_optims.sixteenKb;
                  begin
                     --  Setting the request Payload
                     test_all_optims.CDR.echoSixteenKb30_Set_Args
                       (Request,
                        new test_all_optims.CDR.echoSixteenKb30_Args_Type);
                     --  Processing request
                     PolyORB.Requests.Arguments
                       (PolyORB.Requests.Request_Access
                          (Request),
                        Request.Payload,
                        Error);
                     if PolyORB.Errors.Found
                       (Error)
                     then
                        PolyORB.CORBA_P.Exceptions.Raise_From_Error
                          (Error);
                     end if;
                     --  Setting Parameters
                     data :=
                       test_all_optims.CDR.echoSixteenKb30_Args_Type
                          (Request.Payload.Args.all).data;
                     --  Call Implementation
                     Result_Ü :=
                       test_all_optims.Impl.echoSixteenKb30
                          (test_all_optims.Impl.Object'Class
                             (Self.all)'Access,
                           data);
                     --  Setting the result
                     test_all_optims.CDR.echoSixteenKb30_Args_Type
                       (Request.Payload.Args.all).Returns :=
                       Result_Ü;
                  end;
               when 31 =>
                  declare
                     data : test_all_optims.sixteenKb;
                     Error : PolyORB.Errors.Error_Container;
                     Result_Ü : test_all_optims.sixteenKb;
                  begin
                     --  Setting the request Payload
                     test_all_optims.CDR.echoSixteenKb31_Set_Args
                       (Request,
                        new test_all_optims.CDR.echoSixteenKb31_Args_Type);
                     --  Processing request
                     PolyORB.Requests.Arguments
                       (PolyORB.Requests.Request_Access
                          (Request),
                        Request.Payload,
                        Error);
                     if PolyORB.Errors.Found
                       (Error)
                     then
                        PolyORB.CORBA_P.Exceptions.Raise_From_Error
                          (Error);
                     end if;
                     --  Setting Parameters
                     data :=
                       test_all_optims.CDR.echoSixteenKb31_Args_Type
                          (Request.Payload.Args.all).data;
                     --  Call Implementation
                     Result_Ü :=
                       test_all_optims.Impl.echoSixteenKb31
                          (test_all_optims.Impl.Object'Class
                             (Self.all)'Access,
                           data);
                     --  Setting the result
                     test_all_optims.CDR.echoSixteenKb31_Args_Type
                       (Request.Payload.Args.all).Returns :=
                       Result_Ü;
                  end;
               when 32 =>
                  declare
                     data : test_all_optims.sixteenKb;
                     Error : PolyORB.Errors.Error_Container;
                     Result_Ü : test_all_optims.sixteenKb;
                  begin
                     --  Setting the request Payload
                     test_all_optims.CDR.echoSixteenKb32_Set_Args
                       (Request,
                        new test_all_optims.CDR.echoSixteenKb32_Args_Type);
                     --  Processing request
                     PolyORB.Requests.Arguments
                       (PolyORB.Requests.Request_Access
                          (Request),
                        Request.Payload,
                        Error);
                     if PolyORB.Errors.Found
                       (Error)
                     then
                        PolyORB.CORBA_P.Exceptions.Raise_From_Error
                          (Error);
                     end if;
                     --  Setting Parameters
                     data :=
                       test_all_optims.CDR.echoSixteenKb32_Args_Type
                          (Request.Payload.Args.all).data;
                     --  Call Implementation
                     Result_Ü :=
                       test_all_optims.Impl.echoSixteenKb32
                          (test_all_optims.Impl.Object'Class
                             (Self.all)'Access,
                           data);
                     --  Setting the result
                     test_all_optims.CDR.echoSixteenKb32_Args_Type
                       (Request.Payload.Args.all).Returns :=
                       Result_Ü;
                  end;
               when 33 =>
                  declare
                     data : test_all_optims.sixteenKb;
                     Error : PolyORB.Errors.Error_Container;
                     Result_Ü : test_all_optims.sixteenKb;
                  begin
                     --  Setting the request Payload
                     test_all_optims.CDR.echoSixteenKb33_Set_Args
                       (Request,
                        new test_all_optims.CDR.echoSixteenKb33_Args_Type);
                     --  Processing request
                     PolyORB.Requests.Arguments
                       (PolyORB.Requests.Request_Access
                          (Request),
                        Request.Payload,
                        Error);
                     if PolyORB.Errors.Found
                       (Error)
                     then
                        PolyORB.CORBA_P.Exceptions.Raise_From_Error
                          (Error);
                     end if;
                     --  Setting Parameters
                     data :=
                       test_all_optims.CDR.echoSixteenKb33_Args_Type
                          (Request.Payload.Args.all).data;
                     --  Call Implementation
                     Result_Ü :=
                       test_all_optims.Impl.echoSixteenKb33
                          (test_all_optims.Impl.Object'Class
                             (Self.all)'Access,
                           data);
                     --  Setting the result
                     test_all_optims.CDR.echoSixteenKb33_Args_Type
                       (Request.Payload.Args.all).Returns :=
                       Result_Ü;
                  end;
               when 34 =>
                  declare
                     data : test_all_optims.sixteenKb;
                     Error : PolyORB.Errors.Error_Container;
                     Result_Ü : test_all_optims.sixteenKb;
                  begin
                     --  Setting the request Payload
                     test_all_optims.CDR.echoSixteenKb34_Set_Args
                       (Request,
                        new test_all_optims.CDR.echoSixteenKb34_Args_Type);
                     --  Processing request
                     PolyORB.Requests.Arguments
                       (PolyORB.Requests.Request_Access
                          (Request),
                        Request.Payload,
                        Error);
                     if PolyORB.Errors.Found
                       (Error)
                     then
                        PolyORB.CORBA_P.Exceptions.Raise_From_Error
                          (Error);
                     end if;
                     --  Setting Parameters
                     data :=
                       test_all_optims.CDR.echoSixteenKb34_Args_Type
                          (Request.Payload.Args.all).data;
                     --  Call Implementation
                     Result_Ü :=
                       test_all_optims.Impl.echoSixteenKb34
                          (test_all_optims.Impl.Object'Class
                             (Self.all)'Access,
                           data);
                     --  Setting the result
                     test_all_optims.CDR.echoSixteenKb34_Args_Type
                       (Request.Payload.Args.all).Returns :=
                       Result_Ü;
                  end;
               when 35 =>
                  declare
                     data : test_all_optims.sixteenKb;
                     Error : PolyORB.Errors.Error_Container;
                     Result_Ü : test_all_optims.sixteenKb;
                  begin
                     --  Setting the request Payload
                     test_all_optims.CDR.echoSixteenKb35_Set_Args
                       (Request,
                        new test_all_optims.CDR.echoSixteenKb35_Args_Type);
                     --  Processing request
                     PolyORB.Requests.Arguments
                       (PolyORB.Requests.Request_Access
                          (Request),
                        Request.Payload,
                        Error);
                     if PolyORB.Errors.Found
                       (Error)
                     then
                        PolyORB.CORBA_P.Exceptions.Raise_From_Error
                          (Error);
                     end if;
                     --  Setting Parameters
                     data :=
                       test_all_optims.CDR.echoSixteenKb35_Args_Type
                          (Request.Payload.Args.all).data;
                     --  Call Implementation
                     Result_Ü :=
                       test_all_optims.Impl.echoSixteenKb35
                          (test_all_optims.Impl.Object'Class
                             (Self.all)'Access,
                           data);
                     --  Setting the result
                     test_all_optims.CDR.echoSixteenKb35_Args_Type
                       (Request.Payload.Args.all).Returns :=
                       Result_Ü;
                  end;
               when 36 =>
                  declare
                     data : test_all_optims.sixteenKb;
                     Error : PolyORB.Errors.Error_Container;
                     Result_Ü : test_all_optims.sixteenKb;
                  begin
                     --  Setting the request Payload
                     test_all_optims.CDR.echoSixteenKb36_Set_Args
                       (Request,
                        new test_all_optims.CDR.echoSixteenKb36_Args_Type);
                     --  Processing request
                     PolyORB.Requests.Arguments
                       (PolyORB.Requests.Request_Access
                          (Request),
                        Request.Payload,
                        Error);
                     if PolyORB.Errors.Found
                       (Error)
                     then
                        PolyORB.CORBA_P.Exceptions.Raise_From_Error
                          (Error);
                     end if;
                     --  Setting Parameters
                     data :=
                       test_all_optims.CDR.echoSixteenKb36_Args_Type
                          (Request.Payload.Args.all).data;
                     --  Call Implementation
                     Result_Ü :=
                       test_all_optims.Impl.echoSixteenKb36
                          (test_all_optims.Impl.Object'Class
                             (Self.all)'Access,
                           data);
                     --  Setting the result
                     test_all_optims.CDR.echoSixteenKb36_Args_Type
                       (Request.Payload.Args.all).Returns :=
                       Result_Ü;
                  end;
               when 37 =>
                  declare
                     data : test_all_optims.sixteenKb;
                     Error : PolyORB.Errors.Error_Container;
                     Result_Ü : test_all_optims.sixteenKb;
                  begin
                     --  Setting the request Payload
                     test_all_optims.CDR.echoSixteenKb37_Set_Args
                       (Request,
                        new test_all_optims.CDR.echoSixteenKb37_Args_Type);
                     --  Processing request
                     PolyORB.Requests.Arguments
                       (PolyORB.Requests.Request_Access
                          (Request),
                        Request.Payload,
                        Error);
                     if PolyORB.Errors.Found
                       (Error)
                     then
                        PolyORB.CORBA_P.Exceptions.Raise_From_Error
                          (Error);
                     end if;
                     --  Setting Parameters
                     data :=
                       test_all_optims.CDR.echoSixteenKb37_Args_Type
                          (Request.Payload.Args.all).data;
                     --  Call Implementation
                     Result_Ü :=
                       test_all_optims.Impl.echoSixteenKb37
                          (test_all_optims.Impl.Object'Class
                             (Self.all)'Access,
                           data);
                     --  Setting the result
                     test_all_optims.CDR.echoSixteenKb37_Args_Type
                       (Request.Payload.Args.all).Returns :=
                       Result_Ü;
                  end;
               when 38 =>
                  declare
                     data : test_all_optims.sixteenKb;
                     Error : PolyORB.Errors.Error_Container;
                     Result_Ü : test_all_optims.sixteenKb;
                  begin
                     --  Setting the request Payload
                     test_all_optims.CDR.echoSixteenKb38_Set_Args
                       (Request,
                        new test_all_optims.CDR.echoSixteenKb38_Args_Type);
                     --  Processing request
                     PolyORB.Requests.Arguments
                       (PolyORB.Requests.Request_Access
                          (Request),
                        Request.Payload,
                        Error);
                     if PolyORB.Errors.Found
                       (Error)
                     then
                        PolyORB.CORBA_P.Exceptions.Raise_From_Error
                          (Error);
                     end if;
                     --  Setting Parameters
                     data :=
                       test_all_optims.CDR.echoSixteenKb38_Args_Type
                          (Request.Payload.Args.all).data;
                     --  Call Implementation
                     Result_Ü :=
                       test_all_optims.Impl.echoSixteenKb38
                          (test_all_optims.Impl.Object'Class
                             (Self.all)'Access,
                           data);
                     --  Setting the result
                     test_all_optims.CDR.echoSixteenKb38_Args_Type
                       (Request.Payload.Args.all).Returns :=
                       Result_Ü;
                  end;
               when 39 =>
                  declare
                     data : test_all_optims.sixteenKb;
                     Error : PolyORB.Errors.Error_Container;
                     Result_Ü : test_all_optims.sixteenKb;
                  begin
                     --  Setting the request Payload
                     test_all_optims.CDR.echoSixteenKb39_Set_Args
                       (Request,
                        new test_all_optims.CDR.echoSixteenKb39_Args_Type);
                     --  Processing request
                     PolyORB.Requests.Arguments
                       (PolyORB.Requests.Request_Access
                          (Request),
                        Request.Payload,
                        Error);
                     if PolyORB.Errors.Found
                       (Error)
                     then
                        PolyORB.CORBA_P.Exceptions.Raise_From_Error
                          (Error);
                     end if;
                     --  Setting Parameters
                     data :=
                       test_all_optims.CDR.echoSixteenKb39_Args_Type
                          (Request.Payload.Args.all).data;
                     --  Call Implementation
                     Result_Ü :=
                       test_all_optims.Impl.echoSixteenKb39
                          (test_all_optims.Impl.Object'Class
                             (Self.all)'Access,
                           data);
                     --  Setting the result
                     test_all_optims.CDR.echoSixteenKb39_Args_Type
                       (Request.Payload.Args.all).Returns :=
                       Result_Ü;
                  end;
               when 40 =>
                  declare
                     data : test_all_optims.sixteenKb;
                     Error : PolyORB.Errors.Error_Container;
                     Result_Ü : test_all_optims.sixteenKb;
                  begin
                     --  Setting the request Payload
                     test_all_optims.CDR.echoSixteenKb40_Set_Args
                       (Request,
                        new test_all_optims.CDR.echoSixteenKb40_Args_Type);
                     --  Processing request
                     PolyORB.Requests.Arguments
                       (PolyORB.Requests.Request_Access
                          (Request),
                        Request.Payload,
                        Error);
                     if PolyORB.Errors.Found
                       (Error)
                     then
                        PolyORB.CORBA_P.Exceptions.Raise_From_Error
                          (Error);
                     end if;
                     --  Setting Parameters
                     data :=
                       test_all_optims.CDR.echoSixteenKb40_Args_Type
                          (Request.Payload.Args.all).data;
                     --  Call Implementation
                     Result_Ü :=
                       test_all_optims.Impl.echoSixteenKb40
                          (test_all_optims.Impl.Object'Class
                             (Self.all)'Access,
                           data);
                     --  Setting the result
                     test_all_optims.CDR.echoSixteenKb40_Args_Type
                       (Request.Payload.Args.all).Returns :=
                       Result_Ü;
                  end;
               when 41 =>
                  declare
                     data : test_all_optims.sixteenKb;
                     Error : PolyORB.Errors.Error_Container;
                     Result_Ü : test_all_optims.sixteenKb;
                  begin
                     --  Setting the request Payload
                     test_all_optims.CDR.echoSixteenKb41_Set_Args
                       (Request,
                        new test_all_optims.CDR.echoSixteenKb41_Args_Type);
                     --  Processing request
                     PolyORB.Requests.Arguments
                       (PolyORB.Requests.Request_Access
                          (Request),
                        Request.Payload,
                        Error);
                     if PolyORB.Errors.Found
                       (Error)
                     then
                        PolyORB.CORBA_P.Exceptions.Raise_From_Error
                          (Error);
                     end if;
                     --  Setting Parameters
                     data :=
                       test_all_optims.CDR.echoSixteenKb41_Args_Type
                          (Request.Payload.Args.all).data;
                     --  Call Implementation
                     Result_Ü :=
                       test_all_optims.Impl.echoSixteenKb41
                          (test_all_optims.Impl.Object'Class
                             (Self.all)'Access,
                           data);
                     --  Setting the result
                     test_all_optims.CDR.echoSixteenKb41_Args_Type
                       (Request.Payload.Args.all).Returns :=
                       Result_Ü;
                  end;
               when 42 =>
                  declare
                     data : test_all_optims.sixteenKb;
                     Error : PolyORB.Errors.Error_Container;
                     Result_Ü : test_all_optims.sixteenKb;
                  begin
                     --  Setting the request Payload
                     test_all_optims.CDR.echoSixteenKb42_Set_Args
                       (Request,
                        new test_all_optims.CDR.echoSixteenKb42_Args_Type);
                     --  Processing request
                     PolyORB.Requests.Arguments
                       (PolyORB.Requests.Request_Access
                          (Request),
                        Request.Payload,
                        Error);
                     if PolyORB.Errors.Found
                       (Error)
                     then
                        PolyORB.CORBA_P.Exceptions.Raise_From_Error
                          (Error);
                     end if;
                     --  Setting Parameters
                     data :=
                       test_all_optims.CDR.echoSixteenKb42_Args_Type
                          (Request.Payload.Args.all).data;
                     --  Call Implementation
                     Result_Ü :=
                       test_all_optims.Impl.echoSixteenKb42
                          (test_all_optims.Impl.Object'Class
                             (Self.all)'Access,
                           data);
                     --  Setting the result
                     test_all_optims.CDR.echoSixteenKb42_Args_Type
                       (Request.Payload.Args.all).Returns :=
                       Result_Ü;
                  end;
               when 43 =>
                  declare
                     data : test_all_optims.sixteenKb;
                     Error : PolyORB.Errors.Error_Container;
                     Result_Ü : test_all_optims.sixteenKb;
                  begin
                     --  Setting the request Payload
                     test_all_optims.CDR.echoSixteenKb43_Set_Args
                       (Request,
                        new test_all_optims.CDR.echoSixteenKb43_Args_Type);
                     --  Processing request
                     PolyORB.Requests.Arguments
                       (PolyORB.Requests.Request_Access
                          (Request),
                        Request.Payload,
                        Error);
                     if PolyORB.Errors.Found
                       (Error)
                     then
                        PolyORB.CORBA_P.Exceptions.Raise_From_Error
                          (Error);
                     end if;
                     --  Setting Parameters
                     data :=
                       test_all_optims.CDR.echoSixteenKb43_Args_Type
                          (Request.Payload.Args.all).data;
                     --  Call Implementation
                     Result_Ü :=
                       test_all_optims.Impl.echoSixteenKb43
                          (test_all_optims.Impl.Object'Class
                             (Self.all)'Access,
                           data);
                     --  Setting the result
                     test_all_optims.CDR.echoSixteenKb43_Args_Type
                       (Request.Payload.Args.all).Returns :=
                       Result_Ü;
                  end;
               when 44 =>
                  declare
                     data : test_all_optims.sixteenKb;
                     Error : PolyORB.Errors.Error_Container;
                     Result_Ü : test_all_optims.sixteenKb;
                  begin
                     --  Setting the request Payload
                     test_all_optims.CDR.echoSixteenKb44_Set_Args
                       (Request,
                        new test_all_optims.CDR.echoSixteenKb44_Args_Type);
                     --  Processing request
                     PolyORB.Requests.Arguments
                       (PolyORB.Requests.Request_Access
                          (Request),
                        Request.Payload,
                        Error);
                     if PolyORB.Errors.Found
                       (Error)
                     then
                        PolyORB.CORBA_P.Exceptions.Raise_From_Error
                          (Error);
                     end if;
                     --  Setting Parameters
                     data :=
                       test_all_optims.CDR.echoSixteenKb44_Args_Type
                          (Request.Payload.Args.all).data;
                     --  Call Implementation
                     Result_Ü :=
                       test_all_optims.Impl.echoSixteenKb44
                          (test_all_optims.Impl.Object'Class
                             (Self.all)'Access,
                           data);
                     --  Setting the result
                     test_all_optims.CDR.echoSixteenKb44_Args_Type
                       (Request.Payload.Args.all).Returns :=
                       Result_Ü;
                  end;
               when 45 =>
                  declare
                     data : test_all_optims.sixteenKb;
                     Error : PolyORB.Errors.Error_Container;
                     Result_Ü : test_all_optims.sixteenKb;
                  begin
                     --  Setting the request Payload
                     test_all_optims.CDR.echoSixteenKb45_Set_Args
                       (Request,
                        new test_all_optims.CDR.echoSixteenKb45_Args_Type);
                     --  Processing request
                     PolyORB.Requests.Arguments
                       (PolyORB.Requests.Request_Access
                          (Request),
                        Request.Payload,
                        Error);
                     if PolyORB.Errors.Found
                       (Error)
                     then
                        PolyORB.CORBA_P.Exceptions.Raise_From_Error
                          (Error);
                     end if;
                     --  Setting Parameters
                     data :=
                       test_all_optims.CDR.echoSixteenKb45_Args_Type
                          (Request.Payload.Args.all).data;
                     --  Call Implementation
                     Result_Ü :=
                       test_all_optims.Impl.echoSixteenKb45
                          (test_all_optims.Impl.Object'Class
                             (Self.all)'Access,
                           data);
                     --  Setting the result
                     test_all_optims.CDR.echoSixteenKb45_Args_Type
                       (Request.Payload.Args.all).Returns :=
                       Result_Ü;
                  end;
               when 46 =>
                  declare
                     data : test_all_optims.sixteenKb;
                     Error : PolyORB.Errors.Error_Container;
                     Result_Ü : test_all_optims.sixteenKb;
                  begin
                     --  Setting the request Payload
                     test_all_optims.CDR.echoSixteenKb46_Set_Args
                       (Request,
                        new test_all_optims.CDR.echoSixteenKb46_Args_Type);
                     --  Processing request
                     PolyORB.Requests.Arguments
                       (PolyORB.Requests.Request_Access
                          (Request),
                        Request.Payload,
                        Error);
                     if PolyORB.Errors.Found
                       (Error)
                     then
                        PolyORB.CORBA_P.Exceptions.Raise_From_Error
                          (Error);
                     end if;
                     --  Setting Parameters
                     data :=
                       test_all_optims.CDR.echoSixteenKb46_Args_Type
                          (Request.Payload.Args.all).data;
                     --  Call Implementation
                     Result_Ü :=
                       test_all_optims.Impl.echoSixteenKb46
                          (test_all_optims.Impl.Object'Class
                             (Self.all)'Access,
                           data);
                     --  Setting the result
                     test_all_optims.CDR.echoSixteenKb46_Args_Type
                       (Request.Payload.Args.all).Returns :=
                       Result_Ü;
                  end;
               when 47 =>
                  declare
                     data : test_all_optims.sixteenKb;
                     Error : PolyORB.Errors.Error_Container;
                     Result_Ü : test_all_optims.sixteenKb;
                  begin
                     --  Setting the request Payload
                     test_all_optims.CDR.echoSixteenKb47_Set_Args
                       (Request,
                        new test_all_optims.CDR.echoSixteenKb47_Args_Type);
                     --  Processing request
                     PolyORB.Requests.Arguments
                       (PolyORB.Requests.Request_Access
                          (Request),
                        Request.Payload,
                        Error);
                     if PolyORB.Errors.Found
                       (Error)
                     then
                        PolyORB.CORBA_P.Exceptions.Raise_From_Error
                          (Error);
                     end if;
                     --  Setting Parameters
                     data :=
                       test_all_optims.CDR.echoSixteenKb47_Args_Type
                          (Request.Payload.Args.all).data;
                     --  Call Implementation
                     Result_Ü :=
                       test_all_optims.Impl.echoSixteenKb47
                          (test_all_optims.Impl.Object'Class
                             (Self.all)'Access,
                           data);
                     --  Setting the result
                     test_all_optims.CDR.echoSixteenKb47_Args_Type
                       (Request.Payload.Args.all).Returns :=
                       Result_Ü;
                  end;
               when 48 =>
                  declare
                     data : test_all_optims.sixteenKb;
                     Error : PolyORB.Errors.Error_Container;
                     Result_Ü : test_all_optims.sixteenKb;
                  begin
                     --  Setting the request Payload
                     test_all_optims.CDR.echoSixteenKb48_Set_Args
                       (Request,
                        new test_all_optims.CDR.echoSixteenKb48_Args_Type);
                     --  Processing request
                     PolyORB.Requests.Arguments
                       (PolyORB.Requests.Request_Access
                          (Request),
                        Request.Payload,
                        Error);
                     if PolyORB.Errors.Found
                       (Error)
                     then
                        PolyORB.CORBA_P.Exceptions.Raise_From_Error
                          (Error);
                     end if;
                     --  Setting Parameters
                     data :=
                       test_all_optims.CDR.echoSixteenKb48_Args_Type
                          (Request.Payload.Args.all).data;
                     --  Call Implementation
                     Result_Ü :=
                       test_all_optims.Impl.echoSixteenKb48
                          (test_all_optims.Impl.Object'Class
                             (Self.all)'Access,
                           data);
                     --  Setting the result
                     test_all_optims.CDR.echoSixteenKb48_Args_Type
                       (Request.Payload.Args.all).Returns :=
                       Result_Ü;
                  end;
               when 49 =>
                  declare
                     data : test_all_optims.sixteenKb;
                     Error : PolyORB.Errors.Error_Container;
                     Result_Ü : test_all_optims.sixteenKb;
                  begin
                     --  Setting the request Payload
                     test_all_optims.CDR.echoSixteenKb49_Set_Args
                       (Request,
                        new test_all_optims.CDR.echoSixteenKb49_Args_Type);
                     --  Processing request
                     PolyORB.Requests.Arguments
                       (PolyORB.Requests.Request_Access
                          (Request),
                        Request.Payload,
                        Error);
                     if PolyORB.Errors.Found
                       (Error)
                     then
                        PolyORB.CORBA_P.Exceptions.Raise_From_Error
                          (Error);
                     end if;
                     --  Setting Parameters
                     data :=
                       test_all_optims.CDR.echoSixteenKb49_Args_Type
                          (Request.Payload.Args.all).data;
                     --  Call Implementation
                     Result_Ü :=
                       test_all_optims.Impl.echoSixteenKb49
                          (test_all_optims.Impl.Object'Class
                             (Self.all)'Access,
                           data);
                     --  Setting the result
                     test_all_optims.CDR.echoSixteenKb49_Args_Type
                       (Request.Payload.Args.all).Returns :=
                       Result_Ü;
                  end;
               when 50 =>
                  declare
                     data : test_all_optims.sixteenKb;
                     Error : PolyORB.Errors.Error_Container;
                     Result_Ü : test_all_optims.sixteenKb;
                  begin
                     --  Setting the request Payload
                     test_all_optims.CDR.echoSixteenKb50_Set_Args
                       (Request,
                        new test_all_optims.CDR.echoSixteenKb50_Args_Type);
                     --  Processing request
                     PolyORB.Requests.Arguments
                       (PolyORB.Requests.Request_Access
                          (Request),
                        Request.Payload,
                        Error);
                     if PolyORB.Errors.Found
                       (Error)
                     then
                        PolyORB.CORBA_P.Exceptions.Raise_From_Error
                          (Error);
                     end if;
                     --  Setting Parameters
                     data :=
                       test_all_optims.CDR.echoSixteenKb50_Args_Type
                          (Request.Payload.Args.all).data;
                     --  Call Implementation
                     Result_Ü :=
                       test_all_optims.Impl.echoSixteenKb50
                          (test_all_optims.Impl.Object'Class
                             (Self.all)'Access,
                           data);
                     --  Setting the result
                     test_all_optims.CDR.echoSixteenKb50_Args_Type
                       (Request.Payload.Args.all).Returns :=
                       Result_Ü;
                  end;
               when 51 =>
                  declare
                     data : test_all_optims.sixteenKb;
                     Error : PolyORB.Errors.Error_Container;
                     Result_Ü : test_all_optims.sixteenKb;
                  begin
                     --  Setting the request Payload
                     test_all_optims.CDR.echoSixteenKb51_Set_Args
                       (Request,
                        new test_all_optims.CDR.echoSixteenKb51_Args_Type);
                     --  Processing request
                     PolyORB.Requests.Arguments
                       (PolyORB.Requests.Request_Access
                          (Request),
                        Request.Payload,
                        Error);
                     if PolyORB.Errors.Found
                       (Error)
                     then
                        PolyORB.CORBA_P.Exceptions.Raise_From_Error
                          (Error);
                     end if;
                     --  Setting Parameters
                     data :=
                       test_all_optims.CDR.echoSixteenKb51_Args_Type
                          (Request.Payload.Args.all).data;
                     --  Call Implementation
                     Result_Ü :=
                       test_all_optims.Impl.echoSixteenKb51
                          (test_all_optims.Impl.Object'Class
                             (Self.all)'Access,
                           data);
                     --  Setting the result
                     test_all_optims.CDR.echoSixteenKb51_Args_Type
                       (Request.Payload.Args.all).Returns :=
                       Result_Ü;
                  end;
               when 52 =>
                  declare
                     data : test_all_optims.sixteenKb;
                     Error : PolyORB.Errors.Error_Container;
                     Result_Ü : test_all_optims.sixteenKb;
                  begin
                     --  Setting the request Payload
                     test_all_optims.CDR.echoSixteenKb52_Set_Args
                       (Request,
                        new test_all_optims.CDR.echoSixteenKb52_Args_Type);
                     --  Processing request
                     PolyORB.Requests.Arguments
                       (PolyORB.Requests.Request_Access
                          (Request),
                        Request.Payload,
                        Error);
                     if PolyORB.Errors.Found
                       (Error)
                     then
                        PolyORB.CORBA_P.Exceptions.Raise_From_Error
                          (Error);
                     end if;
                     --  Setting Parameters
                     data :=
                       test_all_optims.CDR.echoSixteenKb52_Args_Type
                          (Request.Payload.Args.all).data;
                     --  Call Implementation
                     Result_Ü :=
                       test_all_optims.Impl.echoSixteenKb52
                          (test_all_optims.Impl.Object'Class
                             (Self.all)'Access,
                           data);
                     --  Setting the result
                     test_all_optims.CDR.echoSixteenKb52_Args_Type
                       (Request.Payload.Args.all).Returns :=
                       Result_Ü;
                  end;
               when 53 =>
                  declare
                     data : test_all_optims.sixteenKb;
                     Error : PolyORB.Errors.Error_Container;
                     Result_Ü : test_all_optims.sixteenKb;
                  begin
                     --  Setting the request Payload
                     test_all_optims.CDR.echoSixteenKb53_Set_Args
                       (Request,
                        new test_all_optims.CDR.echoSixteenKb53_Args_Type);
                     --  Processing request
                     PolyORB.Requests.Arguments
                       (PolyORB.Requests.Request_Access
                          (Request),
                        Request.Payload,
                        Error);
                     if PolyORB.Errors.Found
                       (Error)
                     then
                        PolyORB.CORBA_P.Exceptions.Raise_From_Error
                          (Error);
                     end if;
                     --  Setting Parameters
                     data :=
                       test_all_optims.CDR.echoSixteenKb53_Args_Type
                          (Request.Payload.Args.all).data;
                     --  Call Implementation
                     Result_Ü :=
                       test_all_optims.Impl.echoSixteenKb53
                          (test_all_optims.Impl.Object'Class
                             (Self.all)'Access,
                           data);
                     --  Setting the result
                     test_all_optims.CDR.echoSixteenKb53_Args_Type
                       (Request.Payload.Args.all).Returns :=
                       Result_Ü;
                  end;
               when 54 =>
                  declare
                     data : test_all_optims.sixteenKb;
                     Error : PolyORB.Errors.Error_Container;
                     Result_Ü : test_all_optims.sixteenKb;
                  begin
                     --  Setting the request Payload
                     test_all_optims.CDR.echoSixteenKb54_Set_Args
                       (Request,
                        new test_all_optims.CDR.echoSixteenKb54_Args_Type);
                     --  Processing request
                     PolyORB.Requests.Arguments
                       (PolyORB.Requests.Request_Access
                          (Request),
                        Request.Payload,
                        Error);
                     if PolyORB.Errors.Found
                       (Error)
                     then
                        PolyORB.CORBA_P.Exceptions.Raise_From_Error
                          (Error);
                     end if;
                     --  Setting Parameters
                     data :=
                       test_all_optims.CDR.echoSixteenKb54_Args_Type
                          (Request.Payload.Args.all).data;
                     --  Call Implementation
                     Result_Ü :=
                       test_all_optims.Impl.echoSixteenKb54
                          (test_all_optims.Impl.Object'Class
                             (Self.all)'Access,
                           data);
                     --  Setting the result
                     test_all_optims.CDR.echoSixteenKb54_Args_Type
                       (Request.Payload.Args.all).Returns :=
                       Result_Ü;
                  end;
               when 55 =>
                  declare
                     data : test_all_optims.sixteenKb;
                     Error : PolyORB.Errors.Error_Container;
                     Result_Ü : test_all_optims.sixteenKb;
                  begin
                     --  Setting the request Payload
                     test_all_optims.CDR.echoSixteenKb55_Set_Args
                       (Request,
                        new test_all_optims.CDR.echoSixteenKb55_Args_Type);
                     --  Processing request
                     PolyORB.Requests.Arguments
                       (PolyORB.Requests.Request_Access
                          (Request),
                        Request.Payload,
                        Error);
                     if PolyORB.Errors.Found
                       (Error)
                     then
                        PolyORB.CORBA_P.Exceptions.Raise_From_Error
                          (Error);
                     end if;
                     --  Setting Parameters
                     data :=
                       test_all_optims.CDR.echoSixteenKb55_Args_Type
                          (Request.Payload.Args.all).data;
                     --  Call Implementation
                     Result_Ü :=
                       test_all_optims.Impl.echoSixteenKb55
                          (test_all_optims.Impl.Object'Class
                             (Self.all)'Access,
                           data);
                     --  Setting the result
                     test_all_optims.CDR.echoSixteenKb55_Args_Type
                       (Request.Payload.Args.all).Returns :=
                       Result_Ü;
                  end;
               when 56 =>
                  declare
                     data : test_all_optims.sixteenKb;
                     Error : PolyORB.Errors.Error_Container;
                     Result_Ü : test_all_optims.sixteenKb;
                  begin
                     --  Setting the request Payload
                     test_all_optims.CDR.echoSixteenKb56_Set_Args
                       (Request,
                        new test_all_optims.CDR.echoSixteenKb56_Args_Type);
                     --  Processing request
                     PolyORB.Requests.Arguments
                       (PolyORB.Requests.Request_Access
                          (Request),
                        Request.Payload,
                        Error);
                     if PolyORB.Errors.Found
                       (Error)
                     then
                        PolyORB.CORBA_P.Exceptions.Raise_From_Error
                          (Error);
                     end if;
                     --  Setting Parameters
                     data :=
                       test_all_optims.CDR.echoSixteenKb56_Args_Type
                          (Request.Payload.Args.all).data;
                     --  Call Implementation
                     Result_Ü :=
                       test_all_optims.Impl.echoSixteenKb56
                          (test_all_optims.Impl.Object'Class
                             (Self.all)'Access,
                           data);
                     --  Setting the result
                     test_all_optims.CDR.echoSixteenKb56_Args_Type
                       (Request.Payload.Args.all).Returns :=
                       Result_Ü;
                  end;
               when 57 =>
                  declare
                     data : test_all_optims.sixteenKb;
                     Error : PolyORB.Errors.Error_Container;
                     Result_Ü : test_all_optims.sixteenKb;
                  begin
                     --  Setting the request Payload
                     test_all_optims.CDR.echoSixteenKb57_Set_Args
                       (Request,
                        new test_all_optims.CDR.echoSixteenKb57_Args_Type);
                     --  Processing request
                     PolyORB.Requests.Arguments
                       (PolyORB.Requests.Request_Access
                          (Request),
                        Request.Payload,
                        Error);
                     if PolyORB.Errors.Found
                       (Error)
                     then
                        PolyORB.CORBA_P.Exceptions.Raise_From_Error
                          (Error);
                     end if;
                     --  Setting Parameters
                     data :=
                       test_all_optims.CDR.echoSixteenKb57_Args_Type
                          (Request.Payload.Args.all).data;
                     --  Call Implementation
                     Result_Ü :=
                       test_all_optims.Impl.echoSixteenKb57
                          (test_all_optims.Impl.Object'Class
                             (Self.all)'Access,
                           data);
                     --  Setting the result
                     test_all_optims.CDR.echoSixteenKb57_Args_Type
                       (Request.Payload.Args.all).Returns :=
                       Result_Ü;
                  end;
               when 58 =>
                  declare
                     data : test_all_optims.sixteenKb;
                     Error : PolyORB.Errors.Error_Container;
                     Result_Ü : test_all_optims.sixteenKb;
                  begin
                     --  Setting the request Payload
                     test_all_optims.CDR.echoSixteenKb58_Set_Args
                       (Request,
                        new test_all_optims.CDR.echoSixteenKb58_Args_Type);
                     --  Processing request
                     PolyORB.Requests.Arguments
                       (PolyORB.Requests.Request_Access
                          (Request),
                        Request.Payload,
                        Error);
                     if PolyORB.Errors.Found
                       (Error)
                     then
                        PolyORB.CORBA_P.Exceptions.Raise_From_Error
                          (Error);
                     end if;
                     --  Setting Parameters
                     data :=
                       test_all_optims.CDR.echoSixteenKb58_Args_Type
                          (Request.Payload.Args.all).data;
                     --  Call Implementation
                     Result_Ü :=
                       test_all_optims.Impl.echoSixteenKb58
                          (test_all_optims.Impl.Object'Class
                             (Self.all)'Access,
                           data);
                     --  Setting the result
                     test_all_optims.CDR.echoSixteenKb58_Args_Type
                       (Request.Payload.Args.all).Returns :=
                       Result_Ü;
                  end;
               when 59 =>
                  declare
                     data : test_all_optims.sixteenKb;
                     Error : PolyORB.Errors.Error_Container;
                     Result_Ü : test_all_optims.sixteenKb;
                  begin
                     --  Setting the request Payload
                     test_all_optims.CDR.echoSixteenKb59_Set_Args
                       (Request,
                        new test_all_optims.CDR.echoSixteenKb59_Args_Type);
                     --  Processing request
                     PolyORB.Requests.Arguments
                       (PolyORB.Requests.Request_Access
                          (Request),
                        Request.Payload,
                        Error);
                     if PolyORB.Errors.Found
                       (Error)
                     then
                        PolyORB.CORBA_P.Exceptions.Raise_From_Error
                          (Error);
                     end if;
                     --  Setting Parameters
                     data :=
                       test_all_optims.CDR.echoSixteenKb59_Args_Type
                          (Request.Payload.Args.all).data;
                     --  Call Implementation
                     Result_Ü :=
                       test_all_optims.Impl.echoSixteenKb59
                          (test_all_optims.Impl.Object'Class
                             (Self.all)'Access,
                           data);
                     --  Setting the result
                     test_all_optims.CDR.echoSixteenKb59_Args_Type
                       (Request.Payload.Args.all).Returns :=
                       Result_Ü;
                  end;
               when 60 =>
                  declare
                     data : test_all_optims.sixteenKb;
                     Error : PolyORB.Errors.Error_Container;
                     Result_Ü : test_all_optims.sixteenKb;
                  begin
                     --  Setting the request Payload
                     test_all_optims.CDR.echoSixteenKb60_Set_Args
                       (Request,
                        new test_all_optims.CDR.echoSixteenKb60_Args_Type);
                     --  Processing request
                     PolyORB.Requests.Arguments
                       (PolyORB.Requests.Request_Access
                          (Request),
                        Request.Payload,
                        Error);
                     if PolyORB.Errors.Found
                       (Error)
                     then
                        PolyORB.CORBA_P.Exceptions.Raise_From_Error
                          (Error);
                     end if;
                     --  Setting Parameters
                     data :=
                       test_all_optims.CDR.echoSixteenKb60_Args_Type
                          (Request.Payload.Args.all).data;
                     --  Call Implementation
                     Result_Ü :=
                       test_all_optims.Impl.echoSixteenKb60
                          (test_all_optims.Impl.Object'Class
                             (Self.all)'Access,
                           data);
                     --  Setting the result
                     test_all_optims.CDR.echoSixteenKb60_Args_Type
                       (Request.Payload.Args.all).Returns :=
                       Result_Ü;
                  end;
               when 61 =>
                  declare
                     data : test_all_optims.sixteenKb;
                     Error : PolyORB.Errors.Error_Container;
                     Result_Ü : test_all_optims.sixteenKb;
                  begin
                     --  Setting the request Payload
                     test_all_optims.CDR.echoSixteenKb61_Set_Args
                       (Request,
                        new test_all_optims.CDR.echoSixteenKb61_Args_Type);
                     --  Processing request
                     PolyORB.Requests.Arguments
                       (PolyORB.Requests.Request_Access
                          (Request),
                        Request.Payload,
                        Error);
                     if PolyORB.Errors.Found
                       (Error)
                     then
                        PolyORB.CORBA_P.Exceptions.Raise_From_Error
                          (Error);
                     end if;
                     --  Setting Parameters
                     data :=
                       test_all_optims.CDR.echoSixteenKb61_Args_Type
                          (Request.Payload.Args.all).data;
                     --  Call Implementation
                     Result_Ü :=
                       test_all_optims.Impl.echoSixteenKb61
                          (test_all_optims.Impl.Object'Class
                             (Self.all)'Access,
                           data);
                     --  Setting the result
                     test_all_optims.CDR.echoSixteenKb61_Args_Type
                       (Request.Payload.Args.all).Returns :=
                       Result_Ü;
                  end;
               when 62 =>
                  declare
                     data : test_all_optims.sixteenKb;
                     Error : PolyORB.Errors.Error_Container;
                     Result_Ü : test_all_optims.sixteenKb;
                  begin
                     --  Setting the request Payload
                     test_all_optims.CDR.echoSixteenKb62_Set_Args
                       (Request,
                        new test_all_optims.CDR.echoSixteenKb62_Args_Type);
                     --  Processing request
                     PolyORB.Requests.Arguments
                       (PolyORB.Requests.Request_Access
                          (Request),
                        Request.Payload,
                        Error);
                     if PolyORB.Errors.Found
                       (Error)
                     then
                        PolyORB.CORBA_P.Exceptions.Raise_From_Error
                          (Error);
                     end if;
                     --  Setting Parameters
                     data :=
                       test_all_optims.CDR.echoSixteenKb62_Args_Type
                          (Request.Payload.Args.all).data;
                     --  Call Implementation
                     Result_Ü :=
                       test_all_optims.Impl.echoSixteenKb62
                          (test_all_optims.Impl.Object'Class
                             (Self.all)'Access,
                           data);
                     --  Setting the result
                     test_all_optims.CDR.echoSixteenKb62_Args_Type
                       (Request.Payload.Args.all).Returns :=
                       Result_Ü;
                  end;
               when 63 =>
                  declare
                     data : test_all_optims.sixteenKb;
                     Error : PolyORB.Errors.Error_Container;
                     Result_Ü : test_all_optims.sixteenKb;
                  begin
                     --  Setting the request Payload
                     test_all_optims.CDR.echoSixteenKb63_Set_Args
                       (Request,
                        new test_all_optims.CDR.echoSixteenKb63_Args_Type);
                     --  Processing request
                     PolyORB.Requests.Arguments
                       (PolyORB.Requests.Request_Access
                          (Request),
                        Request.Payload,
                        Error);
                     if PolyORB.Errors.Found
                       (Error)
                     then
                        PolyORB.CORBA_P.Exceptions.Raise_From_Error
                          (Error);
                     end if;
                     --  Setting Parameters
                     data :=
                       test_all_optims.CDR.echoSixteenKb63_Args_Type
                          (Request.Payload.Args.all).data;
                     --  Call Implementation
                     Result_Ü :=
                       test_all_optims.Impl.echoSixteenKb63
                          (test_all_optims.Impl.Object'Class
                             (Self.all)'Access,
                           data);
                     --  Setting the result
                     test_all_optims.CDR.echoSixteenKb63_Args_Type
                       (Request.Payload.Args.all).Returns :=
                       Result_Ü;
                  end;
               when 64 =>
                  declare
                     data : test_all_optims.sixteenKb;
                     Error : PolyORB.Errors.Error_Container;
                     Result_Ü : test_all_optims.sixteenKb;
                  begin
                     --  Setting the request Payload
                     test_all_optims.CDR.echoSixteenKb64_Set_Args
                       (Request,
                        new test_all_optims.CDR.echoSixteenKb64_Args_Type);
                     --  Processing request
                     PolyORB.Requests.Arguments
                       (PolyORB.Requests.Request_Access
                          (Request),
                        Request.Payload,
                        Error);
                     if PolyORB.Errors.Found
                       (Error)
                     then
                        PolyORB.CORBA_P.Exceptions.Raise_From_Error
                          (Error);
                     end if;
                     --  Setting Parameters
                     data :=
                       test_all_optims.CDR.echoSixteenKb64_Args_Type
                          (Request.Payload.Args.all).data;
                     --  Call Implementation
                     Result_Ü :=
                       test_all_optims.Impl.echoSixteenKb64
                          (test_all_optims.Impl.Object'Class
                             (Self.all)'Access,
                           data);
                     --  Setting the result
                     test_all_optims.CDR.echoSixteenKb64_Args_Type
                       (Request.Payload.Args.all).Returns :=
                       Result_Ü;
                  end;
               when 65 =>
                  declare
                     data : test_all_optims.sixteenKb;
                     Error : PolyORB.Errors.Error_Container;
                     Result_Ü : test_all_optims.sixteenKb;
                  begin
                     --  Setting the request Payload
                     test_all_optims.CDR.echoSixteenKb65_Set_Args
                       (Request,
                        new test_all_optims.CDR.echoSixteenKb65_Args_Type);
                     --  Processing request
                     PolyORB.Requests.Arguments
                       (PolyORB.Requests.Request_Access
                          (Request),
                        Request.Payload,
                        Error);
                     if PolyORB.Errors.Found
                       (Error)
                     then
                        PolyORB.CORBA_P.Exceptions.Raise_From_Error
                          (Error);
                     end if;
                     --  Setting Parameters
                     data :=
                       test_all_optims.CDR.echoSixteenKb65_Args_Type
                          (Request.Payload.Args.all).data;
                     --  Call Implementation
                     Result_Ü :=
                       test_all_optims.Impl.echoSixteenKb65
                          (test_all_optims.Impl.Object'Class
                             (Self.all)'Access,
                           data);
                     --  Setting the result
                     test_all_optims.CDR.echoSixteenKb65_Args_Type
                       (Request.Payload.Args.all).Returns :=
                       Result_Ü;
                  end;
               when 66 =>
                  declare
                     data : test_all_optims.sixteenKb;
                     Error : PolyORB.Errors.Error_Container;
                     Result_Ü : test_all_optims.sixteenKb;
                  begin
                     --  Setting the request Payload
                     test_all_optims.CDR.echoSixteenKb66_Set_Args
                       (Request,
                        new test_all_optims.CDR.echoSixteenKb66_Args_Type);
                     --  Processing request
                     PolyORB.Requests.Arguments
                       (PolyORB.Requests.Request_Access
                          (Request),
                        Request.Payload,
                        Error);
                     if PolyORB.Errors.Found
                       (Error)
                     then
                        PolyORB.CORBA_P.Exceptions.Raise_From_Error
                          (Error);
                     end if;
                     --  Setting Parameters
                     data :=
                       test_all_optims.CDR.echoSixteenKb66_Args_Type
                          (Request.Payload.Args.all).data;
                     --  Call Implementation
                     Result_Ü :=
                       test_all_optims.Impl.echoSixteenKb66
                          (test_all_optims.Impl.Object'Class
                             (Self.all)'Access,
                           data);
                     --  Setting the result
                     test_all_optims.CDR.echoSixteenKb66_Args_Type
                       (Request.Payload.Args.all).Returns :=
                       Result_Ü;
                  end;
               when 67 =>
                  declare
                     data : test_all_optims.sixteenKb;
                     Error : PolyORB.Errors.Error_Container;
                     Result_Ü : test_all_optims.sixteenKb;
                  begin
                     --  Setting the request Payload
                     test_all_optims.CDR.echoSixteenKb67_Set_Args
                       (Request,
                        new test_all_optims.CDR.echoSixteenKb67_Args_Type);
                     --  Processing request
                     PolyORB.Requests.Arguments
                       (PolyORB.Requests.Request_Access
                          (Request),
                        Request.Payload,
                        Error);
                     if PolyORB.Errors.Found
                       (Error)
                     then
                        PolyORB.CORBA_P.Exceptions.Raise_From_Error
                          (Error);
                     end if;
                     --  Setting Parameters
                     data :=
                       test_all_optims.CDR.echoSixteenKb67_Args_Type
                          (Request.Payload.Args.all).data;
                     --  Call Implementation
                     Result_Ü :=
                       test_all_optims.Impl.echoSixteenKb67
                          (test_all_optims.Impl.Object'Class
                             (Self.all)'Access,
                           data);
                     --  Setting the result
                     test_all_optims.CDR.echoSixteenKb67_Args_Type
                       (Request.Payload.Args.all).Returns :=
                       Result_Ü;
                  end;
               when 68 =>
                  declare
                     data : test_all_optims.sixteenKb;
                     Error : PolyORB.Errors.Error_Container;
                     Result_Ü : test_all_optims.sixteenKb;
                  begin
                     --  Setting the request Payload
                     test_all_optims.CDR.echoSixteenKb68_Set_Args
                       (Request,
                        new test_all_optims.CDR.echoSixteenKb68_Args_Type);
                     --  Processing request
                     PolyORB.Requests.Arguments
                       (PolyORB.Requests.Request_Access
                          (Request),
                        Request.Payload,
                        Error);
                     if PolyORB.Errors.Found
                       (Error)
                     then
                        PolyORB.CORBA_P.Exceptions.Raise_From_Error
                          (Error);
                     end if;
                     --  Setting Parameters
                     data :=
                       test_all_optims.CDR.echoSixteenKb68_Args_Type
                          (Request.Payload.Args.all).data;
                     --  Call Implementation
                     Result_Ü :=
                       test_all_optims.Impl.echoSixteenKb68
                          (test_all_optims.Impl.Object'Class
                             (Self.all)'Access,
                           data);
                     --  Setting the result
                     test_all_optims.CDR.echoSixteenKb68_Args_Type
                       (Request.Payload.Args.all).Returns :=
                       Result_Ü;
                  end;
               when 69 =>
                  declare
                     data : test_all_optims.sixteenKb;
                     Error : PolyORB.Errors.Error_Container;
                     Result_Ü : test_all_optims.sixteenKb;
                  begin
                     --  Setting the request Payload
                     test_all_optims.CDR.echoSixteenKb69_Set_Args
                       (Request,
                        new test_all_optims.CDR.echoSixteenKb69_Args_Type);
                     --  Processing request
                     PolyORB.Requests.Arguments
                       (PolyORB.Requests.Request_Access
                          (Request),
                        Request.Payload,
                        Error);
                     if PolyORB.Errors.Found
                       (Error)
                     then
                        PolyORB.CORBA_P.Exceptions.Raise_From_Error
                          (Error);
                     end if;
                     --  Setting Parameters
                     data :=
                       test_all_optims.CDR.echoSixteenKb69_Args_Type
                          (Request.Payload.Args.all).data;
                     --  Call Implementation
                     Result_Ü :=
                       test_all_optims.Impl.echoSixteenKb69
                          (test_all_optims.Impl.Object'Class
                             (Self.all)'Access,
                           data);
                     --  Setting the result
                     test_all_optims.CDR.echoSixteenKb69_Args_Type
                       (Request.Payload.Args.all).Returns :=
                       Result_Ü;
                  end;
               when 70 =>
                  declare
                     data : test_all_optims.sixteenKb;
                     Error : PolyORB.Errors.Error_Container;
                     Result_Ü : test_all_optims.sixteenKb;
                  begin
                     --  Setting the request Payload
                     test_all_optims.CDR.echoSixteenKb70_Set_Args
                       (Request,
                        new test_all_optims.CDR.echoSixteenKb70_Args_Type);
                     --  Processing request
                     PolyORB.Requests.Arguments
                       (PolyORB.Requests.Request_Access
                          (Request),
                        Request.Payload,
                        Error);
                     if PolyORB.Errors.Found
                       (Error)
                     then
                        PolyORB.CORBA_P.Exceptions.Raise_From_Error
                          (Error);
                     end if;
                     --  Setting Parameters
                     data :=
                       test_all_optims.CDR.echoSixteenKb70_Args_Type
                          (Request.Payload.Args.all).data;
                     --  Call Implementation
                     Result_Ü :=
                       test_all_optims.Impl.echoSixteenKb70
                          (test_all_optims.Impl.Object'Class
                             (Self.all)'Access,
                           data);
                     --  Setting the result
                     test_all_optims.CDR.echoSixteenKb70_Args_Type
                       (Request.Payload.Args.all).Returns :=
                       Result_Ü;
                  end;
               when 71 =>
                  declare
                     data : test_all_optims.sixteenKb;
                     Error : PolyORB.Errors.Error_Container;
                     Result_Ü : test_all_optims.sixteenKb;
                  begin
                     --  Setting the request Payload
                     test_all_optims.CDR.echoSixteenKb71_Set_Args
                       (Request,
                        new test_all_optims.CDR.echoSixteenKb71_Args_Type);
                     --  Processing request
                     PolyORB.Requests.Arguments
                       (PolyORB.Requests.Request_Access
                          (Request),
                        Request.Payload,
                        Error);
                     if PolyORB.Errors.Found
                       (Error)
                     then
                        PolyORB.CORBA_P.Exceptions.Raise_From_Error
                          (Error);
                     end if;
                     --  Setting Parameters
                     data :=
                       test_all_optims.CDR.echoSixteenKb71_Args_Type
                          (Request.Payload.Args.all).data;
                     --  Call Implementation
                     Result_Ü :=
                       test_all_optims.Impl.echoSixteenKb71
                          (test_all_optims.Impl.Object'Class
                             (Self.all)'Access,
                           data);
                     --  Setting the result
                     test_all_optims.CDR.echoSixteenKb71_Args_Type
                       (Request.Payload.Args.all).Returns :=
                       Result_Ü;
                  end;
               when 72 =>
                  declare
                     data : test_all_optims.sixteenKb;
                     Error : PolyORB.Errors.Error_Container;
                     Result_Ü : test_all_optims.sixteenKb;
                  begin
                     --  Setting the request Payload
                     test_all_optims.CDR.echoSixteenKb72_Set_Args
                       (Request,
                        new test_all_optims.CDR.echoSixteenKb72_Args_Type);
                     --  Processing request
                     PolyORB.Requests.Arguments
                       (PolyORB.Requests.Request_Access
                          (Request),
                        Request.Payload,
                        Error);
                     if PolyORB.Errors.Found
                       (Error)
                     then
                        PolyORB.CORBA_P.Exceptions.Raise_From_Error
                          (Error);
                     end if;
                     --  Setting Parameters
                     data :=
                       test_all_optims.CDR.echoSixteenKb72_Args_Type
                          (Request.Payload.Args.all).data;
                     --  Call Implementation
                     Result_Ü :=
                       test_all_optims.Impl.echoSixteenKb72
                          (test_all_optims.Impl.Object'Class
                             (Self.all)'Access,
                           data);
                     --  Setting the result
                     test_all_optims.CDR.echoSixteenKb72_Args_Type
                       (Request.Payload.Args.all).Returns :=
                       Result_Ü;
                  end;
               when 73 =>
                  declare
                     data : test_all_optims.sixteenKb;
                     Error : PolyORB.Errors.Error_Container;
                     Result_Ü : test_all_optims.sixteenKb;
                  begin
                     --  Setting the request Payload
                     test_all_optims.CDR.echoSixteenKb73_Set_Args
                       (Request,
                        new test_all_optims.CDR.echoSixteenKb73_Args_Type);
                     --  Processing request
                     PolyORB.Requests.Arguments
                       (PolyORB.Requests.Request_Access
                          (Request),
                        Request.Payload,
                        Error);
                     if PolyORB.Errors.Found
                       (Error)
                     then
                        PolyORB.CORBA_P.Exceptions.Raise_From_Error
                          (Error);
                     end if;
                     --  Setting Parameters
                     data :=
                       test_all_optims.CDR.echoSixteenKb73_Args_Type
                          (Request.Payload.Args.all).data;
                     --  Call Implementation
                     Result_Ü :=
                       test_all_optims.Impl.echoSixteenKb73
                          (test_all_optims.Impl.Object'Class
                             (Self.all)'Access,
                           data);
                     --  Setting the result
                     test_all_optims.CDR.echoSixteenKb73_Args_Type
                       (Request.Payload.Args.all).Returns :=
                       Result_Ü;
                  end;
               when 74 =>
                  declare
                     data : test_all_optims.sixteenKb;
                     Error : PolyORB.Errors.Error_Container;
                     Result_Ü : test_all_optims.sixteenKb;
                  begin
                     --  Setting the request Payload
                     test_all_optims.CDR.echoSixteenKb74_Set_Args
                       (Request,
                        new test_all_optims.CDR.echoSixteenKb74_Args_Type);
                     --  Processing request
                     PolyORB.Requests.Arguments
                       (PolyORB.Requests.Request_Access
                          (Request),
                        Request.Payload,
                        Error);
                     if PolyORB.Errors.Found
                       (Error)
                     then
                        PolyORB.CORBA_P.Exceptions.Raise_From_Error
                          (Error);
                     end if;
                     --  Setting Parameters
                     data :=
                       test_all_optims.CDR.echoSixteenKb74_Args_Type
                          (Request.Payload.Args.all).data;
                     --  Call Implementation
                     Result_Ü :=
                       test_all_optims.Impl.echoSixteenKb74
                          (test_all_optims.Impl.Object'Class
                             (Self.all)'Access,
                           data);
                     --  Setting the result
                     test_all_optims.CDR.echoSixteenKb74_Args_Type
                       (Request.Payload.Args.all).Returns :=
                       Result_Ü;
                  end;
               when 75 =>
                  declare
                     data : test_all_optims.sixteenKb;
                     Error : PolyORB.Errors.Error_Container;
                     Result_Ü : test_all_optims.sixteenKb;
                  begin
                     --  Setting the request Payload
                     test_all_optims.CDR.echoSixteenKb75_Set_Args
                       (Request,
                        new test_all_optims.CDR.echoSixteenKb75_Args_Type);
                     --  Processing request
                     PolyORB.Requests.Arguments
                       (PolyORB.Requests.Request_Access
                          (Request),
                        Request.Payload,
                        Error);
                     if PolyORB.Errors.Found
                       (Error)
                     then
                        PolyORB.CORBA_P.Exceptions.Raise_From_Error
                          (Error);
                     end if;
                     --  Setting Parameters
                     data :=
                       test_all_optims.CDR.echoSixteenKb75_Args_Type
                          (Request.Payload.Args.all).data;
                     --  Call Implementation
                     Result_Ü :=
                       test_all_optims.Impl.echoSixteenKb75
                          (test_all_optims.Impl.Object'Class
                             (Self.all)'Access,
                           data);
                     --  Setting the result
                     test_all_optims.CDR.echoSixteenKb75_Args_Type
                       (Request.Payload.Args.all).Returns :=
                       Result_Ü;
                  end;
               when 76 =>
                  declare
                     data : test_all_optims.sixteenKb;
                     Error : PolyORB.Errors.Error_Container;
                     Result_Ü : test_all_optims.sixteenKb;
                  begin
                     --  Setting the request Payload
                     test_all_optims.CDR.echoSixteenKb76_Set_Args
                       (Request,
                        new test_all_optims.CDR.echoSixteenKb76_Args_Type);
                     --  Processing request
                     PolyORB.Requests.Arguments
                       (PolyORB.Requests.Request_Access
                          (Request),
                        Request.Payload,
                        Error);
                     if PolyORB.Errors.Found
                       (Error)
                     then
                        PolyORB.CORBA_P.Exceptions.Raise_From_Error
                          (Error);
                     end if;
                     --  Setting Parameters
                     data :=
                       test_all_optims.CDR.echoSixteenKb76_Args_Type
                          (Request.Payload.Args.all).data;
                     --  Call Implementation
                     Result_Ü :=
                       test_all_optims.Impl.echoSixteenKb76
                          (test_all_optims.Impl.Object'Class
                             (Self.all)'Access,
                           data);
                     --  Setting the result
                     test_all_optims.CDR.echoSixteenKb76_Args_Type
                       (Request.Payload.Args.all).Returns :=
                       Result_Ü;
                  end;
               when 77 =>
                  declare
                     data : test_all_optims.sixteenKb;
                     Error : PolyORB.Errors.Error_Container;
                     Result_Ü : test_all_optims.sixteenKb;
                  begin
                     --  Setting the request Payload
                     test_all_optims.CDR.echoSixteenKb77_Set_Args
                       (Request,
                        new test_all_optims.CDR.echoSixteenKb77_Args_Type);
                     --  Processing request
                     PolyORB.Requests.Arguments
                       (PolyORB.Requests.Request_Access
                          (Request),
                        Request.Payload,
                        Error);
                     if PolyORB.Errors.Found
                       (Error)
                     then
                        PolyORB.CORBA_P.Exceptions.Raise_From_Error
                          (Error);
                     end if;
                     --  Setting Parameters
                     data :=
                       test_all_optims.CDR.echoSixteenKb77_Args_Type
                          (Request.Payload.Args.all).data;
                     --  Call Implementation
                     Result_Ü :=
                       test_all_optims.Impl.echoSixteenKb77
                          (test_all_optims.Impl.Object'Class
                             (Self.all)'Access,
                           data);
                     --  Setting the result
                     test_all_optims.CDR.echoSixteenKb77_Args_Type
                       (Request.Payload.Args.all).Returns :=
                       Result_Ü;
                  end;
               when 78 =>
                  declare
                     data : test_all_optims.sixteenKb;
                     Error : PolyORB.Errors.Error_Container;
                     Result_Ü : test_all_optims.sixteenKb;
                  begin
                     --  Setting the request Payload
                     test_all_optims.CDR.echoSixteenKb78_Set_Args
                       (Request,
                        new test_all_optims.CDR.echoSixteenKb78_Args_Type);
                     --  Processing request
                     PolyORB.Requests.Arguments
                       (PolyORB.Requests.Request_Access
                          (Request),
                        Request.Payload,
                        Error);
                     if PolyORB.Errors.Found
                       (Error)
                     then
                        PolyORB.CORBA_P.Exceptions.Raise_From_Error
                          (Error);
                     end if;
                     --  Setting Parameters
                     data :=
                       test_all_optims.CDR.echoSixteenKb78_Args_Type
                          (Request.Payload.Args.all).data;
                     --  Call Implementation
                     Result_Ü :=
                       test_all_optims.Impl.echoSixteenKb78
                          (test_all_optims.Impl.Object'Class
                             (Self.all)'Access,
                           data);
                     --  Setting the result
                     test_all_optims.CDR.echoSixteenKb78_Args_Type
                       (Request.Payload.Args.all).Returns :=
                       Result_Ü;
                  end;
               when 79 =>
                  declare
                     data : test_all_optims.sixteenKb;
                     Error : PolyORB.Errors.Error_Container;
                     Result_Ü : test_all_optims.sixteenKb;
                  begin
                     --  Setting the request Payload
                     test_all_optims.CDR.echoSixteenKb79_Set_Args
                       (Request,
                        new test_all_optims.CDR.echoSixteenKb79_Args_Type);
                     --  Processing request
                     PolyORB.Requests.Arguments
                       (PolyORB.Requests.Request_Access
                          (Request),
                        Request.Payload,
                        Error);
                     if PolyORB.Errors.Found
                       (Error)
                     then
                        PolyORB.CORBA_P.Exceptions.Raise_From_Error
                          (Error);
                     end if;
                     --  Setting Parameters
                     data :=
                       test_all_optims.CDR.echoSixteenKb79_Args_Type
                          (Request.Payload.Args.all).data;
                     --  Call Implementation
                     Result_Ü :=
                       test_all_optims.Impl.echoSixteenKb79
                          (test_all_optims.Impl.Object'Class
                             (Self.all)'Access,
                           data);
                     --  Setting the result
                     test_all_optims.CDR.echoSixteenKb79_Args_Type
                       (Request.Payload.Args.all).Returns :=
                       Result_Ü;
                  end;
               when 80 =>
                  declare
                     data : test_all_optims.sixteenKb;
                     Error : PolyORB.Errors.Error_Container;
                     Result_Ü : test_all_optims.sixteenKb;
                  begin
                     --  Setting the request Payload
                     test_all_optims.CDR.echoSixteenKb80_Set_Args
                       (Request,
                        new test_all_optims.CDR.echoSixteenKb80_Args_Type);
                     --  Processing request
                     PolyORB.Requests.Arguments
                       (PolyORB.Requests.Request_Access
                          (Request),
                        Request.Payload,
                        Error);
                     if PolyORB.Errors.Found
                       (Error)
                     then
                        PolyORB.CORBA_P.Exceptions.Raise_From_Error
                          (Error);
                     end if;
                     --  Setting Parameters
                     data :=
                       test_all_optims.CDR.echoSixteenKb80_Args_Type
                          (Request.Payload.Args.all).data;
                     --  Call Implementation
                     Result_Ü :=
                       test_all_optims.Impl.echoSixteenKb80
                          (test_all_optims.Impl.Object'Class
                             (Self.all)'Access,
                           data);
                     --  Setting the result
                     test_all_optims.CDR.echoSixteenKb80_Args_Type
                       (Request.Payload.Args.all).Returns :=
                       Result_Ü;
                  end;
               when 81 =>
                  declare
                     data : test_all_optims.sixteenKb;
                     Error : PolyORB.Errors.Error_Container;
                     Result_Ü : test_all_optims.sixteenKb;
                  begin
                     --  Setting the request Payload
                     test_all_optims.CDR.echoSixteenKb81_Set_Args
                       (Request,
                        new test_all_optims.CDR.echoSixteenKb81_Args_Type);
                     --  Processing request
                     PolyORB.Requests.Arguments
                       (PolyORB.Requests.Request_Access
                          (Request),
                        Request.Payload,
                        Error);
                     if PolyORB.Errors.Found
                       (Error)
                     then
                        PolyORB.CORBA_P.Exceptions.Raise_From_Error
                          (Error);
                     end if;
                     --  Setting Parameters
                     data :=
                       test_all_optims.CDR.echoSixteenKb81_Args_Type
                          (Request.Payload.Args.all).data;
                     --  Call Implementation
                     Result_Ü :=
                       test_all_optims.Impl.echoSixteenKb81
                          (test_all_optims.Impl.Object'Class
                             (Self.all)'Access,
                           data);
                     --  Setting the result
                     test_all_optims.CDR.echoSixteenKb81_Args_Type
                       (Request.Payload.Args.all).Returns :=
                       Result_Ü;
                  end;
               when 82 =>
                  declare
                     data : test_all_optims.sixteenKb;
                     Error : PolyORB.Errors.Error_Container;
                     Result_Ü : test_all_optims.sixteenKb;
                  begin
                     --  Setting the request Payload
                     test_all_optims.CDR.echoSixteenKb82_Set_Args
                       (Request,
                        new test_all_optims.CDR.echoSixteenKb82_Args_Type);
                     --  Processing request
                     PolyORB.Requests.Arguments
                       (PolyORB.Requests.Request_Access
                          (Request),
                        Request.Payload,
                        Error);
                     if PolyORB.Errors.Found
                       (Error)
                     then
                        PolyORB.CORBA_P.Exceptions.Raise_From_Error
                          (Error);
                     end if;
                     --  Setting Parameters
                     data :=
                       test_all_optims.CDR.echoSixteenKb82_Args_Type
                          (Request.Payload.Args.all).data;
                     --  Call Implementation
                     Result_Ü :=
                       test_all_optims.Impl.echoSixteenKb82
                          (test_all_optims.Impl.Object'Class
                             (Self.all)'Access,
                           data);
                     --  Setting the result
                     test_all_optims.CDR.echoSixteenKb82_Args_Type
                       (Request.Payload.Args.all).Returns :=
                       Result_Ü;
                  end;
               when 83 =>
                  declare
                     data : test_all_optims.sixteenKb;
                     Error : PolyORB.Errors.Error_Container;
                     Result_Ü : test_all_optims.sixteenKb;
                  begin
                     --  Setting the request Payload
                     test_all_optims.CDR.echoSixteenKb83_Set_Args
                       (Request,
                        new test_all_optims.CDR.echoSixteenKb83_Args_Type);
                     --  Processing request
                     PolyORB.Requests.Arguments
                       (PolyORB.Requests.Request_Access
                          (Request),
                        Request.Payload,
                        Error);
                     if PolyORB.Errors.Found
                       (Error)
                     then
                        PolyORB.CORBA_P.Exceptions.Raise_From_Error
                          (Error);
                     end if;
                     --  Setting Parameters
                     data :=
                       test_all_optims.CDR.echoSixteenKb83_Args_Type
                          (Request.Payload.Args.all).data;
                     --  Call Implementation
                     Result_Ü :=
                       test_all_optims.Impl.echoSixteenKb83
                          (test_all_optims.Impl.Object'Class
                             (Self.all)'Access,
                           data);
                     --  Setting the result
                     test_all_optims.CDR.echoSixteenKb83_Args_Type
                       (Request.Payload.Args.all).Returns :=
                       Result_Ü;
                  end;
               when 84 =>
                  declare
                     data : test_all_optims.sixteenKb;
                     Error : PolyORB.Errors.Error_Container;
                     Result_Ü : test_all_optims.sixteenKb;
                  begin
                     --  Setting the request Payload
                     test_all_optims.CDR.echoSixteenKb84_Set_Args
                       (Request,
                        new test_all_optims.CDR.echoSixteenKb84_Args_Type);
                     --  Processing request
                     PolyORB.Requests.Arguments
                       (PolyORB.Requests.Request_Access
                          (Request),
                        Request.Payload,
                        Error);
                     if PolyORB.Errors.Found
                       (Error)
                     then
                        PolyORB.CORBA_P.Exceptions.Raise_From_Error
                          (Error);
                     end if;
                     --  Setting Parameters
                     data :=
                       test_all_optims.CDR.echoSixteenKb84_Args_Type
                          (Request.Payload.Args.all).data;
                     --  Call Implementation
                     Result_Ü :=
                       test_all_optims.Impl.echoSixteenKb84
                          (test_all_optims.Impl.Object'Class
                             (Self.all)'Access,
                           data);
                     --  Setting the result
                     test_all_optims.CDR.echoSixteenKb84_Args_Type
                       (Request.Payload.Args.all).Returns :=
                       Result_Ü;
                  end;
               when 85 =>
                  declare
                     data : test_all_optims.sixteenKb;
                     Error : PolyORB.Errors.Error_Container;
                     Result_Ü : test_all_optims.sixteenKb;
                  begin
                     --  Setting the request Payload
                     test_all_optims.CDR.echoSixteenKb85_Set_Args
                       (Request,
                        new test_all_optims.CDR.echoSixteenKb85_Args_Type);
                     --  Processing request
                     PolyORB.Requests.Arguments
                       (PolyORB.Requests.Request_Access
                          (Request),
                        Request.Payload,
                        Error);
                     if PolyORB.Errors.Found
                       (Error)
                     then
                        PolyORB.CORBA_P.Exceptions.Raise_From_Error
                          (Error);
                     end if;
                     --  Setting Parameters
                     data :=
                       test_all_optims.CDR.echoSixteenKb85_Args_Type
                          (Request.Payload.Args.all).data;
                     --  Call Implementation
                     Result_Ü :=
                       test_all_optims.Impl.echoSixteenKb85
                          (test_all_optims.Impl.Object'Class
                             (Self.all)'Access,
                           data);
                     --  Setting the result
                     test_all_optims.CDR.echoSixteenKb85_Args_Type
                       (Request.Payload.Args.all).Returns :=
                       Result_Ü;
                  end;
               when 86 =>
                  declare
                     data : test_all_optims.sixteenKb;
                     Error : PolyORB.Errors.Error_Container;
                     Result_Ü : test_all_optims.sixteenKb;
                  begin
                     --  Setting the request Payload
                     test_all_optims.CDR.echoSixteenKb86_Set_Args
                       (Request,
                        new test_all_optims.CDR.echoSixteenKb86_Args_Type);
                     --  Processing request
                     PolyORB.Requests.Arguments
                       (PolyORB.Requests.Request_Access
                          (Request),
                        Request.Payload,
                        Error);
                     if PolyORB.Errors.Found
                       (Error)
                     then
                        PolyORB.CORBA_P.Exceptions.Raise_From_Error
                          (Error);
                     end if;
                     --  Setting Parameters
                     data :=
                       test_all_optims.CDR.echoSixteenKb86_Args_Type
                          (Request.Payload.Args.all).data;
                     --  Call Implementation
                     Result_Ü :=
                       test_all_optims.Impl.echoSixteenKb86
                          (test_all_optims.Impl.Object'Class
                             (Self.all)'Access,
                           data);
                     --  Setting the result
                     test_all_optims.CDR.echoSixteenKb86_Args_Type
                       (Request.Payload.Args.all).Returns :=
                       Result_Ü;
                  end;
               when 87 =>
                  declare
                     data : test_all_optims.sixteenKb;
                     Error : PolyORB.Errors.Error_Container;
                     Result_Ü : test_all_optims.sixteenKb;
                  begin
                     --  Setting the request Payload
                     test_all_optims.CDR.echoSixteenKb87_Set_Args
                       (Request,
                        new test_all_optims.CDR.echoSixteenKb87_Args_Type);
                     --  Processing request
                     PolyORB.Requests.Arguments
                       (PolyORB.Requests.Request_Access
                          (Request),
                        Request.Payload,
                        Error);
                     if PolyORB.Errors.Found
                       (Error)
                     then
                        PolyORB.CORBA_P.Exceptions.Raise_From_Error
                          (Error);
                     end if;
                     --  Setting Parameters
                     data :=
                       test_all_optims.CDR.echoSixteenKb87_Args_Type
                          (Request.Payload.Args.all).data;
                     --  Call Implementation
                     Result_Ü :=
                       test_all_optims.Impl.echoSixteenKb87
                          (test_all_optims.Impl.Object'Class
                             (Self.all)'Access,
                           data);
                     --  Setting the result
                     test_all_optims.CDR.echoSixteenKb87_Args_Type
                       (Request.Payload.Args.all).Returns :=
                       Result_Ü;
                  end;
               when 88 =>
                  declare
                     data : test_all_optims.sixteenKb;
                     Error : PolyORB.Errors.Error_Container;
                     Result_Ü : test_all_optims.sixteenKb;
                  begin
                     --  Setting the request Payload
                     test_all_optims.CDR.echoSixteenKb88_Set_Args
                       (Request,
                        new test_all_optims.CDR.echoSixteenKb88_Args_Type);
                     --  Processing request
                     PolyORB.Requests.Arguments
                       (PolyORB.Requests.Request_Access
                          (Request),
                        Request.Payload,
                        Error);
                     if PolyORB.Errors.Found
                       (Error)
                     then
                        PolyORB.CORBA_P.Exceptions.Raise_From_Error
                          (Error);
                     end if;
                     --  Setting Parameters
                     data :=
                       test_all_optims.CDR.echoSixteenKb88_Args_Type
                          (Request.Payload.Args.all).data;
                     --  Call Implementation
                     Result_Ü :=
                       test_all_optims.Impl.echoSixteenKb88
                          (test_all_optims.Impl.Object'Class
                             (Self.all)'Access,
                           data);
                     --  Setting the result
                     test_all_optims.CDR.echoSixteenKb88_Args_Type
                       (Request.Payload.Args.all).Returns :=
                       Result_Ü;
                  end;
               when 89 =>
                  declare
                     data : test_all_optims.sixteenKb;
                     Error : PolyORB.Errors.Error_Container;
                     Result_Ü : test_all_optims.sixteenKb;
                  begin
                     --  Setting the request Payload
                     test_all_optims.CDR.echoSixteenKb89_Set_Args
                       (Request,
                        new test_all_optims.CDR.echoSixteenKb89_Args_Type);
                     --  Processing request
                     PolyORB.Requests.Arguments
                       (PolyORB.Requests.Request_Access
                          (Request),
                        Request.Payload,
                        Error);
                     if PolyORB.Errors.Found
                       (Error)
                     then
                        PolyORB.CORBA_P.Exceptions.Raise_From_Error
                          (Error);
                     end if;
                     --  Setting Parameters
                     data :=
                       test_all_optims.CDR.echoSixteenKb89_Args_Type
                          (Request.Payload.Args.all).data;
                     --  Call Implementation
                     Result_Ü :=
                       test_all_optims.Impl.echoSixteenKb89
                          (test_all_optims.Impl.Object'Class
                             (Self.all)'Access,
                           data);
                     --  Setting the result
                     test_all_optims.CDR.echoSixteenKb89_Args_Type
                       (Request.Payload.Args.all).Returns :=
                       Result_Ü;
                  end;
               when 90 =>
                  declare
                     data : test_all_optims.sixteenKb;
                     Error : PolyORB.Errors.Error_Container;
                     Result_Ü : test_all_optims.sixteenKb;
                  begin
                     --  Setting the request Payload
                     test_all_optims.CDR.echoSixteenKb90_Set_Args
                       (Request,
                        new test_all_optims.CDR.echoSixteenKb90_Args_Type);
                     --  Processing request
                     PolyORB.Requests.Arguments
                       (PolyORB.Requests.Request_Access
                          (Request),
                        Request.Payload,
                        Error);
                     if PolyORB.Errors.Found
                       (Error)
                     then
                        PolyORB.CORBA_P.Exceptions.Raise_From_Error
                          (Error);
                     end if;
                     --  Setting Parameters
                     data :=
                       test_all_optims.CDR.echoSixteenKb90_Args_Type
                          (Request.Payload.Args.all).data;
                     --  Call Implementation
                     Result_Ü :=
                       test_all_optims.Impl.echoSixteenKb90
                          (test_all_optims.Impl.Object'Class
                             (Self.all)'Access,
                           data);
                     --  Setting the result
                     test_all_optims.CDR.echoSixteenKb90_Args_Type
                       (Request.Payload.Args.all).Returns :=
                       Result_Ü;
                  end;
               when 91 =>
                  declare
                     data : test_all_optims.sixteenKb;
                     Error : PolyORB.Errors.Error_Container;
                     Result_Ü : test_all_optims.sixteenKb;
                  begin
                     --  Setting the request Payload
                     test_all_optims.CDR.echoSixteenKb91_Set_Args
                       (Request,
                        new test_all_optims.CDR.echoSixteenKb91_Args_Type);
                     --  Processing request
                     PolyORB.Requests.Arguments
                       (PolyORB.Requests.Request_Access
                          (Request),
                        Request.Payload,
                        Error);
                     if PolyORB.Errors.Found
                       (Error)
                     then
                        PolyORB.CORBA_P.Exceptions.Raise_From_Error
                          (Error);
                     end if;
                     --  Setting Parameters
                     data :=
                       test_all_optims.CDR.echoSixteenKb91_Args_Type
                          (Request.Payload.Args.all).data;
                     --  Call Implementation
                     Result_Ü :=
                       test_all_optims.Impl.echoSixteenKb91
                          (test_all_optims.Impl.Object'Class
                             (Self.all)'Access,
                           data);
                     --  Setting the result
                     test_all_optims.CDR.echoSixteenKb91_Args_Type
                       (Request.Payload.Args.all).Returns :=
                       Result_Ü;
                  end;
               when 92 =>
                  declare
                     data : test_all_optims.sixteenKb;
                     Error : PolyORB.Errors.Error_Container;
                     Result_Ü : test_all_optims.sixteenKb;
                  begin
                     --  Setting the request Payload
                     test_all_optims.CDR.echoSixteenKb92_Set_Args
                       (Request,
                        new test_all_optims.CDR.echoSixteenKb92_Args_Type);
                     --  Processing request
                     PolyORB.Requests.Arguments
                       (PolyORB.Requests.Request_Access
                          (Request),
                        Request.Payload,
                        Error);
                     if PolyORB.Errors.Found
                       (Error)
                     then
                        PolyORB.CORBA_P.Exceptions.Raise_From_Error
                          (Error);
                     end if;
                     --  Setting Parameters
                     data :=
                       test_all_optims.CDR.echoSixteenKb92_Args_Type
                          (Request.Payload.Args.all).data;
                     --  Call Implementation
                     Result_Ü :=
                       test_all_optims.Impl.echoSixteenKb92
                          (test_all_optims.Impl.Object'Class
                             (Self.all)'Access,
                           data);
                     --  Setting the result
                     test_all_optims.CDR.echoSixteenKb92_Args_Type
                       (Request.Payload.Args.all).Returns :=
                       Result_Ü;
                  end;
               when 93 =>
                  declare
                     data : test_all_optims.sixteenKb;
                     Error : PolyORB.Errors.Error_Container;
                     Result_Ü : test_all_optims.sixteenKb;
                  begin
                     --  Setting the request Payload
                     test_all_optims.CDR.echoSixteenKb93_Set_Args
                       (Request,
                        new test_all_optims.CDR.echoSixteenKb93_Args_Type);
                     --  Processing request
                     PolyORB.Requests.Arguments
                       (PolyORB.Requests.Request_Access
                          (Request),
                        Request.Payload,
                        Error);
                     if PolyORB.Errors.Found
                       (Error)
                     then
                        PolyORB.CORBA_P.Exceptions.Raise_From_Error
                          (Error);
                     end if;
                     --  Setting Parameters
                     data :=
                       test_all_optims.CDR.echoSixteenKb93_Args_Type
                          (Request.Payload.Args.all).data;
                     --  Call Implementation
                     Result_Ü :=
                       test_all_optims.Impl.echoSixteenKb93
                          (test_all_optims.Impl.Object'Class
                             (Self.all)'Access,
                           data);
                     --  Setting the result
                     test_all_optims.CDR.echoSixteenKb93_Args_Type
                       (Request.Payload.Args.all).Returns :=
                       Result_Ü;
                  end;
               when 94 =>
                  declare
                     data : test_all_optims.sixteenKb;
                     Error : PolyORB.Errors.Error_Container;
                     Result_Ü : test_all_optims.sixteenKb;
                  begin
                     --  Setting the request Payload
                     test_all_optims.CDR.echoSixteenKb94_Set_Args
                       (Request,
                        new test_all_optims.CDR.echoSixteenKb94_Args_Type);
                     --  Processing request
                     PolyORB.Requests.Arguments
                       (PolyORB.Requests.Request_Access
                          (Request),
                        Request.Payload,
                        Error);
                     if PolyORB.Errors.Found
                       (Error)
                     then
                        PolyORB.CORBA_P.Exceptions.Raise_From_Error
                          (Error);
                     end if;
                     --  Setting Parameters
                     data :=
                       test_all_optims.CDR.echoSixteenKb94_Args_Type
                          (Request.Payload.Args.all).data;
                     --  Call Implementation
                     Result_Ü :=
                       test_all_optims.Impl.echoSixteenKb94
                          (test_all_optims.Impl.Object'Class
                             (Self.all)'Access,
                           data);
                     --  Setting the result
                     test_all_optims.CDR.echoSixteenKb94_Args_Type
                       (Request.Payload.Args.all).Returns :=
                       Result_Ü;
                  end;
               when 95 =>
                  declare
                     data : test_all_optims.sixteenKb;
                     Error : PolyORB.Errors.Error_Container;
                     Result_Ü : test_all_optims.sixteenKb;
                  begin
                     --  Setting the request Payload
                     test_all_optims.CDR.echoSixteenKb95_Set_Args
                       (Request,
                        new test_all_optims.CDR.echoSixteenKb95_Args_Type);
                     --  Processing request
                     PolyORB.Requests.Arguments
                       (PolyORB.Requests.Request_Access
                          (Request),
                        Request.Payload,
                        Error);
                     if PolyORB.Errors.Found
                       (Error)
                     then
                        PolyORB.CORBA_P.Exceptions.Raise_From_Error
                          (Error);
                     end if;
                     --  Setting Parameters
                     data :=
                       test_all_optims.CDR.echoSixteenKb95_Args_Type
                          (Request.Payload.Args.all).data;
                     --  Call Implementation
                     Result_Ü :=
                       test_all_optims.Impl.echoSixteenKb95
                          (test_all_optims.Impl.Object'Class
                             (Self.all)'Access,
                           data);
                     --  Setting the result
                     test_all_optims.CDR.echoSixteenKb95_Args_Type
                       (Request.Payload.Args.all).Returns :=
                       Result_Ü;
                  end;
               when 96 =>
                  declare
                     data : test_all_optims.sixteenKb;
                     Error : PolyORB.Errors.Error_Container;
                     Result_Ü : test_all_optims.sixteenKb;
                  begin
                     --  Setting the request Payload
                     test_all_optims.CDR.echoSixteenKb96_Set_Args
                       (Request,
                        new test_all_optims.CDR.echoSixteenKb96_Args_Type);
                     --  Processing request
                     PolyORB.Requests.Arguments
                       (PolyORB.Requests.Request_Access
                          (Request),
                        Request.Payload,
                        Error);
                     if PolyORB.Errors.Found
                       (Error)
                     then
                        PolyORB.CORBA_P.Exceptions.Raise_From_Error
                          (Error);
                     end if;
                     --  Setting Parameters
                     data :=
                       test_all_optims.CDR.echoSixteenKb96_Args_Type
                          (Request.Payload.Args.all).data;
                     --  Call Implementation
                     Result_Ü :=
                       test_all_optims.Impl.echoSixteenKb96
                          (test_all_optims.Impl.Object'Class
                             (Self.all)'Access,
                           data);
                     --  Setting the result
                     test_all_optims.CDR.echoSixteenKb96_Args_Type
                       (Request.Payload.Args.all).Returns :=
                       Result_Ü;
                  end;
               when 97 =>
                  declare
                     data : test_all_optims.sixteenKb;
                     Error : PolyORB.Errors.Error_Container;
                     Result_Ü : test_all_optims.sixteenKb;
                  begin
                     --  Setting the request Payload
                     test_all_optims.CDR.echoSixteenKb97_Set_Args
                       (Request,
                        new test_all_optims.CDR.echoSixteenKb97_Args_Type);
                     --  Processing request
                     PolyORB.Requests.Arguments
                       (PolyORB.Requests.Request_Access
                          (Request),
                        Request.Payload,
                        Error);
                     if PolyORB.Errors.Found
                       (Error)
                     then
                        PolyORB.CORBA_P.Exceptions.Raise_From_Error
                          (Error);
                     end if;
                     --  Setting Parameters
                     data :=
                       test_all_optims.CDR.echoSixteenKb97_Args_Type
                          (Request.Payload.Args.all).data;
                     --  Call Implementation
                     Result_Ü :=
                       test_all_optims.Impl.echoSixteenKb97
                          (test_all_optims.Impl.Object'Class
                             (Self.all)'Access,
                           data);
                     --  Setting the result
                     test_all_optims.CDR.echoSixteenKb97_Args_Type
                       (Request.Payload.Args.all).Returns :=
                       Result_Ü;
                  end;
               when 98 =>
                  declare
                     data : test_all_optims.sixteenKb;
                     Error : PolyORB.Errors.Error_Container;
                     Result_Ü : test_all_optims.sixteenKb;
                  begin
                     --  Setting the request Payload
                     test_all_optims.CDR.echoSixteenKb98_Set_Args
                       (Request,
                        new test_all_optims.CDR.echoSixteenKb98_Args_Type);
                     --  Processing request
                     PolyORB.Requests.Arguments
                       (PolyORB.Requests.Request_Access
                          (Request),
                        Request.Payload,
                        Error);
                     if PolyORB.Errors.Found
                       (Error)
                     then
                        PolyORB.CORBA_P.Exceptions.Raise_From_Error
                          (Error);
                     end if;
                     --  Setting Parameters
                     data :=
                       test_all_optims.CDR.echoSixteenKb98_Args_Type
                          (Request.Payload.Args.all).data;
                     --  Call Implementation
                     Result_Ü :=
                       test_all_optims.Impl.echoSixteenKb98
                          (test_all_optims.Impl.Object'Class
                             (Self.all)'Access,
                           data);
                     --  Setting the result
                     test_all_optims.CDR.echoSixteenKb98_Args_Type
                       (Request.Payload.Args.all).Returns :=
                       Result_Ü;
                  end;
               when 99 =>
                  declare
                     data : test_all_optims.sixteenKb;
                     Error : PolyORB.Errors.Error_Container;
                     Result_Ü : test_all_optims.sixteenKb;
                  begin
                     --  Setting the request Payload
                     test_all_optims.CDR.echoSixteenKb99_Set_Args
                       (Request,
                        new test_all_optims.CDR.echoSixteenKb99_Args_Type);
                     --  Processing request
                     PolyORB.Requests.Arguments
                       (PolyORB.Requests.Request_Access
                          (Request),
                        Request.Payload,
                        Error);
                     if PolyORB.Errors.Found
                       (Error)
                     then
                        PolyORB.CORBA_P.Exceptions.Raise_From_Error
                          (Error);
                     end if;
                     --  Setting Parameters
                     data :=
                       test_all_optims.CDR.echoSixteenKb99_Args_Type
                          (Request.Payload.Args.all).data;
                     --  Call Implementation
                     Result_Ü :=
                       test_all_optims.Impl.echoSixteenKb99
                          (test_all_optims.Impl.Object'Class
                             (Self.all)'Access,
                           data);
                     --  Setting the result
                     test_all_optims.CDR.echoSixteenKb99_Args_Type
                       (Request.Payload.Args.all).Returns :=
                       Result_Ü;
                  end;
               when 101 =>
                  begin
                     PolyORB.CORBA_P.Implicit_CORBA_Methods.Handle_Interface
                       (Request,
                        Argument_List_Ü,
                        Repository_Id);
                  end;
               when 102 =>
                  begin
                     PolyORB.CORBA_P.Implicit_CORBA_Methods.Handle_Domain_Managers
                       (Self,
                        Request,
                        Argument_List_Ü);
                  end;
               when 103 =>
                  begin
                     PolyORB.CORBA_P.Implicit_CORBA_Methods.Handle_Non_Existent
                       (Request,
                        Argument_List_Ü);
                  end;
               when 104 =>
                  begin
                     PolyORB.CORBA_P.Implicit_CORBA_Methods.Handle_Non_Existent
                       (Request,
                        Argument_List_Ü);
                  end;
               when 100 =>
                  begin
                     PolyORB.CORBA_P.Implicit_CORBA_Methods.Handle_Is_A
                       (Request,
                        Argument_List_Ü,
                        test_all_optims.Is_A'Access);
                  end;
               when others =>
                  raise Program_Error;

            end case;
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
      Register_Procedure
        ("echoSixteenKb00");
      Register_Procedure
        ("echoSixteenKb01");
      Register_Procedure
        ("echoSixteenKb02");
      Register_Procedure
        ("echoSixteenKb03");
      Register_Procedure
        ("echoSixteenKb04");
      Register_Procedure
        ("echoSixteenKb05");
      Register_Procedure
        ("echoSixteenKb06");
      Register_Procedure
        ("echoSixteenKb07");
      Register_Procedure
        ("echoSixteenKb08");
      Register_Procedure
        ("echoSixteenKb09");
      Register_Procedure
        ("echoSixteenKb10");
      Register_Procedure
        ("echoSixteenKb11");
      Register_Procedure
        ("echoSixteenKb12");
      Register_Procedure
        ("echoSixteenKb13");
      Register_Procedure
        ("echoSixteenKb14");
      Register_Procedure
        ("echoSixteenKb15");
      Register_Procedure
        ("echoSixteenKb16");
      Register_Procedure
        ("echoSixteenKb17");
      Register_Procedure
        ("echoSixteenKb18");
      Register_Procedure
        ("echoSixteenKb19");
      Register_Procedure
        ("echoSixteenKb20");
      Register_Procedure
        ("echoSixteenKb21");
      Register_Procedure
        ("echoSixteenKb22");
      Register_Procedure
        ("echoSixteenKb23");
      Register_Procedure
        ("echoSixteenKb24");
      Register_Procedure
        ("echoSixteenKb25");
      Register_Procedure
        ("echoSixteenKb26");
      Register_Procedure
        ("echoSixteenKb27");
      Register_Procedure
        ("echoSixteenKb28");
      Register_Procedure
        ("echoSixteenKb29");
      Register_Procedure
        ("echoSixteenKb30");
      Register_Procedure
        ("echoSixteenKb31");
      Register_Procedure
        ("echoSixteenKb32");
      Register_Procedure
        ("echoSixteenKb33");
      Register_Procedure
        ("echoSixteenKb34");
      Register_Procedure
        ("echoSixteenKb35");
      Register_Procedure
        ("echoSixteenKb36");
      Register_Procedure
        ("echoSixteenKb37");
      Register_Procedure
        ("echoSixteenKb38");
      Register_Procedure
        ("echoSixteenKb39");
      Register_Procedure
        ("echoSixteenKb40");
      Register_Procedure
        ("echoSixteenKb41");
      Register_Procedure
        ("echoSixteenKb42");
      Register_Procedure
        ("echoSixteenKb43");
      Register_Procedure
        ("echoSixteenKb44");
      Register_Procedure
        ("echoSixteenKb45");
      Register_Procedure
        ("echoSixteenKb46");
      Register_Procedure
        ("echoSixteenKb47");
      Register_Procedure
        ("echoSixteenKb48");
      Register_Procedure
        ("echoSixteenKb49");
      Register_Procedure
        ("echoSixteenKb50");
      Register_Procedure
        ("echoSixteenKb51");
      Register_Procedure
        ("echoSixteenKb52");
      Register_Procedure
        ("echoSixteenKb53");
      Register_Procedure
        ("echoSixteenKb54");
      Register_Procedure
        ("echoSixteenKb55");
      Register_Procedure
        ("echoSixteenKb56");
      Register_Procedure
        ("echoSixteenKb57");
      Register_Procedure
        ("echoSixteenKb58");
      Register_Procedure
        ("echoSixteenKb59");
      Register_Procedure
        ("echoSixteenKb60");
      Register_Procedure
        ("echoSixteenKb61");
      Register_Procedure
        ("echoSixteenKb62");
      Register_Procedure
        ("echoSixteenKb63");
      Register_Procedure
        ("echoSixteenKb64");
      Register_Procedure
        ("echoSixteenKb65");
      Register_Procedure
        ("echoSixteenKb66");
      Register_Procedure
        ("echoSixteenKb67");
      Register_Procedure
        ("echoSixteenKb68");
      Register_Procedure
        ("echoSixteenKb69");
      Register_Procedure
        ("echoSixteenKb70");
      Register_Procedure
        ("echoSixteenKb71");
      Register_Procedure
        ("echoSixteenKb72");
      Register_Procedure
        ("echoSixteenKb73");
      Register_Procedure
        ("echoSixteenKb74");
      Register_Procedure
        ("echoSixteenKb75");
      Register_Procedure
        ("echoSixteenKb76");
      Register_Procedure
        ("echoSixteenKb77");
      Register_Procedure
        ("echoSixteenKb78");
      Register_Procedure
        ("echoSixteenKb79");
      Register_Procedure
        ("echoSixteenKb80");
      Register_Procedure
        ("echoSixteenKb81");
      Register_Procedure
        ("echoSixteenKb82");
      Register_Procedure
        ("echoSixteenKb83");
      Register_Procedure
        ("echoSixteenKb84");
      Register_Procedure
        ("echoSixteenKb85");
      Register_Procedure
        ("echoSixteenKb86");
      Register_Procedure
        ("echoSixteenKb87");
      Register_Procedure
        ("echoSixteenKb88");
      Register_Procedure
        ("echoSixteenKb89");
      Register_Procedure
        ("echoSixteenKb90");
      Register_Procedure
        ("echoSixteenKb91");
      Register_Procedure
        ("echoSixteenKb92");
      Register_Procedure
        ("echoSixteenKb93");
      Register_Procedure
        ("echoSixteenKb94");
      Register_Procedure
        ("echoSixteenKb95");
      Register_Procedure
        ("echoSixteenKb96");
      Register_Procedure
        ("echoSixteenKb97");
      Register_Procedure
        ("echoSixteenKb98");
      Register_Procedure
        ("echoSixteenKb99");
      Register_Procedure
        ("_is_a");
      Register_Procedure
        ("_interface");
      Register_Procedure
        ("_domain_managers");
      Register_Procedure
        ("_non_existent");
      Register_Procedure
        ("_not_existent");

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

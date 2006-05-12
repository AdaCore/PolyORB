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
with test_hash.Impl;
with PolyORB.Errors;
with test_hash.CDR;
with PolyORB.Requests;
with PolyORB.CORBA_P.Exceptions;
with PolyORB.CORBA_P.Implicit_CORBA_Methods;
with test_hash_Hash;
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

package body test_hash.Skel is

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
        test_hash_Hash.Hash
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
        test_hash_Hash.Hash
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
                     data : CORBA.Long;
                     Error : PolyORB.Errors.Error_Container;
                     Result_Ü : CORBA.Long;
                  begin
                     --  Setting the request Payload
                     test_hash.CDR.echoLong00_Set_Args
                       (Request,
                        new test_hash.CDR.echoLong00_Args_Type);
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
                       test_hash.CDR.echoLong00_Args_Type
                          (Request.Payload.Args.all).data;
                     --  Call Implementation
                     Result_Ü :=
                       test_hash.Impl.echoLong00
                          (test_hash.Impl.Object'Class
                             (Self.all)'Access,
                           data);
                     --  Setting the result
                     test_hash.CDR.echoLong00_Args_Type
                       (Request.Payload.Args.all).Returns :=
                       Result_Ü;
                  end;
               when 1 =>
                  declare
                     data : CORBA.Long;
                     Error : PolyORB.Errors.Error_Container;
                     Result_Ü : CORBA.Long;
                  begin
                     --  Setting the request Payload
                     test_hash.CDR.echoLong01_Set_Args
                       (Request,
                        new test_hash.CDR.echoLong01_Args_Type);
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
                       test_hash.CDR.echoLong01_Args_Type
                          (Request.Payload.Args.all).data;
                     --  Call Implementation
                     Result_Ü :=
                       test_hash.Impl.echoLong01
                          (test_hash.Impl.Object'Class
                             (Self.all)'Access,
                           data);
                     --  Setting the result
                     test_hash.CDR.echoLong01_Args_Type
                       (Request.Payload.Args.all).Returns :=
                       Result_Ü;
                  end;
               when 2 =>
                  declare
                     data : CORBA.Long;
                     Error : PolyORB.Errors.Error_Container;
                     Result_Ü : CORBA.Long;
                  begin
                     --  Setting the request Payload
                     test_hash.CDR.echoLong02_Set_Args
                       (Request,
                        new test_hash.CDR.echoLong02_Args_Type);
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
                       test_hash.CDR.echoLong02_Args_Type
                          (Request.Payload.Args.all).data;
                     --  Call Implementation
                     Result_Ü :=
                       test_hash.Impl.echoLong02
                          (test_hash.Impl.Object'Class
                             (Self.all)'Access,
                           data);
                     --  Setting the result
                     test_hash.CDR.echoLong02_Args_Type
                       (Request.Payload.Args.all).Returns :=
                       Result_Ü;
                  end;
               when 3 =>
                  declare
                     data : CORBA.Long;
                     Error : PolyORB.Errors.Error_Container;
                     Result_Ü : CORBA.Long;
                  begin
                     --  Setting the request Payload
                     test_hash.CDR.echoLong03_Set_Args
                       (Request,
                        new test_hash.CDR.echoLong03_Args_Type);
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
                       test_hash.CDR.echoLong03_Args_Type
                          (Request.Payload.Args.all).data;
                     --  Call Implementation
                     Result_Ü :=
                       test_hash.Impl.echoLong03
                          (test_hash.Impl.Object'Class
                             (Self.all)'Access,
                           data);
                     --  Setting the result
                     test_hash.CDR.echoLong03_Args_Type
                       (Request.Payload.Args.all).Returns :=
                       Result_Ü;
                  end;
               when 4 =>
                  declare
                     data : CORBA.Long;
                     Error : PolyORB.Errors.Error_Container;
                     Result_Ü : CORBA.Long;
                  begin
                     --  Setting the request Payload
                     test_hash.CDR.echoLong04_Set_Args
                       (Request,
                        new test_hash.CDR.echoLong04_Args_Type);
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
                       test_hash.CDR.echoLong04_Args_Type
                          (Request.Payload.Args.all).data;
                     --  Call Implementation
                     Result_Ü :=
                       test_hash.Impl.echoLong04
                          (test_hash.Impl.Object'Class
                             (Self.all)'Access,
                           data);
                     --  Setting the result
                     test_hash.CDR.echoLong04_Args_Type
                       (Request.Payload.Args.all).Returns :=
                       Result_Ü;
                  end;
               when 5 =>
                  declare
                     data : CORBA.Long;
                     Error : PolyORB.Errors.Error_Container;
                     Result_Ü : CORBA.Long;
                  begin
                     --  Setting the request Payload
                     test_hash.CDR.echoLong05_Set_Args
                       (Request,
                        new test_hash.CDR.echoLong05_Args_Type);
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
                       test_hash.CDR.echoLong05_Args_Type
                          (Request.Payload.Args.all).data;
                     --  Call Implementation
                     Result_Ü :=
                       test_hash.Impl.echoLong05
                          (test_hash.Impl.Object'Class
                             (Self.all)'Access,
                           data);
                     --  Setting the result
                     test_hash.CDR.echoLong05_Args_Type
                       (Request.Payload.Args.all).Returns :=
                       Result_Ü;
                  end;
               when 6 =>
                  declare
                     data : CORBA.Long;
                     Error : PolyORB.Errors.Error_Container;
                     Result_Ü : CORBA.Long;
                  begin
                     --  Setting the request Payload
                     test_hash.CDR.echoLong06_Set_Args
                       (Request,
                        new test_hash.CDR.echoLong06_Args_Type);
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
                       test_hash.CDR.echoLong06_Args_Type
                          (Request.Payload.Args.all).data;
                     --  Call Implementation
                     Result_Ü :=
                       test_hash.Impl.echoLong06
                          (test_hash.Impl.Object'Class
                             (Self.all)'Access,
                           data);
                     --  Setting the result
                     test_hash.CDR.echoLong06_Args_Type
                       (Request.Payload.Args.all).Returns :=
                       Result_Ü;
                  end;
               when 7 =>
                  declare
                     data : CORBA.Long;
                     Error : PolyORB.Errors.Error_Container;
                     Result_Ü : CORBA.Long;
                  begin
                     --  Setting the request Payload
                     test_hash.CDR.echoLong07_Set_Args
                       (Request,
                        new test_hash.CDR.echoLong07_Args_Type);
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
                       test_hash.CDR.echoLong07_Args_Type
                          (Request.Payload.Args.all).data;
                     --  Call Implementation
                     Result_Ü :=
                       test_hash.Impl.echoLong07
                          (test_hash.Impl.Object'Class
                             (Self.all)'Access,
                           data);
                     --  Setting the result
                     test_hash.CDR.echoLong07_Args_Type
                       (Request.Payload.Args.all).Returns :=
                       Result_Ü;
                  end;
               when 8 =>
                  declare
                     data : CORBA.Long;
                     Error : PolyORB.Errors.Error_Container;
                     Result_Ü : CORBA.Long;
                  begin
                     --  Setting the request Payload
                     test_hash.CDR.echoLong08_Set_Args
                       (Request,
                        new test_hash.CDR.echoLong08_Args_Type);
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
                       test_hash.CDR.echoLong08_Args_Type
                          (Request.Payload.Args.all).data;
                     --  Call Implementation
                     Result_Ü :=
                       test_hash.Impl.echoLong08
                          (test_hash.Impl.Object'Class
                             (Self.all)'Access,
                           data);
                     --  Setting the result
                     test_hash.CDR.echoLong08_Args_Type
                       (Request.Payload.Args.all).Returns :=
                       Result_Ü;
                  end;
               when 9 =>
                  declare
                     data : CORBA.Long;
                     Error : PolyORB.Errors.Error_Container;
                     Result_Ü : CORBA.Long;
                  begin
                     --  Setting the request Payload
                     test_hash.CDR.echoLong09_Set_Args
                       (Request,
                        new test_hash.CDR.echoLong09_Args_Type);
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
                       test_hash.CDR.echoLong09_Args_Type
                          (Request.Payload.Args.all).data;
                     --  Call Implementation
                     Result_Ü :=
                       test_hash.Impl.echoLong09
                          (test_hash.Impl.Object'Class
                             (Self.all)'Access,
                           data);
                     --  Setting the result
                     test_hash.CDR.echoLong09_Args_Type
                       (Request.Payload.Args.all).Returns :=
                       Result_Ü;
                  end;
               when 10 =>
                  declare
                     data : CORBA.Long;
                     Error : PolyORB.Errors.Error_Container;
                     Result_Ü : CORBA.Long;
                  begin
                     --  Setting the request Payload
                     test_hash.CDR.echoLong10_Set_Args
                       (Request,
                        new test_hash.CDR.echoLong10_Args_Type);
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
                       test_hash.CDR.echoLong10_Args_Type
                          (Request.Payload.Args.all).data;
                     --  Call Implementation
                     Result_Ü :=
                       test_hash.Impl.echoLong10
                          (test_hash.Impl.Object'Class
                             (Self.all)'Access,
                           data);
                     --  Setting the result
                     test_hash.CDR.echoLong10_Args_Type
                       (Request.Payload.Args.all).Returns :=
                       Result_Ü;
                  end;
               when 11 =>
                  declare
                     data : CORBA.Long;
                     Error : PolyORB.Errors.Error_Container;
                     Result_Ü : CORBA.Long;
                  begin
                     --  Setting the request Payload
                     test_hash.CDR.echoLong11_Set_Args
                       (Request,
                        new test_hash.CDR.echoLong11_Args_Type);
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
                       test_hash.CDR.echoLong11_Args_Type
                          (Request.Payload.Args.all).data;
                     --  Call Implementation
                     Result_Ü :=
                       test_hash.Impl.echoLong11
                          (test_hash.Impl.Object'Class
                             (Self.all)'Access,
                           data);
                     --  Setting the result
                     test_hash.CDR.echoLong11_Args_Type
                       (Request.Payload.Args.all).Returns :=
                       Result_Ü;
                  end;
               when 12 =>
                  declare
                     data : CORBA.Long;
                     Error : PolyORB.Errors.Error_Container;
                     Result_Ü : CORBA.Long;
                  begin
                     --  Setting the request Payload
                     test_hash.CDR.echoLong12_Set_Args
                       (Request,
                        new test_hash.CDR.echoLong12_Args_Type);
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
                       test_hash.CDR.echoLong12_Args_Type
                          (Request.Payload.Args.all).data;
                     --  Call Implementation
                     Result_Ü :=
                       test_hash.Impl.echoLong12
                          (test_hash.Impl.Object'Class
                             (Self.all)'Access,
                           data);
                     --  Setting the result
                     test_hash.CDR.echoLong12_Args_Type
                       (Request.Payload.Args.all).Returns :=
                       Result_Ü;
                  end;
               when 13 =>
                  declare
                     data : CORBA.Long;
                     Error : PolyORB.Errors.Error_Container;
                     Result_Ü : CORBA.Long;
                  begin
                     --  Setting the request Payload
                     test_hash.CDR.echoLong13_Set_Args
                       (Request,
                        new test_hash.CDR.echoLong13_Args_Type);
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
                       test_hash.CDR.echoLong13_Args_Type
                          (Request.Payload.Args.all).data;
                     --  Call Implementation
                     Result_Ü :=
                       test_hash.Impl.echoLong13
                          (test_hash.Impl.Object'Class
                             (Self.all)'Access,
                           data);
                     --  Setting the result
                     test_hash.CDR.echoLong13_Args_Type
                       (Request.Payload.Args.all).Returns :=
                       Result_Ü;
                  end;
               when 14 =>
                  declare
                     data : CORBA.Long;
                     Error : PolyORB.Errors.Error_Container;
                     Result_Ü : CORBA.Long;
                  begin
                     --  Setting the request Payload
                     test_hash.CDR.echoLong14_Set_Args
                       (Request,
                        new test_hash.CDR.echoLong14_Args_Type);
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
                       test_hash.CDR.echoLong14_Args_Type
                          (Request.Payload.Args.all).data;
                     --  Call Implementation
                     Result_Ü :=
                       test_hash.Impl.echoLong14
                          (test_hash.Impl.Object'Class
                             (Self.all)'Access,
                           data);
                     --  Setting the result
                     test_hash.CDR.echoLong14_Args_Type
                       (Request.Payload.Args.all).Returns :=
                       Result_Ü;
                  end;
               when 15 =>
                  declare
                     data : CORBA.Long;
                     Error : PolyORB.Errors.Error_Container;
                     Result_Ü : CORBA.Long;
                  begin
                     --  Setting the request Payload
                     test_hash.CDR.echoLong15_Set_Args
                       (Request,
                        new test_hash.CDR.echoLong15_Args_Type);
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
                       test_hash.CDR.echoLong15_Args_Type
                          (Request.Payload.Args.all).data;
                     --  Call Implementation
                     Result_Ü :=
                       test_hash.Impl.echoLong15
                          (test_hash.Impl.Object'Class
                             (Self.all)'Access,
                           data);
                     --  Setting the result
                     test_hash.CDR.echoLong15_Args_Type
                       (Request.Payload.Args.all).Returns :=
                       Result_Ü;
                  end;
               when 16 =>
                  declare
                     data : CORBA.Long;
                     Error : PolyORB.Errors.Error_Container;
                     Result_Ü : CORBA.Long;
                  begin
                     --  Setting the request Payload
                     test_hash.CDR.echoLong16_Set_Args
                       (Request,
                        new test_hash.CDR.echoLong16_Args_Type);
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
                       test_hash.CDR.echoLong16_Args_Type
                          (Request.Payload.Args.all).data;
                     --  Call Implementation
                     Result_Ü :=
                       test_hash.Impl.echoLong16
                          (test_hash.Impl.Object'Class
                             (Self.all)'Access,
                           data);
                     --  Setting the result
                     test_hash.CDR.echoLong16_Args_Type
                       (Request.Payload.Args.all).Returns :=
                       Result_Ü;
                  end;
               when 17 =>
                  declare
                     data : CORBA.Long;
                     Error : PolyORB.Errors.Error_Container;
                     Result_Ü : CORBA.Long;
                  begin
                     --  Setting the request Payload
                     test_hash.CDR.echoLong17_Set_Args
                       (Request,
                        new test_hash.CDR.echoLong17_Args_Type);
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
                       test_hash.CDR.echoLong17_Args_Type
                          (Request.Payload.Args.all).data;
                     --  Call Implementation
                     Result_Ü :=
                       test_hash.Impl.echoLong17
                          (test_hash.Impl.Object'Class
                             (Self.all)'Access,
                           data);
                     --  Setting the result
                     test_hash.CDR.echoLong17_Args_Type
                       (Request.Payload.Args.all).Returns :=
                       Result_Ü;
                  end;
               when 18 =>
                  declare
                     data : CORBA.Long;
                     Error : PolyORB.Errors.Error_Container;
                     Result_Ü : CORBA.Long;
                  begin
                     --  Setting the request Payload
                     test_hash.CDR.echoLong18_Set_Args
                       (Request,
                        new test_hash.CDR.echoLong18_Args_Type);
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
                       test_hash.CDR.echoLong18_Args_Type
                          (Request.Payload.Args.all).data;
                     --  Call Implementation
                     Result_Ü :=
                       test_hash.Impl.echoLong18
                          (test_hash.Impl.Object'Class
                             (Self.all)'Access,
                           data);
                     --  Setting the result
                     test_hash.CDR.echoLong18_Args_Type
                       (Request.Payload.Args.all).Returns :=
                       Result_Ü;
                  end;
               when 19 =>
                  declare
                     data : CORBA.Long;
                     Error : PolyORB.Errors.Error_Container;
                     Result_Ü : CORBA.Long;
                  begin
                     --  Setting the request Payload
                     test_hash.CDR.echoLong19_Set_Args
                       (Request,
                        new test_hash.CDR.echoLong19_Args_Type);
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
                       test_hash.CDR.echoLong19_Args_Type
                          (Request.Payload.Args.all).data;
                     --  Call Implementation
                     Result_Ü :=
                       test_hash.Impl.echoLong19
                          (test_hash.Impl.Object'Class
                             (Self.all)'Access,
                           data);
                     --  Setting the result
                     test_hash.CDR.echoLong19_Args_Type
                       (Request.Payload.Args.all).Returns :=
                       Result_Ü;
                  end;
               when 20 =>
                  declare
                     data : CORBA.Long;
                     Error : PolyORB.Errors.Error_Container;
                     Result_Ü : CORBA.Long;
                  begin
                     --  Setting the request Payload
                     test_hash.CDR.echoLong20_Set_Args
                       (Request,
                        new test_hash.CDR.echoLong20_Args_Type);
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
                       test_hash.CDR.echoLong20_Args_Type
                          (Request.Payload.Args.all).data;
                     --  Call Implementation
                     Result_Ü :=
                       test_hash.Impl.echoLong20
                          (test_hash.Impl.Object'Class
                             (Self.all)'Access,
                           data);
                     --  Setting the result
                     test_hash.CDR.echoLong20_Args_Type
                       (Request.Payload.Args.all).Returns :=
                       Result_Ü;
                  end;
               when 21 =>
                  declare
                     data : CORBA.Long;
                     Error : PolyORB.Errors.Error_Container;
                     Result_Ü : CORBA.Long;
                  begin
                     --  Setting the request Payload
                     test_hash.CDR.echoLong21_Set_Args
                       (Request,
                        new test_hash.CDR.echoLong21_Args_Type);
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
                       test_hash.CDR.echoLong21_Args_Type
                          (Request.Payload.Args.all).data;
                     --  Call Implementation
                     Result_Ü :=
                       test_hash.Impl.echoLong21
                          (test_hash.Impl.Object'Class
                             (Self.all)'Access,
                           data);
                     --  Setting the result
                     test_hash.CDR.echoLong21_Args_Type
                       (Request.Payload.Args.all).Returns :=
                       Result_Ü;
                  end;
               when 22 =>
                  declare
                     data : CORBA.Long;
                     Error : PolyORB.Errors.Error_Container;
                     Result_Ü : CORBA.Long;
                  begin
                     --  Setting the request Payload
                     test_hash.CDR.echoLong22_Set_Args
                       (Request,
                        new test_hash.CDR.echoLong22_Args_Type);
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
                       test_hash.CDR.echoLong22_Args_Type
                          (Request.Payload.Args.all).data;
                     --  Call Implementation
                     Result_Ü :=
                       test_hash.Impl.echoLong22
                          (test_hash.Impl.Object'Class
                             (Self.all)'Access,
                           data);
                     --  Setting the result
                     test_hash.CDR.echoLong22_Args_Type
                       (Request.Payload.Args.all).Returns :=
                       Result_Ü;
                  end;
               when 23 =>
                  declare
                     data : CORBA.Long;
                     Error : PolyORB.Errors.Error_Container;
                     Result_Ü : CORBA.Long;
                  begin
                     --  Setting the request Payload
                     test_hash.CDR.echoLong23_Set_Args
                       (Request,
                        new test_hash.CDR.echoLong23_Args_Type);
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
                       test_hash.CDR.echoLong23_Args_Type
                          (Request.Payload.Args.all).data;
                     --  Call Implementation
                     Result_Ü :=
                       test_hash.Impl.echoLong23
                          (test_hash.Impl.Object'Class
                             (Self.all)'Access,
                           data);
                     --  Setting the result
                     test_hash.CDR.echoLong23_Args_Type
                       (Request.Payload.Args.all).Returns :=
                       Result_Ü;
                  end;
               when 24 =>
                  declare
                     data : CORBA.Long;
                     Error : PolyORB.Errors.Error_Container;
                     Result_Ü : CORBA.Long;
                  begin
                     --  Setting the request Payload
                     test_hash.CDR.echoLong24_Set_Args
                       (Request,
                        new test_hash.CDR.echoLong24_Args_Type);
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
                       test_hash.CDR.echoLong24_Args_Type
                          (Request.Payload.Args.all).data;
                     --  Call Implementation
                     Result_Ü :=
                       test_hash.Impl.echoLong24
                          (test_hash.Impl.Object'Class
                             (Self.all)'Access,
                           data);
                     --  Setting the result
                     test_hash.CDR.echoLong24_Args_Type
                       (Request.Payload.Args.all).Returns :=
                       Result_Ü;
                  end;
               when 25 =>
                  declare
                     data : CORBA.Long;
                     Error : PolyORB.Errors.Error_Container;
                     Result_Ü : CORBA.Long;
                  begin
                     --  Setting the request Payload
                     test_hash.CDR.echoLong25_Set_Args
                       (Request,
                        new test_hash.CDR.echoLong25_Args_Type);
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
                       test_hash.CDR.echoLong25_Args_Type
                          (Request.Payload.Args.all).data;
                     --  Call Implementation
                     Result_Ü :=
                       test_hash.Impl.echoLong25
                          (test_hash.Impl.Object'Class
                             (Self.all)'Access,
                           data);
                     --  Setting the result
                     test_hash.CDR.echoLong25_Args_Type
                       (Request.Payload.Args.all).Returns :=
                       Result_Ü;
                  end;
               when 26 =>
                  declare
                     data : CORBA.Long;
                     Error : PolyORB.Errors.Error_Container;
                     Result_Ü : CORBA.Long;
                  begin
                     --  Setting the request Payload
                     test_hash.CDR.echoLong26_Set_Args
                       (Request,
                        new test_hash.CDR.echoLong26_Args_Type);
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
                       test_hash.CDR.echoLong26_Args_Type
                          (Request.Payload.Args.all).data;
                     --  Call Implementation
                     Result_Ü :=
                       test_hash.Impl.echoLong26
                          (test_hash.Impl.Object'Class
                             (Self.all)'Access,
                           data);
                     --  Setting the result
                     test_hash.CDR.echoLong26_Args_Type
                       (Request.Payload.Args.all).Returns :=
                       Result_Ü;
                  end;
               when 27 =>
                  declare
                     data : CORBA.Long;
                     Error : PolyORB.Errors.Error_Container;
                     Result_Ü : CORBA.Long;
                  begin
                     --  Setting the request Payload
                     test_hash.CDR.echoLong27_Set_Args
                       (Request,
                        new test_hash.CDR.echoLong27_Args_Type);
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
                       test_hash.CDR.echoLong27_Args_Type
                          (Request.Payload.Args.all).data;
                     --  Call Implementation
                     Result_Ü :=
                       test_hash.Impl.echoLong27
                          (test_hash.Impl.Object'Class
                             (Self.all)'Access,
                           data);
                     --  Setting the result
                     test_hash.CDR.echoLong27_Args_Type
                       (Request.Payload.Args.all).Returns :=
                       Result_Ü;
                  end;
               when 28 =>
                  declare
                     data : CORBA.Long;
                     Error : PolyORB.Errors.Error_Container;
                     Result_Ü : CORBA.Long;
                  begin
                     --  Setting the request Payload
                     test_hash.CDR.echoLong28_Set_Args
                       (Request,
                        new test_hash.CDR.echoLong28_Args_Type);
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
                       test_hash.CDR.echoLong28_Args_Type
                          (Request.Payload.Args.all).data;
                     --  Call Implementation
                     Result_Ü :=
                       test_hash.Impl.echoLong28
                          (test_hash.Impl.Object'Class
                             (Self.all)'Access,
                           data);
                     --  Setting the result
                     test_hash.CDR.echoLong28_Args_Type
                       (Request.Payload.Args.all).Returns :=
                       Result_Ü;
                  end;
               when 29 =>
                  declare
                     data : CORBA.Long;
                     Error : PolyORB.Errors.Error_Container;
                     Result_Ü : CORBA.Long;
                  begin
                     --  Setting the request Payload
                     test_hash.CDR.echoLong29_Set_Args
                       (Request,
                        new test_hash.CDR.echoLong29_Args_Type);
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
                       test_hash.CDR.echoLong29_Args_Type
                          (Request.Payload.Args.all).data;
                     --  Call Implementation
                     Result_Ü :=
                       test_hash.Impl.echoLong29
                          (test_hash.Impl.Object'Class
                             (Self.all)'Access,
                           data);
                     --  Setting the result
                     test_hash.CDR.echoLong29_Args_Type
                       (Request.Payload.Args.all).Returns :=
                       Result_Ü;
                  end;
               when 30 =>
                  declare
                     data : CORBA.Long;
                     Error : PolyORB.Errors.Error_Container;
                     Result_Ü : CORBA.Long;
                  begin
                     --  Setting the request Payload
                     test_hash.CDR.echoLong30_Set_Args
                       (Request,
                        new test_hash.CDR.echoLong30_Args_Type);
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
                       test_hash.CDR.echoLong30_Args_Type
                          (Request.Payload.Args.all).data;
                     --  Call Implementation
                     Result_Ü :=
                       test_hash.Impl.echoLong30
                          (test_hash.Impl.Object'Class
                             (Self.all)'Access,
                           data);
                     --  Setting the result
                     test_hash.CDR.echoLong30_Args_Type
                       (Request.Payload.Args.all).Returns :=
                       Result_Ü;
                  end;
               when 31 =>
                  declare
                     data : CORBA.Long;
                     Error : PolyORB.Errors.Error_Container;
                     Result_Ü : CORBA.Long;
                  begin
                     --  Setting the request Payload
                     test_hash.CDR.echoLong31_Set_Args
                       (Request,
                        new test_hash.CDR.echoLong31_Args_Type);
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
                       test_hash.CDR.echoLong31_Args_Type
                          (Request.Payload.Args.all).data;
                     --  Call Implementation
                     Result_Ü :=
                       test_hash.Impl.echoLong31
                          (test_hash.Impl.Object'Class
                             (Self.all)'Access,
                           data);
                     --  Setting the result
                     test_hash.CDR.echoLong31_Args_Type
                       (Request.Payload.Args.all).Returns :=
                       Result_Ü;
                  end;
               when 32 =>
                  declare
                     data : CORBA.Long;
                     Error : PolyORB.Errors.Error_Container;
                     Result_Ü : CORBA.Long;
                  begin
                     --  Setting the request Payload
                     test_hash.CDR.echoLong32_Set_Args
                       (Request,
                        new test_hash.CDR.echoLong32_Args_Type);
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
                       test_hash.CDR.echoLong32_Args_Type
                          (Request.Payload.Args.all).data;
                     --  Call Implementation
                     Result_Ü :=
                       test_hash.Impl.echoLong32
                          (test_hash.Impl.Object'Class
                             (Self.all)'Access,
                           data);
                     --  Setting the result
                     test_hash.CDR.echoLong32_Args_Type
                       (Request.Payload.Args.all).Returns :=
                       Result_Ü;
                  end;
               when 33 =>
                  declare
                     data : CORBA.Long;
                     Error : PolyORB.Errors.Error_Container;
                     Result_Ü : CORBA.Long;
                  begin
                     --  Setting the request Payload
                     test_hash.CDR.echoLong33_Set_Args
                       (Request,
                        new test_hash.CDR.echoLong33_Args_Type);
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
                       test_hash.CDR.echoLong33_Args_Type
                          (Request.Payload.Args.all).data;
                     --  Call Implementation
                     Result_Ü :=
                       test_hash.Impl.echoLong33
                          (test_hash.Impl.Object'Class
                             (Self.all)'Access,
                           data);
                     --  Setting the result
                     test_hash.CDR.echoLong33_Args_Type
                       (Request.Payload.Args.all).Returns :=
                       Result_Ü;
                  end;
               when 34 =>
                  declare
                     data : CORBA.Long;
                     Error : PolyORB.Errors.Error_Container;
                     Result_Ü : CORBA.Long;
                  begin
                     --  Setting the request Payload
                     test_hash.CDR.echoLong34_Set_Args
                       (Request,
                        new test_hash.CDR.echoLong34_Args_Type);
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
                       test_hash.CDR.echoLong34_Args_Type
                          (Request.Payload.Args.all).data;
                     --  Call Implementation
                     Result_Ü :=
                       test_hash.Impl.echoLong34
                          (test_hash.Impl.Object'Class
                             (Self.all)'Access,
                           data);
                     --  Setting the result
                     test_hash.CDR.echoLong34_Args_Type
                       (Request.Payload.Args.all).Returns :=
                       Result_Ü;
                  end;
               when 35 =>
                  declare
                     data : CORBA.Long;
                     Error : PolyORB.Errors.Error_Container;
                     Result_Ü : CORBA.Long;
                  begin
                     --  Setting the request Payload
                     test_hash.CDR.echoLong35_Set_Args
                       (Request,
                        new test_hash.CDR.echoLong35_Args_Type);
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
                       test_hash.CDR.echoLong35_Args_Type
                          (Request.Payload.Args.all).data;
                     --  Call Implementation
                     Result_Ü :=
                       test_hash.Impl.echoLong35
                          (test_hash.Impl.Object'Class
                             (Self.all)'Access,
                           data);
                     --  Setting the result
                     test_hash.CDR.echoLong35_Args_Type
                       (Request.Payload.Args.all).Returns :=
                       Result_Ü;
                  end;
               when 36 =>
                  declare
                     data : CORBA.Long;
                     Error : PolyORB.Errors.Error_Container;
                     Result_Ü : CORBA.Long;
                  begin
                     --  Setting the request Payload
                     test_hash.CDR.echoLong36_Set_Args
                       (Request,
                        new test_hash.CDR.echoLong36_Args_Type);
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
                       test_hash.CDR.echoLong36_Args_Type
                          (Request.Payload.Args.all).data;
                     --  Call Implementation
                     Result_Ü :=
                       test_hash.Impl.echoLong36
                          (test_hash.Impl.Object'Class
                             (Self.all)'Access,
                           data);
                     --  Setting the result
                     test_hash.CDR.echoLong36_Args_Type
                       (Request.Payload.Args.all).Returns :=
                       Result_Ü;
                  end;
               when 37 =>
                  declare
                     data : CORBA.Long;
                     Error : PolyORB.Errors.Error_Container;
                     Result_Ü : CORBA.Long;
                  begin
                     --  Setting the request Payload
                     test_hash.CDR.echoLong37_Set_Args
                       (Request,
                        new test_hash.CDR.echoLong37_Args_Type);
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
                       test_hash.CDR.echoLong37_Args_Type
                          (Request.Payload.Args.all).data;
                     --  Call Implementation
                     Result_Ü :=
                       test_hash.Impl.echoLong37
                          (test_hash.Impl.Object'Class
                             (Self.all)'Access,
                           data);
                     --  Setting the result
                     test_hash.CDR.echoLong37_Args_Type
                       (Request.Payload.Args.all).Returns :=
                       Result_Ü;
                  end;
               when 38 =>
                  declare
                     data : CORBA.Long;
                     Error : PolyORB.Errors.Error_Container;
                     Result_Ü : CORBA.Long;
                  begin
                     --  Setting the request Payload
                     test_hash.CDR.echoLong38_Set_Args
                       (Request,
                        new test_hash.CDR.echoLong38_Args_Type);
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
                       test_hash.CDR.echoLong38_Args_Type
                          (Request.Payload.Args.all).data;
                     --  Call Implementation
                     Result_Ü :=
                       test_hash.Impl.echoLong38
                          (test_hash.Impl.Object'Class
                             (Self.all)'Access,
                           data);
                     --  Setting the result
                     test_hash.CDR.echoLong38_Args_Type
                       (Request.Payload.Args.all).Returns :=
                       Result_Ü;
                  end;
               when 39 =>
                  declare
                     data : CORBA.Long;
                     Error : PolyORB.Errors.Error_Container;
                     Result_Ü : CORBA.Long;
                  begin
                     --  Setting the request Payload
                     test_hash.CDR.echoLong39_Set_Args
                       (Request,
                        new test_hash.CDR.echoLong39_Args_Type);
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
                       test_hash.CDR.echoLong39_Args_Type
                          (Request.Payload.Args.all).data;
                     --  Call Implementation
                     Result_Ü :=
                       test_hash.Impl.echoLong39
                          (test_hash.Impl.Object'Class
                             (Self.all)'Access,
                           data);
                     --  Setting the result
                     test_hash.CDR.echoLong39_Args_Type
                       (Request.Payload.Args.all).Returns :=
                       Result_Ü;
                  end;
               when 40 =>
                  declare
                     data : CORBA.Long;
                     Error : PolyORB.Errors.Error_Container;
                     Result_Ü : CORBA.Long;
                  begin
                     --  Setting the request Payload
                     test_hash.CDR.echoLong40_Set_Args
                       (Request,
                        new test_hash.CDR.echoLong40_Args_Type);
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
                       test_hash.CDR.echoLong40_Args_Type
                          (Request.Payload.Args.all).data;
                     --  Call Implementation
                     Result_Ü :=
                       test_hash.Impl.echoLong40
                          (test_hash.Impl.Object'Class
                             (Self.all)'Access,
                           data);
                     --  Setting the result
                     test_hash.CDR.echoLong40_Args_Type
                       (Request.Payload.Args.all).Returns :=
                       Result_Ü;
                  end;
               when 41 =>
                  declare
                     data : CORBA.Long;
                     Error : PolyORB.Errors.Error_Container;
                     Result_Ü : CORBA.Long;
                  begin
                     --  Setting the request Payload
                     test_hash.CDR.echoLong41_Set_Args
                       (Request,
                        new test_hash.CDR.echoLong41_Args_Type);
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
                       test_hash.CDR.echoLong41_Args_Type
                          (Request.Payload.Args.all).data;
                     --  Call Implementation
                     Result_Ü :=
                       test_hash.Impl.echoLong41
                          (test_hash.Impl.Object'Class
                             (Self.all)'Access,
                           data);
                     --  Setting the result
                     test_hash.CDR.echoLong41_Args_Type
                       (Request.Payload.Args.all).Returns :=
                       Result_Ü;
                  end;
               when 42 =>
                  declare
                     data : CORBA.Long;
                     Error : PolyORB.Errors.Error_Container;
                     Result_Ü : CORBA.Long;
                  begin
                     --  Setting the request Payload
                     test_hash.CDR.echoLong42_Set_Args
                       (Request,
                        new test_hash.CDR.echoLong42_Args_Type);
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
                       test_hash.CDR.echoLong42_Args_Type
                          (Request.Payload.Args.all).data;
                     --  Call Implementation
                     Result_Ü :=
                       test_hash.Impl.echoLong42
                          (test_hash.Impl.Object'Class
                             (Self.all)'Access,
                           data);
                     --  Setting the result
                     test_hash.CDR.echoLong42_Args_Type
                       (Request.Payload.Args.all).Returns :=
                       Result_Ü;
                  end;
               when 43 =>
                  declare
                     data : CORBA.Long;
                     Error : PolyORB.Errors.Error_Container;
                     Result_Ü : CORBA.Long;
                  begin
                     --  Setting the request Payload
                     test_hash.CDR.echoLong43_Set_Args
                       (Request,
                        new test_hash.CDR.echoLong43_Args_Type);
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
                       test_hash.CDR.echoLong43_Args_Type
                          (Request.Payload.Args.all).data;
                     --  Call Implementation
                     Result_Ü :=
                       test_hash.Impl.echoLong43
                          (test_hash.Impl.Object'Class
                             (Self.all)'Access,
                           data);
                     --  Setting the result
                     test_hash.CDR.echoLong43_Args_Type
                       (Request.Payload.Args.all).Returns :=
                       Result_Ü;
                  end;
               when 44 =>
                  declare
                     data : CORBA.Long;
                     Error : PolyORB.Errors.Error_Container;
                     Result_Ü : CORBA.Long;
                  begin
                     --  Setting the request Payload
                     test_hash.CDR.echoLong44_Set_Args
                       (Request,
                        new test_hash.CDR.echoLong44_Args_Type);
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
                       test_hash.CDR.echoLong44_Args_Type
                          (Request.Payload.Args.all).data;
                     --  Call Implementation
                     Result_Ü :=
                       test_hash.Impl.echoLong44
                          (test_hash.Impl.Object'Class
                             (Self.all)'Access,
                           data);
                     --  Setting the result
                     test_hash.CDR.echoLong44_Args_Type
                       (Request.Payload.Args.all).Returns :=
                       Result_Ü;
                  end;
               when 45 =>
                  declare
                     data : CORBA.Long;
                     Error : PolyORB.Errors.Error_Container;
                     Result_Ü : CORBA.Long;
                  begin
                     --  Setting the request Payload
                     test_hash.CDR.echoLong45_Set_Args
                       (Request,
                        new test_hash.CDR.echoLong45_Args_Type);
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
                       test_hash.CDR.echoLong45_Args_Type
                          (Request.Payload.Args.all).data;
                     --  Call Implementation
                     Result_Ü :=
                       test_hash.Impl.echoLong45
                          (test_hash.Impl.Object'Class
                             (Self.all)'Access,
                           data);
                     --  Setting the result
                     test_hash.CDR.echoLong45_Args_Type
                       (Request.Payload.Args.all).Returns :=
                       Result_Ü;
                  end;
               when 46 =>
                  declare
                     data : CORBA.Long;
                     Error : PolyORB.Errors.Error_Container;
                     Result_Ü : CORBA.Long;
                  begin
                     --  Setting the request Payload
                     test_hash.CDR.echoLong46_Set_Args
                       (Request,
                        new test_hash.CDR.echoLong46_Args_Type);
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
                       test_hash.CDR.echoLong46_Args_Type
                          (Request.Payload.Args.all).data;
                     --  Call Implementation
                     Result_Ü :=
                       test_hash.Impl.echoLong46
                          (test_hash.Impl.Object'Class
                             (Self.all)'Access,
                           data);
                     --  Setting the result
                     test_hash.CDR.echoLong46_Args_Type
                       (Request.Payload.Args.all).Returns :=
                       Result_Ü;
                  end;
               when 47 =>
                  declare
                     data : CORBA.Long;
                     Error : PolyORB.Errors.Error_Container;
                     Result_Ü : CORBA.Long;
                  begin
                     --  Setting the request Payload
                     test_hash.CDR.echoLong47_Set_Args
                       (Request,
                        new test_hash.CDR.echoLong47_Args_Type);
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
                       test_hash.CDR.echoLong47_Args_Type
                          (Request.Payload.Args.all).data;
                     --  Call Implementation
                     Result_Ü :=
                       test_hash.Impl.echoLong47
                          (test_hash.Impl.Object'Class
                             (Self.all)'Access,
                           data);
                     --  Setting the result
                     test_hash.CDR.echoLong47_Args_Type
                       (Request.Payload.Args.all).Returns :=
                       Result_Ü;
                  end;
               when 48 =>
                  declare
                     data : CORBA.Long;
                     Error : PolyORB.Errors.Error_Container;
                     Result_Ü : CORBA.Long;
                  begin
                     --  Setting the request Payload
                     test_hash.CDR.echoLong48_Set_Args
                       (Request,
                        new test_hash.CDR.echoLong48_Args_Type);
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
                       test_hash.CDR.echoLong48_Args_Type
                          (Request.Payload.Args.all).data;
                     --  Call Implementation
                     Result_Ü :=
                       test_hash.Impl.echoLong48
                          (test_hash.Impl.Object'Class
                             (Self.all)'Access,
                           data);
                     --  Setting the result
                     test_hash.CDR.echoLong48_Args_Type
                       (Request.Payload.Args.all).Returns :=
                       Result_Ü;
                  end;
               when 49 =>
                  declare
                     data : CORBA.Long;
                     Error : PolyORB.Errors.Error_Container;
                     Result_Ü : CORBA.Long;
                  begin
                     --  Setting the request Payload
                     test_hash.CDR.echoLong49_Set_Args
                       (Request,
                        new test_hash.CDR.echoLong49_Args_Type);
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
                       test_hash.CDR.echoLong49_Args_Type
                          (Request.Payload.Args.all).data;
                     --  Call Implementation
                     Result_Ü :=
                       test_hash.Impl.echoLong49
                          (test_hash.Impl.Object'Class
                             (Self.all)'Access,
                           data);
                     --  Setting the result
                     test_hash.CDR.echoLong49_Args_Type
                       (Request.Payload.Args.all).Returns :=
                       Result_Ü;
                  end;
               when 50 =>
                  declare
                     data : CORBA.Long;
                     Error : PolyORB.Errors.Error_Container;
                     Result_Ü : CORBA.Long;
                  begin
                     --  Setting the request Payload
                     test_hash.CDR.echoLong50_Set_Args
                       (Request,
                        new test_hash.CDR.echoLong50_Args_Type);
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
                       test_hash.CDR.echoLong50_Args_Type
                          (Request.Payload.Args.all).data;
                     --  Call Implementation
                     Result_Ü :=
                       test_hash.Impl.echoLong50
                          (test_hash.Impl.Object'Class
                             (Self.all)'Access,
                           data);
                     --  Setting the result
                     test_hash.CDR.echoLong50_Args_Type
                       (Request.Payload.Args.all).Returns :=
                       Result_Ü;
                  end;
               when 51 =>
                  declare
                     data : CORBA.Long;
                     Error : PolyORB.Errors.Error_Container;
                     Result_Ü : CORBA.Long;
                  begin
                     --  Setting the request Payload
                     test_hash.CDR.echoLong51_Set_Args
                       (Request,
                        new test_hash.CDR.echoLong51_Args_Type);
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
                       test_hash.CDR.echoLong51_Args_Type
                          (Request.Payload.Args.all).data;
                     --  Call Implementation
                     Result_Ü :=
                       test_hash.Impl.echoLong51
                          (test_hash.Impl.Object'Class
                             (Self.all)'Access,
                           data);
                     --  Setting the result
                     test_hash.CDR.echoLong51_Args_Type
                       (Request.Payload.Args.all).Returns :=
                       Result_Ü;
                  end;
               when 52 =>
                  declare
                     data : CORBA.Long;
                     Error : PolyORB.Errors.Error_Container;
                     Result_Ü : CORBA.Long;
                  begin
                     --  Setting the request Payload
                     test_hash.CDR.echoLong52_Set_Args
                       (Request,
                        new test_hash.CDR.echoLong52_Args_Type);
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
                       test_hash.CDR.echoLong52_Args_Type
                          (Request.Payload.Args.all).data;
                     --  Call Implementation
                     Result_Ü :=
                       test_hash.Impl.echoLong52
                          (test_hash.Impl.Object'Class
                             (Self.all)'Access,
                           data);
                     --  Setting the result
                     test_hash.CDR.echoLong52_Args_Type
                       (Request.Payload.Args.all).Returns :=
                       Result_Ü;
                  end;
               when 53 =>
                  declare
                     data : CORBA.Long;
                     Error : PolyORB.Errors.Error_Container;
                     Result_Ü : CORBA.Long;
                  begin
                     --  Setting the request Payload
                     test_hash.CDR.echoLong53_Set_Args
                       (Request,
                        new test_hash.CDR.echoLong53_Args_Type);
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
                       test_hash.CDR.echoLong53_Args_Type
                          (Request.Payload.Args.all).data;
                     --  Call Implementation
                     Result_Ü :=
                       test_hash.Impl.echoLong53
                          (test_hash.Impl.Object'Class
                             (Self.all)'Access,
                           data);
                     --  Setting the result
                     test_hash.CDR.echoLong53_Args_Type
                       (Request.Payload.Args.all).Returns :=
                       Result_Ü;
                  end;
               when 54 =>
                  declare
                     data : CORBA.Long;
                     Error : PolyORB.Errors.Error_Container;
                     Result_Ü : CORBA.Long;
                  begin
                     --  Setting the request Payload
                     test_hash.CDR.echoLong54_Set_Args
                       (Request,
                        new test_hash.CDR.echoLong54_Args_Type);
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
                       test_hash.CDR.echoLong54_Args_Type
                          (Request.Payload.Args.all).data;
                     --  Call Implementation
                     Result_Ü :=
                       test_hash.Impl.echoLong54
                          (test_hash.Impl.Object'Class
                             (Self.all)'Access,
                           data);
                     --  Setting the result
                     test_hash.CDR.echoLong54_Args_Type
                       (Request.Payload.Args.all).Returns :=
                       Result_Ü;
                  end;
               when 55 =>
                  declare
                     data : CORBA.Long;
                     Error : PolyORB.Errors.Error_Container;
                     Result_Ü : CORBA.Long;
                  begin
                     --  Setting the request Payload
                     test_hash.CDR.echoLong55_Set_Args
                       (Request,
                        new test_hash.CDR.echoLong55_Args_Type);
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
                       test_hash.CDR.echoLong55_Args_Type
                          (Request.Payload.Args.all).data;
                     --  Call Implementation
                     Result_Ü :=
                       test_hash.Impl.echoLong55
                          (test_hash.Impl.Object'Class
                             (Self.all)'Access,
                           data);
                     --  Setting the result
                     test_hash.CDR.echoLong55_Args_Type
                       (Request.Payload.Args.all).Returns :=
                       Result_Ü;
                  end;
               when 56 =>
                  declare
                     data : CORBA.Long;
                     Error : PolyORB.Errors.Error_Container;
                     Result_Ü : CORBA.Long;
                  begin
                     --  Setting the request Payload
                     test_hash.CDR.echoLong56_Set_Args
                       (Request,
                        new test_hash.CDR.echoLong56_Args_Type);
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
                       test_hash.CDR.echoLong56_Args_Type
                          (Request.Payload.Args.all).data;
                     --  Call Implementation
                     Result_Ü :=
                       test_hash.Impl.echoLong56
                          (test_hash.Impl.Object'Class
                             (Self.all)'Access,
                           data);
                     --  Setting the result
                     test_hash.CDR.echoLong56_Args_Type
                       (Request.Payload.Args.all).Returns :=
                       Result_Ü;
                  end;
               when 57 =>
                  declare
                     data : CORBA.Long;
                     Error : PolyORB.Errors.Error_Container;
                     Result_Ü : CORBA.Long;
                  begin
                     --  Setting the request Payload
                     test_hash.CDR.echoLong57_Set_Args
                       (Request,
                        new test_hash.CDR.echoLong57_Args_Type);
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
                       test_hash.CDR.echoLong57_Args_Type
                          (Request.Payload.Args.all).data;
                     --  Call Implementation
                     Result_Ü :=
                       test_hash.Impl.echoLong57
                          (test_hash.Impl.Object'Class
                             (Self.all)'Access,
                           data);
                     --  Setting the result
                     test_hash.CDR.echoLong57_Args_Type
                       (Request.Payload.Args.all).Returns :=
                       Result_Ü;
                  end;
               when 58 =>
                  declare
                     data : CORBA.Long;
                     Error : PolyORB.Errors.Error_Container;
                     Result_Ü : CORBA.Long;
                  begin
                     --  Setting the request Payload
                     test_hash.CDR.echoLong58_Set_Args
                       (Request,
                        new test_hash.CDR.echoLong58_Args_Type);
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
                       test_hash.CDR.echoLong58_Args_Type
                          (Request.Payload.Args.all).data;
                     --  Call Implementation
                     Result_Ü :=
                       test_hash.Impl.echoLong58
                          (test_hash.Impl.Object'Class
                             (Self.all)'Access,
                           data);
                     --  Setting the result
                     test_hash.CDR.echoLong58_Args_Type
                       (Request.Payload.Args.all).Returns :=
                       Result_Ü;
                  end;
               when 59 =>
                  declare
                     data : CORBA.Long;
                     Error : PolyORB.Errors.Error_Container;
                     Result_Ü : CORBA.Long;
                  begin
                     --  Setting the request Payload
                     test_hash.CDR.echoLong59_Set_Args
                       (Request,
                        new test_hash.CDR.echoLong59_Args_Type);
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
                       test_hash.CDR.echoLong59_Args_Type
                          (Request.Payload.Args.all).data;
                     --  Call Implementation
                     Result_Ü :=
                       test_hash.Impl.echoLong59
                          (test_hash.Impl.Object'Class
                             (Self.all)'Access,
                           data);
                     --  Setting the result
                     test_hash.CDR.echoLong59_Args_Type
                       (Request.Payload.Args.all).Returns :=
                       Result_Ü;
                  end;
               when 60 =>
                  declare
                     data : CORBA.Long;
                     Error : PolyORB.Errors.Error_Container;
                     Result_Ü : CORBA.Long;
                  begin
                     --  Setting the request Payload
                     test_hash.CDR.echoLong60_Set_Args
                       (Request,
                        new test_hash.CDR.echoLong60_Args_Type);
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
                       test_hash.CDR.echoLong60_Args_Type
                          (Request.Payload.Args.all).data;
                     --  Call Implementation
                     Result_Ü :=
                       test_hash.Impl.echoLong60
                          (test_hash.Impl.Object'Class
                             (Self.all)'Access,
                           data);
                     --  Setting the result
                     test_hash.CDR.echoLong60_Args_Type
                       (Request.Payload.Args.all).Returns :=
                       Result_Ü;
                  end;
               when 61 =>
                  declare
                     data : CORBA.Long;
                     Error : PolyORB.Errors.Error_Container;
                     Result_Ü : CORBA.Long;
                  begin
                     --  Setting the request Payload
                     test_hash.CDR.echoLong61_Set_Args
                       (Request,
                        new test_hash.CDR.echoLong61_Args_Type);
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
                       test_hash.CDR.echoLong61_Args_Type
                          (Request.Payload.Args.all).data;
                     --  Call Implementation
                     Result_Ü :=
                       test_hash.Impl.echoLong61
                          (test_hash.Impl.Object'Class
                             (Self.all)'Access,
                           data);
                     --  Setting the result
                     test_hash.CDR.echoLong61_Args_Type
                       (Request.Payload.Args.all).Returns :=
                       Result_Ü;
                  end;
               when 62 =>
                  declare
                     data : CORBA.Long;
                     Error : PolyORB.Errors.Error_Container;
                     Result_Ü : CORBA.Long;
                  begin
                     --  Setting the request Payload
                     test_hash.CDR.echoLong62_Set_Args
                       (Request,
                        new test_hash.CDR.echoLong62_Args_Type);
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
                       test_hash.CDR.echoLong62_Args_Type
                          (Request.Payload.Args.all).data;
                     --  Call Implementation
                     Result_Ü :=
                       test_hash.Impl.echoLong62
                          (test_hash.Impl.Object'Class
                             (Self.all)'Access,
                           data);
                     --  Setting the result
                     test_hash.CDR.echoLong62_Args_Type
                       (Request.Payload.Args.all).Returns :=
                       Result_Ü;
                  end;
               when 63 =>
                  declare
                     data : CORBA.Long;
                     Error : PolyORB.Errors.Error_Container;
                     Result_Ü : CORBA.Long;
                  begin
                     --  Setting the request Payload
                     test_hash.CDR.echoLong63_Set_Args
                       (Request,
                        new test_hash.CDR.echoLong63_Args_Type);
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
                       test_hash.CDR.echoLong63_Args_Type
                          (Request.Payload.Args.all).data;
                     --  Call Implementation
                     Result_Ü :=
                       test_hash.Impl.echoLong63
                          (test_hash.Impl.Object'Class
                             (Self.all)'Access,
                           data);
                     --  Setting the result
                     test_hash.CDR.echoLong63_Args_Type
                       (Request.Payload.Args.all).Returns :=
                       Result_Ü;
                  end;
               when 64 =>
                  declare
                     data : CORBA.Long;
                     Error : PolyORB.Errors.Error_Container;
                     Result_Ü : CORBA.Long;
                  begin
                     --  Setting the request Payload
                     test_hash.CDR.echoLong64_Set_Args
                       (Request,
                        new test_hash.CDR.echoLong64_Args_Type);
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
                       test_hash.CDR.echoLong64_Args_Type
                          (Request.Payload.Args.all).data;
                     --  Call Implementation
                     Result_Ü :=
                       test_hash.Impl.echoLong64
                          (test_hash.Impl.Object'Class
                             (Self.all)'Access,
                           data);
                     --  Setting the result
                     test_hash.CDR.echoLong64_Args_Type
                       (Request.Payload.Args.all).Returns :=
                       Result_Ü;
                  end;
               when 65 =>
                  declare
                     data : CORBA.Long;
                     Error : PolyORB.Errors.Error_Container;
                     Result_Ü : CORBA.Long;
                  begin
                     --  Setting the request Payload
                     test_hash.CDR.echoLong65_Set_Args
                       (Request,
                        new test_hash.CDR.echoLong65_Args_Type);
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
                       test_hash.CDR.echoLong65_Args_Type
                          (Request.Payload.Args.all).data;
                     --  Call Implementation
                     Result_Ü :=
                       test_hash.Impl.echoLong65
                          (test_hash.Impl.Object'Class
                             (Self.all)'Access,
                           data);
                     --  Setting the result
                     test_hash.CDR.echoLong65_Args_Type
                       (Request.Payload.Args.all).Returns :=
                       Result_Ü;
                  end;
               when 66 =>
                  declare
                     data : CORBA.Long;
                     Error : PolyORB.Errors.Error_Container;
                     Result_Ü : CORBA.Long;
                  begin
                     --  Setting the request Payload
                     test_hash.CDR.echoLong66_Set_Args
                       (Request,
                        new test_hash.CDR.echoLong66_Args_Type);
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
                       test_hash.CDR.echoLong66_Args_Type
                          (Request.Payload.Args.all).data;
                     --  Call Implementation
                     Result_Ü :=
                       test_hash.Impl.echoLong66
                          (test_hash.Impl.Object'Class
                             (Self.all)'Access,
                           data);
                     --  Setting the result
                     test_hash.CDR.echoLong66_Args_Type
                       (Request.Payload.Args.all).Returns :=
                       Result_Ü;
                  end;
               when 67 =>
                  declare
                     data : CORBA.Long;
                     Error : PolyORB.Errors.Error_Container;
                     Result_Ü : CORBA.Long;
                  begin
                     --  Setting the request Payload
                     test_hash.CDR.echoLong67_Set_Args
                       (Request,
                        new test_hash.CDR.echoLong67_Args_Type);
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
                       test_hash.CDR.echoLong67_Args_Type
                          (Request.Payload.Args.all).data;
                     --  Call Implementation
                     Result_Ü :=
                       test_hash.Impl.echoLong67
                          (test_hash.Impl.Object'Class
                             (Self.all)'Access,
                           data);
                     --  Setting the result
                     test_hash.CDR.echoLong67_Args_Type
                       (Request.Payload.Args.all).Returns :=
                       Result_Ü;
                  end;
               when 68 =>
                  declare
                     data : CORBA.Long;
                     Error : PolyORB.Errors.Error_Container;
                     Result_Ü : CORBA.Long;
                  begin
                     --  Setting the request Payload
                     test_hash.CDR.echoLong68_Set_Args
                       (Request,
                        new test_hash.CDR.echoLong68_Args_Type);
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
                       test_hash.CDR.echoLong68_Args_Type
                          (Request.Payload.Args.all).data;
                     --  Call Implementation
                     Result_Ü :=
                       test_hash.Impl.echoLong68
                          (test_hash.Impl.Object'Class
                             (Self.all)'Access,
                           data);
                     --  Setting the result
                     test_hash.CDR.echoLong68_Args_Type
                       (Request.Payload.Args.all).Returns :=
                       Result_Ü;
                  end;
               when 69 =>
                  declare
                     data : CORBA.Long;
                     Error : PolyORB.Errors.Error_Container;
                     Result_Ü : CORBA.Long;
                  begin
                     --  Setting the request Payload
                     test_hash.CDR.echoLong69_Set_Args
                       (Request,
                        new test_hash.CDR.echoLong69_Args_Type);
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
                       test_hash.CDR.echoLong69_Args_Type
                          (Request.Payload.Args.all).data;
                     --  Call Implementation
                     Result_Ü :=
                       test_hash.Impl.echoLong69
                          (test_hash.Impl.Object'Class
                             (Self.all)'Access,
                           data);
                     --  Setting the result
                     test_hash.CDR.echoLong69_Args_Type
                       (Request.Payload.Args.all).Returns :=
                       Result_Ü;
                  end;
               when 70 =>
                  declare
                     data : CORBA.Long;
                     Error : PolyORB.Errors.Error_Container;
                     Result_Ü : CORBA.Long;
                  begin
                     --  Setting the request Payload
                     test_hash.CDR.echoLong70_Set_Args
                       (Request,
                        new test_hash.CDR.echoLong70_Args_Type);
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
                       test_hash.CDR.echoLong70_Args_Type
                          (Request.Payload.Args.all).data;
                     --  Call Implementation
                     Result_Ü :=
                       test_hash.Impl.echoLong70
                          (test_hash.Impl.Object'Class
                             (Self.all)'Access,
                           data);
                     --  Setting the result
                     test_hash.CDR.echoLong70_Args_Type
                       (Request.Payload.Args.all).Returns :=
                       Result_Ü;
                  end;
               when 71 =>
                  declare
                     data : CORBA.Long;
                     Error : PolyORB.Errors.Error_Container;
                     Result_Ü : CORBA.Long;
                  begin
                     --  Setting the request Payload
                     test_hash.CDR.echoLong71_Set_Args
                       (Request,
                        new test_hash.CDR.echoLong71_Args_Type);
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
                       test_hash.CDR.echoLong71_Args_Type
                          (Request.Payload.Args.all).data;
                     --  Call Implementation
                     Result_Ü :=
                       test_hash.Impl.echoLong71
                          (test_hash.Impl.Object'Class
                             (Self.all)'Access,
                           data);
                     --  Setting the result
                     test_hash.CDR.echoLong71_Args_Type
                       (Request.Payload.Args.all).Returns :=
                       Result_Ü;
                  end;
               when 72 =>
                  declare
                     data : CORBA.Long;
                     Error : PolyORB.Errors.Error_Container;
                     Result_Ü : CORBA.Long;
                  begin
                     --  Setting the request Payload
                     test_hash.CDR.echoLong72_Set_Args
                       (Request,
                        new test_hash.CDR.echoLong72_Args_Type);
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
                       test_hash.CDR.echoLong72_Args_Type
                          (Request.Payload.Args.all).data;
                     --  Call Implementation
                     Result_Ü :=
                       test_hash.Impl.echoLong72
                          (test_hash.Impl.Object'Class
                             (Self.all)'Access,
                           data);
                     --  Setting the result
                     test_hash.CDR.echoLong72_Args_Type
                       (Request.Payload.Args.all).Returns :=
                       Result_Ü;
                  end;
               when 73 =>
                  declare
                     data : CORBA.Long;
                     Error : PolyORB.Errors.Error_Container;
                     Result_Ü : CORBA.Long;
                  begin
                     --  Setting the request Payload
                     test_hash.CDR.echoLong73_Set_Args
                       (Request,
                        new test_hash.CDR.echoLong73_Args_Type);
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
                       test_hash.CDR.echoLong73_Args_Type
                          (Request.Payload.Args.all).data;
                     --  Call Implementation
                     Result_Ü :=
                       test_hash.Impl.echoLong73
                          (test_hash.Impl.Object'Class
                             (Self.all)'Access,
                           data);
                     --  Setting the result
                     test_hash.CDR.echoLong73_Args_Type
                       (Request.Payload.Args.all).Returns :=
                       Result_Ü;
                  end;
               when 74 =>
                  declare
                     data : CORBA.Long;
                     Error : PolyORB.Errors.Error_Container;
                     Result_Ü : CORBA.Long;
                  begin
                     --  Setting the request Payload
                     test_hash.CDR.echoLong74_Set_Args
                       (Request,
                        new test_hash.CDR.echoLong74_Args_Type);
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
                       test_hash.CDR.echoLong74_Args_Type
                          (Request.Payload.Args.all).data;
                     --  Call Implementation
                     Result_Ü :=
                       test_hash.Impl.echoLong74
                          (test_hash.Impl.Object'Class
                             (Self.all)'Access,
                           data);
                     --  Setting the result
                     test_hash.CDR.echoLong74_Args_Type
                       (Request.Payload.Args.all).Returns :=
                       Result_Ü;
                  end;
               when 75 =>
                  declare
                     data : CORBA.Long;
                     Error : PolyORB.Errors.Error_Container;
                     Result_Ü : CORBA.Long;
                  begin
                     --  Setting the request Payload
                     test_hash.CDR.echoLong75_Set_Args
                       (Request,
                        new test_hash.CDR.echoLong75_Args_Type);
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
                       test_hash.CDR.echoLong75_Args_Type
                          (Request.Payload.Args.all).data;
                     --  Call Implementation
                     Result_Ü :=
                       test_hash.Impl.echoLong75
                          (test_hash.Impl.Object'Class
                             (Self.all)'Access,
                           data);
                     --  Setting the result
                     test_hash.CDR.echoLong75_Args_Type
                       (Request.Payload.Args.all).Returns :=
                       Result_Ü;
                  end;
               when 76 =>
                  declare
                     data : CORBA.Long;
                     Error : PolyORB.Errors.Error_Container;
                     Result_Ü : CORBA.Long;
                  begin
                     --  Setting the request Payload
                     test_hash.CDR.echoLong76_Set_Args
                       (Request,
                        new test_hash.CDR.echoLong76_Args_Type);
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
                       test_hash.CDR.echoLong76_Args_Type
                          (Request.Payload.Args.all).data;
                     --  Call Implementation
                     Result_Ü :=
                       test_hash.Impl.echoLong76
                          (test_hash.Impl.Object'Class
                             (Self.all)'Access,
                           data);
                     --  Setting the result
                     test_hash.CDR.echoLong76_Args_Type
                       (Request.Payload.Args.all).Returns :=
                       Result_Ü;
                  end;
               when 77 =>
                  declare
                     data : CORBA.Long;
                     Error : PolyORB.Errors.Error_Container;
                     Result_Ü : CORBA.Long;
                  begin
                     --  Setting the request Payload
                     test_hash.CDR.echoLong77_Set_Args
                       (Request,
                        new test_hash.CDR.echoLong77_Args_Type);
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
                       test_hash.CDR.echoLong77_Args_Type
                          (Request.Payload.Args.all).data;
                     --  Call Implementation
                     Result_Ü :=
                       test_hash.Impl.echoLong77
                          (test_hash.Impl.Object'Class
                             (Self.all)'Access,
                           data);
                     --  Setting the result
                     test_hash.CDR.echoLong77_Args_Type
                       (Request.Payload.Args.all).Returns :=
                       Result_Ü;
                  end;
               when 78 =>
                  declare
                     data : CORBA.Long;
                     Error : PolyORB.Errors.Error_Container;
                     Result_Ü : CORBA.Long;
                  begin
                     --  Setting the request Payload
                     test_hash.CDR.echoLong78_Set_Args
                       (Request,
                        new test_hash.CDR.echoLong78_Args_Type);
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
                       test_hash.CDR.echoLong78_Args_Type
                          (Request.Payload.Args.all).data;
                     --  Call Implementation
                     Result_Ü :=
                       test_hash.Impl.echoLong78
                          (test_hash.Impl.Object'Class
                             (Self.all)'Access,
                           data);
                     --  Setting the result
                     test_hash.CDR.echoLong78_Args_Type
                       (Request.Payload.Args.all).Returns :=
                       Result_Ü;
                  end;
               when 79 =>
                  declare
                     data : CORBA.Long;
                     Error : PolyORB.Errors.Error_Container;
                     Result_Ü : CORBA.Long;
                  begin
                     --  Setting the request Payload
                     test_hash.CDR.echoLong79_Set_Args
                       (Request,
                        new test_hash.CDR.echoLong79_Args_Type);
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
                       test_hash.CDR.echoLong79_Args_Type
                          (Request.Payload.Args.all).data;
                     --  Call Implementation
                     Result_Ü :=
                       test_hash.Impl.echoLong79
                          (test_hash.Impl.Object'Class
                             (Self.all)'Access,
                           data);
                     --  Setting the result
                     test_hash.CDR.echoLong79_Args_Type
                       (Request.Payload.Args.all).Returns :=
                       Result_Ü;
                  end;
               when 80 =>
                  declare
                     data : CORBA.Long;
                     Error : PolyORB.Errors.Error_Container;
                     Result_Ü : CORBA.Long;
                  begin
                     --  Setting the request Payload
                     test_hash.CDR.echoLong80_Set_Args
                       (Request,
                        new test_hash.CDR.echoLong80_Args_Type);
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
                       test_hash.CDR.echoLong80_Args_Type
                          (Request.Payload.Args.all).data;
                     --  Call Implementation
                     Result_Ü :=
                       test_hash.Impl.echoLong80
                          (test_hash.Impl.Object'Class
                             (Self.all)'Access,
                           data);
                     --  Setting the result
                     test_hash.CDR.echoLong80_Args_Type
                       (Request.Payload.Args.all).Returns :=
                       Result_Ü;
                  end;
               when 81 =>
                  declare
                     data : CORBA.Long;
                     Error : PolyORB.Errors.Error_Container;
                     Result_Ü : CORBA.Long;
                  begin
                     --  Setting the request Payload
                     test_hash.CDR.echoLong81_Set_Args
                       (Request,
                        new test_hash.CDR.echoLong81_Args_Type);
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
                       test_hash.CDR.echoLong81_Args_Type
                          (Request.Payload.Args.all).data;
                     --  Call Implementation
                     Result_Ü :=
                       test_hash.Impl.echoLong81
                          (test_hash.Impl.Object'Class
                             (Self.all)'Access,
                           data);
                     --  Setting the result
                     test_hash.CDR.echoLong81_Args_Type
                       (Request.Payload.Args.all).Returns :=
                       Result_Ü;
                  end;
               when 82 =>
                  declare
                     data : CORBA.Long;
                     Error : PolyORB.Errors.Error_Container;
                     Result_Ü : CORBA.Long;
                  begin
                     --  Setting the request Payload
                     test_hash.CDR.echoLong82_Set_Args
                       (Request,
                        new test_hash.CDR.echoLong82_Args_Type);
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
                       test_hash.CDR.echoLong82_Args_Type
                          (Request.Payload.Args.all).data;
                     --  Call Implementation
                     Result_Ü :=
                       test_hash.Impl.echoLong82
                          (test_hash.Impl.Object'Class
                             (Self.all)'Access,
                           data);
                     --  Setting the result
                     test_hash.CDR.echoLong82_Args_Type
                       (Request.Payload.Args.all).Returns :=
                       Result_Ü;
                  end;
               when 83 =>
                  declare
                     data : CORBA.Long;
                     Error : PolyORB.Errors.Error_Container;
                     Result_Ü : CORBA.Long;
                  begin
                     --  Setting the request Payload
                     test_hash.CDR.echoLong83_Set_Args
                       (Request,
                        new test_hash.CDR.echoLong83_Args_Type);
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
                       test_hash.CDR.echoLong83_Args_Type
                          (Request.Payload.Args.all).data;
                     --  Call Implementation
                     Result_Ü :=
                       test_hash.Impl.echoLong83
                          (test_hash.Impl.Object'Class
                             (Self.all)'Access,
                           data);
                     --  Setting the result
                     test_hash.CDR.echoLong83_Args_Type
                       (Request.Payload.Args.all).Returns :=
                       Result_Ü;
                  end;
               when 84 =>
                  declare
                     data : CORBA.Long;
                     Error : PolyORB.Errors.Error_Container;
                     Result_Ü : CORBA.Long;
                  begin
                     --  Setting the request Payload
                     test_hash.CDR.echoLong84_Set_Args
                       (Request,
                        new test_hash.CDR.echoLong84_Args_Type);
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
                       test_hash.CDR.echoLong84_Args_Type
                          (Request.Payload.Args.all).data;
                     --  Call Implementation
                     Result_Ü :=
                       test_hash.Impl.echoLong84
                          (test_hash.Impl.Object'Class
                             (Self.all)'Access,
                           data);
                     --  Setting the result
                     test_hash.CDR.echoLong84_Args_Type
                       (Request.Payload.Args.all).Returns :=
                       Result_Ü;
                  end;
               when 85 =>
                  declare
                     data : CORBA.Long;
                     Error : PolyORB.Errors.Error_Container;
                     Result_Ü : CORBA.Long;
                  begin
                     --  Setting the request Payload
                     test_hash.CDR.echoLong85_Set_Args
                       (Request,
                        new test_hash.CDR.echoLong85_Args_Type);
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
                       test_hash.CDR.echoLong85_Args_Type
                          (Request.Payload.Args.all).data;
                     --  Call Implementation
                     Result_Ü :=
                       test_hash.Impl.echoLong85
                          (test_hash.Impl.Object'Class
                             (Self.all)'Access,
                           data);
                     --  Setting the result
                     test_hash.CDR.echoLong85_Args_Type
                       (Request.Payload.Args.all).Returns :=
                       Result_Ü;
                  end;
               when 86 =>
                  declare
                     data : CORBA.Long;
                     Error : PolyORB.Errors.Error_Container;
                     Result_Ü : CORBA.Long;
                  begin
                     --  Setting the request Payload
                     test_hash.CDR.echoLong86_Set_Args
                       (Request,
                        new test_hash.CDR.echoLong86_Args_Type);
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
                       test_hash.CDR.echoLong86_Args_Type
                          (Request.Payload.Args.all).data;
                     --  Call Implementation
                     Result_Ü :=
                       test_hash.Impl.echoLong86
                          (test_hash.Impl.Object'Class
                             (Self.all)'Access,
                           data);
                     --  Setting the result
                     test_hash.CDR.echoLong86_Args_Type
                       (Request.Payload.Args.all).Returns :=
                       Result_Ü;
                  end;
               when 87 =>
                  declare
                     data : CORBA.Long;
                     Error : PolyORB.Errors.Error_Container;
                     Result_Ü : CORBA.Long;
                  begin
                     --  Setting the request Payload
                     test_hash.CDR.echoLong87_Set_Args
                       (Request,
                        new test_hash.CDR.echoLong87_Args_Type);
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
                       test_hash.CDR.echoLong87_Args_Type
                          (Request.Payload.Args.all).data;
                     --  Call Implementation
                     Result_Ü :=
                       test_hash.Impl.echoLong87
                          (test_hash.Impl.Object'Class
                             (Self.all)'Access,
                           data);
                     --  Setting the result
                     test_hash.CDR.echoLong87_Args_Type
                       (Request.Payload.Args.all).Returns :=
                       Result_Ü;
                  end;
               when 88 =>
                  declare
                     data : CORBA.Long;
                     Error : PolyORB.Errors.Error_Container;
                     Result_Ü : CORBA.Long;
                  begin
                     --  Setting the request Payload
                     test_hash.CDR.echoLong88_Set_Args
                       (Request,
                        new test_hash.CDR.echoLong88_Args_Type);
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
                       test_hash.CDR.echoLong88_Args_Type
                          (Request.Payload.Args.all).data;
                     --  Call Implementation
                     Result_Ü :=
                       test_hash.Impl.echoLong88
                          (test_hash.Impl.Object'Class
                             (Self.all)'Access,
                           data);
                     --  Setting the result
                     test_hash.CDR.echoLong88_Args_Type
                       (Request.Payload.Args.all).Returns :=
                       Result_Ü;
                  end;
               when 89 =>
                  declare
                     data : CORBA.Long;
                     Error : PolyORB.Errors.Error_Container;
                     Result_Ü : CORBA.Long;
                  begin
                     --  Setting the request Payload
                     test_hash.CDR.echoLong89_Set_Args
                       (Request,
                        new test_hash.CDR.echoLong89_Args_Type);
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
                       test_hash.CDR.echoLong89_Args_Type
                          (Request.Payload.Args.all).data;
                     --  Call Implementation
                     Result_Ü :=
                       test_hash.Impl.echoLong89
                          (test_hash.Impl.Object'Class
                             (Self.all)'Access,
                           data);
                     --  Setting the result
                     test_hash.CDR.echoLong89_Args_Type
                       (Request.Payload.Args.all).Returns :=
                       Result_Ü;
                  end;
               when 90 =>
                  declare
                     data : CORBA.Long;
                     Error : PolyORB.Errors.Error_Container;
                     Result_Ü : CORBA.Long;
                  begin
                     --  Setting the request Payload
                     test_hash.CDR.echoLong90_Set_Args
                       (Request,
                        new test_hash.CDR.echoLong90_Args_Type);
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
                       test_hash.CDR.echoLong90_Args_Type
                          (Request.Payload.Args.all).data;
                     --  Call Implementation
                     Result_Ü :=
                       test_hash.Impl.echoLong90
                          (test_hash.Impl.Object'Class
                             (Self.all)'Access,
                           data);
                     --  Setting the result
                     test_hash.CDR.echoLong90_Args_Type
                       (Request.Payload.Args.all).Returns :=
                       Result_Ü;
                  end;
               when 91 =>
                  declare
                     data : CORBA.Long;
                     Error : PolyORB.Errors.Error_Container;
                     Result_Ü : CORBA.Long;
                  begin
                     --  Setting the request Payload
                     test_hash.CDR.echoLong91_Set_Args
                       (Request,
                        new test_hash.CDR.echoLong91_Args_Type);
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
                       test_hash.CDR.echoLong91_Args_Type
                          (Request.Payload.Args.all).data;
                     --  Call Implementation
                     Result_Ü :=
                       test_hash.Impl.echoLong91
                          (test_hash.Impl.Object'Class
                             (Self.all)'Access,
                           data);
                     --  Setting the result
                     test_hash.CDR.echoLong91_Args_Type
                       (Request.Payload.Args.all).Returns :=
                       Result_Ü;
                  end;
               when 92 =>
                  declare
                     data : CORBA.Long;
                     Error : PolyORB.Errors.Error_Container;
                     Result_Ü : CORBA.Long;
                  begin
                     --  Setting the request Payload
                     test_hash.CDR.echoLong92_Set_Args
                       (Request,
                        new test_hash.CDR.echoLong92_Args_Type);
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
                       test_hash.CDR.echoLong92_Args_Type
                          (Request.Payload.Args.all).data;
                     --  Call Implementation
                     Result_Ü :=
                       test_hash.Impl.echoLong92
                          (test_hash.Impl.Object'Class
                             (Self.all)'Access,
                           data);
                     --  Setting the result
                     test_hash.CDR.echoLong92_Args_Type
                       (Request.Payload.Args.all).Returns :=
                       Result_Ü;
                  end;
               when 93 =>
                  declare
                     data : CORBA.Long;
                     Error : PolyORB.Errors.Error_Container;
                     Result_Ü : CORBA.Long;
                  begin
                     --  Setting the request Payload
                     test_hash.CDR.echoLong93_Set_Args
                       (Request,
                        new test_hash.CDR.echoLong93_Args_Type);
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
                       test_hash.CDR.echoLong93_Args_Type
                          (Request.Payload.Args.all).data;
                     --  Call Implementation
                     Result_Ü :=
                       test_hash.Impl.echoLong93
                          (test_hash.Impl.Object'Class
                             (Self.all)'Access,
                           data);
                     --  Setting the result
                     test_hash.CDR.echoLong93_Args_Type
                       (Request.Payload.Args.all).Returns :=
                       Result_Ü;
                  end;
               when 94 =>
                  declare
                     data : CORBA.Long;
                     Error : PolyORB.Errors.Error_Container;
                     Result_Ü : CORBA.Long;
                  begin
                     --  Setting the request Payload
                     test_hash.CDR.echoLong94_Set_Args
                       (Request,
                        new test_hash.CDR.echoLong94_Args_Type);
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
                       test_hash.CDR.echoLong94_Args_Type
                          (Request.Payload.Args.all).data;
                     --  Call Implementation
                     Result_Ü :=
                       test_hash.Impl.echoLong94
                          (test_hash.Impl.Object'Class
                             (Self.all)'Access,
                           data);
                     --  Setting the result
                     test_hash.CDR.echoLong94_Args_Type
                       (Request.Payload.Args.all).Returns :=
                       Result_Ü;
                  end;
               when 95 =>
                  declare
                     data : CORBA.Long;
                     Error : PolyORB.Errors.Error_Container;
                     Result_Ü : CORBA.Long;
                  begin
                     --  Setting the request Payload
                     test_hash.CDR.echoLong95_Set_Args
                       (Request,
                        new test_hash.CDR.echoLong95_Args_Type);
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
                       test_hash.CDR.echoLong95_Args_Type
                          (Request.Payload.Args.all).data;
                     --  Call Implementation
                     Result_Ü :=
                       test_hash.Impl.echoLong95
                          (test_hash.Impl.Object'Class
                             (Self.all)'Access,
                           data);
                     --  Setting the result
                     test_hash.CDR.echoLong95_Args_Type
                       (Request.Payload.Args.all).Returns :=
                       Result_Ü;
                  end;
               when 96 =>
                  declare
                     data : CORBA.Long;
                     Error : PolyORB.Errors.Error_Container;
                     Result_Ü : CORBA.Long;
                  begin
                     --  Setting the request Payload
                     test_hash.CDR.echoLong96_Set_Args
                       (Request,
                        new test_hash.CDR.echoLong96_Args_Type);
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
                       test_hash.CDR.echoLong96_Args_Type
                          (Request.Payload.Args.all).data;
                     --  Call Implementation
                     Result_Ü :=
                       test_hash.Impl.echoLong96
                          (test_hash.Impl.Object'Class
                             (Self.all)'Access,
                           data);
                     --  Setting the result
                     test_hash.CDR.echoLong96_Args_Type
                       (Request.Payload.Args.all).Returns :=
                       Result_Ü;
                  end;
               when 97 =>
                  declare
                     data : CORBA.Long;
                     Error : PolyORB.Errors.Error_Container;
                     Result_Ü : CORBA.Long;
                  begin
                     --  Setting the request Payload
                     test_hash.CDR.echoLong97_Set_Args
                       (Request,
                        new test_hash.CDR.echoLong97_Args_Type);
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
                       test_hash.CDR.echoLong97_Args_Type
                          (Request.Payload.Args.all).data;
                     --  Call Implementation
                     Result_Ü :=
                       test_hash.Impl.echoLong97
                          (test_hash.Impl.Object'Class
                             (Self.all)'Access,
                           data);
                     --  Setting the result
                     test_hash.CDR.echoLong97_Args_Type
                       (Request.Payload.Args.all).Returns :=
                       Result_Ü;
                  end;
               when 98 =>
                  declare
                     data : CORBA.Long;
                     Error : PolyORB.Errors.Error_Container;
                     Result_Ü : CORBA.Long;
                  begin
                     --  Setting the request Payload
                     test_hash.CDR.echoLong98_Set_Args
                       (Request,
                        new test_hash.CDR.echoLong98_Args_Type);
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
                       test_hash.CDR.echoLong98_Args_Type
                          (Request.Payload.Args.all).data;
                     --  Call Implementation
                     Result_Ü :=
                       test_hash.Impl.echoLong98
                          (test_hash.Impl.Object'Class
                             (Self.all)'Access,
                           data);
                     --  Setting the result
                     test_hash.CDR.echoLong98_Args_Type
                       (Request.Payload.Args.all).Returns :=
                       Result_Ü;
                  end;
               when 99 =>
                  declare
                     data : CORBA.Long;
                     Error : PolyORB.Errors.Error_Container;
                     Result_Ü : CORBA.Long;
                  begin
                     --  Setting the request Payload
                     test_hash.CDR.echoLong99_Set_Args
                       (Request,
                        new test_hash.CDR.echoLong99_Args_Type);
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
                       test_hash.CDR.echoLong99_Args_Type
                          (Request.Payload.Args.all).data;
                     --  Call Implementation
                     Result_Ü :=
                       test_hash.Impl.echoLong99
                          (test_hash.Impl.Object'Class
                             (Self.all)'Access,
                           data);
                     --  Setting the result
                     test_hash.CDR.echoLong99_Args_Type
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
                        test_hash.Is_A'Access);
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
         in test_hash.Impl.Object'Class);
   end Servant_Is_A;

   procedure Deferred_Initialization

   is
   begin
      PortableServer.Internals.Register_Skeleton
        (CORBA.To_CORBA_String
           (test_hash.Repository_Id),
         Servant_Is_A'Access,
         Is_A'Access,
         Invoke'Access);
      Register_Procedure
        ("echoLong00");
      Register_Procedure
        ("echoLong01");
      Register_Procedure
        ("echoLong02");
      Register_Procedure
        ("echoLong03");
      Register_Procedure
        ("echoLong04");
      Register_Procedure
        ("echoLong05");
      Register_Procedure
        ("echoLong06");
      Register_Procedure
        ("echoLong07");
      Register_Procedure
        ("echoLong08");
      Register_Procedure
        ("echoLong09");
      Register_Procedure
        ("echoLong10");
      Register_Procedure
        ("echoLong11");
      Register_Procedure
        ("echoLong12");
      Register_Procedure
        ("echoLong13");
      Register_Procedure
        ("echoLong14");
      Register_Procedure
        ("echoLong15");
      Register_Procedure
        ("echoLong16");
      Register_Procedure
        ("echoLong17");
      Register_Procedure
        ("echoLong18");
      Register_Procedure
        ("echoLong19");
      Register_Procedure
        ("echoLong20");
      Register_Procedure
        ("echoLong21");
      Register_Procedure
        ("echoLong22");
      Register_Procedure
        ("echoLong23");
      Register_Procedure
        ("echoLong24");
      Register_Procedure
        ("echoLong25");
      Register_Procedure
        ("echoLong26");
      Register_Procedure
        ("echoLong27");
      Register_Procedure
        ("echoLong28");
      Register_Procedure
        ("echoLong29");
      Register_Procedure
        ("echoLong30");
      Register_Procedure
        ("echoLong31");
      Register_Procedure
        ("echoLong32");
      Register_Procedure
        ("echoLong33");
      Register_Procedure
        ("echoLong34");
      Register_Procedure
        ("echoLong35");
      Register_Procedure
        ("echoLong36");
      Register_Procedure
        ("echoLong37");
      Register_Procedure
        ("echoLong38");
      Register_Procedure
        ("echoLong39");
      Register_Procedure
        ("echoLong40");
      Register_Procedure
        ("echoLong41");
      Register_Procedure
        ("echoLong42");
      Register_Procedure
        ("echoLong43");
      Register_Procedure
        ("echoLong44");
      Register_Procedure
        ("echoLong45");
      Register_Procedure
        ("echoLong46");
      Register_Procedure
        ("echoLong47");
      Register_Procedure
        ("echoLong48");
      Register_Procedure
        ("echoLong49");
      Register_Procedure
        ("echoLong50");
      Register_Procedure
        ("echoLong51");
      Register_Procedure
        ("echoLong52");
      Register_Procedure
        ("echoLong53");
      Register_Procedure
        ("echoLong54");
      Register_Procedure
        ("echoLong55");
      Register_Procedure
        ("echoLong56");
      Register_Procedure
        ("echoLong57");
      Register_Procedure
        ("echoLong58");
      Register_Procedure
        ("echoLong59");
      Register_Procedure
        ("echoLong60");
      Register_Procedure
        ("echoLong61");
      Register_Procedure
        ("echoLong62");
      Register_Procedure
        ("echoLong63");
      Register_Procedure
        ("echoLong64");
      Register_Procedure
        ("echoLong65");
      Register_Procedure
        ("echoLong66");
      Register_Procedure
        ("echoLong67");
      Register_Procedure
        ("echoLong68");
      Register_Procedure
        ("echoLong69");
      Register_Procedure
        ("echoLong70");
      Register_Procedure
        ("echoLong71");
      Register_Procedure
        ("echoLong72");
      Register_Procedure
        ("echoLong73");
      Register_Procedure
        ("echoLong74");
      Register_Procedure
        ("echoLong75");
      Register_Procedure
        ("echoLong76");
      Register_Procedure
        ("echoLong77");
      Register_Procedure
        ("echoLong78");
      Register_Procedure
        ("echoLong79");
      Register_Procedure
        ("echoLong80");
      Register_Procedure
        ("echoLong81");
      Register_Procedure
        ("echoLong82");
      Register_Procedure
        ("echoLong83");
      Register_Procedure
        ("echoLong84");
      Register_Procedure
        ("echoLong85");
      Register_Procedure
        ("echoLong86");
      Register_Procedure
        ("echoLong87");
      Register_Procedure
        ("echoLong88");
      Register_Procedure
        ("echoLong89");
      Register_Procedure
        ("echoLong90");
      Register_Procedure
        ("echoLong91");
      Register_Procedure
        ("echoLong92");
      Register_Procedure
        ("echoLong93");
      Register_Procedure
        ("echoLong94");
      Register_Procedure
        ("echoLong95");
      Register_Procedure
        ("echoLong96");
      Register_Procedure
        ("echoLong97");
      Register_Procedure
        ("echoLong98");
      Register_Procedure
        ("echoLong99");
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
           ("test_hash.Skel"),
         Conflicts => PolyORB.Utils.Strings.Lists.Empty,
         Depends => PolyORB.Utils.Strings.Lists.Empty,
         Provides => PolyORB.Utils.Strings.Lists.Empty,
         Implicit => False,
         Init => Deferred_Initialization'Access));
end test_hash.Skel;

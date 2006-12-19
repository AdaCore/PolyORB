with test_hash.Impl;
with CORBA;
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

package body test_hash.Skel is

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
      Operation_ü : constant Standard.String :=
        CORBA.To_Standard_String
           (CORBA.ServerRequest.Operation
              (Request.all));
      Argument_List_ü : CORBA.NVList.Ref;
      String_Id : String := Operation_Ü
        (Operation_Ü'Length - 1 .. Operation_Ü'Length);
      Index_Ü : Natural;
   begin
      Index_Ü := Natural'Value (String_Id);
      T1 := Clock;
      CORBA.ORB.Create_List
        (0,
         Argument_List_ü);
      begin
         if (Operation_ü
            = "_is_a")
         then
            begin
               PolyORB.CORBA_P.Implicit_CORBA_Methods.Handle_Is_A
                 (Request,
                  Argument_List_ü,
                  test_hash.Is_A'Access);
            end;
         elsif (Operation_ü
            = "echoLong00")
         then
            declare
               data : CORBA.Long;
               Arg_Name_U_data : constant CORBA.Identifier :=
                 CORBA.To_CORBA_String
                    ("data");
               Argument_U_data : CORBA.Any :=
                 CORBA.Internals.Get_Empty_Any
                    (CORBA.TC_Long);
               Result_ü : CORBA.Long;
            begin
               CORBA.NVList.Add_Item
                 (Argument_List_ü,
                  Arg_Name_U_data,
                  Argument_U_data,
                  CORBA.ARG_IN);
               CORBA.ServerRequest.Arguments
                 (Request,
                  Argument_List_ü);
               --  Convert from Any
               data :=
                 CORBA.From_Any
                    (Argument_U_data);
               --  Call Implementation
               Result_ü :=
                 test_hash.Impl.echoLong00
                    (test_hash.Impl.Object'Class
                       (Self.all)'Access,
                     data);
               --  Setting the result
               CORBA.ServerRequest.Set_Result
                 (Request,
                  CORBA.To_Any
                    (Result_ü));
            end;
         elsif (Operation_ü
            = "echoLong01")
         then
            declare
               data : CORBA.Long;
               Arg_Name_U_data : constant CORBA.Identifier :=
                 CORBA.To_CORBA_String
                    ("data");
               Argument_U_data : CORBA.Any :=
                 CORBA.Internals.Get_Empty_Any
                    (CORBA.TC_Long);
               Result_ü : CORBA.Long;
            begin
               CORBA.NVList.Add_Item
                 (Argument_List_ü,
                  Arg_Name_U_data,
                  Argument_U_data,
                  CORBA.ARG_IN);
               CORBA.ServerRequest.Arguments
                 (Request,
                  Argument_List_ü);
               --  Convert from Any
               data :=
                 CORBA.From_Any
                    (Argument_U_data);
               --  Call Implementation
               Result_ü :=
                 test_hash.Impl.echoLong01
                    (test_hash.Impl.Object'Class
                       (Self.all)'Access,
                     data);
               --  Setting the result
               CORBA.ServerRequest.Set_Result
                 (Request,
                  CORBA.To_Any
                    (Result_ü));
            end;
         elsif (Operation_ü
            = "echoLong02")
         then
            declare
               data : CORBA.Long;
               Arg_Name_U_data : constant CORBA.Identifier :=
                 CORBA.To_CORBA_String
                    ("data");
               Argument_U_data : CORBA.Any :=
                 CORBA.Internals.Get_Empty_Any
                    (CORBA.TC_Long);
               Result_ü : CORBA.Long;
            begin
               CORBA.NVList.Add_Item
                 (Argument_List_ü,
                  Arg_Name_U_data,
                  Argument_U_data,
                  CORBA.ARG_IN);
               CORBA.ServerRequest.Arguments
                 (Request,
                  Argument_List_ü);
               --  Convert from Any
               data :=
                 CORBA.From_Any
                    (Argument_U_data);
               --  Call Implementation
               Result_ü :=
                 test_hash.Impl.echoLong02
                    (test_hash.Impl.Object'Class
                       (Self.all)'Access,
                     data);
               --  Setting the result
               CORBA.ServerRequest.Set_Result
                 (Request,
                  CORBA.To_Any
                    (Result_ü));
            end;
         elsif (Operation_ü
            = "echoLong03")
         then
            declare
               data : CORBA.Long;
               Arg_Name_U_data : constant CORBA.Identifier :=
                 CORBA.To_CORBA_String
                    ("data");
               Argument_U_data : CORBA.Any :=
                 CORBA.Internals.Get_Empty_Any
                    (CORBA.TC_Long);
               Result_ü : CORBA.Long;
            begin
               CORBA.NVList.Add_Item
                 (Argument_List_ü,
                  Arg_Name_U_data,
                  Argument_U_data,
                  CORBA.ARG_IN);
               CORBA.ServerRequest.Arguments
                 (Request,
                  Argument_List_ü);
               --  Convert from Any
               data :=
                 CORBA.From_Any
                    (Argument_U_data);
               --  Call Implementation
               Result_ü :=
                 test_hash.Impl.echoLong03
                    (test_hash.Impl.Object'Class
                       (Self.all)'Access,
                     data);
               --  Setting the result
               CORBA.ServerRequest.Set_Result
                 (Request,
                  CORBA.To_Any
                    (Result_ü));
            end;
         elsif (Operation_ü
            = "echoLong04")
         then
            declare
               data : CORBA.Long;
               Arg_Name_U_data : constant CORBA.Identifier :=
                 CORBA.To_CORBA_String
                    ("data");
               Argument_U_data : CORBA.Any :=
                 CORBA.Internals.Get_Empty_Any
                    (CORBA.TC_Long);
               Result_ü : CORBA.Long;
            begin
               CORBA.NVList.Add_Item
                 (Argument_List_ü,
                  Arg_Name_U_data,
                  Argument_U_data,
                  CORBA.ARG_IN);
               CORBA.ServerRequest.Arguments
                 (Request,
                  Argument_List_ü);
               --  Convert from Any
               data :=
                 CORBA.From_Any
                    (Argument_U_data);
               --  Call Implementation
               Result_ü :=
                 test_hash.Impl.echoLong04
                    (test_hash.Impl.Object'Class
                       (Self.all)'Access,
                     data);
               --  Setting the result
               CORBA.ServerRequest.Set_Result
                 (Request,
                  CORBA.To_Any
                    (Result_ü));
            end;
         elsif (Operation_ü
            = "echoLong05")
         then
            declare
               data : CORBA.Long;
               Arg_Name_U_data : constant CORBA.Identifier :=
                 CORBA.To_CORBA_String
                    ("data");
               Argument_U_data : CORBA.Any :=
                 CORBA.Internals.Get_Empty_Any
                    (CORBA.TC_Long);
               Result_ü : CORBA.Long;
            begin
               CORBA.NVList.Add_Item
                 (Argument_List_ü,
                  Arg_Name_U_data,
                  Argument_U_data,
                  CORBA.ARG_IN);
               CORBA.ServerRequest.Arguments
                 (Request,
                  Argument_List_ü);
               --  Convert from Any
               data :=
                 CORBA.From_Any
                    (Argument_U_data);
               --  Call Implementation
               Result_ü :=
                 test_hash.Impl.echoLong05
                    (test_hash.Impl.Object'Class
                       (Self.all)'Access,
                     data);
               --  Setting the result
               CORBA.ServerRequest.Set_Result
                 (Request,
                  CORBA.To_Any
                    (Result_ü));
            end;
         elsif (Operation_ü
            = "echoLong06")
         then
            declare
               data : CORBA.Long;
               Arg_Name_U_data : constant CORBA.Identifier :=
                 CORBA.To_CORBA_String
                    ("data");
               Argument_U_data : CORBA.Any :=
                 CORBA.Internals.Get_Empty_Any
                    (CORBA.TC_Long);
               Result_ü : CORBA.Long;
            begin
               CORBA.NVList.Add_Item
                 (Argument_List_ü,
                  Arg_Name_U_data,
                  Argument_U_data,
                  CORBA.ARG_IN);
               CORBA.ServerRequest.Arguments
                 (Request,
                  Argument_List_ü);
               --  Convert from Any
               data :=
                 CORBA.From_Any
                    (Argument_U_data);
               --  Call Implementation
               Result_ü :=
                 test_hash.Impl.echoLong06
                    (test_hash.Impl.Object'Class
                       (Self.all)'Access,
                     data);
               --  Setting the result
               CORBA.ServerRequest.Set_Result
                 (Request,
                  CORBA.To_Any
                    (Result_ü));
            end;
         elsif (Operation_ü
            = "echoLong07")
         then
            declare
               data : CORBA.Long;
               Arg_Name_U_data : constant CORBA.Identifier :=
                 CORBA.To_CORBA_String
                    ("data");
               Argument_U_data : CORBA.Any :=
                 CORBA.Internals.Get_Empty_Any
                    (CORBA.TC_Long);
               Result_ü : CORBA.Long;
            begin
               CORBA.NVList.Add_Item
                 (Argument_List_ü,
                  Arg_Name_U_data,
                  Argument_U_data,
                  CORBA.ARG_IN);
               CORBA.ServerRequest.Arguments
                 (Request,
                  Argument_List_ü);
               --  Convert from Any
               data :=
                 CORBA.From_Any
                    (Argument_U_data);
               --  Call Implementation
               Result_ü :=
                 test_hash.Impl.echoLong07
                    (test_hash.Impl.Object'Class
                       (Self.all)'Access,
                     data);
               --  Setting the result
               CORBA.ServerRequest.Set_Result
                 (Request,
                  CORBA.To_Any
                    (Result_ü));
            end;
         elsif (Operation_ü
            = "echoLong08")
         then
            declare
               data : CORBA.Long;
               Arg_Name_U_data : constant CORBA.Identifier :=
                 CORBA.To_CORBA_String
                    ("data");
               Argument_U_data : CORBA.Any :=
                 CORBA.Internals.Get_Empty_Any
                    (CORBA.TC_Long);
               Result_ü : CORBA.Long;
            begin
               CORBA.NVList.Add_Item
                 (Argument_List_ü,
                  Arg_Name_U_data,
                  Argument_U_data,
                  CORBA.ARG_IN);
               CORBA.ServerRequest.Arguments
                 (Request,
                  Argument_List_ü);
               --  Convert from Any
               data :=
                 CORBA.From_Any
                    (Argument_U_data);
               --  Call Implementation
               Result_ü :=
                 test_hash.Impl.echoLong08
                    (test_hash.Impl.Object'Class
                       (Self.all)'Access,
                     data);
               --  Setting the result
               CORBA.ServerRequest.Set_Result
                 (Request,
                  CORBA.To_Any
                    (Result_ü));
            end;
         elsif (Operation_ü
            = "echoLong09")
         then
            declare
               data : CORBA.Long;
               Arg_Name_U_data : constant CORBA.Identifier :=
                 CORBA.To_CORBA_String
                    ("data");
               Argument_U_data : CORBA.Any :=
                 CORBA.Internals.Get_Empty_Any
                    (CORBA.TC_Long);
               Result_ü : CORBA.Long;
            begin
               CORBA.NVList.Add_Item
                 (Argument_List_ü,
                  Arg_Name_U_data,
                  Argument_U_data,
                  CORBA.ARG_IN);
               CORBA.ServerRequest.Arguments
                 (Request,
                  Argument_List_ü);
               --  Convert from Any
               data :=
                 CORBA.From_Any
                    (Argument_U_data);
               --  Call Implementation
               Result_ü :=
                 test_hash.Impl.echoLong09
                    (test_hash.Impl.Object'Class
                       (Self.all)'Access,
                     data);
               --  Setting the result
               CORBA.ServerRequest.Set_Result
                 (Request,
                  CORBA.To_Any
                    (Result_ü));
            end;
         elsif (Operation_ü
            = "echoLong10")
         then
            declare
               data : CORBA.Long;
               Arg_Name_U_data : constant CORBA.Identifier :=
                 CORBA.To_CORBA_String
                    ("data");
               Argument_U_data : CORBA.Any :=
                 CORBA.Internals.Get_Empty_Any
                    (CORBA.TC_Long);
               Result_ü : CORBA.Long;
            begin
               CORBA.NVList.Add_Item
                 (Argument_List_ü,
                  Arg_Name_U_data,
                  Argument_U_data,
                  CORBA.ARG_IN);
               CORBA.ServerRequest.Arguments
                 (Request,
                  Argument_List_ü);
               --  Convert from Any
               data :=
                 CORBA.From_Any
                    (Argument_U_data);
               --  Call Implementation
               Result_ü :=
                 test_hash.Impl.echoLong10
                    (test_hash.Impl.Object'Class
                       (Self.all)'Access,
                     data);
               --  Setting the result
               CORBA.ServerRequest.Set_Result
                 (Request,
                  CORBA.To_Any
                    (Result_ü));
            end;
         elsif (Operation_ü
            = "echoLong11")
         then
            declare
               data : CORBA.Long;
               Arg_Name_U_data : constant CORBA.Identifier :=
                 CORBA.To_CORBA_String
                    ("data");
               Argument_U_data : CORBA.Any :=
                 CORBA.Internals.Get_Empty_Any
                    (CORBA.TC_Long);
               Result_ü : CORBA.Long;
            begin
               CORBA.NVList.Add_Item
                 (Argument_List_ü,
                  Arg_Name_U_data,
                  Argument_U_data,
                  CORBA.ARG_IN);
               CORBA.ServerRequest.Arguments
                 (Request,
                  Argument_List_ü);
               --  Convert from Any
               data :=
                 CORBA.From_Any
                    (Argument_U_data);
               --  Call Implementation
               Result_ü :=
                 test_hash.Impl.echoLong11
                    (test_hash.Impl.Object'Class
                       (Self.all)'Access,
                     data);
               --  Setting the result
               CORBA.ServerRequest.Set_Result
                 (Request,
                  CORBA.To_Any
                    (Result_ü));
            end;
         elsif (Operation_ü
            = "echoLong12")
         then
            declare
               data : CORBA.Long;
               Arg_Name_U_data : constant CORBA.Identifier :=
                 CORBA.To_CORBA_String
                    ("data");
               Argument_U_data : CORBA.Any :=
                 CORBA.Internals.Get_Empty_Any
                    (CORBA.TC_Long);
               Result_ü : CORBA.Long;
            begin
               CORBA.NVList.Add_Item
                 (Argument_List_ü,
                  Arg_Name_U_data,
                  Argument_U_data,
                  CORBA.ARG_IN);
               CORBA.ServerRequest.Arguments
                 (Request,
                  Argument_List_ü);
               --  Convert from Any
               data :=
                 CORBA.From_Any
                    (Argument_U_data);
               --  Call Implementation
               Result_ü :=
                 test_hash.Impl.echoLong12
                    (test_hash.Impl.Object'Class
                       (Self.all)'Access,
                     data);
               --  Setting the result
               CORBA.ServerRequest.Set_Result
                 (Request,
                  CORBA.To_Any
                    (Result_ü));
            end;
         elsif (Operation_ü
            = "echoLong13")
         then
            declare
               data : CORBA.Long;
               Arg_Name_U_data : constant CORBA.Identifier :=
                 CORBA.To_CORBA_String
                    ("data");
               Argument_U_data : CORBA.Any :=
                 CORBA.Internals.Get_Empty_Any
                    (CORBA.TC_Long);
               Result_ü : CORBA.Long;
            begin
               CORBA.NVList.Add_Item
                 (Argument_List_ü,
                  Arg_Name_U_data,
                  Argument_U_data,
                  CORBA.ARG_IN);
               CORBA.ServerRequest.Arguments
                 (Request,
                  Argument_List_ü);
               --  Convert from Any
               data :=
                 CORBA.From_Any
                    (Argument_U_data);
               --  Call Implementation
               Result_ü :=
                 test_hash.Impl.echoLong13
                    (test_hash.Impl.Object'Class
                       (Self.all)'Access,
                     data);
               --  Setting the result
               CORBA.ServerRequest.Set_Result
                 (Request,
                  CORBA.To_Any
                    (Result_ü));
            end;
         elsif (Operation_ü
            = "echoLong14")
         then
            declare
               data : CORBA.Long;
               Arg_Name_U_data : constant CORBA.Identifier :=
                 CORBA.To_CORBA_String
                    ("data");
               Argument_U_data : CORBA.Any :=
                 CORBA.Internals.Get_Empty_Any
                    (CORBA.TC_Long);
               Result_ü : CORBA.Long;
            begin
               CORBA.NVList.Add_Item
                 (Argument_List_ü,
                  Arg_Name_U_data,
                  Argument_U_data,
                  CORBA.ARG_IN);
               CORBA.ServerRequest.Arguments
                 (Request,
                  Argument_List_ü);
               --  Convert from Any
               data :=
                 CORBA.From_Any
                    (Argument_U_data);
               --  Call Implementation
               Result_ü :=
                 test_hash.Impl.echoLong14
                    (test_hash.Impl.Object'Class
                       (Self.all)'Access,
                     data);
               --  Setting the result
               CORBA.ServerRequest.Set_Result
                 (Request,
                  CORBA.To_Any
                    (Result_ü));
            end;
         elsif (Operation_ü
            = "echoLong15")
         then
            declare
               data : CORBA.Long;
               Arg_Name_U_data : constant CORBA.Identifier :=
                 CORBA.To_CORBA_String
                    ("data");
               Argument_U_data : CORBA.Any :=
                 CORBA.Internals.Get_Empty_Any
                    (CORBA.TC_Long);
               Result_ü : CORBA.Long;
            begin
               CORBA.NVList.Add_Item
                 (Argument_List_ü,
                  Arg_Name_U_data,
                  Argument_U_data,
                  CORBA.ARG_IN);
               CORBA.ServerRequest.Arguments
                 (Request,
                  Argument_List_ü);
               --  Convert from Any
               data :=
                 CORBA.From_Any
                    (Argument_U_data);
               --  Call Implementation
               Result_ü :=
                 test_hash.Impl.echoLong15
                    (test_hash.Impl.Object'Class
                       (Self.all)'Access,
                     data);
               --  Setting the result
               CORBA.ServerRequest.Set_Result
                 (Request,
                  CORBA.To_Any
                    (Result_ü));
            end;
         elsif (Operation_ü
            = "echoLong16")
         then
            declare
               data : CORBA.Long;
               Arg_Name_U_data : constant CORBA.Identifier :=
                 CORBA.To_CORBA_String
                    ("data");
               Argument_U_data : CORBA.Any :=
                 CORBA.Internals.Get_Empty_Any
                    (CORBA.TC_Long);
               Result_ü : CORBA.Long;
            begin
               CORBA.NVList.Add_Item
                 (Argument_List_ü,
                  Arg_Name_U_data,
                  Argument_U_data,
                  CORBA.ARG_IN);
               CORBA.ServerRequest.Arguments
                 (Request,
                  Argument_List_ü);
               --  Convert from Any
               data :=
                 CORBA.From_Any
                    (Argument_U_data);
               --  Call Implementation
               Result_ü :=
                 test_hash.Impl.echoLong16
                    (test_hash.Impl.Object'Class
                       (Self.all)'Access,
                     data);
               --  Setting the result
               CORBA.ServerRequest.Set_Result
                 (Request,
                  CORBA.To_Any
                    (Result_ü));
            end;
         elsif (Operation_ü
            = "echoLong17")
         then
            declare
               data : CORBA.Long;
               Arg_Name_U_data : constant CORBA.Identifier :=
                 CORBA.To_CORBA_String
                    ("data");
               Argument_U_data : CORBA.Any :=
                 CORBA.Internals.Get_Empty_Any
                    (CORBA.TC_Long);
               Result_ü : CORBA.Long;
            begin
               CORBA.NVList.Add_Item
                 (Argument_List_ü,
                  Arg_Name_U_data,
                  Argument_U_data,
                  CORBA.ARG_IN);
               CORBA.ServerRequest.Arguments
                 (Request,
                  Argument_List_ü);
               --  Convert from Any
               data :=
                 CORBA.From_Any
                    (Argument_U_data);
               --  Call Implementation
               Result_ü :=
                 test_hash.Impl.echoLong17
                    (test_hash.Impl.Object'Class
                       (Self.all)'Access,
                     data);
               --  Setting the result
               CORBA.ServerRequest.Set_Result
                 (Request,
                  CORBA.To_Any
                    (Result_ü));
            end;
         elsif (Operation_ü
            = "echoLong18")
         then
            declare
               data : CORBA.Long;
               Arg_Name_U_data : constant CORBA.Identifier :=
                 CORBA.To_CORBA_String
                    ("data");
               Argument_U_data : CORBA.Any :=
                 CORBA.Internals.Get_Empty_Any
                    (CORBA.TC_Long);
               Result_ü : CORBA.Long;
            begin
               CORBA.NVList.Add_Item
                 (Argument_List_ü,
                  Arg_Name_U_data,
                  Argument_U_data,
                  CORBA.ARG_IN);
               CORBA.ServerRequest.Arguments
                 (Request,
                  Argument_List_ü);
               --  Convert from Any
               data :=
                 CORBA.From_Any
                    (Argument_U_data);
               --  Call Implementation
               Result_ü :=
                 test_hash.Impl.echoLong18
                    (test_hash.Impl.Object'Class
                       (Self.all)'Access,
                     data);
               --  Setting the result
               CORBA.ServerRequest.Set_Result
                 (Request,
                  CORBA.To_Any
                    (Result_ü));
            end;
         elsif (Operation_ü
            = "echoLong19")
         then
            declare
               data : CORBA.Long;
               Arg_Name_U_data : constant CORBA.Identifier :=
                 CORBA.To_CORBA_String
                    ("data");
               Argument_U_data : CORBA.Any :=
                 CORBA.Internals.Get_Empty_Any
                    (CORBA.TC_Long);
               Result_ü : CORBA.Long;
            begin
               CORBA.NVList.Add_Item
                 (Argument_List_ü,
                  Arg_Name_U_data,
                  Argument_U_data,
                  CORBA.ARG_IN);
               CORBA.ServerRequest.Arguments
                 (Request,
                  Argument_List_ü);
               --  Convert from Any
               data :=
                 CORBA.From_Any
                    (Argument_U_data);
               --  Call Implementation
               Result_ü :=
                 test_hash.Impl.echoLong19
                    (test_hash.Impl.Object'Class
                       (Self.all)'Access,
                     data);
               --  Setting the result
               CORBA.ServerRequest.Set_Result
                 (Request,
                  CORBA.To_Any
                    (Result_ü));
            end;
         elsif (Operation_ü
            = "echoLong20")
         then
            declare
               data : CORBA.Long;
               Arg_Name_U_data : constant CORBA.Identifier :=
                 CORBA.To_CORBA_String
                    ("data");
               Argument_U_data : CORBA.Any :=
                 CORBA.Internals.Get_Empty_Any
                    (CORBA.TC_Long);
               Result_ü : CORBA.Long;
            begin
               CORBA.NVList.Add_Item
                 (Argument_List_ü,
                  Arg_Name_U_data,
                  Argument_U_data,
                  CORBA.ARG_IN);
               CORBA.ServerRequest.Arguments
                 (Request,
                  Argument_List_ü);
               --  Convert from Any
               data :=
                 CORBA.From_Any
                    (Argument_U_data);
               --  Call Implementation
               Result_ü :=
                 test_hash.Impl.echoLong20
                    (test_hash.Impl.Object'Class
                       (Self.all)'Access,
                     data);
               --  Setting the result
               CORBA.ServerRequest.Set_Result
                 (Request,
                  CORBA.To_Any
                    (Result_ü));
            end;
         elsif (Operation_ü
            = "echoLong21")
         then
            declare
               data : CORBA.Long;
               Arg_Name_U_data : constant CORBA.Identifier :=
                 CORBA.To_CORBA_String
                    ("data");
               Argument_U_data : CORBA.Any :=
                 CORBA.Internals.Get_Empty_Any
                    (CORBA.TC_Long);
               Result_ü : CORBA.Long;
            begin
               CORBA.NVList.Add_Item
                 (Argument_List_ü,
                  Arg_Name_U_data,
                  Argument_U_data,
                  CORBA.ARG_IN);
               CORBA.ServerRequest.Arguments
                 (Request,
                  Argument_List_ü);
               --  Convert from Any
               data :=
                 CORBA.From_Any
                    (Argument_U_data);
               --  Call Implementation
               Result_ü :=
                 test_hash.Impl.echoLong21
                    (test_hash.Impl.Object'Class
                       (Self.all)'Access,
                     data);
               --  Setting the result
               CORBA.ServerRequest.Set_Result
                 (Request,
                  CORBA.To_Any
                    (Result_ü));
            end;
         elsif (Operation_ü
            = "echoLong22")
         then
            declare
               data : CORBA.Long;
               Arg_Name_U_data : constant CORBA.Identifier :=
                 CORBA.To_CORBA_String
                    ("data");
               Argument_U_data : CORBA.Any :=
                 CORBA.Internals.Get_Empty_Any
                    (CORBA.TC_Long);
               Result_ü : CORBA.Long;
            begin
               CORBA.NVList.Add_Item
                 (Argument_List_ü,
                  Arg_Name_U_data,
                  Argument_U_data,
                  CORBA.ARG_IN);
               CORBA.ServerRequest.Arguments
                 (Request,
                  Argument_List_ü);
               --  Convert from Any
               data :=
                 CORBA.From_Any
                    (Argument_U_data);
               --  Call Implementation
               Result_ü :=
                 test_hash.Impl.echoLong22
                    (test_hash.Impl.Object'Class
                       (Self.all)'Access,
                     data);
               --  Setting the result
               CORBA.ServerRequest.Set_Result
                 (Request,
                  CORBA.To_Any
                    (Result_ü));
            end;
         elsif (Operation_ü
            = "echoLong23")
         then
            declare
               data : CORBA.Long;
               Arg_Name_U_data : constant CORBA.Identifier :=
                 CORBA.To_CORBA_String
                    ("data");
               Argument_U_data : CORBA.Any :=
                 CORBA.Internals.Get_Empty_Any
                    (CORBA.TC_Long);
               Result_ü : CORBA.Long;
            begin
               CORBA.NVList.Add_Item
                 (Argument_List_ü,
                  Arg_Name_U_data,
                  Argument_U_data,
                  CORBA.ARG_IN);
               CORBA.ServerRequest.Arguments
                 (Request,
                  Argument_List_ü);
               --  Convert from Any
               data :=
                 CORBA.From_Any
                    (Argument_U_data);
               --  Call Implementation
               Result_ü :=
                 test_hash.Impl.echoLong23
                    (test_hash.Impl.Object'Class
                       (Self.all)'Access,
                     data);
               --  Setting the result
               CORBA.ServerRequest.Set_Result
                 (Request,
                  CORBA.To_Any
                    (Result_ü));
            end;
         elsif (Operation_ü
            = "echoLong24")
         then
            declare
               data : CORBA.Long;
               Arg_Name_U_data : constant CORBA.Identifier :=
                 CORBA.To_CORBA_String
                    ("data");
               Argument_U_data : CORBA.Any :=
                 CORBA.Internals.Get_Empty_Any
                    (CORBA.TC_Long);
               Result_ü : CORBA.Long;
            begin
               CORBA.NVList.Add_Item
                 (Argument_List_ü,
                  Arg_Name_U_data,
                  Argument_U_data,
                  CORBA.ARG_IN);
               CORBA.ServerRequest.Arguments
                 (Request,
                  Argument_List_ü);
               --  Convert from Any
               data :=
                 CORBA.From_Any
                    (Argument_U_data);
               --  Call Implementation
               Result_ü :=
                 test_hash.Impl.echoLong24
                    (test_hash.Impl.Object'Class
                       (Self.all)'Access,
                     data);
               --  Setting the result
               CORBA.ServerRequest.Set_Result
                 (Request,
                  CORBA.To_Any
                    (Result_ü));
            end;
         elsif (Operation_ü
            = "echoLong25")
         then
            declare
               data : CORBA.Long;
               Arg_Name_U_data : constant CORBA.Identifier :=
                 CORBA.To_CORBA_String
                    ("data");
               Argument_U_data : CORBA.Any :=
                 CORBA.Internals.Get_Empty_Any
                    (CORBA.TC_Long);
               Result_ü : CORBA.Long;
            begin
               CORBA.NVList.Add_Item
                 (Argument_List_ü,
                  Arg_Name_U_data,
                  Argument_U_data,
                  CORBA.ARG_IN);
               CORBA.ServerRequest.Arguments
                 (Request,
                  Argument_List_ü);
               --  Convert from Any
               data :=
                 CORBA.From_Any
                    (Argument_U_data);
               --  Call Implementation
               Result_ü :=
                 test_hash.Impl.echoLong25
                    (test_hash.Impl.Object'Class
                       (Self.all)'Access,
                     data);
               --  Setting the result
               CORBA.ServerRequest.Set_Result
                 (Request,
                  CORBA.To_Any
                    (Result_ü));
            end;
         elsif (Operation_ü
            = "echoLong26")
         then
            declare
               data : CORBA.Long;
               Arg_Name_U_data : constant CORBA.Identifier :=
                 CORBA.To_CORBA_String
                    ("data");
               Argument_U_data : CORBA.Any :=
                 CORBA.Internals.Get_Empty_Any
                    (CORBA.TC_Long);
               Result_ü : CORBA.Long;
            begin
               CORBA.NVList.Add_Item
                 (Argument_List_ü,
                  Arg_Name_U_data,
                  Argument_U_data,
                  CORBA.ARG_IN);
               CORBA.ServerRequest.Arguments
                 (Request,
                  Argument_List_ü);
               --  Convert from Any
               data :=
                 CORBA.From_Any
                    (Argument_U_data);
               --  Call Implementation
               Result_ü :=
                 test_hash.Impl.echoLong26
                    (test_hash.Impl.Object'Class
                       (Self.all)'Access,
                     data);
               --  Setting the result
               CORBA.ServerRequest.Set_Result
                 (Request,
                  CORBA.To_Any
                    (Result_ü));
            end;
         elsif (Operation_ü
            = "echoLong27")
         then
            declare
               data : CORBA.Long;
               Arg_Name_U_data : constant CORBA.Identifier :=
                 CORBA.To_CORBA_String
                    ("data");
               Argument_U_data : CORBA.Any :=
                 CORBA.Internals.Get_Empty_Any
                    (CORBA.TC_Long);
               Result_ü : CORBA.Long;
            begin
               CORBA.NVList.Add_Item
                 (Argument_List_ü,
                  Arg_Name_U_data,
                  Argument_U_data,
                  CORBA.ARG_IN);
               CORBA.ServerRequest.Arguments
                 (Request,
                  Argument_List_ü);
               --  Convert from Any
               data :=
                 CORBA.From_Any
                    (Argument_U_data);
               --  Call Implementation
               Result_ü :=
                 test_hash.Impl.echoLong27
                    (test_hash.Impl.Object'Class
                       (Self.all)'Access,
                     data);
               --  Setting the result
               CORBA.ServerRequest.Set_Result
                 (Request,
                  CORBA.To_Any
                    (Result_ü));
            end;
         elsif (Operation_ü
            = "echoLong28")
         then
            declare
               data : CORBA.Long;
               Arg_Name_U_data : constant CORBA.Identifier :=
                 CORBA.To_CORBA_String
                    ("data");
               Argument_U_data : CORBA.Any :=
                 CORBA.Internals.Get_Empty_Any
                    (CORBA.TC_Long);
               Result_ü : CORBA.Long;
            begin
               CORBA.NVList.Add_Item
                 (Argument_List_ü,
                  Arg_Name_U_data,
                  Argument_U_data,
                  CORBA.ARG_IN);
               CORBA.ServerRequest.Arguments
                 (Request,
                  Argument_List_ü);
               --  Convert from Any
               data :=
                 CORBA.From_Any
                    (Argument_U_data);
               --  Call Implementation
               Result_ü :=
                 test_hash.Impl.echoLong28
                    (test_hash.Impl.Object'Class
                       (Self.all)'Access,
                     data);
               --  Setting the result
               CORBA.ServerRequest.Set_Result
                 (Request,
                  CORBA.To_Any
                    (Result_ü));
            end;
         elsif (Operation_ü
            = "echoLong29")
         then
            declare
               data : CORBA.Long;
               Arg_Name_U_data : constant CORBA.Identifier :=
                 CORBA.To_CORBA_String
                    ("data");
               Argument_U_data : CORBA.Any :=
                 CORBA.Internals.Get_Empty_Any
                    (CORBA.TC_Long);
               Result_ü : CORBA.Long;
            begin
               CORBA.NVList.Add_Item
                 (Argument_List_ü,
                  Arg_Name_U_data,
                  Argument_U_data,
                  CORBA.ARG_IN);
               CORBA.ServerRequest.Arguments
                 (Request,
                  Argument_List_ü);
               --  Convert from Any
               data :=
                 CORBA.From_Any
                    (Argument_U_data);
               --  Call Implementation
               Result_ü :=
                 test_hash.Impl.echoLong29
                    (test_hash.Impl.Object'Class
                       (Self.all)'Access,
                     data);
               --  Setting the result
               CORBA.ServerRequest.Set_Result
                 (Request,
                  CORBA.To_Any
                    (Result_ü));
            end;
         elsif (Operation_ü
            = "echoLong30")
         then
            declare
               data : CORBA.Long;
               Arg_Name_U_data : constant CORBA.Identifier :=
                 CORBA.To_CORBA_String
                    ("data");
               Argument_U_data : CORBA.Any :=
                 CORBA.Internals.Get_Empty_Any
                    (CORBA.TC_Long);
               Result_ü : CORBA.Long;
            begin
               CORBA.NVList.Add_Item
                 (Argument_List_ü,
                  Arg_Name_U_data,
                  Argument_U_data,
                  CORBA.ARG_IN);
               CORBA.ServerRequest.Arguments
                 (Request,
                  Argument_List_ü);
               --  Convert from Any
               data :=
                 CORBA.From_Any
                    (Argument_U_data);
               --  Call Implementation
               Result_ü :=
                 test_hash.Impl.echoLong30
                    (test_hash.Impl.Object'Class
                       (Self.all)'Access,
                     data);
               --  Setting the result
               CORBA.ServerRequest.Set_Result
                 (Request,
                  CORBA.To_Any
                    (Result_ü));
            end;
         elsif (Operation_ü
            = "echoLong31")
         then
            declare
               data : CORBA.Long;
               Arg_Name_U_data : constant CORBA.Identifier :=
                 CORBA.To_CORBA_String
                    ("data");
               Argument_U_data : CORBA.Any :=
                 CORBA.Internals.Get_Empty_Any
                    (CORBA.TC_Long);
               Result_ü : CORBA.Long;
            begin
               CORBA.NVList.Add_Item
                 (Argument_List_ü,
                  Arg_Name_U_data,
                  Argument_U_data,
                  CORBA.ARG_IN);
               CORBA.ServerRequest.Arguments
                 (Request,
                  Argument_List_ü);
               --  Convert from Any
               data :=
                 CORBA.From_Any
                    (Argument_U_data);
               --  Call Implementation
               Result_ü :=
                 test_hash.Impl.echoLong31
                    (test_hash.Impl.Object'Class
                       (Self.all)'Access,
                     data);
               --  Setting the result
               CORBA.ServerRequest.Set_Result
                 (Request,
                  CORBA.To_Any
                    (Result_ü));
            end;
         elsif (Operation_ü
            = "echoLong32")
         then
            declare
               data : CORBA.Long;
               Arg_Name_U_data : constant CORBA.Identifier :=
                 CORBA.To_CORBA_String
                    ("data");
               Argument_U_data : CORBA.Any :=
                 CORBA.Internals.Get_Empty_Any
                    (CORBA.TC_Long);
               Result_ü : CORBA.Long;
            begin
               CORBA.NVList.Add_Item
                 (Argument_List_ü,
                  Arg_Name_U_data,
                  Argument_U_data,
                  CORBA.ARG_IN);
               CORBA.ServerRequest.Arguments
                 (Request,
                  Argument_List_ü);
               --  Convert from Any
               data :=
                 CORBA.From_Any
                    (Argument_U_data);
               --  Call Implementation
               Result_ü :=
                 test_hash.Impl.echoLong32
                    (test_hash.Impl.Object'Class
                       (Self.all)'Access,
                     data);
               --  Setting the result
               CORBA.ServerRequest.Set_Result
                 (Request,
                  CORBA.To_Any
                    (Result_ü));
            end;
         elsif (Operation_ü
            = "echoLong33")
         then
            declare
               data : CORBA.Long;
               Arg_Name_U_data : constant CORBA.Identifier :=
                 CORBA.To_CORBA_String
                    ("data");
               Argument_U_data : CORBA.Any :=
                 CORBA.Internals.Get_Empty_Any
                    (CORBA.TC_Long);
               Result_ü : CORBA.Long;
            begin
               CORBA.NVList.Add_Item
                 (Argument_List_ü,
                  Arg_Name_U_data,
                  Argument_U_data,
                  CORBA.ARG_IN);
               CORBA.ServerRequest.Arguments
                 (Request,
                  Argument_List_ü);
               --  Convert from Any
               data :=
                 CORBA.From_Any
                    (Argument_U_data);
               --  Call Implementation
               Result_ü :=
                 test_hash.Impl.echoLong33
                    (test_hash.Impl.Object'Class
                       (Self.all)'Access,
                     data);
               --  Setting the result
               CORBA.ServerRequest.Set_Result
                 (Request,
                  CORBA.To_Any
                    (Result_ü));
            end;
         elsif (Operation_ü
            = "echoLong34")
         then
            declare
               data : CORBA.Long;
               Arg_Name_U_data : constant CORBA.Identifier :=
                 CORBA.To_CORBA_String
                    ("data");
               Argument_U_data : CORBA.Any :=
                 CORBA.Internals.Get_Empty_Any
                    (CORBA.TC_Long);
               Result_ü : CORBA.Long;
            begin
               CORBA.NVList.Add_Item
                 (Argument_List_ü,
                  Arg_Name_U_data,
                  Argument_U_data,
                  CORBA.ARG_IN);
               CORBA.ServerRequest.Arguments
                 (Request,
                  Argument_List_ü);
               --  Convert from Any
               data :=
                 CORBA.From_Any
                    (Argument_U_data);
               --  Call Implementation
               Result_ü :=
                 test_hash.Impl.echoLong34
                    (test_hash.Impl.Object'Class
                       (Self.all)'Access,
                     data);
               --  Setting the result
               CORBA.ServerRequest.Set_Result
                 (Request,
                  CORBA.To_Any
                    (Result_ü));
            end;
         elsif (Operation_ü
            = "echoLong35")
         then
            declare
               data : CORBA.Long;
               Arg_Name_U_data : constant CORBA.Identifier :=
                 CORBA.To_CORBA_String
                    ("data");
               Argument_U_data : CORBA.Any :=
                 CORBA.Internals.Get_Empty_Any
                    (CORBA.TC_Long);
               Result_ü : CORBA.Long;
            begin
               CORBA.NVList.Add_Item
                 (Argument_List_ü,
                  Arg_Name_U_data,
                  Argument_U_data,
                  CORBA.ARG_IN);
               CORBA.ServerRequest.Arguments
                 (Request,
                  Argument_List_ü);
               --  Convert from Any
               data :=
                 CORBA.From_Any
                    (Argument_U_data);
               --  Call Implementation
               Result_ü :=
                 test_hash.Impl.echoLong35
                    (test_hash.Impl.Object'Class
                       (Self.all)'Access,
                     data);
               --  Setting the result
               CORBA.ServerRequest.Set_Result
                 (Request,
                  CORBA.To_Any
                    (Result_ü));
            end;
         elsif (Operation_ü
            = "echoLong36")
         then
            declare
               data : CORBA.Long;
               Arg_Name_U_data : constant CORBA.Identifier :=
                 CORBA.To_CORBA_String
                    ("data");
               Argument_U_data : CORBA.Any :=
                 CORBA.Internals.Get_Empty_Any
                    (CORBA.TC_Long);
               Result_ü : CORBA.Long;
            begin
               CORBA.NVList.Add_Item
                 (Argument_List_ü,
                  Arg_Name_U_data,
                  Argument_U_data,
                  CORBA.ARG_IN);
               CORBA.ServerRequest.Arguments
                 (Request,
                  Argument_List_ü);
               --  Convert from Any
               data :=
                 CORBA.From_Any
                    (Argument_U_data);
               --  Call Implementation
               Result_ü :=
                 test_hash.Impl.echoLong36
                    (test_hash.Impl.Object'Class
                       (Self.all)'Access,
                     data);
               --  Setting the result
               CORBA.ServerRequest.Set_Result
                 (Request,
                  CORBA.To_Any
                    (Result_ü));
            end;
         elsif (Operation_ü
            = "echoLong37")
         then
            declare
               data : CORBA.Long;
               Arg_Name_U_data : constant CORBA.Identifier :=
                 CORBA.To_CORBA_String
                    ("data");
               Argument_U_data : CORBA.Any :=
                 CORBA.Internals.Get_Empty_Any
                    (CORBA.TC_Long);
               Result_ü : CORBA.Long;
            begin
               CORBA.NVList.Add_Item
                 (Argument_List_ü,
                  Arg_Name_U_data,
                  Argument_U_data,
                  CORBA.ARG_IN);
               CORBA.ServerRequest.Arguments
                 (Request,
                  Argument_List_ü);
               --  Convert from Any
               data :=
                 CORBA.From_Any
                    (Argument_U_data);
               --  Call Implementation
               Result_ü :=
                 test_hash.Impl.echoLong37
                    (test_hash.Impl.Object'Class
                       (Self.all)'Access,
                     data);
               --  Setting the result
               CORBA.ServerRequest.Set_Result
                 (Request,
                  CORBA.To_Any
                    (Result_ü));
            end;
         elsif (Operation_ü
            = "echoLong38")
         then
            declare
               data : CORBA.Long;
               Arg_Name_U_data : constant CORBA.Identifier :=
                 CORBA.To_CORBA_String
                    ("data");
               Argument_U_data : CORBA.Any :=
                 CORBA.Internals.Get_Empty_Any
                    (CORBA.TC_Long);
               Result_ü : CORBA.Long;
            begin
               CORBA.NVList.Add_Item
                 (Argument_List_ü,
                  Arg_Name_U_data,
                  Argument_U_data,
                  CORBA.ARG_IN);
               CORBA.ServerRequest.Arguments
                 (Request,
                  Argument_List_ü);
               --  Convert from Any
               data :=
                 CORBA.From_Any
                    (Argument_U_data);
               --  Call Implementation
               Result_ü :=
                 test_hash.Impl.echoLong38
                    (test_hash.Impl.Object'Class
                       (Self.all)'Access,
                     data);
               --  Setting the result
               CORBA.ServerRequest.Set_Result
                 (Request,
                  CORBA.To_Any
                    (Result_ü));
            end;
         elsif (Operation_ü
            = "echoLong39")
         then
            declare
               data : CORBA.Long;
               Arg_Name_U_data : constant CORBA.Identifier :=
                 CORBA.To_CORBA_String
                    ("data");
               Argument_U_data : CORBA.Any :=
                 CORBA.Internals.Get_Empty_Any
                    (CORBA.TC_Long);
               Result_ü : CORBA.Long;
            begin
               CORBA.NVList.Add_Item
                 (Argument_List_ü,
                  Arg_Name_U_data,
                  Argument_U_data,
                  CORBA.ARG_IN);
               CORBA.ServerRequest.Arguments
                 (Request,
                  Argument_List_ü);
               --  Convert from Any
               data :=
                 CORBA.From_Any
                    (Argument_U_data);
               --  Call Implementation
               Result_ü :=
                 test_hash.Impl.echoLong39
                    (test_hash.Impl.Object'Class
                       (Self.all)'Access,
                     data);
               --  Setting the result
               CORBA.ServerRequest.Set_Result
                 (Request,
                  CORBA.To_Any
                    (Result_ü));
            end;
         elsif (Operation_ü
            = "echoLong40")
         then
            declare
               data : CORBA.Long;
               Arg_Name_U_data : constant CORBA.Identifier :=
                 CORBA.To_CORBA_String
                    ("data");
               Argument_U_data : CORBA.Any :=
                 CORBA.Internals.Get_Empty_Any
                    (CORBA.TC_Long);
               Result_ü : CORBA.Long;
            begin
               CORBA.NVList.Add_Item
                 (Argument_List_ü,
                  Arg_Name_U_data,
                  Argument_U_data,
                  CORBA.ARG_IN);
               CORBA.ServerRequest.Arguments
                 (Request,
                  Argument_List_ü);
               --  Convert from Any
               data :=
                 CORBA.From_Any
                    (Argument_U_data);
               --  Call Implementation
               Result_ü :=
                 test_hash.Impl.echoLong40
                    (test_hash.Impl.Object'Class
                       (Self.all)'Access,
                     data);
               --  Setting the result
               CORBA.ServerRequest.Set_Result
                 (Request,
                  CORBA.To_Any
                    (Result_ü));
            end;
         elsif (Operation_ü
            = "echoLong41")
         then
            declare
               data : CORBA.Long;
               Arg_Name_U_data : constant CORBA.Identifier :=
                 CORBA.To_CORBA_String
                    ("data");
               Argument_U_data : CORBA.Any :=
                 CORBA.Internals.Get_Empty_Any
                    (CORBA.TC_Long);
               Result_ü : CORBA.Long;
            begin
               CORBA.NVList.Add_Item
                 (Argument_List_ü,
                  Arg_Name_U_data,
                  Argument_U_data,
                  CORBA.ARG_IN);
               CORBA.ServerRequest.Arguments
                 (Request,
                  Argument_List_ü);
               --  Convert from Any
               data :=
                 CORBA.From_Any
                    (Argument_U_data);
               --  Call Implementation
               Result_ü :=
                 test_hash.Impl.echoLong41
                    (test_hash.Impl.Object'Class
                       (Self.all)'Access,
                     data);
               --  Setting the result
               CORBA.ServerRequest.Set_Result
                 (Request,
                  CORBA.To_Any
                    (Result_ü));
            end;
         elsif (Operation_ü
            = "echoLong42")
         then
            declare
               data : CORBA.Long;
               Arg_Name_U_data : constant CORBA.Identifier :=
                 CORBA.To_CORBA_String
                    ("data");
               Argument_U_data : CORBA.Any :=
                 CORBA.Internals.Get_Empty_Any
                    (CORBA.TC_Long);
               Result_ü : CORBA.Long;
            begin
               CORBA.NVList.Add_Item
                 (Argument_List_ü,
                  Arg_Name_U_data,
                  Argument_U_data,
                  CORBA.ARG_IN);
               CORBA.ServerRequest.Arguments
                 (Request,
                  Argument_List_ü);
               --  Convert from Any
               data :=
                 CORBA.From_Any
                    (Argument_U_data);
               --  Call Implementation
               Result_ü :=
                 test_hash.Impl.echoLong42
                    (test_hash.Impl.Object'Class
                       (Self.all)'Access,
                     data);
               --  Setting the result
               CORBA.ServerRequest.Set_Result
                 (Request,
                  CORBA.To_Any
                    (Result_ü));
            end;
         elsif (Operation_ü
            = "echoLong43")
         then
            declare
               data : CORBA.Long;
               Arg_Name_U_data : constant CORBA.Identifier :=
                 CORBA.To_CORBA_String
                    ("data");
               Argument_U_data : CORBA.Any :=
                 CORBA.Internals.Get_Empty_Any
                    (CORBA.TC_Long);
               Result_ü : CORBA.Long;
            begin
               CORBA.NVList.Add_Item
                 (Argument_List_ü,
                  Arg_Name_U_data,
                  Argument_U_data,
                  CORBA.ARG_IN);
               CORBA.ServerRequest.Arguments
                 (Request,
                  Argument_List_ü);
               --  Convert from Any
               data :=
                 CORBA.From_Any
                    (Argument_U_data);
               --  Call Implementation
               Result_ü :=
                 test_hash.Impl.echoLong43
                    (test_hash.Impl.Object'Class
                       (Self.all)'Access,
                     data);
               --  Setting the result
               CORBA.ServerRequest.Set_Result
                 (Request,
                  CORBA.To_Any
                    (Result_ü));
            end;
         elsif (Operation_ü
            = "echoLong44")
         then
            declare
               data : CORBA.Long;
               Arg_Name_U_data : constant CORBA.Identifier :=
                 CORBA.To_CORBA_String
                    ("data");
               Argument_U_data : CORBA.Any :=
                 CORBA.Internals.Get_Empty_Any
                    (CORBA.TC_Long);
               Result_ü : CORBA.Long;
            begin
               CORBA.NVList.Add_Item
                 (Argument_List_ü,
                  Arg_Name_U_data,
                  Argument_U_data,
                  CORBA.ARG_IN);
               CORBA.ServerRequest.Arguments
                 (Request,
                  Argument_List_ü);
               --  Convert from Any
               data :=
                 CORBA.From_Any
                    (Argument_U_data);
               --  Call Implementation
               Result_ü :=
                 test_hash.Impl.echoLong44
                    (test_hash.Impl.Object'Class
                       (Self.all)'Access,
                     data);
               --  Setting the result
               CORBA.ServerRequest.Set_Result
                 (Request,
                  CORBA.To_Any
                    (Result_ü));
            end;
         elsif (Operation_ü
            = "echoLong45")
         then
            declare
               data : CORBA.Long;
               Arg_Name_U_data : constant CORBA.Identifier :=
                 CORBA.To_CORBA_String
                    ("data");
               Argument_U_data : CORBA.Any :=
                 CORBA.Internals.Get_Empty_Any
                    (CORBA.TC_Long);
               Result_ü : CORBA.Long;
            begin
               CORBA.NVList.Add_Item
                 (Argument_List_ü,
                  Arg_Name_U_data,
                  Argument_U_data,
                  CORBA.ARG_IN);
               CORBA.ServerRequest.Arguments
                 (Request,
                  Argument_List_ü);
               --  Convert from Any
               data :=
                 CORBA.From_Any
                    (Argument_U_data);
               --  Call Implementation
               Result_ü :=
                 test_hash.Impl.echoLong45
                    (test_hash.Impl.Object'Class
                       (Self.all)'Access,
                     data);
               --  Setting the result
               CORBA.ServerRequest.Set_Result
                 (Request,
                  CORBA.To_Any
                    (Result_ü));
            end;
         elsif (Operation_ü
            = "echoLong46")
         then
            declare
               data : CORBA.Long;
               Arg_Name_U_data : constant CORBA.Identifier :=
                 CORBA.To_CORBA_String
                    ("data");
               Argument_U_data : CORBA.Any :=
                 CORBA.Internals.Get_Empty_Any
                    (CORBA.TC_Long);
               Result_ü : CORBA.Long;
            begin
               CORBA.NVList.Add_Item
                 (Argument_List_ü,
                  Arg_Name_U_data,
                  Argument_U_data,
                  CORBA.ARG_IN);
               CORBA.ServerRequest.Arguments
                 (Request,
                  Argument_List_ü);
               --  Convert from Any
               data :=
                 CORBA.From_Any
                    (Argument_U_data);
               --  Call Implementation
               Result_ü :=
                 test_hash.Impl.echoLong46
                    (test_hash.Impl.Object'Class
                       (Self.all)'Access,
                     data);
               --  Setting the result
               CORBA.ServerRequest.Set_Result
                 (Request,
                  CORBA.To_Any
                    (Result_ü));
            end;
         elsif (Operation_ü
            = "echoLong47")
         then
            declare
               data : CORBA.Long;
               Arg_Name_U_data : constant CORBA.Identifier :=
                 CORBA.To_CORBA_String
                    ("data");
               Argument_U_data : CORBA.Any :=
                 CORBA.Internals.Get_Empty_Any
                    (CORBA.TC_Long);
               Result_ü : CORBA.Long;
            begin
               CORBA.NVList.Add_Item
                 (Argument_List_ü,
                  Arg_Name_U_data,
                  Argument_U_data,
                  CORBA.ARG_IN);
               CORBA.ServerRequest.Arguments
                 (Request,
                  Argument_List_ü);
               --  Convert from Any
               data :=
                 CORBA.From_Any
                    (Argument_U_data);
               --  Call Implementation
               Result_ü :=
                 test_hash.Impl.echoLong47
                    (test_hash.Impl.Object'Class
                       (Self.all)'Access,
                     data);
               --  Setting the result
               CORBA.ServerRequest.Set_Result
                 (Request,
                  CORBA.To_Any
                    (Result_ü));
            end;
         elsif (Operation_ü
            = "echoLong48")
         then
            declare
               data : CORBA.Long;
               Arg_Name_U_data : constant CORBA.Identifier :=
                 CORBA.To_CORBA_String
                    ("data");
               Argument_U_data : CORBA.Any :=
                 CORBA.Internals.Get_Empty_Any
                    (CORBA.TC_Long);
               Result_ü : CORBA.Long;
            begin
               CORBA.NVList.Add_Item
                 (Argument_List_ü,
                  Arg_Name_U_data,
                  Argument_U_data,
                  CORBA.ARG_IN);
               CORBA.ServerRequest.Arguments
                 (Request,
                  Argument_List_ü);
               --  Convert from Any
               data :=
                 CORBA.From_Any
                    (Argument_U_data);
               --  Call Implementation
               Result_ü :=
                 test_hash.Impl.echoLong48
                    (test_hash.Impl.Object'Class
                       (Self.all)'Access,
                     data);
               --  Setting the result
               CORBA.ServerRequest.Set_Result
                 (Request,
                  CORBA.To_Any
                    (Result_ü));
            end;
         elsif (Operation_ü
            = "echoLong49")
         then
            declare
               data : CORBA.Long;
               Arg_Name_U_data : constant CORBA.Identifier :=
                 CORBA.To_CORBA_String
                    ("data");
               Argument_U_data : CORBA.Any :=
                 CORBA.Internals.Get_Empty_Any
                    (CORBA.TC_Long);
               Result_ü : CORBA.Long;
            begin
               CORBA.NVList.Add_Item
                 (Argument_List_ü,
                  Arg_Name_U_data,
                  Argument_U_data,
                  CORBA.ARG_IN);
               CORBA.ServerRequest.Arguments
                 (Request,
                  Argument_List_ü);
               --  Convert from Any
               data :=
                 CORBA.From_Any
                    (Argument_U_data);
               --  Call Implementation
               Result_ü :=
                 test_hash.Impl.echoLong49
                    (test_hash.Impl.Object'Class
                       (Self.all)'Access,
                     data);
               --  Setting the result
               CORBA.ServerRequest.Set_Result
                 (Request,
                  CORBA.To_Any
                    (Result_ü));
            end;
         elsif (Operation_ü
            = "echoLong50")
         then
            declare
               data : CORBA.Long;
               Arg_Name_U_data : constant CORBA.Identifier :=
                 CORBA.To_CORBA_String
                    ("data");
               Argument_U_data : CORBA.Any :=
                 CORBA.Internals.Get_Empty_Any
                    (CORBA.TC_Long);
               Result_ü : CORBA.Long;
            begin
               CORBA.NVList.Add_Item
                 (Argument_List_ü,
                  Arg_Name_U_data,
                  Argument_U_data,
                  CORBA.ARG_IN);
               CORBA.ServerRequest.Arguments
                 (Request,
                  Argument_List_ü);
               --  Convert from Any
               data :=
                 CORBA.From_Any
                    (Argument_U_data);
               --  Call Implementation
               Result_ü :=
                 test_hash.Impl.echoLong50
                    (test_hash.Impl.Object'Class
                       (Self.all)'Access,
                     data);
               --  Setting the result
               CORBA.ServerRequest.Set_Result
                 (Request,
                  CORBA.To_Any
                    (Result_ü));
            end;
         elsif (Operation_ü
            = "echoLong51")
         then
            declare
               data : CORBA.Long;
               Arg_Name_U_data : constant CORBA.Identifier :=
                 CORBA.To_CORBA_String
                    ("data");
               Argument_U_data : CORBA.Any :=
                 CORBA.Internals.Get_Empty_Any
                    (CORBA.TC_Long);
               Result_ü : CORBA.Long;
            begin
               CORBA.NVList.Add_Item
                 (Argument_List_ü,
                  Arg_Name_U_data,
                  Argument_U_data,
                  CORBA.ARG_IN);
               CORBA.ServerRequest.Arguments
                 (Request,
                  Argument_List_ü);
               --  Convert from Any
               data :=
                 CORBA.From_Any
                    (Argument_U_data);
               --  Call Implementation
               Result_ü :=
                 test_hash.Impl.echoLong51
                    (test_hash.Impl.Object'Class
                       (Self.all)'Access,
                     data);
               --  Setting the result
               CORBA.ServerRequest.Set_Result
                 (Request,
                  CORBA.To_Any
                    (Result_ü));
            end;
         elsif (Operation_ü
            = "echoLong52")
         then
            declare
               data : CORBA.Long;
               Arg_Name_U_data : constant CORBA.Identifier :=
                 CORBA.To_CORBA_String
                    ("data");
               Argument_U_data : CORBA.Any :=
                 CORBA.Internals.Get_Empty_Any
                    (CORBA.TC_Long);
               Result_ü : CORBA.Long;
            begin
               CORBA.NVList.Add_Item
                 (Argument_List_ü,
                  Arg_Name_U_data,
                  Argument_U_data,
                  CORBA.ARG_IN);
               CORBA.ServerRequest.Arguments
                 (Request,
                  Argument_List_ü);
               --  Convert from Any
               data :=
                 CORBA.From_Any
                    (Argument_U_data);
               --  Call Implementation
               Result_ü :=
                 test_hash.Impl.echoLong52
                    (test_hash.Impl.Object'Class
                       (Self.all)'Access,
                     data);
               --  Setting the result
               CORBA.ServerRequest.Set_Result
                 (Request,
                  CORBA.To_Any
                    (Result_ü));
            end;
         elsif (Operation_ü
            = "echoLong53")
         then
            declare
               data : CORBA.Long;
               Arg_Name_U_data : constant CORBA.Identifier :=
                 CORBA.To_CORBA_String
                    ("data");
               Argument_U_data : CORBA.Any :=
                 CORBA.Internals.Get_Empty_Any
                    (CORBA.TC_Long);
               Result_ü : CORBA.Long;
            begin
               CORBA.NVList.Add_Item
                 (Argument_List_ü,
                  Arg_Name_U_data,
                  Argument_U_data,
                  CORBA.ARG_IN);
               CORBA.ServerRequest.Arguments
                 (Request,
                  Argument_List_ü);
               --  Convert from Any
               data :=
                 CORBA.From_Any
                    (Argument_U_data);
               --  Call Implementation
               Result_ü :=
                 test_hash.Impl.echoLong53
                    (test_hash.Impl.Object'Class
                       (Self.all)'Access,
                     data);
               --  Setting the result
               CORBA.ServerRequest.Set_Result
                 (Request,
                  CORBA.To_Any
                    (Result_ü));
            end;
         elsif (Operation_ü
            = "echoLong54")
         then
            declare
               data : CORBA.Long;
               Arg_Name_U_data : constant CORBA.Identifier :=
                 CORBA.To_CORBA_String
                    ("data");
               Argument_U_data : CORBA.Any :=
                 CORBA.Internals.Get_Empty_Any
                    (CORBA.TC_Long);
               Result_ü : CORBA.Long;
            begin
               CORBA.NVList.Add_Item
                 (Argument_List_ü,
                  Arg_Name_U_data,
                  Argument_U_data,
                  CORBA.ARG_IN);
               CORBA.ServerRequest.Arguments
                 (Request,
                  Argument_List_ü);
               --  Convert from Any
               data :=
                 CORBA.From_Any
                    (Argument_U_data);
               --  Call Implementation
               Result_ü :=
                 test_hash.Impl.echoLong54
                    (test_hash.Impl.Object'Class
                       (Self.all)'Access,
                     data);
               --  Setting the result
               CORBA.ServerRequest.Set_Result
                 (Request,
                  CORBA.To_Any
                    (Result_ü));
            end;
         elsif (Operation_ü
            = "echoLong55")
         then
            declare
               data : CORBA.Long;
               Arg_Name_U_data : constant CORBA.Identifier :=
                 CORBA.To_CORBA_String
                    ("data");
               Argument_U_data : CORBA.Any :=
                 CORBA.Internals.Get_Empty_Any
                    (CORBA.TC_Long);
               Result_ü : CORBA.Long;
            begin
               CORBA.NVList.Add_Item
                 (Argument_List_ü,
                  Arg_Name_U_data,
                  Argument_U_data,
                  CORBA.ARG_IN);
               CORBA.ServerRequest.Arguments
                 (Request,
                  Argument_List_ü);
               --  Convert from Any
               data :=
                 CORBA.From_Any
                    (Argument_U_data);
               --  Call Implementation
               Result_ü :=
                 test_hash.Impl.echoLong55
                    (test_hash.Impl.Object'Class
                       (Self.all)'Access,
                     data);
               --  Setting the result
               CORBA.ServerRequest.Set_Result
                 (Request,
                  CORBA.To_Any
                    (Result_ü));
            end;
         elsif (Operation_ü
            = "echoLong56")
         then
            declare
               data : CORBA.Long;
               Arg_Name_U_data : constant CORBA.Identifier :=
                 CORBA.To_CORBA_String
                    ("data");
               Argument_U_data : CORBA.Any :=
                 CORBA.Internals.Get_Empty_Any
                    (CORBA.TC_Long);
               Result_ü : CORBA.Long;
            begin
               CORBA.NVList.Add_Item
                 (Argument_List_ü,
                  Arg_Name_U_data,
                  Argument_U_data,
                  CORBA.ARG_IN);
               CORBA.ServerRequest.Arguments
                 (Request,
                  Argument_List_ü);
               --  Convert from Any
               data :=
                 CORBA.From_Any
                    (Argument_U_data);
               --  Call Implementation
               Result_ü :=
                 test_hash.Impl.echoLong56
                    (test_hash.Impl.Object'Class
                       (Self.all)'Access,
                     data);
               --  Setting the result
               CORBA.ServerRequest.Set_Result
                 (Request,
                  CORBA.To_Any
                    (Result_ü));
            end;
         elsif (Operation_ü
            = "echoLong57")
         then
            declare
               data : CORBA.Long;
               Arg_Name_U_data : constant CORBA.Identifier :=
                 CORBA.To_CORBA_String
                    ("data");
               Argument_U_data : CORBA.Any :=
                 CORBA.Internals.Get_Empty_Any
                    (CORBA.TC_Long);
               Result_ü : CORBA.Long;
            begin
               CORBA.NVList.Add_Item
                 (Argument_List_ü,
                  Arg_Name_U_data,
                  Argument_U_data,
                  CORBA.ARG_IN);
               CORBA.ServerRequest.Arguments
                 (Request,
                  Argument_List_ü);
               --  Convert from Any
               data :=
                 CORBA.From_Any
                    (Argument_U_data);
               --  Call Implementation
               Result_ü :=
                 test_hash.Impl.echoLong57
                    (test_hash.Impl.Object'Class
                       (Self.all)'Access,
                     data);
               --  Setting the result
               CORBA.ServerRequest.Set_Result
                 (Request,
                  CORBA.To_Any
                    (Result_ü));
            end;
         elsif (Operation_ü
            = "echoLong58")
         then
            declare
               data : CORBA.Long;
               Arg_Name_U_data : constant CORBA.Identifier :=
                 CORBA.To_CORBA_String
                    ("data");
               Argument_U_data : CORBA.Any :=
                 CORBA.Internals.Get_Empty_Any
                    (CORBA.TC_Long);
               Result_ü : CORBA.Long;
            begin
               CORBA.NVList.Add_Item
                 (Argument_List_ü,
                  Arg_Name_U_data,
                  Argument_U_data,
                  CORBA.ARG_IN);
               CORBA.ServerRequest.Arguments
                 (Request,
                  Argument_List_ü);
               --  Convert from Any
               data :=
                 CORBA.From_Any
                    (Argument_U_data);
               --  Call Implementation
               Result_ü :=
                 test_hash.Impl.echoLong58
                    (test_hash.Impl.Object'Class
                       (Self.all)'Access,
                     data);
               --  Setting the result
               CORBA.ServerRequest.Set_Result
                 (Request,
                  CORBA.To_Any
                    (Result_ü));
            end;
         elsif (Operation_ü
            = "echoLong59")
         then
            declare
               data : CORBA.Long;
               Arg_Name_U_data : constant CORBA.Identifier :=
                 CORBA.To_CORBA_String
                    ("data");
               Argument_U_data : CORBA.Any :=
                 CORBA.Internals.Get_Empty_Any
                    (CORBA.TC_Long);
               Result_ü : CORBA.Long;
            begin
               CORBA.NVList.Add_Item
                 (Argument_List_ü,
                  Arg_Name_U_data,
                  Argument_U_data,
                  CORBA.ARG_IN);
               CORBA.ServerRequest.Arguments
                 (Request,
                  Argument_List_ü);
               --  Convert from Any
               data :=
                 CORBA.From_Any
                    (Argument_U_data);
               --  Call Implementation
               Result_ü :=
                 test_hash.Impl.echoLong59
                    (test_hash.Impl.Object'Class
                       (Self.all)'Access,
                     data);
               --  Setting the result
               CORBA.ServerRequest.Set_Result
                 (Request,
                  CORBA.To_Any
                    (Result_ü));
            end;
         elsif (Operation_ü
            = "echoLong60")
         then
            declare
               data : CORBA.Long;
               Arg_Name_U_data : constant CORBA.Identifier :=
                 CORBA.To_CORBA_String
                    ("data");
               Argument_U_data : CORBA.Any :=
                 CORBA.Internals.Get_Empty_Any
                    (CORBA.TC_Long);
               Result_ü : CORBA.Long;
            begin
               CORBA.NVList.Add_Item
                 (Argument_List_ü,
                  Arg_Name_U_data,
                  Argument_U_data,
                  CORBA.ARG_IN);
               CORBA.ServerRequest.Arguments
                 (Request,
                  Argument_List_ü);
               --  Convert from Any
               data :=
                 CORBA.From_Any
                    (Argument_U_data);
               --  Call Implementation
               Result_ü :=
                 test_hash.Impl.echoLong60
                    (test_hash.Impl.Object'Class
                       (Self.all)'Access,
                     data);
               --  Setting the result
               CORBA.ServerRequest.Set_Result
                 (Request,
                  CORBA.To_Any
                    (Result_ü));
            end;
         elsif (Operation_ü
            = "echoLong61")
         then
            declare
               data : CORBA.Long;
               Arg_Name_U_data : constant CORBA.Identifier :=
                 CORBA.To_CORBA_String
                    ("data");
               Argument_U_data : CORBA.Any :=
                 CORBA.Internals.Get_Empty_Any
                    (CORBA.TC_Long);
               Result_ü : CORBA.Long;
            begin
               CORBA.NVList.Add_Item
                 (Argument_List_ü,
                  Arg_Name_U_data,
                  Argument_U_data,
                  CORBA.ARG_IN);
               CORBA.ServerRequest.Arguments
                 (Request,
                  Argument_List_ü);
               --  Convert from Any
               data :=
                 CORBA.From_Any
                    (Argument_U_data);
               --  Call Implementation
               Result_ü :=
                 test_hash.Impl.echoLong61
                    (test_hash.Impl.Object'Class
                       (Self.all)'Access,
                     data);
               --  Setting the result
               CORBA.ServerRequest.Set_Result
                 (Request,
                  CORBA.To_Any
                    (Result_ü));
            end;
         elsif (Operation_ü
            = "echoLong62")
         then
            declare
               data : CORBA.Long;
               Arg_Name_U_data : constant CORBA.Identifier :=
                 CORBA.To_CORBA_String
                    ("data");
               Argument_U_data : CORBA.Any :=
                 CORBA.Internals.Get_Empty_Any
                    (CORBA.TC_Long);
               Result_ü : CORBA.Long;
            begin
               CORBA.NVList.Add_Item
                 (Argument_List_ü,
                  Arg_Name_U_data,
                  Argument_U_data,
                  CORBA.ARG_IN);
               CORBA.ServerRequest.Arguments
                 (Request,
                  Argument_List_ü);
               --  Convert from Any
               data :=
                 CORBA.From_Any
                    (Argument_U_data);
               --  Call Implementation
               Result_ü :=
                 test_hash.Impl.echoLong62
                    (test_hash.Impl.Object'Class
                       (Self.all)'Access,
                     data);
               --  Setting the result
               CORBA.ServerRequest.Set_Result
                 (Request,
                  CORBA.To_Any
                    (Result_ü));
            end;
         elsif (Operation_ü
            = "echoLong63")
         then
            declare
               data : CORBA.Long;
               Arg_Name_U_data : constant CORBA.Identifier :=
                 CORBA.To_CORBA_String
                    ("data");
               Argument_U_data : CORBA.Any :=
                 CORBA.Internals.Get_Empty_Any
                    (CORBA.TC_Long);
               Result_ü : CORBA.Long;
            begin
               CORBA.NVList.Add_Item
                 (Argument_List_ü,
                  Arg_Name_U_data,
                  Argument_U_data,
                  CORBA.ARG_IN);
               CORBA.ServerRequest.Arguments
                 (Request,
                  Argument_List_ü);
               --  Convert from Any
               data :=
                 CORBA.From_Any
                    (Argument_U_data);
               --  Call Implementation
               Result_ü :=
                 test_hash.Impl.echoLong63
                    (test_hash.Impl.Object'Class
                       (Self.all)'Access,
                     data);
               --  Setting the result
               CORBA.ServerRequest.Set_Result
                 (Request,
                  CORBA.To_Any
                    (Result_ü));
            end;
         elsif (Operation_ü
            = "echoLong64")
         then
            declare
               data : CORBA.Long;
               Arg_Name_U_data : constant CORBA.Identifier :=
                 CORBA.To_CORBA_String
                    ("data");
               Argument_U_data : CORBA.Any :=
                 CORBA.Internals.Get_Empty_Any
                    (CORBA.TC_Long);
               Result_ü : CORBA.Long;
            begin
               CORBA.NVList.Add_Item
                 (Argument_List_ü,
                  Arg_Name_U_data,
                  Argument_U_data,
                  CORBA.ARG_IN);
               CORBA.ServerRequest.Arguments
                 (Request,
                  Argument_List_ü);
               --  Convert from Any
               data :=
                 CORBA.From_Any
                    (Argument_U_data);
               --  Call Implementation
               Result_ü :=
                 test_hash.Impl.echoLong64
                    (test_hash.Impl.Object'Class
                       (Self.all)'Access,
                     data);
               --  Setting the result
               CORBA.ServerRequest.Set_Result
                 (Request,
                  CORBA.To_Any
                    (Result_ü));
            end;
         elsif (Operation_ü
            = "echoLong65")
         then
            declare
               data : CORBA.Long;
               Arg_Name_U_data : constant CORBA.Identifier :=
                 CORBA.To_CORBA_String
                    ("data");
               Argument_U_data : CORBA.Any :=
                 CORBA.Internals.Get_Empty_Any
                    (CORBA.TC_Long);
               Result_ü : CORBA.Long;
            begin
               CORBA.NVList.Add_Item
                 (Argument_List_ü,
                  Arg_Name_U_data,
                  Argument_U_data,
                  CORBA.ARG_IN);
               CORBA.ServerRequest.Arguments
                 (Request,
                  Argument_List_ü);
               --  Convert from Any
               data :=
                 CORBA.From_Any
                    (Argument_U_data);
               --  Call Implementation
               Result_ü :=
                 test_hash.Impl.echoLong65
                    (test_hash.Impl.Object'Class
                       (Self.all)'Access,
                     data);
               --  Setting the result
               CORBA.ServerRequest.Set_Result
                 (Request,
                  CORBA.To_Any
                    (Result_ü));
            end;
         elsif (Operation_ü
            = "echoLong66")
         then
            declare
               data : CORBA.Long;
               Arg_Name_U_data : constant CORBA.Identifier :=
                 CORBA.To_CORBA_String
                    ("data");
               Argument_U_data : CORBA.Any :=
                 CORBA.Internals.Get_Empty_Any
                    (CORBA.TC_Long);
               Result_ü : CORBA.Long;
            begin
               CORBA.NVList.Add_Item
                 (Argument_List_ü,
                  Arg_Name_U_data,
                  Argument_U_data,
                  CORBA.ARG_IN);
               CORBA.ServerRequest.Arguments
                 (Request,
                  Argument_List_ü);
               --  Convert from Any
               data :=
                 CORBA.From_Any
                    (Argument_U_data);
               --  Call Implementation
               Result_ü :=
                 test_hash.Impl.echoLong66
                    (test_hash.Impl.Object'Class
                       (Self.all)'Access,
                     data);
               --  Setting the result
               CORBA.ServerRequest.Set_Result
                 (Request,
                  CORBA.To_Any
                    (Result_ü));
            end;
         elsif (Operation_ü
            = "echoLong67")
         then
            declare
               data : CORBA.Long;
               Arg_Name_U_data : constant CORBA.Identifier :=
                 CORBA.To_CORBA_String
                    ("data");
               Argument_U_data : CORBA.Any :=
                 CORBA.Internals.Get_Empty_Any
                    (CORBA.TC_Long);
               Result_ü : CORBA.Long;
            begin
               CORBA.NVList.Add_Item
                 (Argument_List_ü,
                  Arg_Name_U_data,
                  Argument_U_data,
                  CORBA.ARG_IN);
               CORBA.ServerRequest.Arguments
                 (Request,
                  Argument_List_ü);
               --  Convert from Any
               data :=
                 CORBA.From_Any
                    (Argument_U_data);
               --  Call Implementation
               Result_ü :=
                 test_hash.Impl.echoLong67
                    (test_hash.Impl.Object'Class
                       (Self.all)'Access,
                     data);
               --  Setting the result
               CORBA.ServerRequest.Set_Result
                 (Request,
                  CORBA.To_Any
                    (Result_ü));
            end;
         elsif (Operation_ü
            = "echoLong68")
         then
            declare
               data : CORBA.Long;
               Arg_Name_U_data : constant CORBA.Identifier :=
                 CORBA.To_CORBA_String
                    ("data");
               Argument_U_data : CORBA.Any :=
                 CORBA.Internals.Get_Empty_Any
                    (CORBA.TC_Long);
               Result_ü : CORBA.Long;
            begin
               CORBA.NVList.Add_Item
                 (Argument_List_ü,
                  Arg_Name_U_data,
                  Argument_U_data,
                  CORBA.ARG_IN);
               CORBA.ServerRequest.Arguments
                 (Request,
                  Argument_List_ü);
               --  Convert from Any
               data :=
                 CORBA.From_Any
                    (Argument_U_data);
               --  Call Implementation
               Result_ü :=
                 test_hash.Impl.echoLong68
                    (test_hash.Impl.Object'Class
                       (Self.all)'Access,
                     data);
               --  Setting the result
               CORBA.ServerRequest.Set_Result
                 (Request,
                  CORBA.To_Any
                    (Result_ü));
            end;
         elsif (Operation_ü
            = "echoLong69")
         then
            declare
               data : CORBA.Long;
               Arg_Name_U_data : constant CORBA.Identifier :=
                 CORBA.To_CORBA_String
                    ("data");
               Argument_U_data : CORBA.Any :=
                 CORBA.Internals.Get_Empty_Any
                    (CORBA.TC_Long);
               Result_ü : CORBA.Long;
            begin
               CORBA.NVList.Add_Item
                 (Argument_List_ü,
                  Arg_Name_U_data,
                  Argument_U_data,
                  CORBA.ARG_IN);
               CORBA.ServerRequest.Arguments
                 (Request,
                  Argument_List_ü);
               --  Convert from Any
               data :=
                 CORBA.From_Any
                    (Argument_U_data);
               --  Call Implementation
               Result_ü :=
                 test_hash.Impl.echoLong69
                    (test_hash.Impl.Object'Class
                       (Self.all)'Access,
                     data);
               --  Setting the result
               CORBA.ServerRequest.Set_Result
                 (Request,
                  CORBA.To_Any
                    (Result_ü));
            end;
         elsif (Operation_ü
            = "echoLong70")
         then
            declare
               data : CORBA.Long;
               Arg_Name_U_data : constant CORBA.Identifier :=
                 CORBA.To_CORBA_String
                    ("data");
               Argument_U_data : CORBA.Any :=
                 CORBA.Internals.Get_Empty_Any
                    (CORBA.TC_Long);
               Result_ü : CORBA.Long;
            begin
               CORBA.NVList.Add_Item
                 (Argument_List_ü,
                  Arg_Name_U_data,
                  Argument_U_data,
                  CORBA.ARG_IN);
               CORBA.ServerRequest.Arguments
                 (Request,
                  Argument_List_ü);
               --  Convert from Any
               data :=
                 CORBA.From_Any
                    (Argument_U_data);
               --  Call Implementation
               Result_ü :=
                 test_hash.Impl.echoLong70
                    (test_hash.Impl.Object'Class
                       (Self.all)'Access,
                     data);
               --  Setting the result
               CORBA.ServerRequest.Set_Result
                 (Request,
                  CORBA.To_Any
                    (Result_ü));
            end;
         elsif (Operation_ü
            = "echoLong71")
         then
            declare
               data : CORBA.Long;
               Arg_Name_U_data : constant CORBA.Identifier :=
                 CORBA.To_CORBA_String
                    ("data");
               Argument_U_data : CORBA.Any :=
                 CORBA.Internals.Get_Empty_Any
                    (CORBA.TC_Long);
               Result_ü : CORBA.Long;
            begin
               CORBA.NVList.Add_Item
                 (Argument_List_ü,
                  Arg_Name_U_data,
                  Argument_U_data,
                  CORBA.ARG_IN);
               CORBA.ServerRequest.Arguments
                 (Request,
                  Argument_List_ü);
               --  Convert from Any
               data :=
                 CORBA.From_Any
                    (Argument_U_data);
               --  Call Implementation
               Result_ü :=
                 test_hash.Impl.echoLong71
                    (test_hash.Impl.Object'Class
                       (Self.all)'Access,
                     data);
               --  Setting the result
               CORBA.ServerRequest.Set_Result
                 (Request,
                  CORBA.To_Any
                    (Result_ü));
            end;
         elsif (Operation_ü
            = "echoLong72")
         then
            declare
               data : CORBA.Long;
               Arg_Name_U_data : constant CORBA.Identifier :=
                 CORBA.To_CORBA_String
                    ("data");
               Argument_U_data : CORBA.Any :=
                 CORBA.Internals.Get_Empty_Any
                    (CORBA.TC_Long);
               Result_ü : CORBA.Long;
            begin
               CORBA.NVList.Add_Item
                 (Argument_List_ü,
                  Arg_Name_U_data,
                  Argument_U_data,
                  CORBA.ARG_IN);
               CORBA.ServerRequest.Arguments
                 (Request,
                  Argument_List_ü);
               --  Convert from Any
               data :=
                 CORBA.From_Any
                    (Argument_U_data);
               --  Call Implementation
               Result_ü :=
                 test_hash.Impl.echoLong72
                    (test_hash.Impl.Object'Class
                       (Self.all)'Access,
                     data);
               --  Setting the result
               CORBA.ServerRequest.Set_Result
                 (Request,
                  CORBA.To_Any
                    (Result_ü));
            end;
         elsif (Operation_ü
            = "echoLong73")
         then
            declare
               data : CORBA.Long;
               Arg_Name_U_data : constant CORBA.Identifier :=
                 CORBA.To_CORBA_String
                    ("data");
               Argument_U_data : CORBA.Any :=
                 CORBA.Internals.Get_Empty_Any
                    (CORBA.TC_Long);
               Result_ü : CORBA.Long;
            begin
               CORBA.NVList.Add_Item
                 (Argument_List_ü,
                  Arg_Name_U_data,
                  Argument_U_data,
                  CORBA.ARG_IN);
               CORBA.ServerRequest.Arguments
                 (Request,
                  Argument_List_ü);
               --  Convert from Any
               data :=
                 CORBA.From_Any
                    (Argument_U_data);
               --  Call Implementation
               Result_ü :=
                 test_hash.Impl.echoLong73
                    (test_hash.Impl.Object'Class
                       (Self.all)'Access,
                     data);
               --  Setting the result
               CORBA.ServerRequest.Set_Result
                 (Request,
                  CORBA.To_Any
                    (Result_ü));
            end;
         elsif (Operation_ü
            = "echoLong74")
         then
            declare
               data : CORBA.Long;
               Arg_Name_U_data : constant CORBA.Identifier :=
                 CORBA.To_CORBA_String
                    ("data");
               Argument_U_data : CORBA.Any :=
                 CORBA.Internals.Get_Empty_Any
                    (CORBA.TC_Long);
               Result_ü : CORBA.Long;
            begin
               CORBA.NVList.Add_Item
                 (Argument_List_ü,
                  Arg_Name_U_data,
                  Argument_U_data,
                  CORBA.ARG_IN);
               CORBA.ServerRequest.Arguments
                 (Request,
                  Argument_List_ü);
               --  Convert from Any
               data :=
                 CORBA.From_Any
                    (Argument_U_data);
               --  Call Implementation
               Result_ü :=
                 test_hash.Impl.echoLong74
                    (test_hash.Impl.Object'Class
                       (Self.all)'Access,
                     data);
               --  Setting the result
               CORBA.ServerRequest.Set_Result
                 (Request,
                  CORBA.To_Any
                    (Result_ü));
            end;
         elsif (Operation_ü
            = "echoLong75")
         then
            declare
               data : CORBA.Long;
               Arg_Name_U_data : constant CORBA.Identifier :=
                 CORBA.To_CORBA_String
                    ("data");
               Argument_U_data : CORBA.Any :=
                 CORBA.Internals.Get_Empty_Any
                    (CORBA.TC_Long);
               Result_ü : CORBA.Long;
            begin
               CORBA.NVList.Add_Item
                 (Argument_List_ü,
                  Arg_Name_U_data,
                  Argument_U_data,
                  CORBA.ARG_IN);
               CORBA.ServerRequest.Arguments
                 (Request,
                  Argument_List_ü);
               --  Convert from Any
               data :=
                 CORBA.From_Any
                    (Argument_U_data);
               --  Call Implementation
               Result_ü :=
                 test_hash.Impl.echoLong75
                    (test_hash.Impl.Object'Class
                       (Self.all)'Access,
                     data);
               --  Setting the result
               CORBA.ServerRequest.Set_Result
                 (Request,
                  CORBA.To_Any
                    (Result_ü));
            end;
         elsif (Operation_ü
            = "echoLong76")
         then
            declare
               data : CORBA.Long;
               Arg_Name_U_data : constant CORBA.Identifier :=
                 CORBA.To_CORBA_String
                    ("data");
               Argument_U_data : CORBA.Any :=
                 CORBA.Internals.Get_Empty_Any
                    (CORBA.TC_Long);
               Result_ü : CORBA.Long;
            begin
               CORBA.NVList.Add_Item
                 (Argument_List_ü,
                  Arg_Name_U_data,
                  Argument_U_data,
                  CORBA.ARG_IN);
               CORBA.ServerRequest.Arguments
                 (Request,
                  Argument_List_ü);
               --  Convert from Any
               data :=
                 CORBA.From_Any
                    (Argument_U_data);
               --  Call Implementation
               Result_ü :=
                 test_hash.Impl.echoLong76
                    (test_hash.Impl.Object'Class
                       (Self.all)'Access,
                     data);
               --  Setting the result
               CORBA.ServerRequest.Set_Result
                 (Request,
                  CORBA.To_Any
                    (Result_ü));
            end;
         elsif (Operation_ü
            = "echoLong77")
         then
            declare
               data : CORBA.Long;
               Arg_Name_U_data : constant CORBA.Identifier :=
                 CORBA.To_CORBA_String
                    ("data");
               Argument_U_data : CORBA.Any :=
                 CORBA.Internals.Get_Empty_Any
                    (CORBA.TC_Long);
               Result_ü : CORBA.Long;
            begin
               CORBA.NVList.Add_Item
                 (Argument_List_ü,
                  Arg_Name_U_data,
                  Argument_U_data,
                  CORBA.ARG_IN);
               CORBA.ServerRequest.Arguments
                 (Request,
                  Argument_List_ü);
               --  Convert from Any
               data :=
                 CORBA.From_Any
                    (Argument_U_data);
               --  Call Implementation
               Result_ü :=
                 test_hash.Impl.echoLong77
                    (test_hash.Impl.Object'Class
                       (Self.all)'Access,
                     data);
               --  Setting the result
               CORBA.ServerRequest.Set_Result
                 (Request,
                  CORBA.To_Any
                    (Result_ü));
            end;
         elsif (Operation_ü
            = "echoLong78")
         then
            declare
               data : CORBA.Long;
               Arg_Name_U_data : constant CORBA.Identifier :=
                 CORBA.To_CORBA_String
                    ("data");
               Argument_U_data : CORBA.Any :=
                 CORBA.Internals.Get_Empty_Any
                    (CORBA.TC_Long);
               Result_ü : CORBA.Long;
            begin
               CORBA.NVList.Add_Item
                 (Argument_List_ü,
                  Arg_Name_U_data,
                  Argument_U_data,
                  CORBA.ARG_IN);
               CORBA.ServerRequest.Arguments
                 (Request,
                  Argument_List_ü);
               --  Convert from Any
               data :=
                 CORBA.From_Any
                    (Argument_U_data);
               --  Call Implementation
               Result_ü :=
                 test_hash.Impl.echoLong78
                    (test_hash.Impl.Object'Class
                       (Self.all)'Access,
                     data);
               --  Setting the result
               CORBA.ServerRequest.Set_Result
                 (Request,
                  CORBA.To_Any
                    (Result_ü));
            end;
         elsif (Operation_ü
            = "echoLong79")
         then
            declare
               data : CORBA.Long;
               Arg_Name_U_data : constant CORBA.Identifier :=
                 CORBA.To_CORBA_String
                    ("data");
               Argument_U_data : CORBA.Any :=
                 CORBA.Internals.Get_Empty_Any
                    (CORBA.TC_Long);
               Result_ü : CORBA.Long;
            begin
               CORBA.NVList.Add_Item
                 (Argument_List_ü,
                  Arg_Name_U_data,
                  Argument_U_data,
                  CORBA.ARG_IN);
               CORBA.ServerRequest.Arguments
                 (Request,
                  Argument_List_ü);
               --  Convert from Any
               data :=
                 CORBA.From_Any
                    (Argument_U_data);
               --  Call Implementation
               Result_ü :=
                 test_hash.Impl.echoLong79
                    (test_hash.Impl.Object'Class
                       (Self.all)'Access,
                     data);
               --  Setting the result
               CORBA.ServerRequest.Set_Result
                 (Request,
                  CORBA.To_Any
                    (Result_ü));
            end;
         elsif (Operation_ü
            = "echoLong80")
         then
            declare
               data : CORBA.Long;
               Arg_Name_U_data : constant CORBA.Identifier :=
                 CORBA.To_CORBA_String
                    ("data");
               Argument_U_data : CORBA.Any :=
                 CORBA.Internals.Get_Empty_Any
                    (CORBA.TC_Long);
               Result_ü : CORBA.Long;
            begin
               CORBA.NVList.Add_Item
                 (Argument_List_ü,
                  Arg_Name_U_data,
                  Argument_U_data,
                  CORBA.ARG_IN);
               CORBA.ServerRequest.Arguments
                 (Request,
                  Argument_List_ü);
               --  Convert from Any
               data :=
                 CORBA.From_Any
                    (Argument_U_data);
               --  Call Implementation
               Result_ü :=
                 test_hash.Impl.echoLong80
                    (test_hash.Impl.Object'Class
                       (Self.all)'Access,
                     data);
               --  Setting the result
               CORBA.ServerRequest.Set_Result
                 (Request,
                  CORBA.To_Any
                    (Result_ü));
            end;
         elsif (Operation_ü
            = "echoLong81")
         then
            declare
               data : CORBA.Long;
               Arg_Name_U_data : constant CORBA.Identifier :=
                 CORBA.To_CORBA_String
                    ("data");
               Argument_U_data : CORBA.Any :=
                 CORBA.Internals.Get_Empty_Any
                    (CORBA.TC_Long);
               Result_ü : CORBA.Long;
            begin
               CORBA.NVList.Add_Item
                 (Argument_List_ü,
                  Arg_Name_U_data,
                  Argument_U_data,
                  CORBA.ARG_IN);
               CORBA.ServerRequest.Arguments
                 (Request,
                  Argument_List_ü);
               --  Convert from Any
               data :=
                 CORBA.From_Any
                    (Argument_U_data);
               --  Call Implementation
               Result_ü :=
                 test_hash.Impl.echoLong81
                    (test_hash.Impl.Object'Class
                       (Self.all)'Access,
                     data);
               --  Setting the result
               CORBA.ServerRequest.Set_Result
                 (Request,
                  CORBA.To_Any
                    (Result_ü));
            end;
         elsif (Operation_ü
            = "echoLong82")
         then
            declare
               data : CORBA.Long;
               Arg_Name_U_data : constant CORBA.Identifier :=
                 CORBA.To_CORBA_String
                    ("data");
               Argument_U_data : CORBA.Any :=
                 CORBA.Internals.Get_Empty_Any
                    (CORBA.TC_Long);
               Result_ü : CORBA.Long;
            begin
               CORBA.NVList.Add_Item
                 (Argument_List_ü,
                  Arg_Name_U_data,
                  Argument_U_data,
                  CORBA.ARG_IN);
               CORBA.ServerRequest.Arguments
                 (Request,
                  Argument_List_ü);
               --  Convert from Any
               data :=
                 CORBA.From_Any
                    (Argument_U_data);
               --  Call Implementation
               Result_ü :=
                 test_hash.Impl.echoLong82
                    (test_hash.Impl.Object'Class
                       (Self.all)'Access,
                     data);
               --  Setting the result
               CORBA.ServerRequest.Set_Result
                 (Request,
                  CORBA.To_Any
                    (Result_ü));
            end;
         elsif (Operation_ü
            = "echoLong83")
         then
            declare
               data : CORBA.Long;
               Arg_Name_U_data : constant CORBA.Identifier :=
                 CORBA.To_CORBA_String
                    ("data");
               Argument_U_data : CORBA.Any :=
                 CORBA.Internals.Get_Empty_Any
                    (CORBA.TC_Long);
               Result_ü : CORBA.Long;
            begin
               CORBA.NVList.Add_Item
                 (Argument_List_ü,
                  Arg_Name_U_data,
                  Argument_U_data,
                  CORBA.ARG_IN);
               CORBA.ServerRequest.Arguments
                 (Request,
                  Argument_List_ü);
               --  Convert from Any
               data :=
                 CORBA.From_Any
                    (Argument_U_data);
               --  Call Implementation
               Result_ü :=
                 test_hash.Impl.echoLong83
                    (test_hash.Impl.Object'Class
                       (Self.all)'Access,
                     data);
               --  Setting the result
               CORBA.ServerRequest.Set_Result
                 (Request,
                  CORBA.To_Any
                    (Result_ü));
            end;
         elsif (Operation_ü
            = "echoLong84")
         then
            declare
               data : CORBA.Long;
               Arg_Name_U_data : constant CORBA.Identifier :=
                 CORBA.To_CORBA_String
                    ("data");
               Argument_U_data : CORBA.Any :=
                 CORBA.Internals.Get_Empty_Any
                    (CORBA.TC_Long);
               Result_ü : CORBA.Long;
            begin
               CORBA.NVList.Add_Item
                 (Argument_List_ü,
                  Arg_Name_U_data,
                  Argument_U_data,
                  CORBA.ARG_IN);
               CORBA.ServerRequest.Arguments
                 (Request,
                  Argument_List_ü);
               --  Convert from Any
               data :=
                 CORBA.From_Any
                    (Argument_U_data);
               --  Call Implementation
               Result_ü :=
                 test_hash.Impl.echoLong84
                    (test_hash.Impl.Object'Class
                       (Self.all)'Access,
                     data);
               --  Setting the result
               CORBA.ServerRequest.Set_Result
                 (Request,
                  CORBA.To_Any
                    (Result_ü));
            end;
         elsif (Operation_ü
            = "echoLong85")
         then
            declare
               data : CORBA.Long;
               Arg_Name_U_data : constant CORBA.Identifier :=
                 CORBA.To_CORBA_String
                    ("data");
               Argument_U_data : CORBA.Any :=
                 CORBA.Internals.Get_Empty_Any
                    (CORBA.TC_Long);
               Result_ü : CORBA.Long;
            begin
               CORBA.NVList.Add_Item
                 (Argument_List_ü,
                  Arg_Name_U_data,
                  Argument_U_data,
                  CORBA.ARG_IN);
               CORBA.ServerRequest.Arguments
                 (Request,
                  Argument_List_ü);
               --  Convert from Any
               data :=
                 CORBA.From_Any
                    (Argument_U_data);
               --  Call Implementation
               Result_ü :=
                 test_hash.Impl.echoLong85
                    (test_hash.Impl.Object'Class
                       (Self.all)'Access,
                     data);
               --  Setting the result
               CORBA.ServerRequest.Set_Result
                 (Request,
                  CORBA.To_Any
                    (Result_ü));
            end;
         elsif (Operation_ü
            = "echoLong86")
         then
            declare
               data : CORBA.Long;
               Arg_Name_U_data : constant CORBA.Identifier :=
                 CORBA.To_CORBA_String
                    ("data");
               Argument_U_data : CORBA.Any :=
                 CORBA.Internals.Get_Empty_Any
                    (CORBA.TC_Long);
               Result_ü : CORBA.Long;
            begin
               CORBA.NVList.Add_Item
                 (Argument_List_ü,
                  Arg_Name_U_data,
                  Argument_U_data,
                  CORBA.ARG_IN);
               CORBA.ServerRequest.Arguments
                 (Request,
                  Argument_List_ü);
               --  Convert from Any
               data :=
                 CORBA.From_Any
                    (Argument_U_data);
               --  Call Implementation
               Result_ü :=
                 test_hash.Impl.echoLong86
                    (test_hash.Impl.Object'Class
                       (Self.all)'Access,
                     data);
               --  Setting the result
               CORBA.ServerRequest.Set_Result
                 (Request,
                  CORBA.To_Any
                    (Result_ü));
            end;
         elsif (Operation_ü
            = "echoLong87")
         then
            declare
               data : CORBA.Long;
               Arg_Name_U_data : constant CORBA.Identifier :=
                 CORBA.To_CORBA_String
                    ("data");
               Argument_U_data : CORBA.Any :=
                 CORBA.Internals.Get_Empty_Any
                    (CORBA.TC_Long);
               Result_ü : CORBA.Long;
            begin
               CORBA.NVList.Add_Item
                 (Argument_List_ü,
                  Arg_Name_U_data,
                  Argument_U_data,
                  CORBA.ARG_IN);
               CORBA.ServerRequest.Arguments
                 (Request,
                  Argument_List_ü);
               --  Convert from Any
               data :=
                 CORBA.From_Any
                    (Argument_U_data);
               --  Call Implementation
               Result_ü :=
                 test_hash.Impl.echoLong87
                    (test_hash.Impl.Object'Class
                       (Self.all)'Access,
                     data);
               --  Setting the result
               CORBA.ServerRequest.Set_Result
                 (Request,
                  CORBA.To_Any
                    (Result_ü));
            end;
         elsif (Operation_ü
            = "echoLong88")
         then
            declare
               data : CORBA.Long;
               Arg_Name_U_data : constant CORBA.Identifier :=
                 CORBA.To_CORBA_String
                    ("data");
               Argument_U_data : CORBA.Any :=
                 CORBA.Internals.Get_Empty_Any
                    (CORBA.TC_Long);
               Result_ü : CORBA.Long;
            begin
               CORBA.NVList.Add_Item
                 (Argument_List_ü,
                  Arg_Name_U_data,
                  Argument_U_data,
                  CORBA.ARG_IN);
               CORBA.ServerRequest.Arguments
                 (Request,
                  Argument_List_ü);
               --  Convert from Any
               data :=
                 CORBA.From_Any
                    (Argument_U_data);
               --  Call Implementation
               Result_ü :=
                 test_hash.Impl.echoLong88
                    (test_hash.Impl.Object'Class
                       (Self.all)'Access,
                     data);
               --  Setting the result
               CORBA.ServerRequest.Set_Result
                 (Request,
                  CORBA.To_Any
                    (Result_ü));
            end;
         elsif (Operation_ü
            = "echoLong89")
         then
            declare
               data : CORBA.Long;
               Arg_Name_U_data : constant CORBA.Identifier :=
                 CORBA.To_CORBA_String
                    ("data");
               Argument_U_data : CORBA.Any :=
                 CORBA.Internals.Get_Empty_Any
                    (CORBA.TC_Long);
               Result_ü : CORBA.Long;
            begin
               CORBA.NVList.Add_Item
                 (Argument_List_ü,
                  Arg_Name_U_data,
                  Argument_U_data,
                  CORBA.ARG_IN);
               CORBA.ServerRequest.Arguments
                 (Request,
                  Argument_List_ü);
               --  Convert from Any
               data :=
                 CORBA.From_Any
                    (Argument_U_data);
               --  Call Implementation
               Result_ü :=
                 test_hash.Impl.echoLong89
                    (test_hash.Impl.Object'Class
                       (Self.all)'Access,
                     data);
               --  Setting the result
               CORBA.ServerRequest.Set_Result
                 (Request,
                  CORBA.To_Any
                    (Result_ü));
            end;
         elsif (Operation_ü
            = "echoLong90")
         then
            declare
               data : CORBA.Long;
               Arg_Name_U_data : constant CORBA.Identifier :=
                 CORBA.To_CORBA_String
                    ("data");
               Argument_U_data : CORBA.Any :=
                 CORBA.Internals.Get_Empty_Any
                    (CORBA.TC_Long);
               Result_ü : CORBA.Long;
            begin
               CORBA.NVList.Add_Item
                 (Argument_List_ü,
                  Arg_Name_U_data,
                  Argument_U_data,
                  CORBA.ARG_IN);
               CORBA.ServerRequest.Arguments
                 (Request,
                  Argument_List_ü);
               --  Convert from Any
               data :=
                 CORBA.From_Any
                    (Argument_U_data);
               --  Call Implementation
               Result_ü :=
                 test_hash.Impl.echoLong90
                    (test_hash.Impl.Object'Class
                       (Self.all)'Access,
                     data);
               --  Setting the result
               CORBA.ServerRequest.Set_Result
                 (Request,
                  CORBA.To_Any
                    (Result_ü));
            end;
         elsif (Operation_ü
            = "echoLong91")
         then
            declare
               data : CORBA.Long;
               Arg_Name_U_data : constant CORBA.Identifier :=
                 CORBA.To_CORBA_String
                    ("data");
               Argument_U_data : CORBA.Any :=
                 CORBA.Internals.Get_Empty_Any
                    (CORBA.TC_Long);
               Result_ü : CORBA.Long;
            begin
               CORBA.NVList.Add_Item
                 (Argument_List_ü,
                  Arg_Name_U_data,
                  Argument_U_data,
                  CORBA.ARG_IN);
               CORBA.ServerRequest.Arguments
                 (Request,
                  Argument_List_ü);
               --  Convert from Any
               data :=
                 CORBA.From_Any
                    (Argument_U_data);
               --  Call Implementation
               Result_ü :=
                 test_hash.Impl.echoLong91
                    (test_hash.Impl.Object'Class
                       (Self.all)'Access,
                     data);
               --  Setting the result
               CORBA.ServerRequest.Set_Result
                 (Request,
                  CORBA.To_Any
                    (Result_ü));
            end;
         elsif (Operation_ü
            = "echoLong92")
         then
            declare
               data : CORBA.Long;
               Arg_Name_U_data : constant CORBA.Identifier :=
                 CORBA.To_CORBA_String
                    ("data");
               Argument_U_data : CORBA.Any :=
                 CORBA.Internals.Get_Empty_Any
                    (CORBA.TC_Long);
               Result_ü : CORBA.Long;
            begin
               CORBA.NVList.Add_Item
                 (Argument_List_ü,
                  Arg_Name_U_data,
                  Argument_U_data,
                  CORBA.ARG_IN);
               CORBA.ServerRequest.Arguments
                 (Request,
                  Argument_List_ü);
               --  Convert from Any
               data :=
                 CORBA.From_Any
                    (Argument_U_data);
               --  Call Implementation
               Result_ü :=
                 test_hash.Impl.echoLong92
                    (test_hash.Impl.Object'Class
                       (Self.all)'Access,
                     data);
               --  Setting the result
               CORBA.ServerRequest.Set_Result
                 (Request,
                  CORBA.To_Any
                    (Result_ü));
            end;
         elsif (Operation_ü
            = "echoLong93")
         then
            declare
               data : CORBA.Long;
               Arg_Name_U_data : constant CORBA.Identifier :=
                 CORBA.To_CORBA_String
                    ("data");
               Argument_U_data : CORBA.Any :=
                 CORBA.Internals.Get_Empty_Any
                    (CORBA.TC_Long);
               Result_ü : CORBA.Long;
            begin
               CORBA.NVList.Add_Item
                 (Argument_List_ü,
                  Arg_Name_U_data,
                  Argument_U_data,
                  CORBA.ARG_IN);
               CORBA.ServerRequest.Arguments
                 (Request,
                  Argument_List_ü);
               --  Convert from Any
               data :=
                 CORBA.From_Any
                    (Argument_U_data);
               --  Call Implementation
               Result_ü :=
                 test_hash.Impl.echoLong93
                    (test_hash.Impl.Object'Class
                       (Self.all)'Access,
                     data);
               --  Setting the result
               CORBA.ServerRequest.Set_Result
                 (Request,
                  CORBA.To_Any
                    (Result_ü));
            end;
         elsif (Operation_ü
            = "echoLong94")
         then
            declare
               data : CORBA.Long;
               Arg_Name_U_data : constant CORBA.Identifier :=
                 CORBA.To_CORBA_String
                    ("data");
               Argument_U_data : CORBA.Any :=
                 CORBA.Internals.Get_Empty_Any
                    (CORBA.TC_Long);
               Result_ü : CORBA.Long;
            begin
               CORBA.NVList.Add_Item
                 (Argument_List_ü,
                  Arg_Name_U_data,
                  Argument_U_data,
                  CORBA.ARG_IN);
               CORBA.ServerRequest.Arguments
                 (Request,
                  Argument_List_ü);
               --  Convert from Any
               data :=
                 CORBA.From_Any
                    (Argument_U_data);
               --  Call Implementation
               Result_ü :=
                 test_hash.Impl.echoLong94
                    (test_hash.Impl.Object'Class
                       (Self.all)'Access,
                     data);
               --  Setting the result
               CORBA.ServerRequest.Set_Result
                 (Request,
                  CORBA.To_Any
                    (Result_ü));
            end;
         elsif (Operation_ü
            = "echoLong95")
         then
            declare
               data : CORBA.Long;
               Arg_Name_U_data : constant CORBA.Identifier :=
                 CORBA.To_CORBA_String
                    ("data");
               Argument_U_data : CORBA.Any :=
                 CORBA.Internals.Get_Empty_Any
                    (CORBA.TC_Long);
               Result_ü : CORBA.Long;
            begin
               CORBA.NVList.Add_Item
                 (Argument_List_ü,
                  Arg_Name_U_data,
                  Argument_U_data,
                  CORBA.ARG_IN);
               CORBA.ServerRequest.Arguments
                 (Request,
                  Argument_List_ü);
               --  Convert from Any
               data :=
                 CORBA.From_Any
                    (Argument_U_data);
               --  Call Implementation
               Result_ü :=
                 test_hash.Impl.echoLong95
                    (test_hash.Impl.Object'Class
                       (Self.all)'Access,
                     data);
               --  Setting the result
               CORBA.ServerRequest.Set_Result
                 (Request,
                  CORBA.To_Any
                    (Result_ü));
            end;
         elsif (Operation_ü
            = "echoLong96")
         then
            declare
               data : CORBA.Long;
               Arg_Name_U_data : constant CORBA.Identifier :=
                 CORBA.To_CORBA_String
                    ("data");
               Argument_U_data : CORBA.Any :=
                 CORBA.Internals.Get_Empty_Any
                    (CORBA.TC_Long);
               Result_ü : CORBA.Long;
            begin
               CORBA.NVList.Add_Item
                 (Argument_List_ü,
                  Arg_Name_U_data,
                  Argument_U_data,
                  CORBA.ARG_IN);
               CORBA.ServerRequest.Arguments
                 (Request,
                  Argument_List_ü);
               --  Convert from Any
               data :=
                 CORBA.From_Any
                    (Argument_U_data);
               --  Call Implementation
               Result_ü :=
                 test_hash.Impl.echoLong96
                    (test_hash.Impl.Object'Class
                       (Self.all)'Access,
                     data);
               --  Setting the result
               CORBA.ServerRequest.Set_Result
                 (Request,
                  CORBA.To_Any
                    (Result_ü));
            end;
         elsif (Operation_ü
            = "echoLong97")
         then
            declare
               data : CORBA.Long;
               Arg_Name_U_data : constant CORBA.Identifier :=
                 CORBA.To_CORBA_String
                    ("data");
               Argument_U_data : CORBA.Any :=
                 CORBA.Internals.Get_Empty_Any
                    (CORBA.TC_Long);
               Result_ü : CORBA.Long;
            begin
               CORBA.NVList.Add_Item
                 (Argument_List_ü,
                  Arg_Name_U_data,
                  Argument_U_data,
                  CORBA.ARG_IN);
               CORBA.ServerRequest.Arguments
                 (Request,
                  Argument_List_ü);
               --  Convert from Any
               data :=
                 CORBA.From_Any
                    (Argument_U_data);
               --  Call Implementation
               Result_ü :=
                 test_hash.Impl.echoLong97
                    (test_hash.Impl.Object'Class
                       (Self.all)'Access,
                     data);
               --  Setting the result
               CORBA.ServerRequest.Set_Result
                 (Request,
                  CORBA.To_Any
                    (Result_ü));
            end;
         elsif (Operation_ü
            = "echoLong98")
         then
            declare
               data : CORBA.Long;
               Arg_Name_U_data : constant CORBA.Identifier :=
                 CORBA.To_CORBA_String
                    ("data");
               Argument_U_data : CORBA.Any :=
                 CORBA.Internals.Get_Empty_Any
                    (CORBA.TC_Long);
               Result_ü : CORBA.Long;
            begin
               CORBA.NVList.Add_Item
                 (Argument_List_ü,
                  Arg_Name_U_data,
                  Argument_U_data,
                  CORBA.ARG_IN);
               CORBA.ServerRequest.Arguments
                 (Request,
                  Argument_List_ü);
               --  Convert from Any
               data :=
                 CORBA.From_Any
                    (Argument_U_data);
               --  Call Implementation
               Result_ü :=
                 test_hash.Impl.echoLong98
                    (test_hash.Impl.Object'Class
                       (Self.all)'Access,
                     data);
               --  Setting the result
               CORBA.ServerRequest.Set_Result
                 (Request,
                  CORBA.To_Any
                    (Result_ü));
            end;
         elsif (Operation_ü
            = "echoLong99")
         then
            declare
               data : CORBA.Long;
               Arg_Name_U_data : constant CORBA.Identifier :=
                 CORBA.To_CORBA_String
                    ("data");
               Argument_U_data : CORBA.Any :=
                 CORBA.Internals.Get_Empty_Any
                    (CORBA.TC_Long);
               Result_ü : CORBA.Long;
            begin
               CORBA.NVList.Add_Item
                 (Argument_List_ü,
                  Arg_Name_U_data,
                  Argument_U_data,
                  CORBA.ARG_IN);
               CORBA.ServerRequest.Arguments
                 (Request,
                  Argument_List_ü);
               --  Convert from Any
               data :=
                 CORBA.From_Any
                    (Argument_U_data);
               --  Call Implementation
               Result_ü :=
                 test_hash.Impl.echoLong99
                    (test_hash.Impl.Object'Class
                       (Self.all)'Access,
                     data);
               --  Setting the result
               CORBA.ServerRequest.Set_Result
                 (Request,
                  CORBA.To_Any
                    (Result_ü));
            end;
         elsif (Operation_ü
            = "_interface")
         then
            begin
               PolyORB.CORBA_P.Implicit_CORBA_Methods.Handle_Interface
                 (Request,
                  Argument_List_ü,
                  Repository_Id);
            end;
         elsif (Operation_ü
            = "_domain_managers")
         then
            begin
               PolyORB.CORBA_P.Implicit_CORBA_Methods.Handle_Domain_Managers
                 (Self,
                  Request,
                  Argument_List_ü);
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

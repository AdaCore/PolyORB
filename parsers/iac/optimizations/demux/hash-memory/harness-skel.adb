with Harness.Impl;
with CORBA;
with CORBA.NVList;
with CORBA.ServerRequest;
with PortableServer;
with CORBA.ORB;
with PolyORB.Utils.Strings;
with PolyORB.Utils.Strings.Lists;
with PolyORB.Initialization;
with Harness_Operation_Hash;

package body Harness.Skel is

   N_Operations  : constant Natural := 21;

   type Procedure_Access is access procedure
     (For_Servant : in PortableServer.Servant;
      Request     : in CORBA.ServerRequest.Object_Ptr);

   Invoke_db :  array (0..2 * N_Operations + 1) of  Procedure_Access
     := (others => null);

   procedure Invoke_Is_A
     (Self : in PortableServer.Servant;
      Request : in CORBA.ServerRequest.Object_Ptr);
   pragma Inline (Invoke_Is_A);

   procedure Invoke_EchoULong1
     (Self : in PortableServer.Servant;
      Request : in CORBA.ServerRequest.Object_Ptr);
   pragma Inline (Invoke_EchoULong1);

   procedure Invoke_EchoULong2
     (Self : in PortableServer.Servant;
      Request : in CORBA.ServerRequest.Object_Ptr);
   pragma Inline (Invoke_EchoULong2);

   procedure Invoke_EchoULong3
     (Self : in PortableServer.Servant;
      Request : in CORBA.ServerRequest.Object_Ptr);
   pragma Inline (Invoke_EchoULong3);

   procedure Invoke_EchoULong4
     (Self : in PortableServer.Servant;
      Request : in CORBA.ServerRequest.Object_Ptr);
   pragma Inline (Invoke_EchoULong4);

   procedure Invoke_EchoULong5
     (Self : in PortableServer.Servant;
      Request : in CORBA.ServerRequest.Object_Ptr);
   pragma Inline (Invoke_EchoULong5);

   procedure Invoke_EchoULong6
     (Self : in PortableServer.Servant;
      Request : in CORBA.ServerRequest.Object_Ptr);
   pragma Inline (Invoke_EchoULong6);

   procedure Invoke_EchoULong7
     (Self : in PortableServer.Servant;
      Request : in CORBA.ServerRequest.Object_Ptr);
   pragma Inline (Invoke_EchoULong7);

   procedure Invoke_EchoULong8
     (Self : in PortableServer.Servant;
      Request : in CORBA.ServerRequest.Object_Ptr);
   pragma Inline (Invoke_EchoULong8);

   procedure Invoke_EchoULong9
     (Self : in PortableServer.Servant;
      Request : in CORBA.ServerRequest.Object_Ptr);
   pragma Inline (Invoke_EchoULong9);

   procedure Invoke_EchoULong10
     (Self : in PortableServer.Servant;
      Request : in CORBA.ServerRequest.Object_Ptr);
   pragma Inline (Invoke_EchoULong10);

   procedure Invoke_EchoULong11
     (Self : in PortableServer.Servant;
      Request : in CORBA.ServerRequest.Object_Ptr);
   pragma Inline (Invoke_EchoULong11);

   procedure Invoke_EchoULong12
     (Self : in PortableServer.Servant;
      Request : in CORBA.ServerRequest.Object_Ptr);
   pragma Inline (Invoke_EchoULong12);

   procedure Invoke_EchoULong13
     (Self : in PortableServer.Servant;
      Request : in CORBA.ServerRequest.Object_Ptr);
   pragma Inline (Invoke_EchoULong13);

   procedure Invoke_EchoULong14
     (Self : in PortableServer.Servant;
      Request : in CORBA.ServerRequest.Object_Ptr);
   pragma Inline (Invoke_EchoULong14);

   procedure Invoke_EchoULong15
     (Self : in PortableServer.Servant;
      Request : in CORBA.ServerRequest.Object_Ptr);
   pragma Inline (Invoke_EchoULong15);

   procedure Invoke_EchoULong16
     (Self : in PortableServer.Servant;
      Request : in CORBA.ServerRequest.Object_Ptr);
   pragma Inline (Invoke_EchoULong16);

   procedure Invoke_EchoULong17
     (Self : in PortableServer.Servant;
      Request : in CORBA.ServerRequest.Object_Ptr);
   pragma Inline (Invoke_EchoULong17);

   procedure Invoke_EchoULong18
     (Self : in PortableServer.Servant;
      Request : in CORBA.ServerRequest.Object_Ptr);
   pragma Inline (Invoke_EchoULong18);

   procedure Invoke_EchoULong19
     (Self : in PortableServer.Servant;
      Request : in CORBA.ServerRequest.Object_Ptr);
   pragma Inline (Invoke_EchoULong19);

   procedure Invoke_EchoULong20
     (Self : in PortableServer.Servant;
      Request : in CORBA.ServerRequest.Object_Ptr);
   pragma Inline (Invoke_EchoULong20);

   function Servant_Is_A
     (Obj : in PortableServer.Servant)
     return Boolean
   is
   begin
      return Obj.all
         in Harness.Impl.Object'Class;
   end Servant_Is_A;

   -----------------
   -- Invoke_Is_A --
   -----------------

   procedure Invoke_Is_A
     (Self : in PortableServer.Servant;
      Request : in CORBA.ServerRequest.Object_Ptr)
   is
      Type_Id : CORBA.String;
      Arg_Name_U_Type_Id : constant CORBA.Identifier :=
        CORBA.To_CORBA_String
        ("Type_Id");
      Argument_U_Type_Id : CORBA.Any :=
        CORBA.To_Any
        (Type_Id);
      Result_ü : CORBA.Boolean;

      Operation_ü : constant Standard.String :=
        CORBA.To_Standard_String
        (CORBA.ServerRequest.Operation
         (Request.all));
      Argument_List_ü : CORBA.NVList.Ref;

   begin
      CORBA.ORB.Create_List
        (0,
         Argument_List_ü);
      CORBA.NVList.Add_Item
        (Argument_List_ü,
         Arg_Name_U_Type_Id,
         Argument_U_Type_Id,
         CORBA.ARG_IN);
      CORBA.ServerRequest.Arguments
        (Request,
         Argument_List_ü);
      Type_Id :=
        CORBA.From_Any
        (Argument_U_Type_Id);
      Result_ü :=
        Harness.Is_A
        (CORBA.To_Standard_String
         (Type_Id));
      CORBA.ServerRequest.Set_Result
        (Request,
         CORBA.To_Any
         (Result_ü));

   end Invoke_Is_A;

   -----------------------
   -- Invoke_EchoULong1 --
   -----------------------

   Echoulong1_Arg_Name_U_Arg : constant CORBA.Identifier :=
     CORBA.To_CORBA_String
     ("arg");
   procedure Invoke_EchoULong1
     (Self : in PortableServer.Servant;
      Request : in CORBA.ServerRequest.Object_Ptr)
   is
      arg : CORBA.Unsigned_Long;
      Argument_U_arg : CORBA.Any :=
        CORBA.Get_Empty_Any
        (CORBA.TC_Unsigned_Long);
      Result_ü : CORBA.Unsigned_Long;
      Argument_List_ü : CORBA.NVList.Ref;
   begin
      CORBA.ORB.Create_List
        (0,
         Argument_List_ü);
      CORBA.NVList.Add_Item
        (Argument_List_ü,
         Echoulong1_Arg_Name_U_Arg,
         Argument_U_arg,
         CORBA.ARG_IN);
      CORBA.ServerRequest.Arguments
        (Request,
         Argument_List_ü);
      arg :=
        CORBA.From_Any
        (Argument_U_arg);
      Result_ü :=
        Harness.Impl.echoULong1
        (Harness.Impl.Object'Class
         (Self.all)'Access,
         arg);
      CORBA.ServerRequest.Set_Result
        (Request,
         CORBA.To_Any
         (Result_ü));
   end Invoke_EchoULong1;

   -----------------------
   -- Invoke_EchoULong2 --
   -----------------------

   Echoulong2_Arg_Name_U_Arg : constant CORBA.Identifier :=
     CORBA.To_CORBA_String
     ("arg");
   procedure Invoke_EchoULong2
     (Self : in PortableServer.Servant;
      Request : in CORBA.ServerRequest.Object_Ptr)
   is
      arg : CORBA.Unsigned_Long;
      Argument_U_arg : CORBA.Any :=
        CORBA.Get_Empty_Any
        (CORBA.TC_Unsigned_Long);
      Result_ü : CORBA.Unsigned_Long;
      Argument_List_ü : CORBA.NVList.Ref;
   begin
      CORBA.ORB.Create_List
        (0,
         Argument_List_ü);
      CORBA.NVList.Add_Item
        (Argument_List_ü,
         Echoulong1_Arg_Name_U_Arg,
         Argument_U_arg,
         CORBA.ARG_IN);
      CORBA.ServerRequest.Arguments
        (Request,
         Argument_List_ü);
      arg :=
        CORBA.From_Any
        (Argument_U_arg);
      Result_ü :=
        Harness.Impl.echoULong2
        (Harness.Impl.Object'Class
         (Self.all)'Access,
         arg);
      CORBA.ServerRequest.Set_Result
        (Request,
         CORBA.To_Any
         (Result_ü));
   end Invoke_EchoULong2;

   -----------------------
   -- Invoke_EchoULong3 --
   -----------------------

   Echoulong3_Arg_Name_U_Arg : constant CORBA.Identifier :=
     CORBA.To_CORBA_String
     ("arg");
   procedure Invoke_EchoULong3
     (Self : in PortableServer.Servant;
      Request : in CORBA.ServerRequest.Object_Ptr)
   is
      arg : CORBA.Unsigned_Long;
      Argument_U_arg : CORBA.Any :=
        CORBA.Get_Empty_Any
        (CORBA.TC_Unsigned_Long);
      Result_ü : CORBA.Unsigned_Long;
      Argument_List_ü : CORBA.NVList.Ref;
   begin
      CORBA.ORB.Create_List
        (0,
         Argument_List_ü);
      CORBA.NVList.Add_Item
        (Argument_List_ü,
         Echoulong3_Arg_Name_U_Arg,
         Argument_U_arg,
         CORBA.ARG_IN);
      CORBA.ServerRequest.Arguments
        (Request,
         Argument_List_ü);
      arg :=
        CORBA.From_Any
        (Argument_U_arg);
      Result_ü :=
        Harness.Impl.echoULong3
        (Harness.Impl.Object'Class
         (Self.all)'Access,
         arg);
      CORBA.ServerRequest.Set_Result
        (Request,
         CORBA.To_Any
         (Result_ü));
   end Invoke_EchoULong3;

   -----------------------
   -- Invoke_EchoULong4 --
   -----------------------

   Echoulong4_Arg_Name_U_Arg : constant CORBA.Identifier :=
     CORBA.To_CORBA_String
     ("arg");
   procedure Invoke_EchoULong4
     (Self : in PortableServer.Servant;
      Request : in CORBA.ServerRequest.Object_Ptr)
   is
      arg : CORBA.Unsigned_Long;
      Argument_U_arg : CORBA.Any :=
        CORBA.Get_Empty_Any
        (CORBA.TC_Unsigned_Long);
      Result_ü : CORBA.Unsigned_Long;
      Argument_List_ü : CORBA.NVList.Ref;
   begin
      CORBA.ORB.Create_List
        (0,
         Argument_List_ü);
      CORBA.NVList.Add_Item
        (Argument_List_ü,
         Echoulong4_Arg_Name_U_Arg,
         Argument_U_arg,
         CORBA.ARG_IN);
      CORBA.ServerRequest.Arguments
        (Request,
         Argument_List_ü);
      arg :=
        CORBA.From_Any
        (Argument_U_arg);
      Result_ü :=
        Harness.Impl.echoULong4
        (Harness.Impl.Object'Class
         (Self.all)'Access,
         arg);
      CORBA.ServerRequest.Set_Result
        (Request,
         CORBA.To_Any
         (Result_ü));
   end Invoke_EchoULong4;

   -----------------------
   -- Invoke_EchoULong5 --
   -----------------------

   Echoulong5_Arg_Name_U_Arg : constant CORBA.Identifier :=
     CORBA.To_CORBA_String
     ("arg");
   procedure Invoke_EchoULong5
     (Self : in PortableServer.Servant;
      Request : in CORBA.ServerRequest.Object_Ptr)
   is
      arg : CORBA.Unsigned_Long;
      Argument_U_arg : CORBA.Any :=
        CORBA.Get_Empty_Any
        (CORBA.TC_Unsigned_Long);
      Result_ü : CORBA.Unsigned_Long;
      Argument_List_ü : CORBA.NVList.Ref;
   begin
      CORBA.ORB.Create_List
        (0,
         Argument_List_ü);
      CORBA.NVList.Add_Item
        (Argument_List_ü,
         Echoulong5_Arg_Name_U_Arg,
         Argument_U_arg,
         CORBA.ARG_IN);
      CORBA.ServerRequest.Arguments
        (Request,
         Argument_List_ü);
      arg :=
        CORBA.From_Any
        (Argument_U_arg);
      Result_ü :=
        Harness.Impl.echoULong1
        (Harness.Impl.Object'Class
         (Self.all)'Access,
         arg);
      CORBA.ServerRequest.Set_Result
        (Request,
         CORBA.To_Any
         (Result_ü));
   end Invoke_EchoULong5;

   -----------------------
   -- Invoke_EchoULong6 --
   -----------------------

   Echoulong6_Arg_Name_U_Arg : constant CORBA.Identifier :=
     CORBA.To_CORBA_String
     ("arg");
   procedure Invoke_EchoULong6
     (Self : in PortableServer.Servant;
      Request : in CORBA.ServerRequest.Object_Ptr)
   is
      arg : CORBA.Unsigned_Long;
      Argument_U_arg : CORBA.Any :=
        CORBA.Get_Empty_Any
        (CORBA.TC_Unsigned_Long);
      Result_ü : CORBA.Unsigned_Long;
      Argument_List_ü : CORBA.NVList.Ref;
   begin
      CORBA.ORB.Create_List
        (0,
         Argument_List_ü);
      CORBA.NVList.Add_Item
        (Argument_List_ü,
         Echoulong6_Arg_Name_U_Arg,
         Argument_U_arg,
         CORBA.ARG_IN);
      CORBA.ServerRequest.Arguments
        (Request,
         Argument_List_ü);
      arg :=
        CORBA.From_Any
        (Argument_U_arg);
      Result_ü :=
        Harness.Impl.echoULong6
        (Harness.Impl.Object'Class
         (Self.all)'Access,
         arg);
      CORBA.ServerRequest.Set_Result
        (Request,
         CORBA.To_Any
         (Result_ü));
   end Invoke_EchoULong6;

   -----------------------
   -- Invoke_EchoULong7 --
   -----------------------

   Echoulong7_Arg_Name_U_Arg : constant CORBA.Identifier :=
     CORBA.To_CORBA_String
     ("arg");
   procedure Invoke_EchoULong7
     (Self : in PortableServer.Servant;
      Request : in CORBA.ServerRequest.Object_Ptr)
   is
      arg : CORBA.Unsigned_Long;
      Argument_U_arg : CORBA.Any :=
        CORBA.Get_Empty_Any
        (CORBA.TC_Unsigned_Long);
      Result_ü : CORBA.Unsigned_Long;
      Argument_List_ü : CORBA.NVList.Ref;
   begin
      CORBA.ORB.Create_List
        (0,
         Argument_List_ü);
      CORBA.NVList.Add_Item
        (Argument_List_ü,
         Echoulong7_Arg_Name_U_Arg,
         Argument_U_arg,
         CORBA.ARG_IN);
      CORBA.ServerRequest.Arguments
        (Request,
         Argument_List_ü);
      arg :=
        CORBA.From_Any
        (Argument_U_arg);
      Result_ü :=
        Harness.Impl.echoULong7
        (Harness.Impl.Object'Class
         (Self.all)'Access,
         arg);
      CORBA.ServerRequest.Set_Result
        (Request,
         CORBA.To_Any
         (Result_ü));
   end Invoke_EchoULong7;

   -----------------------
   -- Invoke_EchoULong8 --
   -----------------------

   Echoulong8_Arg_Name_U_Arg : constant CORBA.Identifier :=
     CORBA.To_CORBA_String
     ("arg");
   procedure Invoke_EchoULong8
     (Self : in PortableServer.Servant;
      Request : in CORBA.ServerRequest.Object_Ptr)
   is
      arg : CORBA.Unsigned_Long;
      Argument_U_arg : CORBA.Any :=
        CORBA.Get_Empty_Any
        (CORBA.TC_Unsigned_Long);
      Result_ü : CORBA.Unsigned_Long;
      Argument_List_ü : CORBA.NVList.Ref;
   begin
      CORBA.ORB.Create_List
        (0,
         Argument_List_ü);
      CORBA.NVList.Add_Item
        (Argument_List_ü,
         Echoulong8_Arg_Name_U_Arg,
         Argument_U_arg,
         CORBA.ARG_IN);
      CORBA.ServerRequest.Arguments
        (Request,
         Argument_List_ü);
      arg :=
        CORBA.From_Any
        (Argument_U_arg);
      Result_ü :=
        Harness.Impl.echoULong8
        (Harness.Impl.Object'Class
         (Self.all)'Access,
         arg);
      CORBA.ServerRequest.Set_Result
        (Request,
         CORBA.To_Any
         (Result_ü));
   end Invoke_EchoULong8;

   -----------------------
   -- Invoke_EchoULong9 --
   -----------------------

   Echoulong9_Arg_Name_U_Arg : constant CORBA.Identifier :=
     CORBA.To_CORBA_String
     ("arg");
   procedure Invoke_EchoULong9
     (Self : in PortableServer.Servant;
      Request : in CORBA.ServerRequest.Object_Ptr)
   is
      arg : CORBA.Unsigned_Long;
      Argument_U_arg : CORBA.Any :=
        CORBA.Get_Empty_Any
        (CORBA.TC_Unsigned_Long);
      Result_ü : CORBA.Unsigned_Long;
      Argument_List_ü : CORBA.NVList.Ref;
   begin
      CORBA.ORB.Create_List
        (0,
         Argument_List_ü);
      CORBA.NVList.Add_Item
        (Argument_List_ü,
         Echoulong9_Arg_Name_U_Arg,
         Argument_U_arg,
         CORBA.ARG_IN);
      CORBA.ServerRequest.Arguments
        (Request,
         Argument_List_ü);
      arg :=
        CORBA.From_Any
        (Argument_U_arg);
      Result_ü :=
        Harness.Impl.echoULong9
        (Harness.Impl.Object'Class
         (Self.all)'Access,
         arg);
      CORBA.ServerRequest.Set_Result
        (Request,
         CORBA.To_Any
         (Result_ü));
   end Invoke_EchoULong9;

   -----------------------
   -- Invoke_EchoULong10 --
   -----------------------

   Echoulong10_Arg_Name_U_Arg : constant CORBA.Identifier :=
     CORBA.To_CORBA_String
     ("arg");
   procedure Invoke_EchoULong10
     (Self : in PortableServer.Servant;
      Request : in CORBA.ServerRequest.Object_Ptr)
   is
      arg : CORBA.Unsigned_Long;
      Argument_U_arg : CORBA.Any :=
        CORBA.Get_Empty_Any
        (CORBA.TC_Unsigned_Long);
      Result_ü : CORBA.Unsigned_Long;
      Argument_List_ü : CORBA.NVList.Ref;
   begin
      CORBA.ORB.Create_List
        (0,
         Argument_List_ü);
      CORBA.NVList.Add_Item
        (Argument_List_ü,
         Echoulong10_Arg_Name_U_Arg,
         Argument_U_arg,
         CORBA.ARG_IN);
      CORBA.ServerRequest.Arguments
        (Request,
         Argument_List_ü);
      arg :=
        CORBA.From_Any
        (Argument_U_arg);
      Result_ü :=
        Harness.Impl.echoULong10
        (Harness.Impl.Object'Class
         (Self.all)'Access,
         arg);
      CORBA.ServerRequest.Set_Result
        (Request,
         CORBA.To_Any
         (Result_ü));
   end Invoke_EchoULong10;

   -----------------------
   -- Invoke_EchoULong11 --
   -----------------------

   Echoulong11_Arg_Name_U_Arg : constant CORBA.Identifier :=
     CORBA.To_CORBA_String
     ("arg");
   procedure Invoke_EchoULong11
     (Self : in PortableServer.Servant;
      Request : in CORBA.ServerRequest.Object_Ptr)
   is
      arg : CORBA.Unsigned_Long;
      Argument_U_arg : CORBA.Any :=
        CORBA.Get_Empty_Any
        (CORBA.TC_Unsigned_Long);
      Result_ü : CORBA.Unsigned_Long;
      Argument_List_ü : CORBA.NVList.Ref;
   begin
      CORBA.ORB.Create_List
        (0,
         Argument_List_ü);
      CORBA.NVList.Add_Item
        (Argument_List_ü,
         Echoulong11_Arg_Name_U_Arg,
         Argument_U_arg,
         CORBA.ARG_IN);
      CORBA.ServerRequest.Arguments
        (Request,
         Argument_List_ü);
      arg :=
        CORBA.From_Any
        (Argument_U_arg);
      Result_ü :=
        Harness.Impl.echoULong11
        (Harness.Impl.Object'Class
         (Self.all)'Access,
         arg);
      CORBA.ServerRequest.Set_Result
        (Request,
         CORBA.To_Any
         (Result_ü));
   end Invoke_EchoULong11;

   -----------------------
   -- Invoke_EchoULong12 --
   -----------------------

   Echoulong12_Arg_Name_U_Arg : constant CORBA.Identifier :=
     CORBA.To_CORBA_String
     ("arg");
   procedure Invoke_EchoULong12
     (Self : in PortableServer.Servant;
      Request : in CORBA.ServerRequest.Object_Ptr)
   is
      arg : CORBA.Unsigned_Long;
      Argument_U_arg : CORBA.Any :=
        CORBA.Get_Empty_Any
        (CORBA.TC_Unsigned_Long);
      Result_ü : CORBA.Unsigned_Long;
      Argument_List_ü : CORBA.NVList.Ref;
   begin
      CORBA.ORB.Create_List
        (0,
         Argument_List_ü);
      CORBA.NVList.Add_Item
        (Argument_List_ü,
         Echoulong12_Arg_Name_U_Arg,
         Argument_U_arg,
         CORBA.ARG_IN);
      CORBA.ServerRequest.Arguments
        (Request,
         Argument_List_ü);
      arg :=
        CORBA.From_Any
        (Argument_U_arg);
      Result_ü :=
        Harness.Impl.echoULong12
        (Harness.Impl.Object'Class
         (Self.all)'Access,
         arg);
      CORBA.ServerRequest.Set_Result
        (Request,
         CORBA.To_Any
         (Result_ü));
   end Invoke_EchoULong12;

   -----------------------
   -- Invoke_EchoULong13 --
   -----------------------

   Echoulong13_Arg_Name_U_Arg : constant CORBA.Identifier :=
     CORBA.To_CORBA_String
     ("arg");
   procedure Invoke_EchoULong13
     (Self : in PortableServer.Servant;
      Request : in CORBA.ServerRequest.Object_Ptr)
   is
      arg : CORBA.Unsigned_Long;
      Argument_U_arg : CORBA.Any :=
        CORBA.Get_Empty_Any
        (CORBA.TC_Unsigned_Long);
      Result_ü : CORBA.Unsigned_Long;
      Argument_List_ü : CORBA.NVList.Ref;
   begin
      CORBA.ORB.Create_List
        (0,
         Argument_List_ü);
      CORBA.NVList.Add_Item
        (Argument_List_ü,
         Echoulong13_Arg_Name_U_Arg,
         Argument_U_arg,
         CORBA.ARG_IN);
      CORBA.ServerRequest.Arguments
        (Request,
         Argument_List_ü);
      arg :=
        CORBA.From_Any
        (Argument_U_arg);
      Result_ü :=
        Harness.Impl.echoULong13
        (Harness.Impl.Object'Class
         (Self.all)'Access,
         arg);
      CORBA.ServerRequest.Set_Result
        (Request,
         CORBA.To_Any
         (Result_ü));
   end Invoke_EchoULong13;

   -----------------------
   -- Invoke_EchoULong14 --
   -----------------------

   Echoulong14_Arg_Name_U_Arg : constant CORBA.Identifier :=
     CORBA.To_CORBA_String
     ("arg");
   procedure Invoke_EchoULong14
     (Self : in PortableServer.Servant;
      Request : in CORBA.ServerRequest.Object_Ptr)
   is
      arg : CORBA.Unsigned_Long;
      Argument_U_arg : CORBA.Any :=
        CORBA.Get_Empty_Any
        (CORBA.TC_Unsigned_Long);
      Result_ü : CORBA.Unsigned_Long;
      Argument_List_ü : CORBA.NVList.Ref;
   begin
      CORBA.ORB.Create_List
        (0,
         Argument_List_ü);
      CORBA.NVList.Add_Item
        (Argument_List_ü,
         Echoulong14_Arg_Name_U_Arg,
         Argument_U_arg,
         CORBA.ARG_IN);
      CORBA.ServerRequest.Arguments
        (Request,
         Argument_List_ü);
      arg :=
        CORBA.From_Any
        (Argument_U_arg);
      Result_ü :=
        Harness.Impl.echoULong14
        (Harness.Impl.Object'Class
         (Self.all)'Access,
         arg);
      CORBA.ServerRequest.Set_Result
        (Request,
         CORBA.To_Any
         (Result_ü));
   end Invoke_EchoULong14;

   -----------------------
   -- Invoke_EchoULong15 --
   -----------------------

   Echoulong15_Arg_Name_U_Arg : constant CORBA.Identifier :=
     CORBA.To_CORBA_String
     ("arg");
   procedure Invoke_EchoULong15
     (Self : in PortableServer.Servant;
      Request : in CORBA.ServerRequest.Object_Ptr)
   is
      arg : CORBA.Unsigned_Long;
      Argument_U_arg : CORBA.Any :=
        CORBA.Get_Empty_Any
        (CORBA.TC_Unsigned_Long);
      Result_ü : CORBA.Unsigned_Long;
      Argument_List_ü : CORBA.NVList.Ref;
   begin
      CORBA.ORB.Create_List
        (0,
         Argument_List_ü);
      CORBA.NVList.Add_Item
        (Argument_List_ü,
         Echoulong15_Arg_Name_U_Arg,
         Argument_U_arg,
         CORBA.ARG_IN);
      CORBA.ServerRequest.Arguments
        (Request,
         Argument_List_ü);
      arg :=
        CORBA.From_Any
        (Argument_U_arg);
      Result_ü :=
        Harness.Impl.echoULong15
        (Harness.Impl.Object'Class
         (Self.all)'Access,
         arg);
      CORBA.ServerRequest.Set_Result
        (Request,
         CORBA.To_Any
         (Result_ü));
   end Invoke_EchoULong15;

   -----------------------
   -- Invoke_EchoULong16 --
   -----------------------

   Echoulong16_Arg_Name_U_Arg : constant CORBA.Identifier :=
     CORBA.To_CORBA_String
     ("arg");
   procedure Invoke_EchoULong16
     (Self : in PortableServer.Servant;
      Request : in CORBA.ServerRequest.Object_Ptr)
   is
      arg : CORBA.Unsigned_Long;
      Argument_U_arg : CORBA.Any :=
        CORBA.Get_Empty_Any
        (CORBA.TC_Unsigned_Long);
      Result_ü : CORBA.Unsigned_Long;
      Argument_List_ü : CORBA.NVList.Ref;
   begin
      CORBA.ORB.Create_List
        (0,
         Argument_List_ü);
      CORBA.NVList.Add_Item
        (Argument_List_ü,
         Echoulong1_Arg_Name_U_Arg,
         Argument_U_arg,
         CORBA.ARG_IN);
      CORBA.ServerRequest.Arguments
        (Request,
         Argument_List_ü);
      arg :=
        CORBA.From_Any
        (Argument_U_arg);
      Result_ü :=
        Harness.Impl.echoULong16
        (Harness.Impl.Object'Class
         (Self.all)'Access,
         arg);
      CORBA.ServerRequest.Set_Result
        (Request,
         CORBA.To_Any
         (Result_ü));
   end Invoke_EchoULong16;

   -----------------------
   -- Invoke_EchoULong17 --
   -----------------------

   Echoulong17_Arg_Name_U_Arg : constant CORBA.Identifier :=
     CORBA.To_CORBA_String
     ("arg");
   procedure Invoke_EchoULong17
     (Self : in PortableServer.Servant;
      Request : in CORBA.ServerRequest.Object_Ptr)
   is
      arg : CORBA.Unsigned_Long;
      Argument_U_arg : CORBA.Any :=
        CORBA.Get_Empty_Any
        (CORBA.TC_Unsigned_Long);
      Result_ü : CORBA.Unsigned_Long;
      Argument_List_ü : CORBA.NVList.Ref;
   begin
      CORBA.ORB.Create_List
        (0,
         Argument_List_ü);
      CORBA.NVList.Add_Item
        (Argument_List_ü,
         Echoulong17_Arg_Name_U_Arg,
         Argument_U_arg,
         CORBA.ARG_IN);
      CORBA.ServerRequest.Arguments
        (Request,
         Argument_List_ü);
      arg :=
        CORBA.From_Any
        (Argument_U_arg);
      Result_ü :=
        Harness.Impl.echoULong17
        (Harness.Impl.Object'Class
         (Self.all)'Access,
         arg);
      CORBA.ServerRequest.Set_Result
        (Request,
         CORBA.To_Any
         (Result_ü));
   end Invoke_EchoULong17;

   -----------------------
   -- Invoke_EchoULong18 --
   -----------------------

   Echoulong18_Arg_Name_U_Arg : constant CORBA.Identifier :=
     CORBA.To_CORBA_String
     ("arg");
   procedure Invoke_EchoULong18
     (Self : in PortableServer.Servant;
      Request : in CORBA.ServerRequest.Object_Ptr)
   is
      arg : CORBA.Unsigned_Long;
      Argument_U_arg : CORBA.Any :=
        CORBA.Get_Empty_Any
        (CORBA.TC_Unsigned_Long);
      Result_ü : CORBA.Unsigned_Long;
      Argument_List_ü : CORBA.NVList.Ref;
   begin
      CORBA.ORB.Create_List
        (0,
         Argument_List_ü);
      CORBA.NVList.Add_Item
        (Argument_List_ü,
         Echoulong18_Arg_Name_U_Arg,
         Argument_U_arg,
         CORBA.ARG_IN);
      CORBA.ServerRequest.Arguments
        (Request,
         Argument_List_ü);
      arg :=
        CORBA.From_Any
        (Argument_U_arg);
      Result_ü :=
        Harness.Impl.echoULong18
        (Harness.Impl.Object'Class
         (Self.all)'Access,
         arg);
      CORBA.ServerRequest.Set_Result
        (Request,
         CORBA.To_Any
         (Result_ü));
   end Invoke_EchoULong18;

   -----------------------
   -- Invoke_EchoULong19 --
   -----------------------

   Echoulong19_Arg_Name_U_Arg : constant CORBA.Identifier :=
     CORBA.To_CORBA_String
     ("arg");
   procedure Invoke_EchoULong19
     (Self : in PortableServer.Servant;
      Request : in CORBA.ServerRequest.Object_Ptr)
   is
      arg : CORBA.Unsigned_Long;
      Argument_U_arg : CORBA.Any :=
        CORBA.Get_Empty_Any
        (CORBA.TC_Unsigned_Long);
      Result_ü : CORBA.Unsigned_Long;
      Argument_List_ü : CORBA.NVList.Ref;
   begin
      CORBA.ORB.Create_List
        (0,
         Argument_List_ü);
      CORBA.NVList.Add_Item
        (Argument_List_ü,
         Echoulong19_Arg_Name_U_Arg,
         Argument_U_arg,
         CORBA.ARG_IN);
      CORBA.ServerRequest.Arguments
        (Request,
         Argument_List_ü);
      arg :=
        CORBA.From_Any
        (Argument_U_arg);
      Result_ü :=
        Harness.Impl.echoULong19
        (Harness.Impl.Object'Class
         (Self.all)'Access,
         arg);
      CORBA.ServerRequest.Set_Result
        (Request,
         CORBA.To_Any
         (Result_ü));
   end Invoke_EchoULong19;

   -----------------------
   -- Invoke_EchoULong20 --
   -----------------------

   Echoulong20_Arg_Name_U_Arg : constant CORBA.Identifier :=
     CORBA.To_CORBA_String
     ("arg");
   procedure Invoke_EchoULong20
     (Self : in PortableServer.Servant;
      Request : in CORBA.ServerRequest.Object_Ptr)
   is
      arg : CORBA.Unsigned_Long;
      Argument_U_arg : CORBA.Any :=
        CORBA.Get_Empty_Any
        (CORBA.TC_Unsigned_Long);
      Result_ü : CORBA.Unsigned_Long;
      Argument_List_ü : CORBA.NVList.Ref;
   begin
      CORBA.ORB.Create_List
        (0,
         Argument_List_ü);
      CORBA.NVList.Add_Item
        (Argument_List_ü,
         Echoulong20_Arg_Name_U_Arg,
         Argument_U_arg,
         CORBA.ARG_IN);
      CORBA.ServerRequest.Arguments
        (Request,
         Argument_List_ü);
      arg :=
        CORBA.From_Any
        (Argument_U_arg);
      Result_ü :=
        Harness.Impl.echoULong20
        (Harness.Impl.Object'Class
         (Self.all)'Access,
         arg);
      CORBA.ServerRequest.Set_Result
        (Request,
         CORBA.To_Any
         (Result_ü));
   end Invoke_EchoULong20;

   ------------------------
   -- Register_Procedure --
   ------------------------

   procedure Register_Procedure
     (Operation_Name : String;
      Invoke_Access  : Procedure_Access)
   is
      Index : Natural;
   begin
      Index := Harness_Operation_Hash.Hash (Operation_Name);

      if Invoke_Db (Index) /= null then
         raise Program_Error;
      end if;

      Invoke_db (Index) := Invoke_Access;
   end Register_Procedure;

   ------------
   -- Invoke --
   ------------

   procedure Invoke
     (Self : in PortableServer.Servant;
      Request : in CORBA.ServerRequest.Object_Ptr)
   is
      Operation_ü : constant Standard.String :=
        CORBA.To_Standard_String
           (CORBA.ServerRequest.Operation
              (Request.all));
      Argument_List_ü : CORBA.NVList.Ref;
      Index : Natural;
   begin
      Index := Harness_Operation_Hash.Hash (Operation_ü);

      if Invoke_Db (Index) /= null then
         Invoke_Db (Index).all (Self, Request);
      elsif Operation_ü
         = "_Is_A"
      then
         Invoke_Is_A (Self, Request);
              Invoke_EchoULong20 (Self, Request);
      else
         CORBA.Raise_Bad_Operation
           (CORBA.Default_Sys_Member);
      end if;
   end Invoke;

   procedure Deferred_Initialization

   is
   begin
      PortableServer.Register_Skeleton
        (CORBA.To_CORBA_String
           (Harness.Repository_Id),
         Servant_Is_A'Access,
         Invoke'Access);
      --  there's a problem when I insert in the hash greater than 20 elements
      --  Register_Procedure ("_Is_A", Invoke_Is_A'Access);
      Register_Procedure ("echoULong1", Invoke_EchoULong1'Access);
      Register_Procedure ("echoULong2", Invoke_EchoULong2'Access);
      Register_Procedure ("echoULong3", Invoke_EchoULong3'Access);
      Register_Procedure ("echoULong4", Invoke_EchoULong4'Access);
      Register_Procedure ("echoULong5", Invoke_EchoULong5'Access);
      Register_Procedure ("echoULong6", Invoke_EchoULong6'Access);
      Register_Procedure ("echoULong7", Invoke_EchoULong7'Access);
      Register_Procedure ("echoULong8", Invoke_EchoULong8'Access);
      Register_Procedure ("echoULong9", Invoke_EchoULong9'Access);
      Register_Procedure ("echoULong10", Invoke_EchoULong10'Access);
      Register_Procedure ("echoULong11", Invoke_EchoULong11'Access);
      Register_Procedure ("echoULong12", Invoke_EchoULong12'Access);
      Register_Procedure ("echoULong13", Invoke_EchoULong13'Access);
      Register_Procedure ("echoULong14", Invoke_EchoULong14'Access);
      Register_Procedure ("echoULong15", Invoke_EchoULong15'Access);
      Register_Procedure ("echoULong16", Invoke_EchoULong16'Access);
      Register_Procedure ("echoULong17", Invoke_EchoULong17'Access);
      Register_Procedure ("echoULong18", Invoke_EchoULong18'Access);
      Register_Procedure ("echoULong19", Invoke_EchoULong19'Access);
      Register_Procedure ("echoULong20", Invoke_EchoULong20'Access);
   end Deferred_Initialization;

begin
   PolyORB.Initialization.Register_Module
     (PolyORB.Initialization.Module_Info'
        (Name => PolyORB.Utils.Strings."+"
           ("Harness.Skel"),
         Conflicts => PolyORB.Utils.Strings.Lists.Empty,
         Depends => PolyORB.Utils.Strings.Lists.Empty,
         Provides => PolyORB.Utils.Strings.Lists.Empty,
         Implicit => False,
         Init => Deferred_Initialization'Access));
end Harness.Skel;

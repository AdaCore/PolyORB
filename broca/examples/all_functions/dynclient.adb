with Ada.Command_Line;
with Ada.Text_IO;
with Ada.Exceptions;

with CORBA; use CORBA;
with CORBA.Object;
with CORBA.Context;
with CORBA.Request;
with CORBA.NVList;
with CORBA.ORB;

with All_Functions; use All_Functions;
with Report; use Report;

procedure Dynclient is
   IOR : CORBA.String;
   Myall_Functions : CORBA.Object.Ref;
   I, J, K, L, M : CORBA.Short;
   Ok : Boolean;

   function Get_The_Attribute (Self : in CORBA.Object.Ref)
                               return CORBA.Short is
      Operation_Name : CORBA.Identifier := To_CORBA_String ("_get_the_attribute");
      Request : CORBA.Request.Object;
      Ctx : CORBA.Context.Ref;
      Arg_List : CORBA.NVList.Ref;
      Result_Name : CORBA.String := To_CORBA_String ("Result");
      Result : CORBA.NamedValue;
   begin
      --  creating the argument list
      CORBA.ORB.Create_List (0, Arg_List);
      --  setting the result type
      Result := (Name => Identifier (Result_Name),
                 Argument => Get_Empty_Any (CORBA.TC_Short),
                 Arg_Modes => 0);
      --  creating a request
      CORBA.Object.Create_Request (Myall_Functions,
                                   Ctx,
                                   Operation_Name,
                                   Arg_List,
                                   Result,
                                   Request,
                                   0);
      --  sending message
      CORBA.Request.Invoke (Request, 0);
      --  getting the answer
      return From_Any (CORBA.Request.Return_Value (Request).Argument);
   end Get_The_Attribute;

   procedure Set_The_Attribute (Self : in CORBA.Object.Ref;
                                To   : in CORBA.Short) is
      Operation_Name : CORBA.Identifier := To_CORBA_String ("_set_the_attribute");
      Arg_Name_To : CORBA.Identifier := To_CORBA_String ("to");
      Request : CORBA.Request.Object;
      Ctx : CORBA.Context.Ref;
      Argument : CORBA.Any;
      Arg_List : CORBA.NVList.Ref;
      Result : CORBA.NamedValue;
      Result_Name : CORBA.String := To_CORBA_String ("Result");
   begin
      --  creating the argument list
      CORBA.ORB.Create_List (0, Arg_List);
      Argument := CORBA.To_Any (To);
      CORBA.NVList.Add_Item (Arg_List,
                             Arg_Name_To,
                             Argument,
                             CORBA.ARG_IN);
      --  setting the result type
      Result := (Name => Identifier (Result_Name),
                 Argument => Get_Empty_Any (CORBA.TC_Void),
                 Arg_Modes => 0);
      --  creating a request
      CORBA.Object.Create_Request (Myall_Functions,
                                   Ctx,
                                   Operation_Name,
                                   Arg_List,
                                   Result,
                                   Request,
                                   0);
      --  sending message
      CORBA.Request.Invoke (Request, 0);
   end Set_The_Attribute;

   function Get_The_Readonly_Attribute (Self : in CORBA.Object.Ref)
                                        return CORBA.Short is
      Operation_Name : CORBA.Identifier := To_CORBA_String ("_get_the_readonly_attribute");
      Request : CORBA.Request.Object;
      Ctx : CORBA.Context.Ref;
      Arg_List : CORBA.NVList.Ref;
      Result : CORBA.NamedValue;
      Result_Name : CORBA.String := To_CORBA_String ("Result");
   begin
      --  creating the argument list
      CORBA.ORB.Create_List (0, Arg_List);
      --  setting the result type
      Result := (Name => Identifier (Result_Name),
                 Argument => Get_Empty_Any (CORBA.TC_Short),
                 Arg_Modes => 0);
      --  creating a request
      CORBA.Object.Create_Request (Myall_Functions,
                                   Ctx,
                                   Operation_Name,
                                   Arg_List,
                                   Result,
                                   Request,
                                   0);
      --  sending message
      CORBA.Request.Invoke (Request, 0);
      --  getting the answer
      return From_Any (CORBA.Request.Return_Value (Request).Argument);
   end Get_The_Readonly_Attribute;

   procedure Void_Proc (Self : in CORBA.Object.Ref) is
      Operation_Name : CORBA.Identifier := To_CORBA_String ("void_proc");
      Request : CORBA.Request.Object;
      Ctx : CORBA.Context.Ref;
      Arg_List : CORBA.NVList.Ref;
      Result : CORBA.NamedValue;
      Result_Name : CORBA.String := To_CORBA_String ("Result");
   begin
      --  creating the argument list
      CORBA.ORB.Create_List (0, Arg_List);
      --  setting the result type
      Result := (Name => Identifier (Result_Name),
                 Argument => Get_Empty_Any (CORBA.TC_Void),
                 Arg_Modes => 0);
      --  creating a request
      CORBA.Object.Create_Request (Myall_Functions,
                                   Ctx,
                                   Operation_Name,
                                   Arg_List,
                                   Result,
                                   Request,
                                   0);
      --  sending message
      CORBA.Request.Invoke (Request, 0);
   end Void_Proc;

   procedure In_Proc (Self : in CORBA.Object.Ref;
                      A, B, C : in CORBA.Short) is
      Operation_Name : CORBA.Identifier := To_CORBA_String ("in_proc");
      Arg_Name_A : CORBA.Identifier := To_CORBA_String ("a");
      Arg_Name_B : CORBA.Identifier := To_CORBA_String ("b");
      Arg_Name_C : CORBA.Identifier := To_CORBA_String ("c");
      Request : CORBA.Request.Object;
      Ctx : CORBA.Context.Ref;
      Argument_A : CORBA.Any:= CORBA.To_Any (A);
      Argument_B : CORBA.Any:= CORBA.To_Any (B);
      Argument_C : CORBA.Any:= CORBA.To_Any (C);
      Arg_List : CORBA.NVList.Ref;
      Result : CORBA.NamedValue;
      Result_Name : CORBA.String := To_CORBA_String ("Result");
   begin
      --  creating the argument list
      CORBA.ORB.Create_List (0, Arg_List);
      CORBA.NVList.Add_Item (Arg_List,
                             Arg_Name_A,
                             Argument_A,
                             CORBA.ARG_IN);
      CORBA.NVList.Add_Item (Arg_List,
                             Arg_Name_B,
                             Argument_B,
                             CORBA.ARG_IN);
      CORBA.NVList.Add_Item (Arg_List,
                             Arg_Name_C,
                             Argument_C,
                             CORBA.ARG_IN);
      --  setting the result type
      Result := (Name => Identifier (Result_Name),
                 Argument => Get_Empty_Any (CORBA.TC_Void),
                 Arg_Modes => 0);
      --  creating a request
      CORBA.Object.Create_Request (Myall_Functions,
                                   Ctx,
                                   Operation_Name,
                                   Arg_List,
                                   Result,
                                   Request,
                                   0);
      --  sending message
      CORBA.Request.Invoke (Request, 0);
   end In_Proc;

   procedure Out_Proc (Self : in CORBA.Object.Ref;
                       A, B, C : out CORBA.Short) is
      Operation_Name : CORBA.Identifier := To_CORBA_String ("out_proc");
      Arg_Name_A : CORBA.Identifier := To_CORBA_String ("a");
      Arg_Name_B : CORBA.Identifier := To_CORBA_String ("b");
      Arg_Name_C : CORBA.Identifier := To_CORBA_String ("c");
      Request : CORBA.Request.Object;
      Ctx : CORBA.Context.Ref;
      Argument_A : CORBA.Any := CORBA.To_Any (A);
      Argument_B : CORBA.Any := CORBA.To_Any (B);
      Argument_C : CORBA.Any := CORBA.To_Any (C);
      Arg_List : CORBA.NVList.Ref;
      Result : CORBA.NamedValue;
      Result_Name : CORBA.String := To_CORBA_String ("Result");
   begin
      --  creating the argument list
      CORBA.ORB.Create_List (0, Arg_List);
      CORBA.NVList.Add_Item (Arg_List,
                             Arg_Name_A,
                             Argument_A,
                             CORBA.ARG_OUT);
      CORBA.NVList.Add_Item (Arg_List,
                             Arg_Name_B,
                             Argument_B,
                             CORBA.ARG_OUT);
      CORBA.NVList.Add_Item (Arg_List,
                             Arg_Name_C,
                             Argument_C,
                             CORBA.ARG_OUT);
      --  setting the result type
      Result := (Name => Identifier (Result_Name),
                 Argument => Get_Empty_Any (CORBA.TC_Void),
                 Arg_Modes => 0);
      --  creating a request
      CORBA.Object.Create_Request (Myall_Functions,
                                   Ctx,
                                   Operation_Name,
                                   Arg_List,
                                   Result,
                                   Request,
                                   0);
      --  sending message
      CORBA.Request.Invoke (Request, 0);
      --  get out arguments
      A := CORBA.From_Any (Argument_A);
      B := CORBA.From_Any (Argument_B);
      C := CORBA.From_Any (Argument_C);
   end Out_Proc;

   procedure Inout_Proc (Self : in CORBA.Object.Ref;
                         A, B : in out CORBA.Short) is
      Operation_Name : CORBA.Identifier := To_CORBA_String ("inout_proc");
      Arg_Name_A : CORBA.Identifier := To_CORBA_String ("a");
      Arg_Name_B : CORBA.Identifier := To_CORBA_String ("b");
      Request : CORBA.Request.Object;
      Ctx : CORBA.Context.Ref;
      Argument_A : CORBA.Any := CORBA.To_Any (A);
      Argument_B : CORBA.Any := CORBA.To_Any (B);
      Arg_List : CORBA.NVList.Ref;
      Result : CORBA.NamedValue;
      Result_Name : CORBA.String := To_CORBA_String ("Result");
   begin
      --  creating the argument list
      CORBA.ORB.Create_List (0, Arg_List);
      CORBA.NVList.Add_Item (Arg_List,
                             Arg_Name_A,
                             Argument_A,
                             CORBA.ARG_INOUT);
      CORBA.NVList.Add_Item (Arg_List,
                             Arg_Name_B,
                             Argument_B,
                             CORBA.ARG_INOUT);
      --  setting the result type
      Result := (Name => Identifier (Result_Name),
                 Argument => Get_Empty_Any (CORBA.TC_Void),
                 Arg_Modes => 0);
      --  creating a request
      CORBA.Object.Create_Request (Myall_Functions,
                                   Ctx,
                                   Operation_Name,
                                   Arg_List,
                                   Result,
                                   Request,
                                   0);
      --  sending message
      CORBA.Request.Invoke (Request, 0);
      --  get out arguments
      A := CORBA.From_Any (Argument_A);
      B := CORBA.From_Any (Argument_B);
   end Inout_Proc;

   procedure In_Out_Proc (Self : in CORBA.Object.Ref;
                          A, B : in CORBA.Short;
                          C, D : out CORBA.Short) is
      Operation_Name : CORBA.Identifier := To_CORBA_String ("in_out_proc");
      Arg_Name_A : CORBA.Identifier := To_CORBA_String ("a");
      Arg_Name_B : CORBA.Identifier := To_CORBA_String ("b");
      Arg_Name_C : CORBA.Identifier := To_CORBA_String ("c");
      Arg_Name_D : CORBA.Identifier := To_CORBA_String ("d");
      Request : CORBA.Request.Object;
      Ctx : CORBA.Context.Ref;
      Argument_A : CORBA.Any := CORBA.To_Any (A);
      Argument_B : CORBA.Any := CORBA.To_Any (B);
      Argument_C : CORBA.Any := CORBA.To_Any (C);
      Argument_D : CORBA.Any := CORBA.To_Any (D);
      Arg_List : CORBA.NVList.Ref;
      Result : CORBA.NamedValue;
      Result_Name : CORBA.String := To_CORBA_String ("Result");
   begin
      --  creating the argument list
      CORBA.ORB.Create_List (0, Arg_List);
      CORBA.NVList.Add_Item (Arg_List,
                             Arg_Name_A,
                             Argument_A,
                             CORBA.ARG_IN);
      CORBA.NVList.Add_Item (Arg_List,
                             Arg_Name_B,
                             Argument_B,
                             CORBA.ARG_IN);
      CORBA.NVList.Add_Item (Arg_List,
                             Arg_Name_C,
                             Argument_C,
                             CORBA.ARG_OUT);
      CORBA.NVList.Add_Item (Arg_List,
                             Arg_Name_D,
                             Argument_D,
                             CORBA.ARG_OUT);
      --  setting the result type
      Result := (Name => Identifier (Result_Name),
                 Argument => Get_Empty_Any (CORBA.TC_Void),
                 Arg_Modes => 0);
      --  creating a request
      CORBA.Object.Create_Request (Myall_Functions,
                                   Ctx,
                                   Operation_Name,
                                   Arg_List,
                                   Result,
                                   Request,
                                   0);
      --  sending message
      CORBA.Request.Invoke (Request, 0);
      --  get out arguments
      C := CORBA.From_Any (Argument_C);
      D := CORBA.From_Any (Argument_D);
   end In_Out_Proc;

   procedure In_Inout_Proc (Self : in CORBA.Object.Ref;
                            A : in CORBA.Short;
                            B : in out CORBA.Short;
                            C : in CORBA.Short;
                            D : in out CORBA.Short) is
      Operation_Name : CORBA.Identifier := To_CORBA_String ("in_inout_proc");
      Arg_Name_A : CORBA.Identifier := To_CORBA_String ("a");
      Arg_Name_B : CORBA.Identifier := To_CORBA_String ("b");
      Arg_Name_C : CORBA.Identifier := To_CORBA_String ("c");
      Arg_Name_D : CORBA.Identifier := To_CORBA_String ("d");
      Request : CORBA.Request.Object;
      Ctx : CORBA.Context.Ref;
      Argument_A : CORBA.Any := CORBA.To_Any (A);
      Argument_B : CORBA.Any := CORBA.To_Any (B);
      Argument_C : CORBA.Any := CORBA.To_Any (C);
      Argument_D : CORBA.Any := CORBA.To_Any (D);
      Arg_List : CORBA.NVList.Ref;
      Result : CORBA.NamedValue;
      Result_Name : CORBA.String := To_CORBA_String ("Result");
   begin
      --  creating the argument list
      CORBA.ORB.Create_List (0, Arg_List);
      CORBA.NVList.Add_Item (Arg_List,
                             Arg_Name_A,
                             Argument_A,
                             CORBA.ARG_IN);
      CORBA.NVList.Add_Item (Arg_List,
                             Arg_Name_B,
                             Argument_B,
                             CORBA.ARG_INOUT);
      CORBA.NVList.Add_Item (Arg_List,
                             Arg_Name_C,
                             Argument_C,
                             CORBA.ARG_IN);
      CORBA.NVList.Add_Item (Arg_List,
                             Arg_Name_D,
                             Argument_D,
                             CORBA.ARG_INOUT);
      --  setting the result type
      Result := (Name => Identifier (Result_Name),
                 Argument => Get_Empty_Any (CORBA.TC_Void),
                 Arg_Modes => 0);
      --  creating a request
      CORBA.Object.Create_Request (Myall_Functions,
                                   Ctx,
                                   Operation_Name,
                                   Arg_List,
                                   Result,
                                   Request,
                                   0);
      --  sending message
      CORBA.Request.Invoke (Request, 0);
      --  get out arguments
      B := CORBA.From_Any (Argument_B);
      D := CORBA.From_Any (Argument_D);
   end In_Inout_Proc;

   procedure Out_Inout_Proc (Self : in CORBA.Object.Ref;
                             A : out CORBA.Short;
                             B : in out CORBA.Short;
                             C : in out CORBA.Short;
                             D : out CORBA.Short) is
      Operation_Name : CORBA.Identifier := To_CORBA_String ("out_inout_proc");
      Arg_Name_A : CORBA.Identifier := To_CORBA_String ("a");
      Arg_Name_B : CORBA.Identifier := To_CORBA_String ("b");
      Arg_Name_C : CORBA.Identifier := To_CORBA_String ("c");
      Arg_Name_D : CORBA.Identifier := To_CORBA_String ("d");
      Request : CORBA.Request.Object;
      Ctx : CORBA.Context.Ref;
      Argument_A : CORBA.Any := CORBA.To_Any (A);
      Argument_B : CORBA.Any := CORBA.To_Any (B);
      Argument_C : CORBA.Any := CORBA.To_Any (C);
      Argument_D : CORBA.Any := CORBA.To_Any (D);
      Arg_List : CORBA.NVList.Ref;
      Result : CORBA.NamedValue;
      Result_Name : CORBA.String := To_CORBA_String ("Result");
   begin
      --  creating the argument list
      CORBA.ORB.Create_List (0, Arg_List);
      CORBA.NVList.Add_Item (Arg_List,
                             Arg_Name_A,
                             Argument_A,
                             CORBA.ARG_OUT);
      CORBA.NVList.Add_Item (Arg_List,
                             Arg_Name_B,
                             Argument_B,
                             CORBA.ARG_INOUT);
      CORBA.NVList.Add_Item (Arg_List,
                             Arg_Name_C,
                             Argument_C,
                             CORBA.ARG_INOUT);
      CORBA.NVList.Add_Item (Arg_List,
                             Arg_Name_D,
                             Argument_D,
                             CORBA.ARG_OUT);
      --  setting the result type
      Result := (Name => Identifier (Result_Name),
                 Argument => Get_Empty_Any (CORBA.TC_Void),
                 Arg_Modes => 0);
      --  creating a request
      CORBA.Object.Create_Request (Myall_Functions,
                                   Ctx,
                                   Operation_Name,
                                   Arg_List,
                                   Result,
                                   Request,
                                   0);
      --  sending message
      CORBA.Request.Invoke (Request, 0);
      --  get out arguments
      A := CORBA.From_Any (Argument_A);
      B := CORBA.From_Any (Argument_B);
      C := CORBA.From_Any (Argument_C);
      D := CORBA.From_Any (Argument_D);
   end Out_Inout_Proc;

   procedure In_Out_Inout_Proc (Self : in CORBA.Object.Ref;
                                A : in CORBA.Short;
                                B : out CORBA.Short;
                                C : in out CORBA.Short) is
      Operation_Name : CORBA.Identifier :=
        To_CORBA_String ("in_out_inout_proc");
      Arg_Name_A : CORBA.Identifier := To_CORBA_String ("a");
      Arg_Name_B : CORBA.Identifier := To_CORBA_String ("b");
      Arg_Name_C : CORBA.Identifier := To_CORBA_String ("c");
      Request : CORBA.Request.Object;
      Ctx : CORBA.Context.Ref;
      Argument_A : CORBA.Any := CORBA.To_Any (A);
      Argument_B : CORBA.Any := CORBA.To_Any (B);
      Argument_C : CORBA.Any := CORBA.To_Any (C);
      Arg_List : CORBA.NVList.Ref;
      Result : CORBA.NamedValue;
      Result_Name : CORBA.String := To_CORBA_String ("Result");
   begin
      --  creating the argument list
      CORBA.ORB.Create_List (0, Arg_List);
      CORBA.NVList.Add_Item (Arg_List,
                             Arg_Name_A,
                             Argument_A,
                             CORBA.ARG_IN);
      CORBA.NVList.Add_Item (Arg_List,
                             Arg_Name_B,
                             Argument_B,
                             CORBA.ARG_OUT);
      CORBA.NVList.Add_Item (Arg_List,
                             Arg_Name_C,
                             Argument_C,
                             CORBA.ARG_INOUT);
      --  setting the result type
      Result := (Name => Identifier (Result_Name),
                 Argument => Get_Empty_Any (CORBA.TC_Void),
                 Arg_Modes => 0);
      --  creating a request
      CORBA.Object.Create_Request (Myall_Functions,
                                   Ctx,
                                   Operation_Name,
                                   Arg_List,
                                   Result,
                                   Request,
                                   0);
      --  sending message
      CORBA.Request.Invoke (Request, 0);
      --  get out arguments
      B := CORBA.From_Any (Argument_B);
      C := CORBA.From_Any (Argument_C);
   end In_Out_Inout_Proc;

   function Void_Fun (Self : in CORBA.Object.Ref)
                      return CORBA.Short is
      Operation_Name : CORBA.Identifier := To_CORBA_String ("void_fun");
      Request : CORBA.Request.Object;
      Ctx : CORBA.Context.Ref;
      Arg_List : CORBA.NVList.Ref;
      Result : CORBA.NamedValue;
      Result_Name : CORBA.String := To_CORBA_String ("Result");
   begin
      --  creating the argument list
      CORBA.ORB.Create_List (0, Arg_List);
      --  setting the result type
      Result := (Name => Identifier (Result_Name),
                 Argument => To_Any (CORBA.Short (0)),
                 Arg_Modes => 0);
      --  creating a request
      CORBA.Object.Create_Request (Myall_Functions,
                                   Ctx,
                                   Operation_Name,
                                   Arg_List,
                                   Result,
                                   Request,
                                   0);
      --  sending message
      CORBA.Request.Invoke (Request, 0);
      --  getting the answer
      return From_Any (CORBA.Request.Return_Value (Request).Argument);
   end Void_Fun;

   function In_Fun (Self : in CORBA.Object.Ref;
                    A, B, C : in CORBA.Short)
                    return CORBA.Short is
      Operation_Name : CORBA.Identifier := To_CORBA_String ("in_fun");
      Arg_Name_A : CORBA.Identifier := To_CORBA_String ("a");
      Arg_Name_B : CORBA.Identifier := To_CORBA_String ("b");
      Arg_Name_C : CORBA.Identifier := To_CORBA_String ("c");
      Request : CORBA.Request.Object;
      Ctx : CORBA.Context.Ref;
      Argument_A : CORBA.Any := CORBA.To_Any (A);
      Argument_B : CORBA.Any := CORBA.To_Any (B);
      Argument_C : CORBA.Any := CORBA.To_Any (C);
      Arg_List : CORBA.NVList.Ref;
      Result : CORBA.NamedValue;
      Result_Name : CORBA.String := To_CORBA_String ("Result");
   begin
      --  creating the argument list
      CORBA.ORB.Create_List (0, Arg_List);
      CORBA.NVList.Add_Item (Arg_List,
                             Arg_Name_A,
                             Argument_A,
                             CORBA.ARG_IN);
      CORBA.NVList.Add_Item (Arg_List,
                             Arg_Name_B,
                             Argument_B,
                             CORBA.ARG_IN);
      CORBA.NVList.Add_Item (Arg_List,
                             Arg_Name_C,
                             Argument_C,
                             CORBA.ARG_IN);
      --  setting the result type
      Result := (Name => Identifier (Result_Name),
                 Argument => To_Any (CORBA.Short (0)),
                 Arg_Modes => 0);
      --  creating a request
      CORBA.Object.Create_Request (Myall_Functions,
                                   Ctx,
                                   Operation_Name,
                                   Arg_List,
                                   Result,
                                   Request,
                                   0);
      --  sending message
      CORBA.Request.Invoke (Request, 0);
      --  getting the answer
      return From_Any (CORBA.Request.Return_Value (Request).Argument);
   end In_Fun;

   procedure Out_Fun
     (Self : in CORBA.Object.Ref;
      A, B, C, Returns : out CORBA.Short)
   is
      Operation_Name : CORBA.Identifier := To_CORBA_String ("out_fun");
      Arg_Name_A : CORBA.Identifier := To_CORBA_String ("a");
      Arg_Name_B : CORBA.Identifier := To_CORBA_String ("b");
      Arg_Name_C : CORBA.Identifier := To_CORBA_String ("c");
      Request : CORBA.Request.Object;
      Ctx : CORBA.Context.Ref;
      Argument_A : CORBA.Any := CORBA.To_Any (A);
      Argument_B : CORBA.Any := CORBA.To_Any (B);
      Argument_C : CORBA.Any := CORBA.To_Any (C);
      Arg_List : CORBA.NVList.Ref;
      Result : CORBA.NamedValue;
      Result_Name : CORBA.String := To_CORBA_String ("Result");
   begin
      --  creating the argument list
      CORBA.ORB.Create_List (0, Arg_List);
      CORBA.NVList.Add_Item (Arg_List,
                             Arg_Name_A,
                             Argument_A,
                             CORBA.ARG_OUT);
      CORBA.NVList.Add_Item (Arg_List,
                             Arg_Name_B,
                             Argument_B,
                             CORBA.ARG_OUT);
      CORBA.NVList.Add_Item (Arg_List,
                             Arg_Name_C,
                             Argument_C,
                             CORBA.ARG_OUT);
      --  Set the result type
      Result := (Name => Identifier (Result_Name),
                 Argument => To_Any (CORBA.Short (0)),
                 Arg_Modes => 0);
      --  Create a request
      CORBA.Object.Create_Request (Myall_Functions,
                                   Ctx,
                                   Operation_Name,
                                   Arg_List,
                                   Result,
                                   Request,
                                   0);
      --  Send the request
      CORBA.Request.Invoke (Request, 0);
      --  Get out arguments
      A := CORBA.From_Any (Argument_A);
      B := CORBA.From_Any (Argument_B);
      C := CORBA.From_Any (Argument_C);
      Returns := From_Any (CORBA.Request.Return_Value (Request).Argument);
   end Out_Fun;

   procedure Inout_Fun
     (Self : in CORBA.Object.Ref;
      A, B : in out CORBA.Short;
      Returns : out CORBA.Short)
   is
      Operation_Name : CORBA.Identifier := To_CORBA_String ("inout_fun");
      Arg_Name_A : CORBA.Identifier := To_CORBA_String ("a");
      Arg_Name_B : CORBA.Identifier := To_CORBA_String ("b");
      Request : CORBA.Request.Object;
      Ctx : CORBA.Context.Ref;
      Argument_A : CORBA.Any := CORBA.To_Any (A);
      Argument_B : CORBA.Any := CORBA.To_Any (B);
      Arg_List : CORBA.NVList.Ref;
      Result : CORBA.NamedValue;
      Result_Name : CORBA.String := To_CORBA_String ("Result");
   begin
      --  creating the argument list
      CORBA.ORB.Create_List (0, Arg_List);
      CORBA.NVList.Add_Item (Arg_List,
                             Arg_Name_A,
                             Argument_A,
                             CORBA.ARG_INOUT);
      CORBA.NVList.Add_Item (Arg_List,
                             Arg_Name_B,
                             Argument_B,
                             CORBA.ARG_INOUT);
      --  setting the result type
      Result := (Name => Identifier (Result_Name),
                 Argument => To_Any (CORBA.Short (0)),
                 Arg_Modes => 0);
      --  creating a request
      CORBA.Object.Create_Request (Myall_Functions,
                                   Ctx,
                                   Operation_Name,
                                   Arg_List,
                                   Result,
                                   Request,
                                   0);
      --  sending message
      CORBA.Request.Invoke (Request, 0);
      --  get out arguments
      A := CORBA.From_Any (Argument_A);
      B := CORBA.From_Any (Argument_B);
      Returns := From_Any (CORBA.Request.Return_Value (Request).Argument);
   end Inout_Fun;

   procedure In_Out_Fun
     (Self : in CORBA.Object.Ref;
      A, B : in CORBA.Short;
      C, D, Returns : out CORBA.Short)
   is
      Operation_Name : CORBA.Identifier := To_CORBA_String ("in_out_fun");
      Arg_Name_A : CORBA.Identifier := To_CORBA_String ("a");
      Arg_Name_B : CORBA.Identifier := To_CORBA_String ("b");
      Arg_Name_C : CORBA.Identifier := To_CORBA_String ("c");
      Arg_Name_D : CORBA.Identifier := To_CORBA_String ("d");
      Request : CORBA.Request.Object;
      Ctx : CORBA.Context.Ref;
      Argument_A : CORBA.Any := CORBA.To_Any (A);
      Argument_B : CORBA.Any := CORBA.To_Any (B);
      Argument_C : CORBA.Any := CORBA.To_Any (C);
      Argument_D : CORBA.Any := CORBA.To_Any (D);
      Arg_List : CORBA.NVList.Ref;
      Result : CORBA.NamedValue;
      Result_Name : CORBA.String := To_CORBA_String ("Result");
   begin
      --  creating the argument list
      CORBA.ORB.Create_List (0, Arg_List);
      CORBA.NVList.Add_Item (Arg_List,
                             Arg_Name_A,
                             Argument_A,
                             CORBA.ARG_IN);
      CORBA.NVList.Add_Item (Arg_List,
                             Arg_Name_B,
                             Argument_B,
                             CORBA.ARG_IN);
      CORBA.NVList.Add_Item (Arg_List,
                             Arg_Name_C,
                             Argument_C,
                             CORBA.ARG_OUT);
      CORBA.NVList.Add_Item (Arg_List,
                             Arg_Name_D,
                             Argument_D,
                             CORBA.ARG_OUT);
      --  setting the result type
      Result := (Name => Identifier (Result_Name),
                 Argument => To_Any (CORBA.Short (0)),
                 Arg_Modes => 0);
      --  creating a request
      CORBA.Object.Create_Request (Myall_Functions,
                                   Ctx,
                                   Operation_Name,
                                   Arg_List,
                                   Result,
                                   Request,
                                   0);
      --  sending message
      CORBA.Request.Invoke (Request, 0);
      --  get out arguments
      C := CORBA.From_Any (Argument_C);
      D := CORBA.From_Any (Argument_D);
      Returns := From_Any (CORBA.Request.Return_Value (Request).Argument);
   end In_Out_Fun;

   procedure In_Inout_Fun
     (Self : in CORBA.Object.Ref;
      A : in CORBA.Short;
      B : in out CORBA.Short;
      C : in CORBA.Short;
      D : in out CORBA.Short;
      Returns : out CORBA.Short)
   is
      Operation_Name : CORBA.Identifier := To_CORBA_String ("in_inout_fun");
      Arg_Name_A : CORBA.Identifier := To_CORBA_String ("a");
      Arg_Name_B : CORBA.Identifier := To_CORBA_String ("b");
      Arg_Name_C : CORBA.Identifier := To_CORBA_String ("c");
      Arg_Name_D : CORBA.Identifier := To_CORBA_String ("d");
      Request : CORBA.Request.Object;
      Ctx : CORBA.Context.Ref;
      Argument_A : CORBA.Any := CORBA.To_Any (A);
      Argument_B : CORBA.Any := CORBA.To_Any (B);
      Argument_C : CORBA.Any := CORBA.To_Any (C);
      Argument_D : CORBA.Any := CORBA.To_Any (D);
      Arg_List : CORBA.NVList.Ref;
      Result : CORBA.NamedValue;
      Result_Name : CORBA.String := To_CORBA_String ("Result");
   begin
      --  creating the argument list
      CORBA.ORB.Create_List (0, Arg_List);
      CORBA.NVList.Add_Item (Arg_List,
                             Arg_Name_A,
                             Argument_A,
                             CORBA.ARG_IN);
      CORBA.NVList.Add_Item (Arg_List,
                             Arg_Name_B,
                             Argument_B,
                             CORBA.ARG_INOUT);
      CORBA.NVList.Add_Item (Arg_List,
                             Arg_Name_C,
                             Argument_C,
                             CORBA.ARG_IN);
      CORBA.NVList.Add_Item (Arg_List,
                             Arg_Name_D,
                             Argument_D,
                             CORBA.ARG_INOUT);
      --  setting the result type
      Result := (Name => Identifier (Result_Name),
                 Argument => To_Any (CORBA.Short (0)),
                 Arg_Modes => 0);
      --  creating a request
      CORBA.Object.Create_Request (Myall_Functions,
                                   Ctx,
                                   Operation_Name,
                                   Arg_List,
                                   Result,
                                   Request,
                                   0);
      --  sending message
      CORBA.Request.Invoke (Request, 0);
      --  get out arguments
      B := CORBA.From_Any (Argument_B);
      D := CORBA.From_Any (Argument_D);
      Returns := From_Any (CORBA.Request.Return_Value (Request).Argument);
   end In_Inout_Fun;

   procedure Out_Inout_Fun
     (Self : in CORBA.Object.Ref;
      A : out CORBA.Short;
      B : in out CORBA.Short;
      C : in out CORBA.Short;
      D : out CORBA.Short;
      Returns : out CORBA.Short)
   is
      Operation_Name : CORBA.Identifier := To_CORBA_String ("out_inout_fun");
      Arg_Name_A : CORBA.Identifier := To_CORBA_String ("a");
      Arg_Name_B : CORBA.Identifier := To_CORBA_String ("b");
      Arg_Name_C : CORBA.Identifier := To_CORBA_String ("c");
      Arg_Name_D : CORBA.Identifier := To_CORBA_String ("d");
      Request : CORBA.Request.Object;
      Ctx : CORBA.Context.Ref;
      Argument_A : CORBA.Any := CORBA.To_Any (A);
      Argument_B : CORBA.Any := CORBA.To_Any (B);
      Argument_C : CORBA.Any := CORBA.To_Any (C);
      Argument_D : CORBA.Any := CORBA.To_Any (D);
      Arg_List : CORBA.NVList.Ref;
      Result : CORBA.NamedValue;
      Result_Name : CORBA.String := To_CORBA_String ("Result");
   begin
      --  creating the argument list
      CORBA.ORB.Create_List (0, Arg_List);
      CORBA.NVList.Add_Item (Arg_List,
                             Arg_Name_A,
                             Argument_A,
                             CORBA.ARG_OUT);
      CORBA.NVList.Add_Item (Arg_List,
                             Arg_Name_B,
                             Argument_B,
                             CORBA.ARG_INOUT);
      CORBA.NVList.Add_Item (Arg_List,
                             Arg_Name_C,
                             Argument_C,
                             CORBA.ARG_INOUT);
      CORBA.NVList.Add_Item (Arg_List,
                             Arg_Name_D,
                             Argument_D,
                             CORBA.ARG_OUT);
      --  setting the result type
      Result := (Name => Identifier (Result_Name),
                 Argument => To_Any (CORBA.Short (0)),
                 Arg_Modes => 0);
      --  creating a request
      CORBA.Object.Create_Request (Myall_Functions,
                                   Ctx,
                                   Operation_Name,
                                   Arg_List,
                                   Result,
                                   Request,
                                   0);
      --  sending message
      CORBA.Request.Invoke (Request, 0);
      --  get out arguments
      A := CORBA.From_Any (Argument_A);
      B := CORBA.From_Any (Argument_B);
      C := CORBA.From_Any (Argument_C);
      D := CORBA.From_Any (Argument_D);
      Returns := From_Any (CORBA.Request.Return_Value (Request).Argument);
   end Out_Inout_Fun;

   procedure In_Out_Inout_Fun (Self : in CORBA.Object.Ref;
                               A : in CORBA.Short;
                               B : out CORBA.Short;
                               C : in out CORBA.Short;
                               Returns : out CORBA.Short) is
      Operation_Name : CORBA.Identifier :=
        To_CORBA_String ("in_out_inout_fun");
      Arg_Name_A : CORBA.Identifier := To_CORBA_String ("a");
      Arg_Name_B : CORBA.Identifier := To_CORBA_String ("b");
      Arg_Name_C : CORBA.Identifier := To_CORBA_String ("c");
      Request : CORBA.Request.Object;
      Ctx : CORBA.Context.Ref;
      Argument_A : CORBA.Any := CORBA.To_Any (A);
      Argument_B : CORBA.Any := CORBA.To_Any (B);
      Argument_C : CORBA.Any := CORBA.To_Any (C);
      Arg_List : CORBA.NVList.Ref;
      Result : CORBA.NamedValue;
      Result_Name : CORBA.String := To_CORBA_String ("Result");
   begin
      --  creating the argument list
      CORBA.ORB.Create_List (0, Arg_List);
      CORBA.NVList.Add_Item (Arg_List,
                             Arg_Name_A,
                             Argument_A,
                             CORBA.ARG_IN);
      CORBA.NVList.Add_Item (Arg_List,
                             Arg_Name_B,
                             Argument_B,
                             CORBA.ARG_OUT);
      CORBA.NVList.Add_Item (Arg_List,
                             Arg_Name_C,
                             Argument_C,
                             CORBA.ARG_INOUT);
      --  setting the result type
      Result := (Name => Identifier (Result_Name),
                 Argument => To_Any (CORBA.Short (0)),
                 Arg_Modes => 0);
      --  creating a request
      CORBA.Object.Create_Request (Myall_Functions,
                                   Ctx,
                                   Operation_Name,
                                   Arg_List,
                                   Result,
                                   Request,
                                   0);
      --  sending message
      CORBA.Request.Invoke (Request, 0);
      --  get out arguments
      B := CORBA.From_Any (Argument_B);
      C := CORBA.From_Any (Argument_C);
      Returns := From_Any (CORBA.Request.Return_Value (Request).Argument);
   end In_Out_Inout_Fun;

   procedure Oneway_Void_Proc (Self : in CORBA.Object.Ref) is
      Operation_Name : CORBA.Identifier := To_CORBA_String ("oneway_void_proc");
      Request : CORBA.Request.Object;
      Ctx : CORBA.Context.Ref;
      Arg_List : CORBA.NVList.Ref;
      Result : CORBA.NamedValue;
      Result_Name : CORBA.String := To_CORBA_String ("Result");
   begin
      --  creating the argument list
      CORBA.ORB.Create_List (0, Arg_List);
      --  setting the result type
      Result := (Name => Identifier (Result_Name),
                 Argument => Get_Empty_Any (CORBA.TC_Void),
                 Arg_Modes => 0);
      --  creating a request
      CORBA.Object.Create_Request (Myall_Functions,
                                   Ctx,
                                   Operation_Name,
                                   Arg_List,
                                   Result,
                                   Request,
                                   0);
      --  sending message
      CORBA.Request.Invoke (Request, 0);
   end Oneway_Void_Proc;

   procedure Oneway_In_Proc (Self : in CORBA.Object.Ref;
                             A, B : in CORBA.Short) is
      Operation_Name : CORBA.Identifier := To_CORBA_String ("oneway_in_proc");
      Arg_Name_A : CORBA.Identifier := To_CORBA_String ("a");
      Arg_Name_B : CORBA.Identifier := To_CORBA_String ("b");
      Request : CORBA.Request.Object;
      Ctx : CORBA.Context.Ref;
      Argument_A : CORBA.Any := CORBA.To_Any (A);
      Argument_B : CORBA.Any := CORBA.To_Any (B);
      Arg_List : CORBA.NVList.Ref;
      Result : CORBA.NamedValue;
      Result_Name : CORBA.String := To_CORBA_String ("Result");
   begin
      --  creating the argument list
      CORBA.ORB.Create_List (0, Arg_List);
      CORBA.NVList.Add_Item (Arg_List,
                             Arg_Name_A,
                             Argument_A,
                             CORBA.ARG_IN);
      CORBA.NVList.Add_Item (Arg_List,
                             Arg_Name_B,
                             Argument_B,
                             CORBA.ARG_IN);
      --  setting the result type
      Result := (Name => Identifier (Result_Name),
                 Argument => Get_Empty_Any (CORBA.TC_Void),
                 Arg_Modes => 0);
      --  creating a request
      CORBA.Object.Create_Request (Myall_Functions,
                                   Ctx,
                                   Operation_Name,
                                   Arg_List,
                                   Result,
                                   Request,
                                   0);
      --  sending message
      CORBA.Request.Invoke (Request, 0);
   end Oneway_In_Proc;

   function Oneway_Checker (Self : in CORBA.Object.Ref)
                            return CORBA.Short is
      Operation_Name : CORBA.Identifier := To_CORBA_String ("oneway_checker");
      Request : CORBA.Request.Object;
      Ctx : CORBA.Context.Ref;
      Arg_List : CORBA.NVList.Ref;
      Result : CORBA.NamedValue;
      Result_Name : CORBA.String := To_CORBA_String ("Result");
   begin
      --  creating the argument list
      CORBA.ORB.Create_List (0, Arg_List);
      --  setting the result type
      Result := (Name => Identifier (Result_Name),
                 Argument => To_Any (CORBA.Short (0)),
                 Arg_Modes => 0);
      --  creating a request
      CORBA.Object.Create_Request (Myall_Functions,
                                   Ctx,
                                   Operation_Name,
                                   Arg_List,
                                   Result,
                                   Request,
                                   0);
      --  sending message
      CORBA.Request.Invoke (Request, 0);
      --  getting the answer
      return From_Any (CORBA.Request.Return_Value (Request).Argument);
   end Oneway_Checker;

begin

   if Ada.Command_Line.Argument_Count < 1 then
      Ada.Text_IO.Put_Line ("usage : client <IOR_string_from_server>");
      return;
   end if;

   --  transforms the Ada string into CORBA.String
   IOR := CORBA.To_CORBA_String (Ada.Command_Line.Argument (1)) ;

   --  getting the CORBA.Object
   CORBA.ORB.String_To_Object (IOR, Myall_Functions);

   Set_The_Attribute (Myall_Functions, 24);
   Output ("test attribute", Get_The_Attribute (Myall_Functions) = 24);

   Output ("test readonly attribute", Get_The_Readonly_Attribute (Myall_Functions) = 18);

   begin
      Ok := True;
      Void_Proc (Myall_Functions);
   exception when others =>
      Ok := False;
   end;
   Output ("test void procedure", Ok);

   begin
      In_Proc (Myall_Functions, 1, 2, 3);
      Ok := True;
   exception when others =>
      Ok := False;
   end;
   Output ("test in param procedure", Ok);

   begin
      Ok := False;
      Out_Proc (Myall_Functions, I, J, K);
      Ok := (I = 10) and then (J = 11) and then (K = 12);
   exception when others =>
      Ok := False;
   end;
   Output ("test out param procedure", Ok);

   begin
      Ok := False;
      I  := 2;
      J  := 3;
      Inout_Proc (Myall_Functions, I, J);
      Ok := (I = 3 and then J = 4);
   exception when others =>
      Ok := False;
   end;
   Output ("test in out param procedure", Ok);

   begin
      Ok := False;
      I := 1;
      J := 2;
      In_Out_Proc (Myall_Functions, 1, 2, I, J);
      Ok := (I = 3 and then J = 4);
   exception when others =>
      Ok := False;
   end;
   Output ("test in and out param procedure", Ok);

   begin
      Ok := False;
      I  := -4;
      J  := -5;
      In_Inout_Proc (Myall_Functions, 1, I, 3, J);
      Ok := (I = 36) and then (J = 40);
   exception when others =>
      Ok := False;
   end;
   Output ("test in and inout param procedure", Ok);

   begin
      I := -11;
      J := -21;
      K := -31;
      L := -41;
      Out_Inout_Proc (Myall_Functions, I, J, K, L);
      Ok := (I = 45) and then (J = 46) and then (K = 47) and then (L = 48);
   exception when others =>
      Ok := False;
   end;
   Output ("test inout and out param procedure", Ok);

   begin
      Ok := False;
      I := 78;
      J := 79;
      In_Out_Inout_Proc (Myall_Functions, 1, I, J);
      Ok := (I = -54) and then (J = 80);
   exception when others =>
      Ok := False;
   end;
   Output ("test in and out and inout param procedure", Ok);

   Output ("test void function", Void_Fun (Myall_Functions) = 3);
   Output ("test in param function", In_Fun (Myall_Functions, 1, 2, 3) = 7);

   begin
      Ok := False;
      I := 1;
      J := 2;
      K := 3;
      L := 4;
      Out_Fun (Myall_Functions, I, J, K, L);
      Ok := (I = 5) and then (J = 6) and then (K = 7) and then (L = 10);
   exception when others =>
      Ok := False;
   end;
   Output ("test out param function", Ok);

   begin
      Ok := False;
      I := 1;
      J := 2;
      K := 3;
      Inout_Fun (Myall_Functions, I, J, L);
      Ok := (I = 2) and then (J = 3) and then (L = 5);
   exception when others =>
      Ok := False;
   end;
   Output ("test inout param function", Ok);

   begin
      Ok := False;
      I := 10;
      J := 11;
      In_Out_Fun (Myall_Functions, 1, 2, I, J, K);
      Ok := (I = 2) and then (J = 1) and then (K = 3);
   exception when others =>
      Ok := False;
   end;
   Output ("test in and out param function", Ok);

   begin
      Ok := False;
      I := -1;
      J := -2;
      K := -3;
      In_Inout_Fun (Myall_Functions, -1, I, -2, J, K);
      Ok := (I = -2) and then (J = -4) and then (K = -6);
   exception when others =>
      Ok := False;
   end;
   Output ("test in and inout param function", Ok);

   begin
      Ok := False;
      I := -1;
      J := -2;
      K := -3;
      L := -4;
      M := -5;
      Out_Inout_Fun (Myall_Functions, I, J, K, L, M);
      Ok := (I = -2) and then (J = -1) and then (K = -2)
        and then (L = -3) and then (M = -7);
   exception when others =>
      Ok := False;
   end;
   Output ("test out and inout param function", Ok);

   begin
      Ok := False;
      I := -1;
      J := -2;
      K := -3;
      In_Out_Inout_Fun (Myall_Functions, 85, I, J, K);
      Ok := (I = 86) and then (J = 83) and then (K = -1);
   exception when others =>
      Ok := False;
   end;
   Output ("test in and out and inout param function", Ok);

   begin
      Oneway_Void_Proc (Myall_Functions);
      delay 1.0;
      Ok := Oneway_Checker (Myall_Functions) = 1;
      if Ok then
         delay 5.0;
         Ok := Oneway_Checker (Myall_Functions) = 2;
      end if;
--   exception when others =>
--      Ok := False;
   end;
    Output ("test void one way procedure", Ok);

    begin
       Oneway_In_Proc (Myall_Functions, 10, 20);
       delay 1.0;
       Ok := Oneway_Checker (Myall_Functions) = 10;
       if Ok then
          delay 5.0;
          Ok := Oneway_Checker (Myall_Functions) = 20;
       end if;
    exception when others =>
       Ok := False;
    end;
    Output ("test in param one way procedure", Ok);

end Dynclient;





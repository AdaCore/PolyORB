------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                            D Y N C L I E N T                             --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2002-2007, Free Software Foundation, Inc.          --
--                                                                          --
-- PolyORB is free software; you  can  redistribute  it and/or modify it    --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. PolyORB is distributed  in the hope that it will be  useful,    --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with PolyORB; see file COPYING. If    --
-- not, write to the Free Software Foundation, 51 Franklin Street, Fifth    --
-- Floor, Boston, MA 02111-1301, USA.                                       --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
--                  PolyORB is maintained by AdaCore                        --
--                     (email: sales@adacore.com)                           --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Command_Line;
with Ada.Text_IO;
--  with Ada.Exceptions;

with CORBA; use CORBA;
with CORBA.Object;
with CORBA.Context;
with CORBA.Request;
with CORBA.NVList;
with CORBA.ORB;

with PolyORB.Utils.Report;

with PolyORB.Setup.Client;
pragma Warnings (Off, PolyORB.Setup.Client);

procedure Dynclient is

   use PolyORB.Utils.Report;

   IOR : CORBA.String;
   Myall_Functions : CORBA.Object.Ref;
   I, J, K, L, M : CORBA.Short;
   Ok : Boolean;

   function Get_The_Attribute return CORBA.Short;

   function Get_The_Attribute
     return CORBA.Short
   is
      Operation_Name : constant CORBA.Identifier :=
                         To_CORBA_String ("_get_the_attribute");
      Request : CORBA.Request.Object;
      Ctx : CORBA.Context.Ref;
      Arg_List : CORBA.NVList.Ref;
      Result_Name : constant CORBA.String := To_CORBA_String ("Result");
      Result : CORBA.NamedValue;
   begin
      --  creating the argument list
      CORBA.ORB.Create_List (0, Arg_List);
      --  setting the result type
      Result := (Name => Identifier (Result_Name),
                 Argument => Internals.Get_Empty_Any (CORBA.TC_Short),
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
      return From_Any (Result.Argument);
   end Get_The_Attribute;

   procedure Set_The_Attribute (To   : in CORBA.Short);

   procedure Set_The_Attribute (To   : in CORBA.Short)
   is
      Operation_Name : constant CORBA.Identifier :=
                         To_CORBA_String ("_set_the_attribute");
      Arg_Name_To : constant CORBA.Identifier := To_CORBA_String ("to");
      Request : CORBA.Request.Object;
      Ctx : CORBA.Context.Ref;
      Argument : CORBA.Any;
      Arg_List : CORBA.NVList.Ref;
      Result : CORBA.NamedValue;
      Result_Name : constant CORBA.String := To_CORBA_String ("Result");
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
                 Argument => Internals.Get_Empty_Any (CORBA.TC_Void),
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

   function Get_The_Readonly_Attribute return CORBA.Short;

   function Get_The_Readonly_Attribute
     return CORBA.Short
   is
      Operation_Name : constant CORBA.Identifier :=
                         To_CORBA_String ("_get_the_readonly_attribute");
      Request : CORBA.Request.Object;
      Ctx : CORBA.Context.Ref;
      Arg_List : CORBA.NVList.Ref;
      Result : CORBA.NamedValue;
      Result_Name : constant CORBA.String := To_CORBA_String ("Result");
   begin
      --  creating the argument list
      CORBA.ORB.Create_List (0, Arg_List);
      --  setting the result type
      Result := (Name => Identifier (Result_Name),
                 Argument => Internals.Get_Empty_Any (CORBA.TC_Short),
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
      return From_Any (Result.Argument);
   end Get_The_Readonly_Attribute;

   procedure Void_Proc;

   procedure Void_Proc
   is
      Operation_Name : constant CORBA.Identifier :=
                         To_CORBA_String ("void_proc");
      Request : CORBA.Request.Object;
      Ctx : CORBA.Context.Ref;
      Arg_List : CORBA.NVList.Ref;
      Result : CORBA.NamedValue;
      Result_Name : constant CORBA.String := To_CORBA_String ("Result");
   begin
      --  creating the argument list
      CORBA.ORB.Create_List (0, Arg_List);
      --  setting the result type
      Result := (Name => Identifier (Result_Name),
                 Argument => Internals.Get_Empty_Any (CORBA.TC_Void),
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

   procedure In_Proc (A, B, C : in CORBA.Short);

   procedure In_Proc (A, B, C : in CORBA.Short)
   is
      Operation_Name : constant CORBA.Identifier :=
                         To_CORBA_String ("in_proc");
      Arg_Name_A : constant CORBA.Identifier := To_CORBA_String ("a");
      Arg_Name_B : constant CORBA.Identifier := To_CORBA_String ("b");
      Arg_Name_C : constant CORBA.Identifier := To_CORBA_String ("c");
      Request : CORBA.Request.Object;
      Ctx : CORBA.Context.Ref;
      Argument_A : constant CORBA.Any := CORBA.To_Any (A);
      Argument_B : constant CORBA.Any := CORBA.To_Any (B);
      Argument_C : constant CORBA.Any := CORBA.To_Any (C);
      Arg_List : CORBA.NVList.Ref;
      Result : CORBA.NamedValue;
      Result_Name : constant CORBA.String := To_CORBA_String ("Result");
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
                 Argument => Internals.Get_Empty_Any (CORBA.TC_Void),
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

--     procedure Out_Proc (Self : in CORBA.Object.Ref;
--                         A, B, C : out CORBA.Short);

--     procedure Out_Proc (Self : in CORBA.Object.Ref;
--                         A, B, C : out CORBA.Short) is
--        Operation_Name : CORBA.Identifier := To_CORBA_String ("out_proc");
--        Request : CORBA.Request.Object;
--        Ctx : CORBA.Context.Ref;
--        Arg_List : CORBA.NVList.Ref;
--        Result : CORBA.NamedValue;
--        Result_Name : CORBA.String := To_CORBA_String ("Result");
--     begin
--        --  creating an empty argument list
--        CORBA.ORB.Create_List (0, Arg_List);
--        --  setting the result type
--        Result := (Name => Identifier (Result_Name),
--                   Argument => Get_Empty_Any (CORBA.TC_Void),
--                   Arg_Modes => 0);
--        --  creating a request
--        CORBA.Object.Create_Request (Myall_Functions,
--                                     Ctx,
--                                     Operation_Name,
--                                     Arg_List,
--                                     Result,
--                                     Request,
--                                     0);
--        --  adding some arguments to the request
--        CORBA.Request.Add_Arg (Request,
--                               CORBA.TC_Short,
--                               A'Address,
--                               CORBA.Short'Size,
--                               CORBA.ARG_OUT);
--        CORBA.Request.Add_Arg (Request,
--                               CORBA.TC_Short,
--                               B'Address,
--                               CORBA.Short'Size,
--                               CORBA.ARG_OUT);
--        CORBA.Request.Add_Arg (Request,
--                               CORBA.TC_Short,
--                               C'Address,
--                               CORBA.Short'Size,
--                               CORBA.ARG_OUT);
--        --  sending message
--        CORBA.Request.Invoke (Request, 0);
--     end Out_Proc;

--     procedure Inout_Proc (Self : in CORBA.Object.Ref;
--                           A, B : in out CORBA.Short);

--     procedure Inout_Proc (Self : in CORBA.Object.Ref;
--                           A, B : in out CORBA.Short) is
--        Operation_Name : CORBA.Identifier := To_CORBA_String ("inout_proc");
--        Arg_Name_A : CORBA.Identifier := To_CORBA_String ("a");
--        Arg_Name_B : CORBA.Identifier := To_CORBA_String ("b");
--        Request : CORBA.Request.Object;
--        Ctx : CORBA.Context.Ref;
--        Arg_List : CORBA.NVList.Ref;
--        Result : CORBA.NamedValue;
--        Result_Name : CORBA.String := To_CORBA_String ("Result");
--     begin
--        --  creating the argument list
--        CORBA.ORB.Create_List (0, Arg_List);
--        CORBA.NVList.Add_Item (Arg_List,
--                               Arg_Name_A,
--                               CORBA.TC_Short,
--                               A'Address,
--                               CORBA.Short'Size,
--                               CORBA.ARG_INOUT);
--        CORBA.NVList.Add_Item (Arg_List,
--                               Arg_Name_B,
--                               CORBA.TC_Short,
--                               B'Address,
--                               CORBA.Short'Size,
--                               CORBA.ARG_INOUT);
--        --  setting the result type
--        Result := (Name => Identifier (Result_Name),
--                   Argument => Get_Empty_Any (CORBA.TC_Void),
--                   Arg_Modes => 0);
--        --  creating a request
--        CORBA.Object.Create_Request (Myall_Functions,
--                                     Ctx,
--                                     Operation_Name,
--                                     Arg_List,
--                                     Result,
--                                     Request,
--                                     0);
--        --  sending message
--        CORBA.Request.Invoke (Request, 0);
--     end Inout_Proc;

   procedure In_Out_Proc (A, B : in CORBA.Short;
                          C, D : out CORBA.Short);

   procedure In_Out_Proc (A, B : in CORBA.Short;
                          C, D : out CORBA.Short)
   is
      Operation_Name : constant CORBA.Identifier :=
                         To_CORBA_String ("in_out_proc");
      Arg_Name_A : constant CORBA.Identifier := To_CORBA_String ("a");
      Arg_Name_B : constant CORBA.Identifier := To_CORBA_String ("b");
      Arg_Name_C : constant CORBA.Identifier := To_CORBA_String ("c");
      Arg_Name_D : constant CORBA.Identifier := To_CORBA_String ("d");
      Request : CORBA.Request.Object;
      Ctx : CORBA.Context.Ref;
      Argument_A : constant CORBA.Any := CORBA.To_Any (A);
      Argument_B : constant CORBA.Any := CORBA.To_Any (B);

      pragma Warnings (Off);
      --  C and D are referenced before they have a value.
      Argument_C : constant CORBA.Any := CORBA.To_Any (C);
      Argument_D : constant CORBA.Any := CORBA.To_Any (D);
      pragma Warnings (On);

      Arg_List : CORBA.NVList.Ref;
      Result : CORBA.NamedValue;
      Result_Name : constant CORBA.String := To_CORBA_String ("Result");
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
                 Argument => Internals.Get_Empty_Any (CORBA.TC_Void),
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

   procedure In_Inout_Proc (A : in CORBA.Short;
                            B : in out CORBA.Short;
                            C : in CORBA.Short;
                            D : in out CORBA.Short);

   procedure In_Inout_Proc (A : in CORBA.Short;
                            B : in out CORBA.Short;
                            C : in CORBA.Short;
                            D : in out CORBA.Short)
   is
      Operation_Name : constant CORBA.Identifier :=
                         To_CORBA_String ("in_inout_proc");
      Arg_Name_A : constant CORBA.Identifier := To_CORBA_String ("a");
      Arg_Name_B : constant CORBA.Identifier := To_CORBA_String ("b");
      Arg_Name_C : constant CORBA.Identifier := To_CORBA_String ("c");
      Arg_Name_D : constant CORBA.Identifier := To_CORBA_String ("d");
      Request : CORBA.Request.Object;
      Ctx : CORBA.Context.Ref;
      Argument_A : constant CORBA.Any := CORBA.To_Any (A);
      Argument_B : constant CORBA.Any := CORBA.To_Any (B);
      Argument_C : constant CORBA.Any := CORBA.To_Any (C);
      Argument_D : constant CORBA.Any := CORBA.To_Any (D);
      Arg_List : CORBA.NVList.Ref;
      Result : CORBA.NamedValue;
      Result_Name : constant CORBA.String := To_CORBA_String ("Result");
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
                 Argument => Internals.Get_Empty_Any (CORBA.TC_Void),
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

--     procedure Out_Inout_Proc (Self : in CORBA.Object.Ref;
--                               A : out CORBA.Short;
--                               B : in out CORBA.Short;
--                               C : in out CORBA.Short;
--                               D : out CORBA.Short);

--     procedure Out_Inout_Proc (Self : in CORBA.Object.Ref;
--                               A : out CORBA.Short;
--                               B : in out CORBA.Short;
--                               C : in out CORBA.Short;
--                               D : out CORBA.Short)
--     is
--     Operation_Name : CORBA.Identifier := To_CORBA_String ("out_inout_proc");
--        Arg_Name_A : CORBA.Identifier := To_CORBA_String ("a");
--        Arg_Name_B : CORBA.Identifier := To_CORBA_String ("b");
--        Arg_Name_C : CORBA.Identifier := To_CORBA_String ("c");
--        Arg_Name_D : CORBA.Identifier := To_CORBA_String ("d");
--        Request : CORBA.Request.Object;
--        Ctx : CORBA.Context.Ref;
--        Arg_List : CORBA.NVList.Ref;
--        Result : CORBA.NamedValue;
--        Result_Name : CORBA.String := To_CORBA_String ("Result");
--     begin
--        --  creating the argument list
--        CORBA.ORB.Create_List (0, Arg_List);
--        CORBA.NVList.Add_Item (Arg_List,
--                               Arg_Name_A,
--                               CORBA.TC_Short,
--                               A'Address,
--                               CORBA.Short'Size,
--                               CORBA.ARG_OUT);
--        CORBA.NVList.Add_Item (Arg_List,
--                               Arg_Name_B,
--                               CORBA.TC_Short,
--                               B'Address,
--                               CORBA.Short'Size,
--                               CORBA.ARG_INOUT);
--        CORBA.NVList.Add_Item (Arg_List,
--                               Arg_Name_C,
--                               CORBA.TC_Short,
--                               C'Address,
--                               CORBA.Short'Size,
--                               CORBA.ARG_INOUT);
--        CORBA.NVList.Add_Item (Arg_List,
--                               Arg_Name_D,
--                               CORBA.TC_Short,
--                               D'Address,
--                               CORBA.Short'Size,
--                               CORBA.ARG_OUT);
--        --  setting the result type
--        Result := (Name => Identifier (Result_Name),
--                   Argument => Get_Empty_Any (CORBA.TC_Void),
--                   Arg_Modes => 0);
--        --  creating a request
--        CORBA.Object.Create_Request (Myall_Functions,
--                                     Ctx,
--                                     Operation_Name,
--                                     Arg_List,
--                                     Result,
--                                     Request,
--                                     0);
--        --  sending message
--        CORBA.Request.Invoke (Request, 0);
--     end Out_Inout_Proc;

   procedure In_Out_Inout_Proc (A : in CORBA.Short;
                                B : out CORBA.Short;
                                C : in out CORBA.Short);

   procedure In_Out_Inout_Proc (A : in CORBA.Short;
                                B : out CORBA.Short;
                                C : in out CORBA.Short)
   is
      Operation_Name : constant CORBA.Identifier :=
                         To_CORBA_String ("in_out_inout_proc");
      Arg_Name_A : constant CORBA.Identifier := To_CORBA_String ("a");
      Arg_Name_B : constant CORBA.Identifier := To_CORBA_String ("b");
      Arg_Name_C : constant CORBA.Identifier := To_CORBA_String ("c");
      Request : CORBA.Request.Object;
      Ctx : CORBA.Context.Ref;
      Argument_A : constant CORBA.Any := CORBA.To_Any (A);
      pragma Warnings (Off);
      --  B is referenced before it has a value.
      Argument_B : constant CORBA.Any := CORBA.To_Any (B);
      pragma Warnings (On);
      Argument_C : constant CORBA.Any := CORBA.To_Any (C);
      Arg_List : CORBA.NVList.Ref;
      Result : CORBA.NamedValue;
      Result_Name : constant CORBA.String := To_CORBA_String ("Result");
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
                 Argument => Internals.Get_Empty_Any (CORBA.TC_Void),
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

   function Void_Fun return CORBA.Short;

   function Void_Fun return CORBA.Short
   is
      Operation_Name : constant CORBA.Identifier :=
                         To_CORBA_String ("void_fun");
      Request : CORBA.Request.Object;
      Ctx : CORBA.Context.Ref;
      Arg_List : CORBA.NVList.Ref;
      Result : CORBA.NamedValue;
      Result_Name : constant CORBA.String := To_CORBA_String ("Result");
   begin
      --  creating the argument list
      CORBA.ORB.Create_List (0, Arg_List);
      --  setting the result type
      Result := (Name => Identifier (Result_Name),
                 Argument => Internals.Get_Empty_Any (CORBA.TC_Short),
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
      return From_Any (Result.Argument);
   end Void_Fun;

   function In_Fun (A, B, C : in CORBA.Short)
                   return CORBA.Short;

   function In_Fun (A, B, C : in CORBA.Short)
                   return CORBA.Short
   is
      Operation_Name : constant CORBA.Identifier := To_CORBA_String ("in_fun");
      Arg_Name_A : constant CORBA.Identifier := To_CORBA_String ("a");
      Arg_Name_B : constant CORBA.Identifier := To_CORBA_String ("b");
      Arg_Name_C : constant CORBA.Identifier := To_CORBA_String ("c");
      Request : CORBA.Request.Object;
      Ctx : CORBA.Context.Ref;
      Argument_A : constant CORBA.Any := CORBA.To_Any (A);
      Argument_B : constant CORBA.Any := CORBA.To_Any (B);
      Argument_C : constant CORBA.Any := CORBA.To_Any (C);
      Arg_List : CORBA.NVList.Ref;
      Result : CORBA.NamedValue;
      Result_Name : constant CORBA.String := To_CORBA_String ("Result");
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
                 Argument => Internals.Get_Empty_Any (CORBA.TC_Short),
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
      return From_Any (Result.Argument);
   end In_Fun;

   procedure Out_Fun (A, B, C, Returns : out CORBA.Short);

   procedure Out_Fun
     (A, B, C, Returns : out CORBA.Short)
   is
      Operation_Name : constant CORBA.Identifier :=
                         To_CORBA_String ("out_fun");
      Arg_Name_A : constant CORBA.Identifier := To_CORBA_String ("a");
      Arg_Name_B : constant CORBA.Identifier := To_CORBA_String ("b");
      Arg_Name_C : constant CORBA.Identifier := To_CORBA_String ("c");
      Request : CORBA.Request.Object;
      Ctx : CORBA.Context.Ref;
      pragma Warnings (Off);
      --  A, B, and C are referenced before they have a value.
      Argument_A : constant CORBA.Any := CORBA.To_Any (A);
      Argument_B : constant CORBA.Any := CORBA.To_Any (B);
      Argument_C : constant CORBA.Any := CORBA.To_Any (C);
      pragma Warnings (On);
      Arg_List : CORBA.NVList.Ref;
      Result : CORBA.NamedValue;
      Result_Name : constant CORBA.String := To_CORBA_String ("Result");
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
                 Argument => Internals.Get_Empty_Any (CORBA.TC_Short),
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
      Returns := From_Any (Result.Argument);
   end Out_Fun;

   procedure Inout_Fun
     (A, B : in out CORBA.Short;
      Returns : out CORBA.Short);

   procedure Inout_Fun
     (A, B : in out CORBA.Short;
      Returns : out CORBA.Short)
   is
      Operation_Name : constant CORBA.Identifier :=
                         To_CORBA_String ("inout_fun");
      Arg_Name_A : constant CORBA.Identifier := To_CORBA_String ("a");
      Arg_Name_B : constant CORBA.Identifier := To_CORBA_String ("b");
      Request : CORBA.Request.Object;
      Ctx : CORBA.Context.Ref;
      Argument_A : constant CORBA.Any := CORBA.To_Any (A);
      Argument_B : constant CORBA.Any := CORBA.To_Any (B);
      Arg_List : CORBA.NVList.Ref;
      Result : CORBA.NamedValue;
      Result_Name : constant CORBA.String := To_CORBA_String ("Result");
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
                 Argument => Internals.Get_Empty_Any (CORBA.TC_Short),
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
      Returns := From_Any (Result.Argument);
   end Inout_Fun;

   procedure In_Out_Fun
     (A, B : in CORBA.Short;
      C, D, Returns : out CORBA.Short);

   procedure In_Out_Fun
     (A, B : in CORBA.Short;
      C, D, Returns : out CORBA.Short)
   is
      Operation_Name : constant CORBA.Identifier :=
                         To_CORBA_String ("in_out_fun");
      Arg_Name_A : constant CORBA.Identifier := To_CORBA_String ("a");
      Arg_Name_B : constant CORBA.Identifier := To_CORBA_String ("b");
      Arg_Name_C : constant CORBA.Identifier := To_CORBA_String ("c");
      Arg_Name_D : constant CORBA.Identifier := To_CORBA_String ("d");
      Request : CORBA.Request.Object;
      Ctx : CORBA.Context.Ref;
      Argument_A : constant CORBA.Any := CORBA.To_Any (A);
      Argument_B : constant CORBA.Any := CORBA.To_Any (B);
      pragma Warnings (Off);
      --  C and D are referenced before they have a value.
      Argument_C : constant CORBA.Any := CORBA.To_Any (C);
      Argument_D : constant CORBA.Any := CORBA.To_Any (D);
      pragma Warnings (On);
      Arg_List : CORBA.NVList.Ref;
      Result : CORBA.NamedValue;
      Result_Name : constant CORBA.String := To_CORBA_String ("Result");
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
                 Argument => Internals.Get_Empty_Any (CORBA.TC_Short),
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
      Returns := From_Any (Result.Argument);
   end In_Out_Fun;

   procedure In_Inout_Fun
     (A : in CORBA.Short;
      B : in out CORBA.Short;
      C : in CORBA.Short;
      D : in out CORBA.Short;
      Returns : out CORBA.Short);

   procedure In_Inout_Fun
     (A : in CORBA.Short;
      B : in out CORBA.Short;
      C : in CORBA.Short;
      D : in out CORBA.Short;
      Returns : out CORBA.Short)
   is
      Operation_Name : constant CORBA.Identifier :=
                         To_CORBA_String ("in_inout_fun");
      Arg_Name_A : constant CORBA.Identifier := To_CORBA_String ("a");
      Arg_Name_B : constant CORBA.Identifier := To_CORBA_String ("b");
      Arg_Name_C : constant CORBA.Identifier := To_CORBA_String ("c");
      Arg_Name_D : constant CORBA.Identifier := To_CORBA_String ("d");
      Request : CORBA.Request.Object;
      Ctx : CORBA.Context.Ref;
      Argument_A : constant CORBA.Any := CORBA.To_Any (A);
      Argument_B : constant CORBA.Any := CORBA.To_Any (B);
      Argument_C : constant CORBA.Any := CORBA.To_Any (C);
      Argument_D : constant CORBA.Any := CORBA.To_Any (D);
      Arg_List : CORBA.NVList.Ref;
      Result : CORBA.NamedValue;
      Result_Name : constant CORBA.String := To_CORBA_String ("Result");
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
                 Argument => Internals.Get_Empty_Any (CORBA.TC_Short),
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
      Returns := From_Any (Result.Argument);
   end In_Inout_Fun;

   procedure Out_Inout_Fun
     (A : out CORBA.Short;
      B : in out CORBA.Short;
      C : in out CORBA.Short;
      D : out CORBA.Short;
      Returns : out CORBA.Short);

   procedure Out_Inout_Fun
     (A : out CORBA.Short;
      B : in out CORBA.Short;
      C : in out CORBA.Short;
      D : out CORBA.Short;
      Returns : out CORBA.Short)
   is
      Operation_Name : constant CORBA.Identifier :=
                         To_CORBA_String ("out_inout_fun");
      Arg_Name_A : constant CORBA.Identifier := To_CORBA_String ("a");
      Arg_Name_B : constant CORBA.Identifier := To_CORBA_String ("b");
      Arg_Name_C : constant CORBA.Identifier := To_CORBA_String ("c");
      Arg_Name_D : constant CORBA.Identifier := To_CORBA_String ("d");
      Request : CORBA.Request.Object;
      Ctx : CORBA.Context.Ref;
      pragma Warnings (Off);
      --  A and D are referenced before they have a value
      Argument_A : constant CORBA.Any := CORBA.To_Any (A);
      Argument_D : constant CORBA.Any := CORBA.To_Any (D);
      pragma Warnings (On);
      Argument_B : constant CORBA.Any := CORBA.To_Any (B);
      Argument_C : constant CORBA.Any := CORBA.To_Any (C);
      Arg_List : CORBA.NVList.Ref;
      Result : CORBA.NamedValue;
      Result_Name : constant CORBA.String := To_CORBA_String ("Result");
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
                 Argument => Internals.Get_Empty_Any (CORBA.TC_Short),
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
      Returns := From_Any (Result.Argument);
   end Out_Inout_Fun;

   procedure In_Out_Inout_Fun (A : in CORBA.Short;
                               B : out CORBA.Short;
                               C : in out CORBA.Short;
                               Returns : out CORBA.Short);

   procedure In_Out_Inout_Fun (A : in CORBA.Short;
                               B : out CORBA.Short;
                               C : in out CORBA.Short;
                               Returns : out CORBA.Short)
   is
      Operation_Name : constant CORBA.Identifier :=
                         To_CORBA_String ("in_out_inout_fun");
      Arg_Name_A : constant CORBA.Identifier := To_CORBA_String ("a");
      Arg_Name_B : constant CORBA.Identifier := To_CORBA_String ("b");
      Arg_Name_C : constant CORBA.Identifier := To_CORBA_String ("c");
      Request : CORBA.Request.Object;
      Ctx : CORBA.Context.Ref;
      Argument_A : constant CORBA.Any := CORBA.To_Any (A);
      pragma Warnings (Off);
      --  B is referenced before it has a value.
      Argument_B : constant CORBA.Any := CORBA.To_Any (B);
      pragma Warnings (On);
      Argument_C : constant CORBA.Any := CORBA.To_Any (C);
      Arg_List : CORBA.NVList.Ref;
      Result : CORBA.NamedValue;
      Result_Name : constant CORBA.String := To_CORBA_String ("Result");
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
                 Argument => Internals.Get_Empty_Any (CORBA.TC_Short),
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
      Returns := From_Any (Result.Argument);
   end In_Out_Inout_Fun;

   procedure Oneway_Void_Proc;

   procedure Oneway_Void_Proc
   is
      Operation_Name : constant CORBA.Identifier :=
                         To_CORBA_String ("oneway_void_proc");
      Request : CORBA.Request.Object;
      Ctx : CORBA.Context.Ref;
      Arg_List : CORBA.NVList.Ref;
      Result : CORBA.NamedValue;
      Result_Name : constant CORBA.String := To_CORBA_String ("Result");
   begin
      --  creating the argument list
      CORBA.ORB.Create_List (0, Arg_List);
      --  setting the result type
      Result := (Name => Identifier (Result_Name),
                 Argument => Internals.Get_Empty_Any (CORBA.TC_Void),
                 Arg_Modes => 0);
      --  creating a request
      CORBA.Object.Create_Request (Myall_Functions,
                                   Ctx,
                                   Operation_Name,
                                   Arg_List,
                                   Result,
                                   Request,
                                   2); --  value for Sync_With_Transport
      --  sending message
      CORBA.Request.Invoke (Request, 0);
   end Oneway_Void_Proc;

   procedure Oneway_In_Proc (A, B : in CORBA.Short);

   procedure Oneway_In_Proc (A, B : in CORBA.Short)
   is
      Operation_Name : constant CORBA.Identifier :=
                         To_CORBA_String ("oneway_in_proc");
      Arg_Name_A : constant CORBA.Identifier := To_CORBA_String ("a");
      Arg_Name_B : constant CORBA.Identifier := To_CORBA_String ("b");
      Request : CORBA.Request.Object;
      Ctx : CORBA.Context.Ref;
      Argument_A : constant CORBA.Any := CORBA.To_Any (A);
      Argument_B : constant CORBA.Any := CORBA.To_Any (B);
      Arg_List : CORBA.NVList.Ref;
      Result : CORBA.NamedValue;
      Result_Name : constant CORBA.String := To_CORBA_String ("Result");
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
                 Argument => Internals.Get_Empty_Any (CORBA.TC_Void),
                 Arg_Modes => 0);
      --  creating a request
      CORBA.Object.Create_Request (Myall_Functions,
                                   Ctx,
                                   Operation_Name,
                                   Arg_List,
                                   Result,
                                   Request,
                                   2); --  value for Sync_With_Transport
      --  sending message
      CORBA.Request.Invoke (Request, 0);
   end Oneway_In_Proc;

   function Oneway_Checker return CORBA.Short;

   function Oneway_Checker return CORBA.Short
   is
      Operation_Name : constant CORBA.Identifier :=
                         To_CORBA_String ("oneway_checker");
      Request : CORBA.Request.Object;
      Ctx : CORBA.Context.Ref;
      Arg_List : CORBA.NVList.Ref;
      Result : CORBA.NamedValue;
      Result_Name : constant CORBA.String := To_CORBA_String ("Result");
   begin
      --  creating the argument list
      CORBA.ORB.Create_List (0, Arg_List);
      --  setting the result type
      Result := (Name => Identifier (Result_Name),
                 Argument => Internals.Get_Empty_Any (CORBA.TC_Short),
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
      return From_Any (Result.Argument);
   end Oneway_Checker;

begin
   New_Test ("Different invocation modes");

   CORBA.ORB.Initialize ("ORB");

   if Ada.Command_Line.Argument_Count < 1 then
      Ada.Text_IO.Put_Line ("usage : client <IOR_string_from_server>");
      return;
   end if;

   --  transforms the Ada string into CORBA.String
   IOR := CORBA.To_CORBA_String (Ada.Command_Line.Argument (1));

   --  getting the CORBA.Object
   CORBA.ORB.String_To_Object (IOR, Myall_Functions);

   Set_The_Attribute (24);
   Output ("test attribute", Get_The_Attribute = 24);

   Output ("test readonly attribute",
           Get_The_Readonly_Attribute = 18);

   begin
      Ok := True;
      Void_Proc;
   exception when others =>
      Ok := False;
   end;
   Output ("test void procedure", Ok);

   begin
      In_Proc (1, 2, 3);
      Ok := True;
   exception when others =>
      Ok := False;
   end;
   Output ("test in param procedure", Ok);

   begin
      Ok := False;
--      Out_Proc (Myall_Functions, I, J, K);
--      Ok := (I = 10) and then (J = 11) and then (K = 12);
   exception when others =>
      Ok := False;
   end;
   Output ("test out param procedure", Ok);

   begin
      Ok := False;
      I  := 2;
      J  := 3;
--      Inout_Proc (Myall_Functions, I, J);
--      Ok := (I = 3 and then J = 4);
   exception when others =>
      Ok := False;
   end;
   Output ("test in out param procedure", Ok);

   begin
      Ok := False;
      I := 1;
      J := 2;
      In_Out_Proc (1, 2, I, J);
      Ok := (I = 3 and then J = 4);
   exception when others =>
      Ok := False;
   end;
   Output ("test in and out param procedure", Ok);

   begin
      Ok := False;
      I  := -4;
      J  := -5;
      In_Inout_Proc (1, I, 3, J);
      Ok := (I = 36) and then (J = 40);
   exception when others =>
      Ok := False;
   end;
   Output ("test in and inout param procedure", Ok);

   begin
      Ok := False;
      I := -11;
      J := -21;
      K := -31;
      L := -41;
--      Out_Inout_Proc (Myall_Functions, I, J, K, L);
--      Ok := (I = 45) and then (J = 46) and then (K = 47) and then (L = 48);
   exception when others =>
      Ok := False;
   end;
   Output ("test inout and out param procedure", Ok);

   begin
      Ok := False;
      I := 78;
      J := 79;
      In_Out_Inout_Proc (1, I, J);
      Ok := (I = -54) and then (J = 80);
   exception when others =>
      Ok := False;
   end;
   Output ("test in and out and inout param procedure", Ok);

   Output ("test void function", Void_Fun = 3);
   Output ("test in param function", In_Fun (1, 2, 3) = 7);

   begin
      Ok := False;
      I := 1;
      J := 2;
      K := 3;
      L := 4;
      Out_Fun (I, J, K, L);
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
      Inout_Fun (I, J, L);
      Ok := (I = 2) and then (J = 3) and then (L = 5);
   exception when others =>
      Ok := False;
   end;
   Output ("test inout param function", Ok);

   begin
      Ok := False;
      I := 10;
      J := 11;
      In_Out_Fun (1, 2, I, J, K);
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
      In_Inout_Fun (-1, I, -2, J, K);
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
      Out_Inout_Fun (I, J, K, L, M);
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
      In_Out_Inout_Fun (85, I, J, K);
      Ok := (I = 86) and then (J = 83) and then (K = -1);
   exception when others =>
      Ok := False;
   end;
   Output ("test in and out and inout param function", Ok);

   begin
      Oneway_Void_Proc;
      delay 1.0;
      Ok := Oneway_Checker = 1;
      if Ok then
         delay 5.0;
         Ok := Oneway_Checker = 2;
      end if;
   exception when others =>
      Ok := False;
   end;
   Output ("test void one way procedure", Ok);

   begin
      Oneway_In_Proc (10, 20);
      delay 1.0;
      Ok := Oneway_Checker = 10;
      if Ok then
         delay 5.0;
         Ok := Oneway_Checker = 20;
      end if;
   exception when others =>
      Ok := False;
   end;
   Output ("test in param one way procedure", Ok);

   End_Report;
end Dynclient;

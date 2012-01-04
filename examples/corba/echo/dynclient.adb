------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                            D Y N C L I E N T                             --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2002-2012, Free Software Foundation, Inc.          --
--                                                                          --
-- This is free software;  you can redistribute it  and/or modify it  under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for  more details.                                               --
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

--  echo dynamic client, using the Dynamic Invocation Interface (DII)

with Ada.Command_Line;
with Ada.Text_IO;

with CORBA.Object;
with CORBA.Context;
with CORBA.Request;
with CORBA.NVList;
with CORBA.ORB;

with PolyORB.Setup.Client;
pragma Warnings (Off, PolyORB.Setup.Client);

with PolyORB.Utils.Report;

procedure DynClient is
   use Ada.Text_IO;
   use PolyORB.Utils.Report;
   use CORBA;

   myecho : CORBA.Object.Ref;

   -------------
   -- Do_Test --
   -------------

   procedure Do_Test;

   procedure Do_Test is
      Sent_Msg : constant CORBA.String :=
                   To_CORBA_String ("Hello Dynamic World");
      Operation_Name : constant CORBA.Identifier :=
                         To_CORBA_String ("echoString");
      Arg_Name : constant CORBA.Identifier := To_CORBA_String ("Mesg");
      Request : CORBA.Request.Object;
      Ctx : CORBA.Context.Ref;
      Argument : CORBA.Any;
      Arg_List : CORBA.NVList.Ref;
      Result : CORBA.NamedValue;
      Result_Name : constant CORBA.String := To_CORBA_String ("Result");
      Recv_Msg : CORBA.String;

   begin
      --  Creating the argument list

      CORBA.ORB.Create_List (0, Arg_List);
      Argument := CORBA.To_Any (Sent_Msg);
      CORBA.NVList.Add_Item (Arg_List,
                             Arg_Name,
                             Argument,
                             CORBA.ARG_IN);

      --  Setting the result type

      Result := (Name => CORBA.Identifier (Result_Name),
                 Argument => Internals.Get_Empty_Any (CORBA.TC_String),
                 Arg_Modes => 0);

      --  Creating a request

      CORBA.Object.Create_Request (myecho,
                                   Ctx,
                                   Operation_Name,
                                   Arg_List,
                                   Result,
                                   Request,
                                   0);

      --  Sending message

      CORBA.Request.Invoke (Request, 0);

      --  Getting the answer

      Recv_Msg := From_Any (Result.Argument);

      --  Printing the result

      Put_Line ("I said : " & CORBA.To_Standard_String (Sent_Msg));
      Put_Line ("The object answered : "
                & CORBA.To_Standard_String (Recv_Msg));
   end Do_Test;

   Iter : Natural := 1;

begin
   New_Test ("Echo dynamic client using the DII");

   CORBA.ORB.Initialize ("ORB");

   if Ada.Command_Line.Argument_Count < 1 then
      Put_Line ("usage : dynclient <IOR_string_from_server> [niter]");
      return;
   end if;

   --  Getting a reference on the CORBA object

   CORBA.ORB.String_To_Object
     (To_CORBA_String (Ada.Command_Line.Argument (1)), myecho);

   if Ada.Command_Line.Argument_Count > 1 then
      Iter := Integer'Value (Ada.Command_Line.Argument (2));
   end if;

   for J in 1 .. Iter loop
      Do_Test;
   end loop;

   End_Report;
end DynClient;

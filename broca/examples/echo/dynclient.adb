------------------------------------------------------------------------------
--                                                                          --
--                          ADABROKER COMPONENTS                            --
--                                                                          --
--                           D Y N C L I E N T                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $LastChangedRevision$
--                                                                          --
--            Copyright (C) 1999 ENST Paris University, France.             --
--                                                                          --
-- AdaBroker is free software; you  can  redistribute  it and/or modify it  --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. AdaBroker  is distributed  in the hope that it will be  useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with AdaBroker; see file COPYING. If  --
-- not, write to the Free Software Foundation, 59 Temple Place - Suite 330, --
-- Boston, MA 02111-1307, USA.                                              --
--                                                                          --
--             AdaBroker is maintained by ENST Paris University.            --
--                     (email: broker@inf.enst.fr)                          --
--                                                                          --
------------------------------------------------------------------------------

--   echo dynamic client.
with Ada.Command_Line;
with Ada.Text_IO;      use Ada.Text_IO;
with CORBA;            use CORBA;
with CORBA.Object;
with CORBA.Context;
with CORBA.Request;
with CORBA.NVList;
with CORBA.ORB;
with Naming_Tools;

procedure DynClient is
   Sent_Msg : CORBA.String := To_CORBA_String ("Hello Dynamic World");
   Operation_Name : CORBA.Identifier := To_CORBA_String ("echoString");
   Arg_Name : CORBA.Identifier := To_CORBA_String ("Mesg");
   myecho : CORBA.Object.Ref;
   Request : CORBA.Request.Object;
   Ctx : CORBA.Context.Ref;
   Argument : CORBA.Any;
   Arg_List : CORBA.NVList.Ref;
   Result : CORBA.NamedValue;
   Result_Name : CORBA.String := To_CORBA_String ("Result");
   Recv_Msg : CORBA.String;
begin

   if Ada.Command_Line.Argument_Count < 1 then
      Put_Line ("usage : client <IOR_string_from_server>|-i");
      return;
   end if;

   --  getting the CORBA.Object
   if Ada.Command_Line.Argument (1) = "-i" then
      myecho := Naming_Tools.Locate ("echo");
   else
      myecho := Naming_Tools.Locate (Ada.Command_Line.Argument (1));
   end if;

   --  creating the argument list
   CORBA.ORB.Create_List (0, Arg_List);
   Argument := CORBA.To_Any (Sent_Msg);
   CORBA.NVList.Add_Item (Arg_List,
                          Arg_Name,
                          Argument,
                          CORBA.ARG_IN);

   --  setting the result type
   Result := (Name => Identifier (Result_Name),
              Argument => Get_Empty_Any (CORBA.TC_String),
              Arg_Modes => 0);

   --  creating a request
   CORBA.Object.Create_Request (myecho,
                                Ctx,
                                Operation_Name,
                                Arg_List,
                                Result,
                                Request,
                                0);

   --  sending message
   CORBA.Request.Invoke (Request, 0);

   --  getting the answer
   Recv_Msg := From_Any (Result.Argument);

   --  printing result
   Put_Line ("I said : " & CORBA.To_Standard_String (Sent_Msg));
   Put_Line ("The object answered : " & CORBA.To_Standard_String (Recv_Msg));

end DynClient;

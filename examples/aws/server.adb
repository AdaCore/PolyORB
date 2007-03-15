------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                               S E R V E R                                --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--            Copyright (C) 2003 Free Software Foundation, Inc.             --
--                                                                          --
-- PolyORB is free software; you  can  redistribute  it and/or modify it    --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. PolyORB is distributed  in the hope that it will be  useful,    --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with PolyORB; see file COPYING. If    --
-- not, write to the Free Software Foundation, 59 Temple Place - Suite 330, --
-- Boston, MA 02111-1307, USA.                                              --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
--                PolyORB is maintained by ACT Europe.                      --
--                    (email: sales@act-europe.fr)                          --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Text_IO; use Ada.Text_IO;

with AWS.Server;
with SOAP.Types;
with SOAP.Parameters;
with SOAP.Message;
with SOAP.Message.Payload;
with SOAP.Message.Response;
with AWS.Server.Servants;
with AWS.Status;
with AWS.Response;
with AWS.Parameters;

with PolyORB.Setup.No_Tasking_Server;
pragma Warnings (Off, PolyORB.Setup.No_Tasking_Server);

with PolyORB.References.IOR;
with PolyORB.References.URI;

procedure Server is

   function Callback
     (Request : in AWS.Status.Data)
      return AWS.Response.Data;

   function Callback
     (Request : in AWS.Status.Data)
      return AWS.Response.Data
   is
      use AWS.Status;
      use AWS.Response;

   begin
      if Is_SOAP (Request) then
         declare
            use SOAP.Types;
            use SOAP.Parameters;
            use SOAP.Message;
            use SOAP.Message.Payload;

            SOAP_Object : SOAP.Message.Payload.Object
              := AWS.Status.Payload (Request);
            P_List : SOAP.Parameters.List := Parameters (SOAP_Object);
            Mesg : constant String := SOAP.Parameters.Get (P_List, "Mesg");
            R  : SOAP.Message.Response.Object;
            RP : SOAP.Parameters.List;
         begin
            Put_Line ("Server: "
                      & Natural'Image (Argument_Count (P_List))
                      & " SOAP parameters");
            Put_Line ("Server: method is "
                      & Procedure_Name (SOAP_Object));
            Put_Line ("Server: echoing """ & Mesg & """");
            R := SOAP.Message.Response.From (SOAP_Object);
            RP := +S (Mesg, "result");
            SOAP.Message.Set_Parameters (R, RP);
            return AWS.Response.Build (R);
         end;
      else
         declare
            P_List : constant AWS.Parameters.List
              := AWS.Status.Parameters (D => Request);
            Mesg : constant String := AWS.Parameters.Get (P_List, "Mesg");
         begin
            Put_Line ("Server: the request is a web one");
            Put_Line ("Server: echoing """ & Mesg & """");
            return AWS.Response.Build ("text/html", Mesg);
         end;
      end if;

   end Callback;

begin
   Put_Line ("initializing PolyORB");
   AWS.Server.Initialization;
   Put_Line ("initialized");

   declare
      SOAP_Server : AWS.Server.Servants.SOAP_Servant;
      Web_Server : AWS.Server.Servants.Web_Servant;
   begin
      Put_Line ("starting servers");
      AWS.Server.Start (SOAP_Server, "soap_server",
                        Max_Connection => 1,
                        Callback       => Callback'Unrestricted_Access);
      AWS.Server.Start (Web_Server, "web_server",
                        Max_Connection => 1,
                        Callback       => Callback'Unrestricted_Access);
      Put_Line ("servers started");

      Put_Line ("SOAP_Server:");
      Put_Line (PolyORB.References.URI.Object_To_String
                (AWS.Server.Get_Server_Reference (SOAP_Server)));
      Put_Line ((PolyORB.References.IOR.Object_To_String
                 (AWS.Server.Get_Server_Reference (SOAP_Server))));

      Put_Line ("Web_Server:");
      Put_Line (PolyORB.References.URI.Object_To_String
                (AWS.Server.Get_Server_Reference (Web_Server)));
      Put_Line ((PolyORB.References.IOR.Object_To_String
                 (AWS.Server.Get_Server_Reference (Web_Server))));

      AWS.Server.Run;
   end;

end Server;

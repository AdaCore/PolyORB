with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_IO; use Ada.Text_IO;

with AWS.Server;
with SOAP.Client;
with SOAP.Parameters;
with SOAP.Types;
with SOAP.Message.Payload;
with SOAP.Message.Response;

with PolyORB.Setup.No_Tasking_Server;
pragma Elaborate_All (PolyORB.Setup.No_Tasking_Server);
pragma Warnings (Off, PolyORB.Setup.No_Tasking_Server);

procedure Client_SOAP is
   use SOAP.Types;
   use SOAP.Client;
   use SOAP.Parameters;

begin
   Put_Line ("client: initializing PolyORB");
   AWS.Server.Initialization;
   Put_Line ("client: initialized");

   if Argument_Count < 1 then
      Put_Line ("usage : client <URI_string_from_server>");
      return;
   else
      declare
         P_Set : SOAP.Parameters.List :=
           +SOAP.Types.S ("Hello, SOAP world!", "Mesg");
         P     : SOAP.Message.Payload.Object
           := SOAP.Message.Payload.Build ("echoString", P_Set);
         R : constant SOAP.Message.Response.Object'Class :=
           SOAP.Client.Call (Ada.Command_Line.Argument (1), P);
         Rep : constant SOAP.Parameters.List
           := SOAP.Message.Parameters (R);
         Response : constant String := SOAP.Parameters.Get (Rep, "result");
      begin
         Put_Line ("Client: sent SOAP request to "
                   & Ada.Command_Line.Argument (1));
         Put_Line ("Client: the server answered "
                   & Response);
      end;
   end if;

end Client_SOAP;

with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_IO; use Ada.Text_IO;

with AWS.Client;
with AWS.Response;
with AWS.Server;

with PolyORB.Setup.No_Tasking_Server;
pragma Elaborate_All (PolyORB.Setup.No_Tasking_Server);
pragma Warnings (Off, PolyORB.Setup.No_Tasking_Server);

procedure Client_Web is

begin
   Put_Line ("client: initializing PolyORB");
   AWS.Server.Initialization;
   Put_Line ("client: initialized");

   if Argument_Count < 1 then
      Put_Line ("usage : client <URI_string_from_server>");
      return;
   else
      declare
         use AWS.Client;
         use AWS.Response;

         Connection : HTTP_Connection;
         Res : AWS.Response.Data;
      begin
         Create (Connection, Ada.Command_Line.Argument (1));
         Get (Connection, Res, Ada.Command_Line.Argument (1)
              & "?Mesg=Hello, Web world!");
         Close (Connection);
         Put_Line ("Client: sent Web request to "
                   & Ada.Command_Line.Argument (1));
         Put_Line ("Client: the server answered "
                   & AWS.Response.Message_Body (Res));
      end;
   end if;

end Client_Web;

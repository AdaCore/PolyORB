with Ada.Text_IO; use Ada.Text_IO;
with Server;
procedure Client is
begin
   Put_Line ("The client has started!");
   Put ("Thus spake my server upon me:");
   Put_Line (Server.Echo_String ("Hi!"));
end Client; 

with Ada.Text_IO;
with System.RPC;
with RCI_1;

procedure Main is
   Failures : Natural := 0;
begin
   while Failures < 20 loop
      begin
         Ada.Text_IO.Put_Line (RCI_1.F ("Test string"));
         Failures := 0;
      exception when System.RPC.Communication_Error =>
         Ada.Text_IO.Put_Line ("Fail to execute RCI_1.F");
         Failures := Failures + 1;
      end;
      delay 1.0;
   end loop;
end Main;

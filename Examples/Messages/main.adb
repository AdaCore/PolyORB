with System.RPC;           use System.RPC;
with System.RPC.Stream_IO; use System.RPC.Stream_IO;
with Ada.Streams;          use Ada.Streams;

with RCI;
with Text_IO;

procedure Main is

   Partner : Partition_ID;

begin

   RCI.Exchange (Main'Partition_Id, Partner);

   declare
      Stream  : aliased Partition_Stream_Type (Partner);
      Message : String := "ping";
   begin
      Text_IO.Put_Line (Message);
      String'Write (Stream'Access, Message);
      String'Read  (Stream'Access, Message);
      Text_IO.Put_Line (Message);
   end;

end Main;

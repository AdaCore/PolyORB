with System.RPC;           use System.RPC;
with System.RPC.Stream_IO; use System.RPC.Stream_IO;
with Ada.Streams;          use Ada.Streams;

package body RCI is

   task Anonymous is
      entry Start (PID : Partition_ID);
   end Anonymous;


   task body Anonymous is
      Partner : Partition_ID;
      Message : String := "xxxx";
   begin
      accept Start (PID : Partition_ID) do
         Partner := PID;
      end Start;
      declare
         Stream : aliased Partition_Stream_Type (Partner);
      begin
         String'Read (Stream'Access, Message);
         if Message = "ping" then
            String'Write (Stream'Access, "pong");
         end if;
      end;
   end Anonymous;

   procedure Exchange
     (Client : in  Partition_ID;
      Server : out Partition_ID) is
   begin
      Server := RCI'Partition_ID;
      Anonymous.Start (Client);
   end Exchange;

end RCI;


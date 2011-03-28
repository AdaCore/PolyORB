with RCI;                  use RCI;
with System.RPC;           use System.RPC;
with System.RPC.Stream_IO; use System.RPC.Stream_IO;
with Ada.Streams;          use Ada.Streams;
with Ada.Text_IO;          use Ada.Text_IO;
with Pure;                 use Pure;

procedure Main is

   task type Robot_Type is
      entry Start (I : Integer);
   end Robot_Type;

   task body Robot_Type is
      M : Message_Type;
      S : aliased Partition_Stream_Type;
   begin
      accept Start (I : Integer) do
         Put_Line ("Start robot" & I'Img);
         M.Value := I;
      end Start;
      while M.Count < N_Counts loop
         Open (S, RCI'Partition_ID, Out_Mode);
         Message_Type'Write (S'Access, M);
         Put_Line ("Send message" & M.Value'Img &
                   " with count" & M.Count'Img);
         Close (S);
         Open (S, RCI'Partition_ID, In_Mode);
         Message_Type'Read  (S'Access, M);
         Close (S);
      end loop;
   end Robot_Type;

   Robots : array (1 .. N_Robots) of Robot_Type;

begin
   RCI.Start (Main'Partition_ID);
   for N in Robots'Range loop
      Robots (N).Start (N);
   end loop;
end Main;


with Ada.Text_IO;          use Ada.Text_IO;
with System.RPC;           use System.RPC;
with System.RPC.Stream_IO; use System.RPC.Stream_IO;
with Pure;                 use Pure;

package body RCI is

   task type Robot_Type is
      entry Start (I : Natural);
   end Robot_Type;

   task body Robot_Type is
      M : Message_Type;
      P : Natural;
      S : aliased Partition_Stream_Type;
   begin
      accept Start (I : Natural) do
         P := I;
      end Start;
      Ada.Text_IO.Put_Line ("Listen to" & P'Img);
      while M.Count < N_Counts loop
         Open (S, Any_Partition, In_Mode);
         Message_Type'Read  (S'Access, M);
         Ada.Text_IO.Put_Line ("Received message" & M.Value'Img &
                               " with count" & M.Count'Img);
         Close (S);
         M.Count := M.Count + 1;
         Open (S, Partition_ID (P), Out_Mode);
         Message_Type'Write (S'Access, M);
         Close (S);
      end loop;
   end Robot_Type;

   Robots : array (1 .. N_Robots) of Robot_Type;

   procedure Start (P : Natural) is
   begin
      for N in Robots'Range loop
         Robots (N).Start (P);
      end loop;
   end Start;

end RCI;

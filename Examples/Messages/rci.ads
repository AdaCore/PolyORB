with System.RPC; use System.RPC;

package RCI is

   pragma Remote_Call_Interface;

   procedure Start (P : Partition_ID);

end RCI;

with System.RPC; use System.RPC;

package RCI is

   pragma Remote_Call_Interface;

   procedure Exchange
     (Client : in  Partition_ID;
      Server : out Partition_ID);

end RCI;

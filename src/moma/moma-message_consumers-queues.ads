with MOMA.Destinations.Queues;

package MOMA.Message_Consumers.Queues is

   -------------------
   --  Queue Object --
   -------------------
   type Queue is new Message_Consumer with null record;

   -------------------------
   --  Get_Queue Function --
   -------------------------
   function Get_Queue return MOMA.Destinations.Queues.Queue;

end MOMA.Message_Consumers.Queues;

with MOMA.Destinations.Queues;
with MOMA.Message_Consumers.Queues;
with MOMA.Message_Producers.Queues;

package MOMA.Sessions.Queues is

   -------------------
   --  Object Queue --
   -------------------

   type Queue is new Session with null record;

   -------------------
   --  Create_Queue --
   -------------------

   function Create_Queue (Queue_Name : String)
                         return MOMA.Destinations.Queues.Queue;

   ----------------------
   --  Create_Receiver --
   ----------------------

   function Create_Receiver (Queue : MOMA.Destinations.Queues.Queue)
                            return MOMA.Message_Consumers.Queues.Queue;

   ----------------------
   --  Create_Receiver --
   ----------------------

   function Create_Receiver (Queue : MOMA.Destinations.Queues.Queue;
                             Message_Selector : String)
                            return MOMA.Message_Consumers.Queues.Queue;

   --------------------
   --  Create_Sender --
   --------------------

   function Create_Sender (Queue : MOMA.Destinations.Queues.Queue)
                          return MOMA.Message_Producers.Queues.Queue;

   -----------------------
   --  Create_Temporary --
   -----------------------

   function Create_Temporary return MOMA.Destinations.Queues.Temporary;

end MOMA.Sessions.Queues;

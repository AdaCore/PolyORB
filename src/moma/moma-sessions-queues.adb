package body MOMA.Sessions.Queues is

   ------------------
   -- Create_Queue --
   ------------------

   function Create_Queue
     (Queue_Name : String)
      return MOMA.Destinations.Queues.Queue
   is
   begin
      pragma Warnings (Off);
      return Create_Queue (Queue_Name);
      pragma Warnings (On);
   end Create_Queue;

   ---------------------
   -- Create_Receiver --
   ---------------------

   function Create_Receiver
     (Queue : MOMA.Destinations.Queues.Queue)
      return MOMA.Message_Consumers.Queues.Queue
   is
   begin
      pragma Warnings (Off);
      return Create_Receiver (Queue);
      pragma Warnings (On);
   end Create_Receiver;

   ---------------------
   -- Create_Receiver --
   ---------------------

   function Create_Receiver
     (Queue : MOMA.Destinations.Queues.Queue;
      Message_Selector : String)
      return MOMA.Message_Consumers.Queues.Queue
   is
   begin
      pragma Warnings (Off);
      return Create_Receiver (Queue, Message_Selector);
      pragma Warnings (On);
   end Create_Receiver;

   -------------------
   -- Create_Sender --
   -------------------

   function Create_Sender
     (Queue : MOMA.Destinations.Queues.Queue)
      return MOMA.Message_Producers.Queues.Queue
   is
   begin
      pragma Warnings (Off);
      return Create_Sender (Queue);
      pragma Warnings (On);
   end Create_Sender;

   ----------------------
   -- Create_Temporary --
   ----------------------

   function Create_Temporary
     return MOMA.Destinations.Queues.Temporary is
   begin
      pragma Warnings (Off);
      return Create_Temporary;
      pragma Warnings (On);
   end Create_Temporary;

end MOMA.Sessions.Queues;


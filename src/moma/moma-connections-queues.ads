with MOMA.Destinations;
with MOMA.Destinations.Queues;
with MOMA.Message_Consumers.Queues;
with MOMA.Sessions.Queues;

package MOMA.Connections.Queues is

   -------------------
   --  Object Queue --
   -------------------
   type Queue is new Connection with null record;

   -------------------------------
   --  Create_Consumer Function --
   -------------------------------
   function Create_Consumer (Queue : Destinations.Queues.Queue;
                             Message_Selector : String)
                            return Message_Consumers.Queues.Queue;

   ------------------------------
   --  Create_Session Function --
   ------------------------------
   function Create_Session (Transacted : Boolean;
                            Ackowledge_Mode : Acknowledge_Type)
                           return Sessions.Queues.Queue;

end MOMA.Connections.Queues;

package body MOMA.Connections.Queues is

   -------------------------------
   --  Create_Consumer Function --
   -------------------------------
   function Create_Consumer (Queue : Destinations.Queues.Queue;
                             Message_Selector : String)
                            return Message_Consumers.Queues.Queue is
      Temp : Message_Consumers.Queues.Queue;
   begin
      return Temp;
   end Create_Consumer;

   ------------------------------
   --  Create_Session Function --
   ------------------------------
   function Create_Session (Transacted : Boolean;
                            Ackowledge_Mode : Acknowledge_Type)
                           return Sessions.Queues.Queue is
      Temp : Sessions.Queues.Queue;
   begin
      return Temp;
   end Create_Session;

end MOMA.Connections.Queues;

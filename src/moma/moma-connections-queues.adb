package body MOMA.Connections.Queues is

   -------------------------------
   --  Create_Consumer Function --
   -------------------------------
   function Create_Consumer (Queue : Destinations.Queues.Queue;
                             Message_Selector : String)
                            return Message_Consumers.Queues.Queue is
      Temp : Message_Consumers.Queues.Queue;
   begin
      pragma Warnings (Off);
      pragma Unreferenced (Queue);
      pragma Unreferenced (Message_Selector);
      pragma Warnings (On);
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
      pragma Warnings (Off);
      pragma Unreferenced (Transacted);
      pragma Unreferenced (Ackowledge_Mode);
      pragma Warnings (On);
      return Temp;
   end Create_Session;

end MOMA.Connections.Queues;

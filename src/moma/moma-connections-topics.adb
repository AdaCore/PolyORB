package body MOMA.Connections.Topics is

   ---------------------
   -- Create_Consumer --
   ---------------------

   function Create_Consumer
     (Topic : Destinations.Topics.Topic;
      Message_Selector : String)
      return Message_Consumers.Topics.Topic
   is
   begin
      pragma Warnings (Off);
      return Create_Consumer (Topic, Message_Selector);
      pragma Warnings (On);
   end Create_Consumer;

   -----------------------------
   -- Create_Durable_Consumer --
   -----------------------------

   function Create_Durable_Consumer
     (Topic : Destinations.Topics.Topic;
      Message_Selector : String)
      return Message_Consumers.Topics.Topic
   is
   begin
      pragma Warnings (Off);
      return Create_Durable_Consumer (Topic, Message_Selector);
      pragma Warnings (On);
   end Create_Durable_Consumer;

   --------------------
   -- Create_Session --
   --------------------

   function Create_Session
     (Transacted : Boolean;
      Ackowledge_Mode : Acknowledge_Type)
      return Sessions.Topics.Topic
   is
   begin
      pragma Warnings (Off);
      return Create_Session (Transacted, Ackowledge_Mode);
      pragma Warnings (On);
   end Create_Session;

end MOMA.Connections.Topics;


with MOMA.Destinations.Topics;
with MOMA.Sessions.Topics;
with MOMA.Message_Consumers.Topics;

package MOMA.Connections.Topics is

   -------------------
   --  Object Topic --
   -------------------

   type Topic is new Connection with null record;

   -------------------------------
   --  Create_Consumer Function --
   -------------------------------

   function Create_Consumer (Topic : Destinations.Topics.Topic;
                             Message_Selector : String)
                            return Message_Consumers.Topics.Topic;

   ------------------------------
   --  Create_Session Function --
   ------------------------------

   function Create_Session (Transacted : Boolean;
                            Ackowledge_Mode : Acknowledge_Type)
                           return Sessions.Topics.Topic;

   --------------------------------------
   -- Create_Durable_Consumer Function --
   --------------------------------------

   function Create_Durable_Consumer  (Topic : Destinations.Topics.Topic;
                                      Message_Selector : String)
                                     return Message_Consumers.Topics.Topic;

end MOMA.Connections.Topics;

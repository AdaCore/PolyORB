with MOMA.Destinations.Topics;
with MOMA.Message_Consumers.Topics;
with MOMA.Message_Producers.Topics;

package MOMA.Sessions.Topics is

   -------------------
   --  Object Topic --
   -------------------

   type Topic is new Session with null record;

   --------------------------------
   --  Create_Durable_Subscriber --
   --------------------------------

   function Create_Durable_Subscriber
     (Topic : MOMA.Destinations.Topics.Topic;
      Name : String)
      return MOMA.Message_Consumers.Topics.Topic;

   --------------------------------
   --  Create_Durable_Subscriber --
   --------------------------------

   function Create_Durable_Subscriber
     (Topic : MOMA.Destinations.Topics.Topic;
      Name : String;
      Message_Selector : String;
      No_Local : Boolean)
      return MOMA.Message_Consumers.Topics.Topic;

   -----------------------
   --  Create_Publisher --
   -----------------------

   function Create_Publisher (Topic : MOMA.Destinations.Topics.Topic)
                             return MOMA.Message_Producers.Topics.Topic;

   ------------------------
   --  Create_Subscriber --
   ------------------------

   function Create_Subscriber (Topic : MOMA.Destinations.Topics.Topic;
                               Name : String)
                               return MOMA.Message_Producers.Topics.Topic;

   ------------------------
   --  Create_Subscriber --
   ------------------------

   function Create_Subscriber (Topic : MOMA.Destinations.Topics.Topic;
                               Name : String;
                               Message_Selector : String;
                               No_Local : Boolean)
                              return MOMA.Message_Producers.Topics.Topic;

   ------------------
   --  Unsubscribe --
   ------------------

   procedure Unsubscribe (Name : String);

end MOMA.Sessions.Topics;

with PolyORB;

package body MOMA.Sessions.Topics is

   -------------------------------
   -- Create_Durable_Subscriber --
   -------------------------------

   function Create_Durable_Subscriber
     (Topic : MOMA.Destinations.Destination;
      Name : String)
      return MOMA.Message_Consumers.Topics.Topic
   is
   begin
      raise PolyORB.Not_Implemented;
      pragma Warnings (Off);
      return Create_Durable_Subscriber (Topic, Name);
      pragma Warnings (On);
   end Create_Durable_Subscriber;

   -------------------------------
   -- Create_Durable_Subscriber --
   -------------------------------

   function Create_Durable_Subscriber
     (Topic : MOMA.Destinations.Destination;
      Name : String;
      Message_Selector : String;
      No_Local : Boolean)
      return MOMA.Message_Consumers.Topics.Topic
   is
   begin
      raise PolyORB.Not_Implemented;
      pragma Warnings (Off);
      return Create_Durable_Subscriber (Topic, Name,
                                        Message_Selector,
                                        No_Local);
      pragma Warnings (On);
   end Create_Durable_Subscriber;

   ----------------------
   -- Create_Publisher --
   ----------------------

   function Create_Publisher
     (Topic : MOMA.Destinations.Destination)
      return MOMA.Message_Producers.Topics.Topic
   is
   begin
      raise PolyORB.Not_Implemented;
      pragma Warnings (Off);
      return Create_Publisher (Topic);
      pragma Warnings (On);
   end Create_Publisher;

   -----------------------
   -- Create_Subscriber --
   -----------------------

   function Create_Subscriber
     (Topic : MOMA.Destinations.Destination;
      Name : String)
      return MOMA.Message_Producers.Topics.Topic
   is
   begin
      raise PolyORB.Not_Implemented;
      pragma Warnings (Off);
      return Create_Subscriber (Topic, Name);
      pragma Warnings (On);
   end Create_Subscriber;

   -----------------------
   -- Create_Subscriber --
   -----------------------

   function Create_Subscriber
     (Topic : MOMA.Destinations.Destination;
      Name : String;
      Message_Selector : String;
      No_Local : Boolean)
      return MOMA.Message_Producers.Topics.Topic
   is
   begin
      raise PolyORB.Not_Implemented;
      pragma Warnings (Off);
      return Create_Subscriber (Topic, Name, Message_Selector, No_Local);
      pragma Warnings (On);
   end Create_Subscriber;

   -----------------
   -- Unsubscribe --
   -----------------

   procedure Unsubscribe (Name : String) is
   begin
      pragma Warnings (Off);
      pragma Unreferenced (Name);
      pragma Warnings (On);
      null;
      --  XXX Not Implemented
   end Unsubscribe;

end MOMA.Sessions.Topics;


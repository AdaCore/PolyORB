package body MOMA.Message_Producers.Topics is

   ---------------
   -- Get_Topic --
   ---------------

   function Get_Topic return MOMA.Destinations.Topics.Topic is
   begin
      pragma Warnings (Off);
      return Get_Topic;
      pragma Warnings (On);
   end Get_Topic;

   -------------
   -- Publish --
   -------------

   procedure Publish (Message : MOMA.Messages.Message'Class) is
   begin
      null;
   end Publish;

   -------------
   -- Publish --
   -------------

   procedure Publish
     (Message : MOMA.Messages.Message'Class;
      Persistent : Boolean;
      Priority_Value : Priority;
      TTL : Time)
   is
   begin
      null;
   end Publish;

end MOMA.Message_Producers.Topics;


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
      pragma Warnings (Off);
      pragma Unreferenced (Message);
      pragma Warnings (On);
      null;
   end Publish;

   -------------
   -- Publish --
   -------------

   procedure Publish
     (Message : MOMA.Messages.Message'Class;
      Persistent : Boolean;
      Priority_Value : MOMA.Types.Priority;
      TTL : Time)
   is
   begin
      pragma Warnings (Off);
      pragma Unreferenced (Message);
      pragma Unreferenced (Persistent);
      pragma Unreferenced (Priority_Value);
      pragma Unreferenced (TTL);
      pragma Warnings (On);
      null;
   end Publish;

end MOMA.Message_Producers.Topics;


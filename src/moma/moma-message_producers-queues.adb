package body MOMA.Message_Producers.Queues is

   ---------------
   -- Get_Queue --
   ---------------

   function Get_Queue return MOMA.Destinations.Queues.Queue is
   begin
      pragma Warnings (Off);
      return Get_Queue;
      pragma Warnings (On);
   end Get_Queue;

   ----------
   -- Send --
   ----------

   procedure Send (Message : MOMA.Messages.Message'Class) is
   begin
      pragma Warnings (Off);
      pragma Unreferenced (Message);
      pragma Warnings (On);
      null;
   end Send;

   ----------
   -- Send --
   ----------

   procedure Send
     (Message : MOMA.Messages.Message'Class;
      Persistent : Boolean;
      Priority_Value : Priority;
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
   end Send;

end MOMA.Message_Producers.Queues;


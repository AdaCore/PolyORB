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
      null;
   end Send;

end MOMA.Message_Producers.Queues;


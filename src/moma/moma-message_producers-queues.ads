with MOMA.Destinations.Queues;
with MOMA.Messages;
with Temp; use Temp;

package MOMA.Message_Producers.Queues is

   -------------------
   --  Object Queue --
   -------------------

   type Queue is new Message_Producer with null record;

   ----------------
   --  Get_Queue --
   ----------------

   function Get_Queue return MOMA.Destinations.Queues.Queue;

   -----------
   --  Send --
   -----------

   procedure Send (Message : MOMA.Messages.Message'Class);

   -----------
   --  Send --
   -----------

   procedure Send (Message : MOMA.Messages.Message'Class;
                   Persistent : Boolean;
                   Priority_Value : Priority;
                   TTL : Time);

end MOMA.Message_Producers.Queues;

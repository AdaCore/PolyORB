with MOMA.Destinations.Topics;
with MOMA.Messages;
with Temp; use Temp;

package MOMA.Message_Producers.Topics is

   -------------------
   --  Object Topic --
   -------------------

   type Topic is new Message_Producer with null record;

   ----------------
   --  Get_Topic --
   ----------------

   function Get_Topic return MOMA.Destinations.Topics.Topic;

   --------------
   --  Publish --
   --------------

   procedure Publish (Message : MOMA.Messages.Message'Class);

   --------------
   --  Publish --
   --------------

   procedure Publish (Message : MOMA.Messages.Message'Class;
                      Persistent : Boolean;
                      Priority_Value : Priority;
                      TTL : Time);

end MOMA.Message_Producers.Topics;

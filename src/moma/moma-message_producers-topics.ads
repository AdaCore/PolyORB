with MOMA.Destinations.Topics;
with MOMA.Messages;
with MOMA.Types;

package MOMA.Message_Producers.Topics is

   type Topic is new Message_Producer with null record;

   function Get_Topic return MOMA.Destinations.Topics.Topic;

   procedure Publish (Message : MOMA.Messages.Message'Class);

   procedure Publish (Message : MOMA.Messages.Message'Class;
                      Persistent : Boolean;
                      Priority_Value : MOMA.Types.Priority;
                      TTL : Time);

end MOMA.Message_Producers.Topics;

package body MOMA.Message_Consumers.Queues is

   -------------------------
   --  Get_Queue Function --
   -------------------------
   function Get_Queue return MOMA.Destinations.Queues.Queue is
      Temp : MOMA.Destinations.Queues.Queue;
   begin
      return Temp;
   end Get_Queue;

end MOMA.Message_Consumers.Queues;


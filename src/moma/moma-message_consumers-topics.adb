package body MOMA.Message_Consumers.Topics is

   -------------------------
   --  Get_Topic Function --
   -------------------------
   function Get_Topic return MOMA.Destinations.Topics.Topic is
      Temp : MOMA.Destinations.Topics.Topic;
   begin
      return Temp;
   end Get_Topic;

   ----------------------------
   --  Get_No_Local Function --
   ----------------------------
   function Get_No_Local return Boolean is
      Temp : Boolean := True;
   begin
      return Temp;
   end Get_No_Local;

end MOMA.Message_Consumers.Topics;

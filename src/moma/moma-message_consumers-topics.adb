package body MOMA.Message_Consumers.Topics is

   -------------------------
   --  Get_Topic Function --
   -------------------------
   function Get_Topic return MOMA.Destinations.Topics.Topic is
   begin
      pragma Warnings (Off);
      return Get_Topic;
      pragma Warnings (On);
   end Get_Topic;

   ----------------------------
   --  Get_No_Local Function --
   ----------------------------
   function Get_No_Local return Boolean is
   begin
      return False;
   end Get_No_Local;

end MOMA.Message_Consumers.Topics;

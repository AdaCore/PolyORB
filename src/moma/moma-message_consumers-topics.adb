with PolyORB;

package body MOMA.Message_Consumers.Topics is

   -------------------------
   --  Get_Topic Function --
   -------------------------
   function Get_Topic return MOMA.Destinations.Destination is
   begin
      raise PolyORB.Not_Implemented;
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
      --  XXX Not Implemented (?)
   end Get_No_Local;

end MOMA.Message_Consumers.Topics;

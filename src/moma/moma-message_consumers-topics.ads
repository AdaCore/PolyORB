with MOMA.Destinations;

package MOMA.Message_Consumers.Topics is

   -------------------
   --  Topic Object --
   -------------------

   type Topic is new Message_Consumer with null record;

   -------------------------
   --  Get_Topic Function --
   -------------------------
   function Get_Topic return MOMA.Destinations.Destination;

   ----------------------------
   --  Get_No_Local Function --
   ----------------------------
   function Get_No_Local return Boolean;

end MOMA.Message_Consumers.Topics;

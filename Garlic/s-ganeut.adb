package body System.Garlic.Network_Utilities is

   use Interfaces.C;

   ---------------------
   -- Network_To_Port --
   ---------------------

   function Network_To_Port (Net_Port : unsigned_short)
     return unsigned_short
     renames Port_To_Network;

   ---------------------
   -- Port_To_Network --
   ---------------------

   function Port_To_Network (Port : unsigned_short)
     return unsigned_short
   is
   begin
      if Default_Bit_Order = High_Order_First then

         --  No conversion needed. On these platforms, htons() defaults
         --  to a null procedure.

         return Port;
      else

         --  We need to swap the high and low byte on this short to make
         --  the port number network compliant.

         return (Port / 256) + (Port mod 256) * 256;
      end if;
   end Port_To_Network;

end System.Garlic.Network_Utilities;

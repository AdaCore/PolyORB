package MOMA.Destinations.Topics is

   -------------------
   --  Topic Object --
   -------------------

   type Topic is new Destination with null record;

   ---------------
   --  Get_Name --
   ---------------

   function Get_Name return String;

   -----------------------
   --  Temporary Object --
   -----------------------

   type Temporary is new Topic with null record;

   -------------
   --  Delete --
   -------------

   procedure Delete;

end MOMA.Destinations.Topics;

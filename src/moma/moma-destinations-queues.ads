package MOMA.Destinations.Queues is

   -------------------
   --  Queue Object --
   -------------------
   type Queue is new Destination with null record;

   ------------------------
   --  Get_Name Function --
   ------------------------
   function Get_Name return String;

   -----------------------
   --  Temporary Object --
   -----------------------
   type Temporary is new Queue with null record;

   -----------------------
   --  Delete Procedure --
   -----------------------
   procedure Delete;

end MOMA.Destinations.Queues;

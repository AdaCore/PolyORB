package MOMA.Destinations is

   ----------------------------------
   --  Abstract Destination Object --
   ----------------------------------
   type Destination is abstract tagged private;

   ---------------------------------
   --  Abstract Get_Name Function --
   ---------------------------------
   function Get_Name return String is abstract;

private
   type Destination is abstract tagged null record;

end MOMA.Destinations;

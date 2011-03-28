package Common is

   pragma Pure;
   type Pool_Type is abstract tagged limited private;

   type Partition_ID is new Natural;

   procedure Test_Primarity
     (Pool    : access Pool_Type;
      Number  : in     Natural) is abstract;

private
   type Pool_Type is abstract tagged limited null record;
end Common;

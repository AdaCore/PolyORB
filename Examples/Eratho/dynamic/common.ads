package Common is

   pragma Pure;
   type Prime_Pool_Type is abstract tagged limited private;

   type Partition_ID is new Natural;

   procedure Test_Primarity
     (Pool    : access Prime_Pool_Type;
      Number  : in     Natural;
      Divider :    out Natural;
      Where   :    out Partition_ID) is abstract;

private
   type Prime_Pool_Type is abstract tagged limited null record;
end Common;

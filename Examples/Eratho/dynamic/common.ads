package Common is

   pragma Pure;
   type Prime_Pool_Type is abstract tagged limited private;

   procedure Test_Number
     (Pool    : access Prime_Pool_Type;
      Number  : in     Natural;
      Divider :    out Natural;
      Where   :    out Natural) is abstract;

private
   type Prime_Pool_Type is abstract tagged limited null record;
end Common;

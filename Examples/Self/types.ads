package Types is

   pragma Pure;

   type Node_Type is abstract tagged limited private;

   procedure Print (N : access Node_Type) is abstract;

private

   type Node_Type is abstract tagged limited null record;

end Types;

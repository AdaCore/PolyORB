package Types is
   pragma Pure;

   type Word is new String (1 .. 16);
   procedure Set (W : in out Word; S : String);
end Types;

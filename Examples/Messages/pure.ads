package Pure is

   pragma Pure;

   N_Counts : constant := 10;
   N_Robots : constant := 10;

   type Message_Type is
      record
         Value : Integer;
         Count : Integer  := 0;
      end record;

end Pure;

package Vector is
   pragma Shared_Passive;

   N_Partitions : constant := 4;
   Block_Size   : constant := 5;
   Length       : constant := N_Partitions * Block_Size;

   type Content_Type is array (0 .. Length - 1) of Natural;

   Content : Content_Type := (others => 0);

end Vector;

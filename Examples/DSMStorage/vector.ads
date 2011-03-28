package Vector is
   pragma Shared_Passive;

   N_Partitions : constant := 4;
   Block_Size   : constant := 5;
   Length       : constant := N_Partitions * Block_Size;

   type Content_Type is array (0 .. Length - 1) of Natural;

   protected Content is
      procedure Increment (N : Natural; I : Natural);
      function Value (N : Natural) return Natural;
   private
      X : Content_Type := (others => 0);
   end Content;

end Vector;

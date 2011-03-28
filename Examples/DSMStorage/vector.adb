package body Vector is

   protected body Content is
      procedure Increment (N : Natural; I : Natural) is
      begin
         X (N) := X (N) + I;
      end Increment;

      function Value (N : Natural) return Natural is
      begin
         return X (N);
      end Value;
   end Content;

end Vector;

package body MOMA.Messages.MArrays is

   ----------------
   -- Read_Array --
   ----------------

   function Read_Array return Array_Type is
   begin
      pragma Warnings (Off);
      return Read_Array;
      pragma Warnings (On);
   end Read_Array;

   ---------------
   -- Set_Array --
   ---------------

   procedure Set_Array (Value : Array_Type) is
   begin
      pragma Warnings (Off);
      pragma Unreferenced (Value);
      pragma Warnings (On);
      null;
   end Set_Array;

end MOMA.Messages.MArrays;


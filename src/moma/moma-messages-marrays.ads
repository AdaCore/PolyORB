package MOMA.Messages.MArrays is

   --------------------
   --  MArray Object --
   --------------------
   type MArray is new Message with null record;

   -----------------
   --  Read_Array --
   -----------------
   function Read_Array return Array_Type;

   ----------------
   --  Set_Array --
   ----------------
   procedure Set_Array (Value : Array_Type);

end MOMA.Messages.MArrays;

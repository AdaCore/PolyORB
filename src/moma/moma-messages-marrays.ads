package MOMA.Messages.MArrays is

   type MArray is new Message with null record;

   function Read_Array return MOMA.Types.Array_Type;

   procedure Set_Array (Value : MOMA.Types.Array_Type);

end MOMA.Messages.MArrays;

with MOMA.Types;

package MOMA.Messages.MRecords is

   type MRecord is new Message with null record;

   function Read_Record return MOMA.Types.Record_Type;

   procedure Set_Record (Value : MOMA.Types.Record_Type);

end MOMA.Messages.MRecords;

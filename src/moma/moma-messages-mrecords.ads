with Temp; use Temp;

package MOMA.Messages.MRecords is

   ---------------------
   --  MRecord Object --
   ---------------------
   type MRecord is new Message with null record;

   ------------------
   --  Read_Record --
   ------------------
   function Read_Record return Record_Type;

   -----------------
   --  Set_Record --
   -----------------
   procedure Set_Record (Value : Record_Type);

end MOMA.Messages.MRecords;

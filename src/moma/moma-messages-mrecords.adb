package body MOMA.Messages.MRecords is

   -----------------
   -- Read_Record --
   -----------------

   function Read_Record return MOMA.Types.Record_Type is
   begin
      pragma Warnings (Off);
      return Read_Record;
      pragma Warnings (On);
   end Read_Record;

   ----------------
   -- Set_Record --
   ----------------

   procedure Set_Record (Value : MOMA.Types.Record_Type) is
   begin
      pragma Warnings (Off);
      pragma Unreferenced (Value);
      pragma Warnings (On);
      null;
   end Set_Record;

end MOMA.Messages.MRecords;


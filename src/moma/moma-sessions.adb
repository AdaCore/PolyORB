package body MOMA.Sessions is

   ------------
   --  Close --
   ------------

   procedure Close is
   begin
      null;
   end Close;

   -------------
   --  Commit --
   -------------

   procedure Commit is
   begin
      null;
   end Commit;

   ---------------------------
   --  Create_Array_Message --
   ---------------------------

   function Create_Array_Message return Messages.MArrays.MArray is
   begin
      pragma Warnings (Off);
      return Create_Array_Message;
      pragma Warnings (On);
   end Create_Array_Message;

   --------------------------
   --  Create_Byte_Message --
   --------------------------

   function Create_Byte_Message return Messages.MBytes.MByte is
   begin
      pragma Warnings (Off);
      return Create_Byte_Message;
      pragma Warnings (On);
   end Create_Byte_Message;

   ----------------------------
   --  Create_Record_Message --
   ----------------------------

   function Create_Record_Message return Messages.MRecords.MRecord is
   begin
      pragma Warnings (Off);
      return Create_Record_Message;
      pragma Warnings (On);
   end Create_Record_Message;

   ----------------------------
   --  Create_Stream_Message --
   ----------------------------

   function Create_Stream_Message return Messages.MStreams.MStream is
   begin
      pragma Warnings (Off);
      return Create_Stream_Message;
      pragma Warnings (On);
   end Create_Stream_Message;

   --------------------------
   --  Create_Text_Message --
   --------------------------

   function Create_Text_Message return Messages.MTexts.MText is
   begin
      pragma Warnings (Off);
      return Create_Text_Message;
      pragma Warnings (On);
   end Create_Text_Message;

   --------------------------
   --  Create_Text_Message --
   --------------------------

   function Create_Text_Message
     (Value : String)
      return Messages.MTexts.MText
   is
   begin
      pragma Warnings (Off);
      return Create_Text_Message (Value);
      pragma Warnings (On);
   end Create_Text_Message;

   ---------------------
   --  Get_Transacted --
   ---------------------

   function Get_Transacted return Boolean is
   begin
      pragma Warnings (Off);
      return Get_Transacted;
      pragma Warnings (On);
   end Get_Transacted;

   --------------
   --  Recover --
   --------------

   procedure Recover is
   begin
      null;
   end Recover;

   ---------------
   --  Rollback --
   ---------------

   procedure Rollback is
   begin
      null;
   end Rollback;

end MOMA.Sessions;


with MOMA.Messages.MArrays;
with MOMA.Messages.MBytes;
with MOMA.Messages.MRecords;
with MOMA.Messages.MStreams;
with MOMA.Messages.MTexts;

package MOMA.Sessions is

   ------------------------------
   --  Abstract Session Object --
   ------------------------------
   type Session is abstract tagged private;

   ------------
   --  Close --
   ------------
   procedure Close;

   -------------
   --  Commit --
   -------------
   procedure Commit;

   --------------------------
   --  Create_Byte_Message --
   --------------------------
   function Create_Byte_Message return MOMA.Messages.MBytes.MByte;

   --------------------------
   --  Create_Text_Message --
   --------------------------
   function Create_Text_Message return MOMA.Messages.MTexts.MText;

   --------------------------
   --  Create_Text_Message --
   --------------------------
   function Create_Text_Message (Value : String)
                                 return MOMA.Messages.MTexts.MText;

   ---------------------------
   --  Create_Array_Message --
   ---------------------------
   function Create_Array_Message
     return MOMA.Messages.MArrays.MArray;

   ----------------------------
   --  Create_Record_Message --
   ----------------------------
   function Create_Record_Message
     return MOMA.Messages.MRecords.MRecord;

   ----------------------------
   --  Create_Stream_Message --
   ----------------------------
   function Create_Stream_Message
     return MOMA.Messages.MStreams.MStream;

   ---------------------
   --  Get_Transacted --
   ---------------------
   function Get_Transacted return Boolean;

   --------------
   --  Recover --
   --------------
   procedure Recover;

   ---------------
   --  Rollabck --
   ---------------
   procedure Rollback;

private
   type Session is abstract tagged null record;

end MOMA.Sessions;

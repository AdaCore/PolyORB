with Tapes, Name_Server;
with Text_IO;
-- Tape_Driver is not needed and thus not mentioned in the with_clause
procedure Tape_Client is
   T1, T2 : Name_Server.Tape_Ptr;
begin
   Text_IO.Put_Line ("-0-");
   T1 := Name_Server.Find ("NINE-TRACK");
   Text_IO.Put_Line ("-1-");
   T2 := Name_Server.Find ("SEVEN-TRACK");
   Text_IO.Put_Line ("-2-");
   Tapes.Rewind (T1);
   Text_IO.Put_Line ("-3-");
   Tapes.Rewind (T2);
   Text_IO.Put_Line ("-4-");
   Tapes.Copy (T1, T2, 3);
   Text_IO.Put_Line ("-5-");
end Tape_Client;

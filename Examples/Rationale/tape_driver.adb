with Tapes, Name_Server;
with Text_IO;
package body Tape_Driver is

   procedure Copy (From, To : access New_Tape; Num_Recs: in Natural) is
   begin
      Text_IO.Put_Line ("Copy from " & Name_Server.Name (From)  &
							   " to "       & Name_Server.Name (To)    & 
                        Num_Recs'Img & " records");
   end Copy;

   procedure Rewind (T : access New_Tape) is
   begin
      Text_IO.Put_Line ("Rewind " & Name_Server.Name (T));
   end Rewind;

   -- Objects remotely accessible through use
   -- of Name_Server operations

   Tape1, Tape2 : aliased New_Tape;

begin
   Name_Server.Register ("NINE-TRACK",  Tape1'Access);
   Name_Server.Register ("SEVEN-TRACK", Tape2'Access);
end Tape_Driver;

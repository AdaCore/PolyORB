with Tapes;
package Tape_Driver is
   type New_Tape is new Tapes.Tape with null record;

   procedure Copy (From, To : access New_Tape; Num_Recs: in Natural);
   procedure Rewind (T : access New_Tape);

end Tape_Driver;

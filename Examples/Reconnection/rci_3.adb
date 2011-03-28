package body RCI_3 is

   function F (S : String) return String is
   begin
      return "(via RCI_3.F) " & S;
   end F;

end RCI_3;

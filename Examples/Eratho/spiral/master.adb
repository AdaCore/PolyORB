with Prime_1;
with Text_IO; use Text_IO;
procedure master is
   Prime   : Natural;
   Initial : Natural := 0;
begin
   for Number in 2 .. 50 loop
      Prime_1.Test_Number (Number, Initial, Prime);
      if Prime = Number then
	      Put_Line (Number'Img & " (prime)");
      else
         Put_Line (Number'Img & "         (divided by" & Prime'Img & ")");
      end if;
   end loop;
end Master;

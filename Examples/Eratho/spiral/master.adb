with Prime_1;
with Text_IO; use Text_IO;
procedure master is
   Divider   : Natural;
   Where     : Natural;
begin
   for Number in 2 .. 50 loop
      Prime_1.Test_Number (Number, Divider, Where);
      if Divider = Number then
	      Put_Line (Number'Img & " (prime on" & Where'Img & ")");
      else
         Put_Line (Number'Img & "             (divided by" &
                   Divider'Img & " on" & Where'Img & ")");
      end if;
   end loop;
end Master;

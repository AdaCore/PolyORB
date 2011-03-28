with Prime_1;
with Text_IO;
with Types;
use Types;
procedure master is
   Success : Boolean;
   Where   : Std_Node;
begin
   for Number in Std_Number(2) .. Std_Number(150) loop
      Prime_1.Initiate (Number, Where, Success);
      if Success then
	 Text_IO.Put_Line (Std_Number'Image (Number) & " (node" &
                           Std_Node'Image (Where) & ")");
	 Text_IO.Flush;
      end if;
   end loop;
   Text_IO.Put_Line ("Main procedure completed");
end Master;

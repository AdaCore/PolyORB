--
-- Copyright (c) Laurent Pautet (ENST, Paris) email : pautet@inf.enst.fr
--

with Prime_1;
with System.IO;
with Types;
use Types;
procedure master is
   Success : Boolean;
   Where   : Std_Node;
begin
   for Number in Std_Number(2) .. Std_Number(200) loop
      Prime_1.Initiate (Number, Where, Success);
      if Success then
	 System.IO.Put_Line (Std_Number'Image (Number) & " (node" &
        	             Std_Node'Image (Where) & ")");
      end if;
   end loop;
end Master;

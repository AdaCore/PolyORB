with Text_IO; use Text_IO;
with Prime_1;
with Results;
with Common; use Common;

procedure Mainloop is

   Divider   : Natural;
   Where     : Partition_ID;

begin

   for Number in 2 .. 50 loop

      Prime_1.Test_Primarity (Number);
      Results.Load_When_Ready (Divider, Where);
      if Divider = Number then
              Put_Line (Natural'Image (Number) &
                   " (prime on" &
                   Partition_ID'Image (Where) &
                   ")");

      else
         Put_Line (Natural'Image (Number) &
                   "             (divided by" &
                   Natural'Image (Divider) &
                   " on" &
                   Partition_ID'Image (Where) &
                   ")");

      end if;

   end loop;

end Mainloop;

with Text_IO; use Text_IO;
with Controller; use Controller;
with Results;
with Common; use Common;

procedure Mainloop is

   Divider : Natural;
   Where   : Partition_ID;
   Starter : Pool_Access;

begin

   Starter := Next (Mainloop'Partition_ID);
   for Number in 2 .. 50 loop

      Test_Primarity (Starter, Number);
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

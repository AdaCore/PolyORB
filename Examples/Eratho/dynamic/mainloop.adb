--  Predefined Libraries
with Ada.Text_IO; use Ada.Text_IO;

--  User Defined Libraries
with Controller; use Controller;
pragma Elaborate (Controller);

with Prime;
pragma Elaborate (Prime);

with Common; use Common;

procedure Mainloop is

   Divider   : Natural;
   Where     : Partition_ID;
   First     : Prime_Pool_Access;

begin

   First := Controller.First;
   for Number in 2 .. 50 loop

      Test_Primarity (First, Number, Divider, Where);
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

with Prime_2;
with Alarm;
package body Prime_1 is

   package Next_Pool renames Prime_2;

   Last    : Natural  := 0;
   Length  : constant := 10;
   Table   : array (1 .. Length) of Natural;

   Current : Natural := 1;
   Testing : Natural := 0;

   procedure Test_Number (Number   : in  Natural) is
   begin
      if Testing /= Number then
         Testing := Number;
         Current := 1;
      end if;
      if Current <= Last then
         if Number mod Table (Current) = 0 then
            Alarm.Write (Table (Current), Prime_1'Partition_ID);
         else
            Current := Current + 1;
            Next_Pool.Test_Number (Number);
         end if;
      else
         Last := Last + 1;
         Table (Last) := Number;
         Alarm.Write (Table (Current), Prime_1'Partition_ID);
      end if;
   end Test_Number;
            
end Prime_1;

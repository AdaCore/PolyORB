with Prime_1;
package body Prime_3 is

   package Next_Pool renames Prime_1;

   Last    : Natural  := 0;
   Length  : constant := 10;
   Table   : array (1 .. Length) of Natural;

   Current : Natural := 1;

   procedure Test_Number
     (Number   : in  Natural;
      Divider  : out Natural;
      Where    : out Natural) is
   begin
      if Current <= Last then
         if Number mod Table (Current) = 0 then
            Divider := Table (Current);
            Where := Prime_3'Partition_ID;
         else
            Current := Current + 1;
            Next_Pool.Test_Number
              (Number,
               Divider,
               Where);
            Current := Current - 1;
         end if;
      else
         Last := Last + 1;
         Table (Last) := Number;
         Divider := Number;
         Where := Prime_3'Partition_ID;
      end if;
   end Test_Number;
            
end Prime_3;

with Prime_2;
with Results;
with Common; use Common;

package body Prime_1 is


   --  In this table are stored the different prime number this unit
   --  is in charge of. Last_Prime_Index is the prime table index
   --  in which is stored the last prime this unit is in charge of.
   --  Current_Prime_Index points to the prime to test againt the number.

   type Prime_Index is range 0 .. 10;
   Local_Prime_Table   : array (Prime_Index) of Natural;
   Last_Prime_Index    : Prime_Index := 0;
   Current_Prime_Index : Prime_Index := 1;

   --  Remember which number we are testing in order to reset
   --  Current_Prime_Index when a new number is tested.
   Number_To_Test      : Natural := 0;

   --  U'Partition_ID returns a integer which is a unique id used
   --  to identify the partition on which unit U is really located.
   Local_Partition_ID  :Partition_ID := Prime_1'Partition_ID;

   procedure Test_Primarity (Number : in  Natural) is
   begin

      --  Did we change of number to test.
      if Number_To_Test /= Number then
         Number_To_Test := Number;
         Current_Prime_Index := 1;
      end if;

      --  Is there any other prime to test againt Number.
      if Current_Prime_Index <= Last_Prime_Index then
         if Number mod Local_Prime_Table (Current_Prime_Index) = 0 then

            -- Signal to the mainloop that this number has been analyzed.
            Results.Save
              (Local_Prime_Table (Current_Prime_Index), Local_Partition_ID);

         else

            --  Next time, test against the next prime number the
            --  partition is in charge of.

            Current_Prime_Index := Current_Prime_Index + 1;
            Prime_2.Test_Primarity (Number);

         end if;
      else
         Last_Prime_Index := Last_Prime_Index + 1;
         Local_Prime_Table (Last_Prime_Index) := Number;

         Results.Save
           (Local_Prime_Table (Current_Prime_Index), Local_Partition_ID);

      end if;
   end Test_Primarity;

end Prime_1;

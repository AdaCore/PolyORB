--  User Defined Libraries
with Prime_2;
with Results;
with Common; use Common;

package body Prime_1 is

   --  Local Prime Table stuff
   type Prime_Index is range 0 .. 10;
   Local_Prime_Table   : array (Prime_Index) of Natural;

   Last_Prime_Index    : Prime_Index := 0;
   Current_Prime_Index : Prime_Index := 1;

   Testing_Primarity_Number : Natural := 0;

   procedure Test_Primarity (Number : in  Natural) is
   begin

      if Testing_Primarity_Number /= Number then
         Testing_Primarity_Number := Number;
         Current_Prime_Index := 1;
      end if;

      if Current_Prime_Index <= Last_Prime_Index then
         if Number mod Local_Prime_Table (Current_Prime_Index) = 0 then

            Results.Save
              (Local_Prime_Table (Current_Prime_Index),
               Prime_1'Partition_ID);

         else

            Current_Prime_Index := Current_Prime_Index + 1;
            Prime_2.Test_Primarity (Number);

         end if;
      else
         Last_Prime_Index := Last_Prime_Index + 1;
         Local_Prime_Table (Last_Prime_Index) := Number;

         Results.Save
           (Local_Prime_Table (Current_Prime_Index),
            Prime_1'Partition_ID);

      end if;
   end Test_Primarity;

end Prime_1;

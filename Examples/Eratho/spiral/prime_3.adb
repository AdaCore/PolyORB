--  User Defined Libraries
with Prime_1;
with Common; use Common;

package body Prime_3 is

   --  Local Prime Table stuff
   type Prime_Index is range 0 .. 10;
   Local_Prime_Table   : array (Prime_Index) of Natural;

   Last_Prime_Index    : Prime_Index := 0;
   Current_Prime_Index : Prime_Index := 1;

   procedure Test_Primarity
     (Number   : in  Natural;
      Divider  : out Natural;
      Where    : out Partition_ID) is
   begin

      if Current_Prime_Index <= Last_Prime_Index then
         if Number mod Local_Prime_Table (Current_Prime_Index) = 0 then

            Divider := Local_Prime_Table (Current_Prime_Index);
            Where   := Prime_3'Partition_ID;

         else

            Current_Prime_Index := Current_Prime_Index + 1;
            Prime_1.Test_Primarity
              (Number,
               Divider,
               Where);
            Current_Prime_Index := Current_Prime_Index - 1;

         end if;
      else
         Last_Prime_Index := Last_Prime_Index + 1;
         Local_Prime_Table (Last_Prime_Index) := Number;

         Divider := Number;
         Where   := Prime_3'Partition_ID;

      end if;
   end Test_Primarity;

end Prime_3;

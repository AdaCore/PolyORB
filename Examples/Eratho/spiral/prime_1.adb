--  Predefined Libraries
with System.RPC; use System.RPC;

--  User Defined Libraries
with Prime_2;

package body Prime_1 is

   --  Local Prime Table stuff
   type Index is range 0 .. 10;
   Local_Prime_Table   : array (Index) of Natural;

   Last_Prime_Index    : Index := 0;
   Current_Prime_Index : Index := 1;

   procedure Test_Primarity
     (Number   : in  Natural;
      Divider  : out Natural;
      Where    : out Partition_ID) is
   begin

      if Current_Prime_Index <= Last_Prime_Index then
         if Number mod Local_Prime_Table (Current_Prime_Index) = 0 then

            Divider := Local_Prime_Table (Current_Prime_Index);
            Where   := Prime_1'Partition_ID;

         else

            Current_Prime_Index := Current_Prime_Index + 1;
            Prime_2.Test_Primarity
              (Number,
               Divider,
               Where);
            Current_Prime_Index := Current_Prime_Index - 1;

         end if;
      else
         Last_Prime_Index := Last_Prime_Index + 1;
         Local_Prime_Table (Last_Prime_Index) := Number;

         Divider := Number;
         Where   := Prime_1'Partition_ID;

      end if;
   end Test_Primarity;

end Prime_1;

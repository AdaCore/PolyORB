--  User Defined Libraries
with Controller; use Controller;
with Common; use Common;

package body Prime is

   Self_Pool_Reference : aliased New_Pool_Type;
   Self_Pool_Index     : Pool_Index;

   Next_Pool_Reference : Prime_Pool_Access;
   Next_Pool_Ref_Known : Boolean := False;

   --  Local Prime Table stuff
   type Prime_Index is range 0 .. 10;
   Local_Prime_Table   : array (Prime_Index) of Natural;

   Last_Prime_Index    : Prime_Index := 0;
   Current_Prime_Index : Prime_Index := 1;

   procedure Test_Primarity
     (Pool     : access New_Pool_Type;
      Number   : in  Natural;
      Divider  : out Natural;
      Where    : out Partition_ID) is
   begin

      if Current_Prime_Index <= Last_Prime_Index then
         if Number mod Local_Prime_Table (Current_Prime_Index) = 0 then

            Divider := Local_Prime_Table (Current_Prime_Index);
            Where   := Prime'Partition_ID;

         else

            if not Next_Pool_Ref_Known then
               Next_Pool_Reference := Controller.Next (Self_Pool_Index);
               Next_Pool_Ref_Known := True;
            end if;

            Current_Prime_Index := Current_Prime_Index + 1;
            Test_Primarity
              (Next_Pool_Reference,
               Number,
               Divider,
               Where);
            Current_Prime_Index := Current_Prime_Index - 1;

         end if;
      else
         Last_Prime_Index := Last_Prime_Index + 1;
         Local_Prime_Table (Last_Prime_Index) := Number;

         Divider := Number;
         Where   := Prime'Partition_ID;

      end if;
   end Test_Primarity;

begin
   Controller.Register (Self_Pool_Reference'Access, Self_Pool_Index);
end Prime;

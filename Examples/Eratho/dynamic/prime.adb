with Controller; use Controller;
with Common; use Common;
package body Prime is

   Local : aliased New_Pool_Type;
   Index : Natural;
   Next  : Prime_Pool_Access;
   Ready : Boolean := False;

   Last    : Natural  := 0;
   Length  : constant := 10;
   Table   : array (1 .. Length) of Natural;

   Current : Natural := 1;

   procedure Test_Number
     (Pool     : access New_Pool_Type;
      Number   : in     Natural;
      Divider  : out Natural;
      Where    : out Natural) is
   begin
      if Current <= Last then
         if Number mod Table (Current) = 0 then
            Divider := Table (Current);
            Where   := Prime'Partition_ID;
         else
            if not Ready then
               Next := Controller.Next (Index);
            end if;
            Current := Current + 1;
            Test_Number
              (Next,
               Number,
               Divider,
               Where);
            Current := Current - 1;
         end if;
      else
         Last := Last + 1;
         Table (Last) := Number;
         Divider := Number;
         Where   := Prime'Partition_ID;
      end if;
   end Test_Number;

begin
   Controller.Register (Local'Access, Index);
end Prime;

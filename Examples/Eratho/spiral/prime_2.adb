with Prime_3;
package body Prime_2 is

   package Next_Pool renames Prime_3;

   type Cell_Type is 
   record
      Prime : Natural := 0;
      Next  : Natural := 0;
   end record;

   Last : Natural := 0;
   Cells : array (1 .. 10) of Cell_Type;

   procedure Test_Number
     (Number  : in     Natural;
      Cell    : in out Natural;
      Prime   : out    Natural) is
   begin
      if Cell = 0 then
         Last  := Last + 1;
         Cell  := Last;
         Prime := Number;
         Cells (Last).Prime := Number;
      else
         if Number mod Cells (Cell).Prime = 0 then
            Prime := Cells (Cell).Prime;
         else
            Next_Pool.Test_Number
              (Number,
               Cells (Cell).Next,
               Prime);
         end if;
      end if;
   end Test_Number;
            
end Prime_2;

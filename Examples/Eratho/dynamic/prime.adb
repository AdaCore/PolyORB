with Controller; use Controller;
with Common; use Common;
package body Prime is

   type Cell_Type is 
   record
      Prime : Natural := 0;
      Next  : Natural := 0;
   end record;

   Local : aliased New_Pool_Type;
   Index : Natural;
   Next  : Prime_Pool_Access;
   Ready : Boolean := False;

   Last : Natural := 0;
   Cells : array (1 .. 20) of Cell_Type;

   procedure Test_Number
     (Pool    : access New_Pool_Type;
      Number  : in     Natural;
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
            if not Ready then
               Next := Controller.Next (Index);
            end if;
            Test_Number
              (Next,
               Number,
               Cells (Cell).Next,
               Prime);
         end if;
      end if;
   end Test_Number;
            
begin
   Controller.Register (Local'Access, Index);
end Prime;

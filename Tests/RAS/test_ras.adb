--
--  $Id$
--

with Ada.Text_IO;     use Ada.Text_IO;
with Test_RAS_Remote; use Test_RAS_Remote;

procedure Test_RAS is

begin
   if Perform (2, 3, Add'Access) = 5 then
      Put_Line ("Addition performed correctly");
   else
      Put_Line ("Warning, error in addition");
   end if;
   if Perform (2, 3, Multiply'Access) = 6 then
      Put_Line ("Multiplication performed correctly");
   else
      Put_Line ("Warning, error in multiplication");
   end if;
end Test_RAS;

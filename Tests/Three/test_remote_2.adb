--
--  $Id$
--

with Test_Remote_3; use Test_Remote_3;

package body Test_Remote_2 is

   function Test_2 (S : String) return String is
   begin
      return Test_3 (S) & " (via Test_2)";
   end Test_2;

end Test_Remote_2;

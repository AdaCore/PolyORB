--
--  $Id$
--

package body Test_Remote_3 is

   function Test_3 (S : String) return String is
   begin
      return S & " (via Test_3)";
   end Test_3;

end Test_Remote_3;

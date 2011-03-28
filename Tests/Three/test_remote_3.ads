--
-- $Id$
--

package Test_Remote_3 is

   pragma Remote_Call_Interface;

   function Test_3 (S : String) return String;

end Test_Remote_3;

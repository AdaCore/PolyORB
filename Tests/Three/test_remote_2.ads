--
-- $Id$
--

package Test_Remote_2 is

   pragma Remote_Call_Interface;

   function Test_2 (S : String) return String;

end Test_Remote_2;

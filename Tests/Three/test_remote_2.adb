--
--  $Id$
--

with Test_Remote_3; use Test_Remote_3;
with System.RPC;    use System.RPC;
with Ada.Text_IO;   use Ada.Text_IO;

package body Test_Remote_2 is

   function Test_2 (S : String) return String is
   begin
      loop
         declare
            Substring : String := Test_3 (S) & " (via Test_2)";
         begin
            return Substring;
         exception when Communication_Error =>
            Put_LIne ("Cannot execute Tests_3");
         end;
      end loop;
   end Test_2;

end Test_Remote_2;

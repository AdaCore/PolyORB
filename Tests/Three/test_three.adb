--
--  $Id$
--

with Ada.Text_IO; use Ada.Text_IO;
with System.RPC;  use System.RPC;
with Test_Remote_2; use Test_Remote_2;
with Test_Remote_3; use Test_Remote_3;

procedure Test_Three is
begin
   loop
      declare
         Substring : String := Test_3 ("Test string");
      begin
         Put_Line (Substring);
      exception when Communication_Error =>
         Put_Line ("Cannot execute Test_3");
      end;
      delay 2.0;

      declare
         Substring : String := Test_2 ("Test string");
      begin
         Put_Line (Substring);
      exception when Communication_Error =>
         Put_Line ("Cannot execute Test_2");
      end;
      delay 2.0;
   end loop;
end Test_Three;

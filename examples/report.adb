with Ada.Text_IO;

package body Report is

   procedure Output
     (Message : in String;
      Result  : in Boolean)
   is
      Line : String (1 .. 40) := (others => ' ');
   begin
      Line (1 .. Message'Length) := Message;
      Ada.Text_IO.Put (Line);
      if Result then
         Ada.Text_IO.Put_Line (": PASSED");
      else
         Ada.Text_IO.Put_Line (": FAILED");
      end if;
   end Output;
end;


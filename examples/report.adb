with Ada.Text_IO;

package body Report is

   Max : constant Natural := 60;
   procedure Output
     (Message : in String;
      Result  : in Boolean)
   is
      Line : String (1 .. Max) := (others => '.');
      Last : Natural := Message'Length;
   begin
      if Last > Max then
         Last := Max;
      end if;
      Line (1 .. Last) := Message (Message'First .. Message'First + Last - 1);
      Ada.Text_IO.Put (Line);
      if Result then
         Ada.Text_IO.Put_Line (": PASSED");
      else
         Ada.Text_IO.Put_Line (": FAILED");
      end if;
   end Output;
end;


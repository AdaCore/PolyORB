with Ada.Text_IO;

package body Report is

   Max : constant Natural := 60;

   ------------
   -- Output --
   ------------

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
      if Result then
         Ada.Text_IO.Put_Line (Line & ": PASSED");
      else
         Ada.Text_IO.Put_Line (Line & ": FAILED");
      end if;
   end Output;

   ----------------
   -- End_Report --
   ----------------

   procedure End_Report is
   begin
      Output ("END TESTS", True);
   end End_Report;

end Report;


--
--  remote.adb,v 1.2 1996/04/10 15:42:46 tardieu Exp
--

with Ada.Text_IO; use Ada.Text_IO;

package body Remote is

   function Revert (S : String) return String is
      Result : String (S'Range);
   begin
      Put_Line ("In revert: " & S);
      for I in S'Range loop
         Result (I) := S (S'Last + S'First - I);
      end loop;
      Put_Line ("End of revert: " & Result);
      return Result;
   end Revert;

end Remote;

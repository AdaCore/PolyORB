--
--  remote.adb,v 1.2 1996/04/10 15:42:58 tardieu Exp
--

with Ada.Text_IO; use Ada.Text_IO;

package body Remote is

   function Double (X : Natural) return Natural
   is
   begin
      Put_Line ("Oh oh, looks like there is a Text_IO output here..."); Flush;
      return 2 * X;
   end Double;

end Remote;

with Ada.Text_IO;
with System.RPC;
with RCI_3;

package body RCI_2 is

   function F (S : String) return String is
      Failures : Natural := 0;
   begin
      while Failures < 20 loop
         begin
            return "(via RCI_2.F) " & RCI_3.F (S);
         exception when System.RPC.Communication_Error =>
            Ada.Text_IO.Put_Line ("Fail to execute RPC RCI_3.F");
            delay 1.0;
         end;
         Failures := Failures + 1;
      end loop;
      raise Program_Error;
   end F;

end RCI_2;

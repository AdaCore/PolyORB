with SPIF_IO;
use  SPIF_IO;
package body SPIF_RCI is

   procedure Synchronous (M : String; S : out Boolean;) is
   begin
      SPIF_IO.Put_Line ("From server (S) > " & M);
      S := True;
   exception when others =>
      S := False;
   end Synchronous;

   procedure Asynchronous is 
   begin
      SPIF_IO.Put_Line ("From server (A) > " & M);
   end Asynchronous;

end SPIF_RCI;

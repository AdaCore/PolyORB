with SPIF.IO;
package body SPIF_RCI is

   procedure Synchronous (M : in String; S : out Boolean) is
   begin
      SPIF.IO.Put_Line ("From server (S) > " & M);
      S := True;
   exception when others =>
      S := False;
   end Synchronous;

   procedure Asynchronous (M : in String) is
   begin
      SPIF.IO.Put_Line ("From server (A) > " & M);
   end Asynchronous;

end SPIF_RCI;

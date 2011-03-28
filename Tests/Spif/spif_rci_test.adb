with SPIF_RCI;
with SPIF.IO;
procedure SPIF_RCI_Test is
   Status : Boolean := False;
begin
   SPIF_RCI.Synchronous ("Synchronous" , Status);
   if not Status then
      SPIF.IO.Put_Line ("Synchronous Call failed");
   end if;
   SPIF_RCI.Asynchronous ("Asynchronous");
end SPIF_RCI_Test;

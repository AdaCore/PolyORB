with SPIF_RCI;
procedure SPIF_RCI_Test is
   Status : Boolean := False;
begin
   SPIF_RCI.Synchronous ("Synchronous" , Status);
   if not Status then
      SPIF_IO.Put_Line ("Synchronous RPC failed");
   end if;
   SPIF_RCI.Asynchronous ("Asynchronous");
end SPIF_RCI_Test;

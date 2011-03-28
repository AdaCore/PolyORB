with SP1, SP2, SP3;
with RCI;
with Types; use Types;
procedure Main is
begin
   Set (SP1.X_P1, "Set by P1");
   Set (SP2.X_P1, "Set by P1");
   Set (SP3.X_P1, "Set by P1");
   RCI.P;
end Main;

with SP1, SP2, SP3;
with Types; use Types;
package body RCI is
   procedure P is
   begin
      Set (SP1.X_P2, "Set by P2");
      Set (SP2.X_P2, "Set by P2");
      Set (SP3.X_P2, "Set by P2");
   end P;
end RCI;

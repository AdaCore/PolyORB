with Ada.Interrupts; use Ada.Interrupts;
with Ada.Text_Io;

package body Handler is

   protected body Handle_Kill is

      entry Wait when Occured is
      begin
         Occured := False;
      end Wait;

      procedure Response is
      begin
         Ada.Text_Io.Flush;
         Occured := True;
      end Response;
   end Handle_Kill;

end Handler;

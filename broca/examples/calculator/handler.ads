with Ada.Interrupts;
with Ada.Interrupts.Names;

package Handler is

   protected Handle_Kill is
      entry Wait;
      pragma Unreserve_All_Interrupts;
      procedure Response;
      pragma Interrupt_Handler (Response);
   private
      Occured : Boolean := False;
   end Handle_Kill;

end Handler;

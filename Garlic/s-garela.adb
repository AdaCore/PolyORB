with System.Garlic.Options; use System.Garlic.Options;
package body System.Garlic.Elaboration is
begin
   Set_Boot_Server ("tcp");
   Set_Connection_Hits (128);
   Set_Detach   (False);
   Set_Is_Slave (False);
   Set_Nolaunch (False);
end System.Garlic.Elaboration;




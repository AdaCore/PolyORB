package Remote is

   pragma Remote_Call_Interface;
   pragma All_Calls_Remote;

   function Call_OK (P : Natural) return Boolean;

end Remote;

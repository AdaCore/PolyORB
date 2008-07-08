package Server is 
   pragma Remote_Call_Interface;
   function Echo_String (S : String) return String;
end Server;  

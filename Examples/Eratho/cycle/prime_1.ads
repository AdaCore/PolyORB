package Prime_1 is
   pragma Remote_Call_Interface;

   procedure Test_Number
     (Number  : in  Natural);
   pragma Asynchronous (Test_Number);

end Prime_1;

package Alarm is

   pragma Remote_Call_Interface;

   procedure Write
     (Divider : in Natural;
      Where   : in Natural);
   pragma Asynchronous (Write);

   procedure Read
     (Divider : out Natural;
      Where   : out Natural);

end Alarm;
 

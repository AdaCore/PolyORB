package SPIF_RCI is

   pragma Remote_Call_Interface;

   procedure Synchronous (M : in String; S : out Boolean);

   procedure Asynchronous (M : in String);
   pragma Asynchronous (Asynchronous);

end SPIF_RCI;

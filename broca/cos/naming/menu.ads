package Menu is

   type String_Access is access String;

   function Argument
     (Index : Natural)
     return String_Access;

   function Count
     (Prompt : String := "> ")
     return Natural;

   procedure To_Lower (S : String_Access);

end Menu;

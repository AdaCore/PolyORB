package MOMA.Messages.MTexts is

   -------------------
   --  MText Object --
   -------------------
   type MText is new Message with null record;

   ---------------
   --  Get_Text --
   ---------------
   function Get_Text return String;

   ---------------
   --  Set_Text --
   ---------------
   procedure Set_Text (Value : String);

end MOMA.Messages.MTexts;

package body MOMA.Messages.MTexts is

   --------------
   -- Get_Text --
   --------------

   function Get_Text return String is
   begin
      pragma Warnings (Off);
      return Get_Text;
      pragma Warnings (On);
   end Get_Text;

   --------------
   -- Set_Text --
   --------------

   procedure Set_Text (Value : String) is
   begin
      pragma Warnings (Off);
      pragma Unreferenced (Value);
      pragma Warnings (On);
      null;
   end Set_Text;

end MOMA.Messages.MTexts;


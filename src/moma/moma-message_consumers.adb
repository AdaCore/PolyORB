package body MOMA.Message_Consumers is

   ------------
   --  Close --
   ------------
   procedure Close is
   begin
      null;
   end Close;

   ---------------------------
   --  Get_Message_Selector --
   ---------------------------
   function Get_Message_Selector return String is
   begin
      pragma Warnings (Off);
      return Get_Message_Selector;
      pragma Warnings (On);
   end Get_Message_Selector;

end MOMA.Message_Consumers;


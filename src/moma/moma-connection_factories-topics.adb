package body MOMA.Connection_Factories.Topics is

   ------------
   -- Create --
   ------------

   function Create return Connections.Topics.Topic is
   begin
      pragma Warnings (Off);
      return Create;
      pragma Warnings (On);
   end Create;

   ------------
   -- Create --
   ------------

   function Create
     (Username : String;
      Password : String)
      return Connections.Topics.Topic
   is
   begin
      pragma Warnings (Off);
      return Create (Username, Password);
      pragma Warnings (On);
   end Create;

end MOMA.Connection_Factories.Topics;


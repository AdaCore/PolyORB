package body MOMA.Message_Producers is

   -----------
   -- Close --
   -----------

   procedure Close is
   begin
      null;
   end Close;

   --------------------
   -- Get_Persistent --
   --------------------

   function Get_Persistent return Boolean is
   begin
      pragma Warnings (Off);
      return Get_Persistent;
      pragma Warnings (On);
   end Get_Persistent;

   ------------------
   -- Get_Priority --
   ------------------

   function Get_Priority return Priority is
   begin
      pragma Warnings (Off);
      return Get_Priority;
      pragma Warnings (On);
   end Get_Priority;

   ----------------------
   -- Get_Time_To_Live --
   ----------------------

   function Get_Time_To_Live return Time is
   begin
      pragma Warnings (Off);
      return Get_Time_To_Live;
      pragma Warnings (On);
   end Get_Time_To_Live;

   --------------------
   -- Set_Persistent --
   --------------------

   procedure Set_Persistent (Persistent : Boolean) is
   begin
      null;
   end Set_Persistent;

   ------------------
   -- Set_Priority --
   ------------------

   procedure Set_Priority (Value : Priority) is
   begin
      null;
   end Set_Priority;

   ----------------------
   -- Set_Time_To_Live --
   ----------------------

   procedure Set_Time_To_Live (TTL : Time) is
   begin
      null;
   end Set_Time_To_Live;

end MOMA.Message_Producers;

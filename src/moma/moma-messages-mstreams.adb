package body MOMA.Messages.MStreams is

   ------------------
   -- Read_Boolean --
   ------------------

   function Read_Boolean return Boolean is
   begin
      raise PolyORB.Not_Implemented;
      pragma Warnings (Off);
      return Read_Boolean;
      pragma Warnings (On);
   end Read_Boolean;

   ---------------
   -- Read_Char --
   ---------------

   function Read_Char return Character is
   begin
      raise PolyORB.Not_Implemented;
      pragma Warnings (Off);
      return Read_Char;
      pragma Warnings (On);
   end Read_Char;

   ----------------
   -- Read_Float --
   ----------------

   function Read_Float return Float is
   begin
      raise PolyORB.Not_Implemented;
      pragma Warnings (Off);
      return Read_Float;
      pragma Warnings (On);
   end Read_Float;

   ------------------
   -- Read_Integer --
   ------------------

   function Read_Integer return Integer is
   begin
      raise PolyORB.Not_Implemented;
      pragma Warnings (Off);
      return Read_Integer;
      pragma Warnings (On);
   end Read_Integer;

   -----------------
   -- Read_String --
   -----------------

   function Read_String return String is
   begin
      raise PolyORB.Not_Implemented;
      pragma Warnings (Off);
      return Read_String;
      pragma Warnings (On);
   end Read_String;

   -----------
   -- Reset --
   -----------

   procedure Reset is
   begin
      null;
      --  XXX Not Implemented
   end Reset;

   -----------------
   -- Set_Boolean --
   -----------------

   procedure Set_Boolean (Value : Boolean) is
   begin
      pragma Warnings (Off);
      pragma Unreferenced (Value);
      pragma Warnings (On);
      null;
      --  XXX Not Implemented
   end Set_Boolean;

   --------------
   -- Set_Char --
   --------------

   procedure Set_Char (Value : Character) is
   begin
      pragma Warnings (Off);
      pragma Unreferenced (Value);
      pragma Warnings (On);
      null;
      --  XXX Not Implemented
   end Set_Char;

   ---------------
   -- Set_Float --
   ---------------

   procedure Set_Float (Value : Float) is
   begin
      pragma Warnings (Off);
      pragma Unreferenced (Value);
      pragma Warnings (On);
      null;
      --  XXX Not Implemented
   end Set_Float;

   -----------------
   -- Set_Integer --
   -----------------

   procedure Set_Integer (Value : Integer) is
   begin
      pragma Warnings (Off);
      pragma Unreferenced (Value);
      pragma Warnings (On);
      null;
      --  XXX Not Implemented
   end Set_Integer;

   ----------------
   -- Set_String --
   ----------------

   procedure Set_String (Value : String) is
   begin
      pragma Warnings (Off);
      pragma Unreferenced (Value);
      pragma Warnings (On);
      null;
      --  XXX Not Implemented
   end Set_String;

end MOMA.Messages.MStreams;

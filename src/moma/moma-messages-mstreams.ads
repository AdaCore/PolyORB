--  XXX should we keep it ?

package MOMA.Messages.MStreams is

   ---------------------
   --  MStream Object --
   ---------------------
   type MStream is new Message with null record;

   -------------------
   --  Read_Boolean --
   -------------------
   function Read_Boolean return Boolean;

   ----------------
   --  Read_Char --
   ----------------
   function Read_Char return Character;

   -----------------
   --  Read_Float --
   -----------------
   function Read_Float return Float;

   -------------------
   --  Read_Integer --
   -------------------
   function Read_Integer return Integer;

   ------------------
   --  Read_String --
   ------------------
   function Read_String return String;

   ------------
   --  Reset --
   ------------
   procedure Reset;

   ------------------------
   --  Set_Boolean_Value --
   ------------------------
   procedure Set_Boolean (Value : Boolean);

   ---------------
   --  Set_Char --
   ---------------
   procedure Set_Char (Value : Character);

   ----------------
   --  Set_Float --
   ----------------
   procedure Set_Float (Value : Float);

   ------------------
   --  Set_Integer --
   ------------------
   procedure Set_Integer (Value : Integer);

   -----------------
   --  Set_String --
   -----------------
   procedure Set_String (Value : String);

end MOMA.Messages.MStreams;


package Utils is

   --  This package contains some utilities that will be used throughout
   --  the whole system.

   function Integer_To_String (I : Integer) return String;
   --  Return the image of an integer

   function String_To_Integer (S : String) return Integer;
   --  Return the integer corresponding to a string, or raise
   --  Constraint_Error if we have a malformed integer.

   function Get_Line (Prompt : String := "") return String;
   --  Input a line on standard input, using Prompt as a prompt if not empty.
   --  Will raise Ada.IO_Exceptions.End_Error if control-D is pressed.

end Utils;

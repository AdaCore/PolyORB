with Ada.IO_Exceptions;
with Interfaces.C.Strings; use Interfaces.C, Interfaces.C.Strings;

package body Utils is

   --------------
   -- Get_Line --
   --------------

   function Get_Line (Prompt : String := "") return String is
      function readline (Prompt : chars_ptr := Null_Ptr) return chars_ptr;
      pragma Import (C, readline, "readline");
      pragma Linker_Options ("-lreadline");
      pragma Linker_Options ("-lncurses");
      C_Prompt : chars_ptr;
      Result   : chars_ptr;
   begin
      if Prompt = "" then
         Result := readline;
      else
         C_Prompt := New_String (Prompt);
         Result := readline (C_Prompt);
         Free (C_Prompt);
      end if;
      if Result = Null_Ptr then
         raise Ada.IO_Exceptions.End_Error;
      end if;
      return Value (Result);
   end Get_Line;

   -----------------------
   -- Integer_To_String --
   -----------------------

   function Integer_To_String (I : Integer) return String is
      Image : constant String := Integer'Image (I);
   begin
      if Image (1) = ' ' then
         return Image (2 .. Image'Last);
      else
         return Image;
      end if;
   end Integer_To_String;

   -----------------------
   -- String_To_Integer --
   -----------------------

   function String_To_Integer (S : String) return Integer is
   begin
      return Integer'Value (S);
   end String_To_Integer;

end Utils;

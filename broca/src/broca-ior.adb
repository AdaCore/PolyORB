with Ada.Strings.Unbounded;
with Broca.Exceptions;

package body Broca.Ior is
   --  FIXME: to do: null IOR

   --  Convert a string representing an IOR, as decribed in CORBA V2.2 11.6.6,
   --  into a buffer ready for unmarshalling.
   function Ior_String_To_Buffer (Str : CORBA.String) return Buffer_Access is
      use Ada.Strings.Unbounded;
      Unbounded_Str : constant Unbounded_String := Unbounded_String (Str);
      Res : Buffer_Access;
      Length : Natural;
      El : Character;
      Nibble : CORBA.Octet;
      Res_Index : Buffer_Index_Type;
   begin
      Length := Ada.Strings.Unbounded.Length (Unbounded_Str);

      --  Check the prefix.
      if Length <= 4
        or else Length mod 2 /= 0
        or else Slice (Str, 1, 4) /= "IOR:" then
         Broca.Exceptions.Raise_Bad_Param;
      end if;

      --  The prefix is 4 characters long and there is one octet
      --  for 2 characters.
      Res := new Buffer_Type (0 .. Buffer_Index_Type ((Length - 4) / 2 - 1));
      Res_Index := 0;
      for I in 5 .. Length loop
         El := Element (Str, I);
         if El >= '0' and then El <= '9' then
            Nibble := Character'Pos (El) - Character'Pos ('0');
         elsif El >= 'A' and then El <= 'F' then
            Nibble := 10 + Character'Pos (El) - Character'Pos ('A');
         elsif El >= 'a' and then El <= 'f' then
            Nibble := 10 + Character'Pos (El) - Character'Pos ('a');
         else
            Unchecked_Deallocation (Res);
            Broca.Exceptions.Raise_Bad_Param;
         end if;
         if I mod 2 = 1 then
            Res (Res_Index) := Nibble * 16;
         else
            Res (Res_Index) := Res (Res_Index) + Nibble;
            Res_Index := Res_Index + 1;
         end if;
      end loop;
      return Res;
   end Ior_String_To_Buffer;

   type Char_Array is array (CORBA.Octet range <>) of Character;
   Xdigits : constant Char_Array (0 .. 15) := "0123456789abcdef";

   --  Convert a buffer containing an marshalled contents of an IOR into
   --  a string.
   function Buffer_To_Ior_String (Buffer : Buffer_Descriptor)
                                  return CORBA.String is
      Target : CORBA.String;
      Len : Natural;
   begin
      Len := Natural (Buffer.Pos);
      Target := To_Unbounded_String (4 + 2 * Len);

      Replace_Element (Target, 1, 'I');
      Replace_Element (Target, 2, 'O');
      Replace_Element (Target, 3, 'R');
      Replace_Element (Target, 4, ':');
      for I in 0 .. Len - 1 loop
         Replace_Element
           (Target, 5 + 2 * I,
            Xdigits (Buffer.Buffer (Buffer_Index_Type (I)) / 16));
         Replace_Element
           (Target, 5 + 2 * I + 1,
            Xdigits (Buffer.Buffer (Buffer_Index_Type (I)) mod 16));
      end loop;
      return Target;
   end Buffer_To_Ior_String;

end Broca.Ior;

with Broca.Exceptions;

package body Broca.IOR is

   --  FIXME: to do: null IOR

   type Char_Array is array (Byte range <>) of Character;
   Xdigits : constant Char_Array (0 .. 15) := "0123456789abcdef";

   --  Convert a string representing an IOR, as decribed in CORBA V2.2
   --  11.6.6, into a buffer ready for unmarshalling.

   function IOR_String_To_Buffer
     (IOR : CORBA.String)
     return Buffer_Descriptor
   is
      S      : constant String := To_Standard_String (IOR);
      Buffer : Buffer_Descriptor;
      Length : Natural := S'Length;
   begin
      --  Check the prefix.
      if Length <= 4
        or else Length mod 2 /= 0
        or else S (S'First .. S'First + 3) /= "IOR:" then
         Broca.Exceptions.Raise_Bad_Param;
      end if;

      --  Prefix is 4 characters long, then 1 octet for 2 characters.
      declare
         Index : Buffer_Index_Type := 0;
         Bytes : Buffer_Type (0 .. Buffer_Index_Type ((Length - 4) / 2 - 1));
         O     : Byte;
      begin
         for I in S'First + 4 .. S'Last loop
            if S (I) >= '0' and then S (I) <= '9' then
               O := Character'Pos (S (I)) - Character'Pos ('0');
            elsif S (I) >= 'A' and then S (I) <= 'F' then
               O := 10 + Character'Pos (S (I)) - Character'Pos ('A');
            elsif S (I) >= 'a' and then S (I) <= 'f' then
               O := 10 + Character'Pos (S (I)) - Character'Pos ('a');
            else
               Broca.Exceptions.Raise_Bad_Param;
            end if;
            if I mod 2 = 1 then
               Bytes (Index) := O * 16;
            else
               Bytes (Index) := Bytes (Index) + O;
               Index := Index + 1;
            end if;
         end loop;
         Allocate_Buffer_And_Clear_Pos (Buffer, Bytes'Last + 1);
         Write (Buffer, Bytes);
      end;
      return Buffer;
   end IOR_String_To_Buffer;

   --------------------------
   -- Buffer_To_IOR_String --
   --------------------------

   function Buffer_To_IOR_String
     (Buffer : Buffer_Descriptor)
     return CORBA.String
   is
      Length : Buffer_Index_Type := Size (Buffer);
      Bytes  : Buffer_Type (0 .. Length - 1);
      IOR    : String (1 .. 4 + Natural (Length) * 2);
      Local  : Buffer_Descriptor := Buffer;
   begin
      Read (Local, Bytes);
      IOR (1 .. 4) := "IOR:";

      for I in Bytes'Range loop
         IOR (5 + 2 * Natural (I)) := Xdigits (Bytes (I) / 16);
         IOR (6 + 2 * Natural (I)) := Xdigits (Bytes (I) mod 16);
      end loop;
      return To_CORBA_String (IOR);
   end Buffer_To_IOR_String;

end Broca.IOR;

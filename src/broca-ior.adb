------------------------------------------------------------------------------
--                                                                          --
--                          ADABROKER COMPONENTS                            --
--                                                                          --
--                            B R O C A . I O R                             --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.11 $
--                                                                          --
--         Copyright (C) 1999, 2000 ENST Paris University, France.          --
--                                                                          --
-- AdaBroker is free software; you  can  redistribute  it and/or modify it  --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. AdaBroker  is distributed  in the hope that it will be  useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with AdaBroker; see file COPYING. If  --
-- not, write to the Free Software Foundation, 59 Temple Place - Suite 330, --
-- Boston, MA 02111-1307, USA.                                              --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
--             AdaBroker is maintained by ENST Paris University.            --
--                     (email: broker@inf.enst.fr)                          --
--                                                                          --
------------------------------------------------------------------------------

with Broca.Exceptions;

with Interfaces; use Interfaces;
--  For operators on Unsigned_8.

with Broca.Buffers;
--  For Dump.

package body Broca.IOR is

   use Broca.Opaque;

   --  FIXME: TODO: null IOR

   type Char_Array is array (Broca.Opaque.Octet range <>)
     of Character;
   Xdigits : constant Char_Array (0 .. 15)
     := "0123456789abcdef";

   --  Convert a string representing an IOR, as decribed in CORBA V2.2
   --  11.6.6, into a buffer ready for unmarshalling.

   --------------------------
   -- IOR_String_To_Octets --
   --------------------------

   function IOR_String_To_Octets
     (IOR : CORBA.String)
     return Broca.Opaque.Octet_Array
   is
      S      : constant String := To_Standard_String (IOR);
      Length : Natural := S'Length;
      Bytes  : Octet_Array
        (1 .. Index_Type ((Length - 4) / 2));
   begin

      --  Check the prefix.
      if Length <= 4
        or else Length mod 2 /= 0
        or else S (S'First .. S'First + 3) /= "IOR:" then
         Broca.Exceptions.Raise_Bad_Param;
      end if;

      --  Prefix is 4 characters long, then 1 octet for 2 characters.
      declare
         Index : Index_Type := Bytes'First;
         O     : Broca.Opaque.Octet;
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
      end;
      return Bytes;
   end IOR_String_To_Octets;

   --------------------------
   -- Buffer_To_IOR_String --
   --------------------------

   function Buffer_To_IOR_String
      (Buffer : access Buffer_Type)
       return CORBA.String
   is
      Marshalled_IOR  : constant Broca.Opaque.Octet_Array
        := Encapsulate (Buffer);

      Bytes : constant Broca.Opaque.Octet_Array
        := Marshalled_IOR (Marshalled_IOR'First + 4 ..
                           Marshalled_IOR'Last);
      --  FIXME: This is kludgey and inelegant.
      --    Buffer containts a marshalled IOR,
      --    i. e. a four-octet length indication
      --    followed by an encapsulation. We only want
      --    the encapsulation, so we strip the first
      --    four bytes. Alternatively we could write:
      --    IOR_Buffer_For_Reading : Buffer_Type;
      --    Decapsulate (Marshalled_IOR, IOR_Buffer_For_Reading)
      --    Bytes : Encapsulation := Unmarshall (IOR_Buffer...).

      Length : constant Index_Type := Bytes'Length;
      IOR    : String (1 .. 4 + Natural (Length) * 2);
   begin
      IOR (1 .. 4) := "IOR:";

      for I in Bytes'Range loop
         IOR (5 + 2 * Natural (I - Bytes'First))
           := Xdigits (Bytes (I) / 16);
         IOR (6 + 2 * Natural (I - Bytes'First))
           := Xdigits (Bytes (I) mod 16);
      end loop;
      return To_CORBA_String (IOR);
   end Buffer_To_IOR_String;

end Broca.IOR;

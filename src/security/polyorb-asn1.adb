------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                         P O L Y O R B . A S N 1                          --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2005-2007, Free Software Foundation, Inc.          --
--                                                                          --
-- PolyORB is free software; you  can  redistribute  it and/or modify it    --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. PolyORB is distributed  in the hope that it will be  useful,    --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with PolyORB; see file COPYING. If    --
-- not, write to the Free Software Foundation, 51 Franklin Street, Fifth    --
-- Floor, Boston, MA 02111-1301, USA.                                       --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
--                  PolyORB is maintained by AdaCore                        --
--                     (email: sales@adacore.com)                           --
--                                                                          --
------------------------------------------------------------------------------

with Interfaces.C.Pointers;
with PolyORB.Platform.SSL_Linker_Options;
pragma Warnings (Off, PolyORB.Platform.SSL_Linker_Options);
--  No entity referenced

package body PolyORB.ASN1 is

   package Stream_Element_Pointers is
     new Interfaces.C.Pointers
     (Ada.Streams.Stream_Element_Offset,
      Ada.Streams.Stream_Element,
      Ada.Streams.Stream_Element_Array,
      0);

   OID : constant String := "oid:";

   ------------
   -- Decode --
   ------------

   function Decode
     (Item : Ada.Streams.Stream_Element_Array)
      return Object_Identifier
   is

      function d2i_ASN1_OBJECT
        (Buffer : Ada.Streams.Stream_Element_Array;
         Length : Interfaces.C.int)
         return Object_Identifier;
      pragma Import (C, d2i_ASN1_OBJECT, "__PolyORB_d2i_ASN1_OBJECT");

      Result : Object_Identifier;

   begin
      Result := d2i_ASN1_OBJECT (Item, Item'Length);

      if Result = null then
         raise ASN1_Error;
      end if;

      return Result;
   end Decode;

   ---------------
   -- Duplicate --
   ---------------

   function Duplicate (Item : Object_Identifier) return Object_Identifier is

      function ASN1_OBJECT_dup
        (Item : Object_Identifier)
         return Object_Identifier;
      pragma Import (C, ASN1_OBJECT_dup, "__PolyORB_ASN1_OBJECT_dup");

   begin
      return ASN1_OBJECT_dup (Item);
   end Duplicate;

   ------------
   -- Encode --
   ------------

   function Encode
     (Item : in Object_Identifier)
      return Ada.Streams.Stream_Element_Array
   is
      use type Interfaces.C.int;
      use type Stream_Element_Pointers.Pointer;

      procedure i2d_ASN1_OBJECT
        (Item   :     Object_Identifier;
         Buffer : out Stream_Element_Pointers.Pointer;
         Length : out Interfaces.C.int);
      pragma Import (C, i2d_ASN1_OBJECT, "__PolyORB_i2d_ASN1_OBJECT");

      procedure OPENSSL_free (Item : Stream_Element_Pointers.Pointer);
      pragma Import (C, OPENSSL_free, "__PolyORB_OPENSSL_free");

      Buffer : Stream_Element_Pointers.Pointer := null;
      Length : Interfaces.C.int;

   begin
      i2d_ASN1_OBJECT (Item, Buffer, Length);

      if Length < 0
        or else Buffer = null
      then
         raise ASN1_Error;
      end if;

      declare
         Result : constant Ada.Streams.Stream_Element_Array
           := Stream_Element_Pointers.Value
           (Buffer, Interfaces.C.ptrdiff_t (Length));

      begin
         OPENSSL_free (Buffer);

         return Result;
      end;
   end Encode;

   ----------
   -- Free --
   ----------

   procedure Free (Item : in out Object_Identifier) is

      procedure ASN1_OBJECT_free (Item : Object_Identifier);
      pragma Import (C, ASN1_OBJECT_free, "ASN1_OBJECT_free");

   begin
      ASN1_OBJECT_free (Item);
      Item := null;
   end Free;

   -------------------
   -- Is_Equivalent --
   -------------------

   function Is_Equivalent
     (OID1 : Object_Identifier;
      OID2 : Object_Identifier)
      return Boolean
   is
      use type Ada.Streams.Stream_Element_Array;

   begin
      return Encode (OID1) = Encode (OID2);
   end Is_Equivalent;

   --------------------------
   -- To_Object_Identifier --
   --------------------------

   function To_Object_Identifier (Item : String) return Object_Identifier is

      function OBJ_txt2obj
        (S       : Interfaces.C.char_array;
         No_Name : Interfaces.C.int)
         return Object_Identifier;
      pragma Import (C, OBJ_txt2obj, "OBJ_txt2obj");

   begin
      if Item'Length < OID'Length
        or else Item (Item'First .. Item'First + OID'Length - 1) /= OID
      then
         raise ASN1_Error;
      end if;

      return
        OBJ_txt2obj
        (Interfaces.C.To_C (Item (Item'First + OID'Length .. Item'Last)),
         1);
   end To_Object_Identifier;

   ---------------
   -- To_String --
   ---------------

   function To_String (Item : Object_Identifier) return String is

      procedure OBJ_obj2txt
        (Buffer  : Interfaces.C.char_array;
         Length  : Interfaces.C.int;
         Item    : Object_Identifier;
         No_Name : Interfaces.C.int);
      pragma Import (C, OBJ_obj2txt, "OBJ_obj2txt");

      Buffer : Interfaces.C.char_array (1 .. 80);
      pragma Warnings (Off, Buffer);
      --  Buffer changed as side effect of OBJ_obj2txt

   begin
      OBJ_obj2txt (Buffer, Buffer'Length, Item, 1);

      return OID & Interfaces.C.To_Ada (Buffer);
   end To_String;

end PolyORB.ASN1;

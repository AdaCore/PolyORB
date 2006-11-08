------------------------------------------------------------------------------
--                                                                          --
--                            GLADE COMPONENTS                              --
--                                                                          --
--                  S Y S T E M . G A R L I C . U T I L S                   --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 1996-2006 Free Software Foundation, Inc.           --
--                                                                          --
-- GARLIC is free software;  you can redistribute it and/or modify it under --
-- terms of the  GNU General Public License  as published by the Free Soft- --
-- ware Foundation;  either version 2,  or (at your option)  any later ver- --
-- sion.  GARLIC is distributed  in the hope that  it will be  useful,  but --
-- WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHANTABI- --
-- LITY or  FITNESS FOR A PARTICULAR PURPOSE.  See the  GNU General Public  --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License  distributed with GARLIC;  see file COPYING.  If  --
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
--               GLADE  is maintained by ACT Europe.                        --
--               (email: glade-report@act-europe.fr)                        --
--                                                                          --
------------------------------------------------------------------------------

with GNAT.Strings;

package System.Garlic.Utils is

   pragma Elaborate_Body;

   Location_Separator : constant Character := ' ';

   function Merge_String
     (S : GNAT.Strings.String_List_Access;
      C : Character := Location_Separator)
     return String;
   --  Concatenate S in a string and separate them with C.

   function Split_String
     (S : String;
      C : Character := Location_Separator)
     return GNAT.Strings.String_List_Access;
   --  Return an array of substrings sperated by C in S.

   function Quote   (S : String; C : Character := '"') return String; --  "
   function Unquote (S : String) return String;
   --  If S is quoted, return the content.

   Null_String : constant String := "";

   function String_To_Access (S : String) return GNAT.Strings.String_Access;
   function Access_To_String (S : GNAT.Strings.String_Access) return String;
   --     pragma Stream_Convert (Entity => String_Access,
   --                            Read   => String_To_Access,
   --                            Write  => Access_To_String);

   --  Stream attributes Access on string and deallocation
   --  procedure. This access type can be transmitted accross
   --  partitions.

   procedure Destroy (S : in out GNAT.Strings.String_Access);
   procedure Destroy (S : in out GNAT.Strings.String_List_Access);

   function Copy (S : GNAT.Strings.String_List_Access)
     return GNAT.Strings.String_List_Access;
   --  Duplicate array and elements

   function Missing
     (Elt : String;
      Set : GNAT.Strings.String_List)
     return Boolean;
   --  Is Elt missing in array Set.

   procedure To_Lower (Item : in out String);
   pragma Inline (To_Lower);
   --  In place transformation of a string with all the upper-case letters
   --  changed into corresponding lower-case ones.

   type Portable_Address is mod 2 ** 64;
   --  This type can contain an object of type System.Address on any platform
   --  where GNAT is supported. It is made public on purpose so that it is
   --  possible to take a 'Image of it.

   function To_Address (Addr : Portable_Address) return Address;
   function To_Portable_Address (Addr : Address) return Portable_Address;
   --  Conversion routines

private

   pragma Inline (To_Address);
   pragma Inline (To_Portable_Address);

end System.Garlic.Utils;

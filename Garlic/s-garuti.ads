------------------------------------------------------------------------------
--                                                                          --
--                            GLADE COMPONENTS                              --
--                                                                          --
--                  S Y S T E M . G A R L I C . U T I L S                   --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                            $Revision$
--                                                                          --
--         Copyright (C) 1996-1999 Free Software Foundation, Inc.           --
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

package System.Garlic.Utils is

   pragma Elaborate_Body;

   type String_Access is access String;
   type String_Array is array (Natural range <>) of String_Access;
   type String_Array_Access is access String_Array;
   Location_Separator : constant Character := ' ';

   function Merge_String
     (S : String_Array_Access;
      C : Character := Location_Separator)
     return String;
   --  Concatenate S in a string and separate them with C.

   function Split_String
     (S : String;
      C : Character := Location_Separator)
     return String_Array_Access;
   --  Return an array of substrings sperated by C in S.

   function Unquote      (S : String) return String;
   --  If S is quoted, return the content.

   function String_To_Access (S : String) return String_Access;
   function Access_To_String (S : String_Access) return String;
   --     pragma Stream_Convert (Entity => String_Access,
   --                            Read   => String_To_Access,
   --                            Write  => Access_To_String);

   --  Stream attributes Access on string and deallocation
   --  procedure. This access type can be transmitted accross
   --  partitions.

   procedure Destroy (S : in out String_Access);
   procedure Destroy (S : in out String_Array_Access);

   function Copy  (S : String_Array_Access) return String_Array_Access;
   --  Duplicate array and elements

   function Missing
     (Elt : String;
      Set : String_Array)
     return Boolean;
   --  Is Elt missing in array Set.

   procedure To_Lower (Item : in out String);
   pragma Inline (To_Lower);
   --  In place transformation of a string with all the upper-case letters
   --  changed into corresponding lower-case ones.

   type Error_Type is limited private;
   function Found (Error : Error_Type) return Boolean;
   procedure Throw (Error : in out Error_Type; Message : in String);
   procedure Catch (Error : in out Error_Type);
   procedure Raise_Communication_Error (Error : in out Error_Type);
   function Content (Error : access Error_Type) return String;
   --  Error type and associated primitives. By default, an Error_Type is
   --  not considered as being an error until Throw has been called.
   --  Catch, Raise_Communication_Error and Content cancel the error.

   type Version_Id is mod 2 ** 8;
   No_Version : constant Version_Id := 0;

   function "<" (L, R : Version_Id) return Boolean;

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

   type Error_Type is access String;

end System.Garlic.Utils;

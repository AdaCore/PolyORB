------------------------------------------------------------------------------
--                                                                          --
--                            GLADE COMPONENTS                              --
--                                                                          --
--             S Y S T E M . G A R L I C . N A M E _ T A B L E              --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                            $Revision$                             --
--                                                                          --
--         Copyright (C) 1996-1998 Free Software Foundation, Inc.           --
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

with Ada.Streams;

package System.Garlic.Name_Table is

   pragma Elaborate_Body;

   --  Name table used by Garlic. A value of type Name_Id can be safely
   --  exchanged between partitions. When a new Name_Id is read from a
   --  stream, its new information will be set to Empty_Info.

   type Name_Id is private;
   Null_Name  : constant Name_Id;
   First_Name : constant Name_Id;

   function  Get (S : String)  return Name_Id;
   --  Save this string in name table and return its name id. Create a new
   --  entry if this string is not already in name table Otherwise, return
   --  the old entry.

   function  Get (N : Name_Id) return String;
   --  Return the string corresponding to N

   function  Get_Info (N : Name_Id) return Integer;
   procedure Set_Info (N : Name_Id; I : Integer);
   --  To each entry in the name table corresponds a field Info. The
   --  previous procedures allow to get and set this field.

   Empty_Info : constant Integer;
   --  Empty info slot

   function To_Natural (N : Name_Id) return Natural;
   function To_Name_Id (N : Natural) return Name_Id;
   --  Conversion routines from Name_Id to Natural and vice-versa

private

   type Name_Id is new Natural;
   procedure Write (S : access Ada.Streams.Root_Stream_Type'Class;
                    N : in Name_Id);
   for Name_Id'Write use Write;
   procedure Read (S : access Ada.Streams.Root_Stream_Type'Class;
                   N : out Name_Id);
   for Name_Id'Read use Read;

   Null_Name  : constant Name_Id := 0;
   First_Name : constant Name_Id := 1_000_000;

   Empty_Info : constant Integer := 0;

   pragma Inline (To_Natural);
   pragma Inline (To_Name_Id);

end System.Garlic.Name_Table;

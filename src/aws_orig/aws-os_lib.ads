------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                           A W S . O S _ L I B                            --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2000-2006, Free Software Foundation, Inc.          --
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

--  @@@ uses ada.calendar

with Ada.Calendar;
with Ada.Streams;

package AWS.OS_Lib is

   No_Such_File : exception;

   function Is_Regular_File (Filename : String) return Boolean;
   pragma Inline (Is_Regular_File);
   --  Returns True if Filename is a regular file and is readable.

   function Is_Directory (Filename : String) return Boolean;
   pragma Inline (Is_Directory);
   --  Returns True if Filename is a directory.

   function File_Size (Filename : String)
      return Ada.Streams.Stream_Element_Offset;
   pragma Inline (File_Size);
   --  Returns Filename's size in bytes.

   function File_Timestamp (Filename : String) return Ada.Calendar.Time;
   pragma Inline (File_Timestamp);
   --  Get the time for last modification to a file in UTC/GMT.

   function GMT_Clock return Ada.Calendar.Time;
   pragma Inline (GMT_Clock);
   --  Returns current UTC/GMT time.

end AWS.OS_Lib;

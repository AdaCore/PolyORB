------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                           A W S . O S _ L I B                            --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2000-2012, Free Software Foundation, Inc.          --
--                                                                          --
-- This is free software;  you can redistribute it  and/or modify it  under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for  more details.                                               --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
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

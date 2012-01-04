------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                        A W S . R E S O U R C E S                         --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2002-2012, Free Software Foundation, Inc.          --
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

package AWS.Resources is

   use Ada.Streams;

   Resource_Error : exception;

   type File_Type is limited private;

   procedure Open
     (File :    out File_Type;
      Name : String;
      Form : String    := "");
   --  Open file in mode In_File. Only reading from the file is supported.
   --  This procedure open the in-memory file if present, otherwise the file
   --  on disk is opened.

   procedure Close (Resource : in out File_Type);
   --  Close the file.

   procedure Read
     (Resource : in out File_Type;
      Buffer   :    out Stream_Element_Array;
      Last     :    out Stream_Element_Offset);
   --  Returns a set of bytes from the file.

   procedure Get_Line
     (Resource  : in out File_Type;
      Buffer    :    out String;
      Last      :    out Natural);
   --  Returns a line from the file. A line is a set of character terminated
   --  by ASCII.LF (UNIX style EOF) or ASCII.CR+ASCII.LF (DOS style EOF).

   function End_Of_File (Resource : File_Type) return Boolean;
   --  Returns true if there is no more data to read.

   function LF_Terminated (Resource : File_Type) return Boolean;
   --  Returns True if last line returned by Get_Line was terminated with a LF
   --  or CR+LF on DOS based systems.

   function Is_Regular_File
     (Name : String)
      return Boolean;
   --  Returns True if Filename is a regular file and is readable. Checks first
   --  for in memory file then for disk file.

   function File_Size
     (Name : String)
      return Ada.Streams.Stream_Element_Offset;
   --  Returns Filename's size in bytes. Checks first for in memory file
   --  then for disk file.

   function File_Timestamp
     (Name : String)
      return Ada.Calendar.Time;
   --  Get the time for last modification to a file in UTC/GMT. Checks first
   --  for in memory file then for disk file.

private

   type File_Tagged is abstract tagged limited record
      LFT : Boolean; -- LF terminated state
   end record;

   --  Abstract file, operations below must be implemented. The goal here is
   --  to abstract the file location. Currently there is two implementations,
   --  one for files on a hard disk and files in memory (array of bytes).

   type File_Type is access all File_Tagged'Class;

   function End_Of_File
     (Resource : File_Tagged)
      return Boolean
      is abstract;

   procedure Read
     (Resource : in out File_Tagged;
      Buffer   :    out Stream_Element_Array;
      Last     :    out Stream_Element_Offset)
      is abstract;

   procedure Close (File : in out File_Tagged)
      is abstract;

end AWS.Resources;

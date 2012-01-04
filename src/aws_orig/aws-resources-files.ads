------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                  A W S . R E S O U R C E S . F I L E S                   --
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

with Ada.Streams.Stream_IO;

package AWS.Resources.Files is

   procedure Open
     (File :    out File_Type;
      Name : String;
      Form : String    := "");

   function Is_Regular_File (Name : String) return Boolean;

   function File_Size
     (Name : String)
      return Ada.Streams.Stream_Element_Offset;

   function File_Timestamp (Name : String) return Ada.Calendar.Time;

private

   type Stream_File_Access is access Stream_IO.File_Type;

   Buffer_Size : constant := 8_192;

   type File_Tagged is new Resources.File_Tagged with record
      File    : Stream_IO.File_Type;
      Stream  : Stream_IO.Stream_Access;
      --  below are data for buffered access to the file.
      Buffer  : Stream_Element_Array (1 .. Buffer_Size);
      Current : Stream_Element_Offset := 1;
      Last    : Stream_Element_Offset := 0;
   end record;

   function End_Of_File (Resource : File_Tagged) return Boolean;

   procedure Read
     (Resource : in out File_Tagged;
      Buffer   :    out Stream_Element_Array;
      Last     :    out Stream_Element_Offset);

   procedure Close (Resource : in out File_Tagged);

end AWS.Resources.Files;

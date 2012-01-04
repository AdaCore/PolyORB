------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--               A W S . R E S O U R C E S . E M B E D D E D                --
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

with Ada.Streams;

package AWS.Resources.Embedded is

   use Ada;

   type Buffer_Access is access constant Streams.Stream_Element_Array;

   procedure Open
     (File :    out File_Type;
      Name : String;
      Form : String    := "");
   --  Open resource from registered data.

   procedure Create
     (File   :    out File_Type;
      Buffer : Buffer_Access);
   --  Create the resource directly from memory data.

   function Is_Regular_File (Name : String) return Boolean;

   function File_Size
     (Name : String)
      return Ada.Streams.Stream_Element_Offset;

   function File_Timestamp (Name : String) return Ada.Calendar.Time;

   procedure Register
     (Name      : String;
      Content   : Buffer_Access;
      File_Time : Calendar.Time);
   --  Register a new file named Named into the embedded resources. The file
   --  content is pointed to by Content, the File_Time must be that last
   --  modification time stamp for the file.

   function Exists (Name : String) return Boolean;
   pragma Inline (Exists);
   --  Returns True if file named Name has been registered (i.e. it is an
   --  in-memory file).

private

   type File_Tagged is new Resources.File_Tagged with record
      Buffer : Buffer_Access;
      K      : Streams.Stream_Element_Offset;
   end record;

   function End_Of_File (Resource : File_Tagged) return Boolean;

   procedure Read
     (Resource : in out File_Tagged;
      Buffer   :    out Stream_Element_Array;
      Last     :    out Stream_Element_Offset);

   procedure Close (Resource : in out File_Tagged);

end AWS.Resources.Embedded;

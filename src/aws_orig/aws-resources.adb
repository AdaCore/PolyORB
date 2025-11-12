------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                        A W S . R E S O U R C E S                         --
--                                                                          --
--                                 B o d y                                  --
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

with PolyORB.Utils.Unchecked_Deallocation;

with AWS.Resources.Files;
with AWS.Resources.Embedded;

package body AWS.Resources is

   use Ada;

   procedure Free is
      new PolyORB.Utils.Unchecked_Deallocation.Free


     (Object => Resources.File_Tagged'Class,


      Name   => File_Type);

   -----------
   -- Close --
   -----------

   procedure Close (Resource : in out File_Type) is
   begin
      Close (Resource.all);
      Free (Resource);
   end Close;

   -----------------
   -- End_Of_File --
   -----------------

   function End_Of_File (Resource : File_Type) return Boolean is
   begin
      return End_Of_File (Resource.all);
   end End_Of_File;

   ---------------
   -- File_Size --
   ---------------

   function File_Size
     (Name : String)
      return Ada.Streams.Stream_Element_Offset is
   begin
      if Resources.Embedded.Exists (Name) then
         return Resources.Embedded.File_Size (Name);
      else
         return Resources.Files.File_Size (Name);
      end if;
   end File_Size;

   --------------------
   -- File_Timestamp --
   --------------------

   function File_Timestamp
     (Name : String)
      return Ada.Calendar.Time is
   begin
      if Resources.Embedded.Exists (Name) then
         return Resources.Embedded.File_Timestamp (Name);
      else
         return Resources.Files.File_Timestamp (Name);
      end if;
   end File_Timestamp;

   --------------
   -- Get_Line --
   --------------

   procedure Get_Line
     (Resource : in out File_Type;
      Buffer   :    out String;
      Last     :    out Natural)
   is
      Byte     : Stream_Element_Array (1 .. 1);
      Last_Ind : Stream_Element_Offset;
   begin
      Last         := 0;
      Resource.LFT := False;

      for I in Buffer'Range loop

         Read (Resource.all, Byte, Last_Ind);

         exit when Last_Ind < Byte'Last;

         Buffer (I) := Character'Val (Byte (1));

         --  Check for end of line

         if Buffer (I) = ASCII.LF then
            --  This is LF
            if I > Buffer'First
              and then Buffer (I - 1) = ASCII.CR
            then
               --  And previous char was a CR, skip it
               Last := Last - 1;
            end if;

            Resource.LFT := True;
            exit;
         end if;

         Last := Last + 1;
      end loop;
   end Get_Line;

   ---------------------
   -- Is_Regular_File --
   ---------------------

   function Is_Regular_File
     (Name : String)
      return Boolean is
   begin
      if Resources.Embedded.Exists (Name) then
         return Resources.Embedded.Is_Regular_File (Name);
      else
         return Resources.Files.Is_Regular_File (Name);
      end if;
   end Is_Regular_File;

   -------------------
   -- LF_Terminated --
   -------------------

   function LF_Terminated (Resource : File_Type) return Boolean is
   begin
      return Resource.all.LFT;
   end LF_Terminated;

   ----------
   -- Open --
   ----------

   procedure Open
     (File :    out File_Type;
      Name : String;
      Form : String    := "") is
   begin
      --  Try to open the file in memory, if not found open the file on disk.
      Resources.Embedded.Open (File, Name, Form);

      if File = null then
         Resources.Files.Open (File, Name, Form);
      end if;
   end Open;

   ----------
   -- Read --
   ----------

   procedure Read
     (Resource : in out File_Type;
      Buffer   :    out Stream_Element_Array;
      Last     :    out Stream_Element_Offset) is
   begin
      Read (Resource.all, Buffer, Last);
   end Read;

end AWS.Resources;

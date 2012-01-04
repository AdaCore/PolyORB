------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                  A W S . R E S O U R C E S . F I L E S                   --
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

with AWS.OS_Lib;

package body AWS.Resources.Files is

   -----------
   -- Close --
   -----------

   procedure Close (Resource : in out File_Tagged) is
   begin
      Stream_IO.Close (Resource.File);
   end Close;

   -----------------
   -- End_Of_File --
   -----------------

   function End_Of_File (Resource : File_Tagged) return Boolean is
   begin
      return Resource.Current > Resource.Last
        and then Stream_IO.End_Of_File (Resource.File);
   end End_Of_File;

   ---------------
   -- File_Size --
   ---------------

   function File_Size
     (Name : String)
      return Ada.Streams.Stream_Element_Offset is
   begin
      return OS_Lib.File_Size (Name);
   exception
      when others =>
         raise Resource_Error;
   end File_Size;

   --------------------
   -- File_Timestamp --
   --------------------

   function File_Timestamp (Name : String) return Ada.Calendar.Time is
   begin
      return OS_Lib.File_Timestamp (Name);
   exception
      when others =>
         raise Resource_Error;
   end File_Timestamp;

   ---------------------
   -- Is_Regular_File --
   ---------------------

   function Is_Regular_File (Name : String) return Boolean is
   begin
      return OS_Lib.Is_Regular_File (Name);
   exception
      when others =>
         raise Resource_Error;
   end Is_Regular_File;

   ----------
   -- Open --
   ----------

   procedure Open
     (File :    out File_Type;
      Name : String;
      Form : String    := "") is
   begin
      File := new File_Tagged;

      Stream_IO.Open
        (File_Tagged (File.all).File,
         Stream_IO.In_File, Name, Form);

      File_Tagged (File.all).Stream :=
        Stream_IO.Stream (File_Tagged (File.all).File);
   end Open;

   ----------
   -- Read --
   ----------

   procedure Read
     (Resource : in out File_Tagged;
      Buffer   :    out Stream_Element_Array;
      Last     :    out Stream_Element_Offset)
   is
      use type Stream_Element_Offset;

      Buf_Len : constant Stream_Element_Offset
        := Resource.Last - Resource.Current + 1;
   begin
      if Buffer'Length <= Natural (Buf_Len) then
         --  Enough chars in the buffer, return them
         Buffer := Resource.Buffer
           (Resource.Current .. Resource.Current + Buffer'Length - 1);
         Resource.Current := Resource.Current + Buffer'Length;
         Last := Buffer'Last;

      else
         --  Return the current buffer

         Buffer
           (Buffer'First .. Buffer'First + Buf_Len - 1)
           := Resource.Buffer (Resource.Current .. Resource.Last);

         --  And read the remaining data directly on the file

         Read (Resource.Stream.all,
               Buffer (Buffer'First + Buf_Len .. Buffer'Last),
               Last);

         Resource.Current := Resource.Buffer'First;

         if Last < Buffer'Last then
            --  There is no more data, set the Resource object
            Resource.Last := 0;

         else
            --  Fill Resource buffer
            Read (Resource.Stream.all, Resource.Buffer, Resource.Last);
         end if;
      end if;
   end Read;

end AWS.Resources.Files;

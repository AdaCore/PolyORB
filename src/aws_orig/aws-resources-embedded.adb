------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--               A W S . R E S O U R C E S . E M B E D D E D                --
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

with Table_Of_Strings_And_Static_Values_G;
pragma Elaborate_All (Table_Of_Strings_And_Static_Values_G);

package body AWS.Resources.Embedded is

   type Node is record
      File_Buffer : Buffer_Access;
      File_Time   : Calendar.Time;
   end record;

   package Res_Files is new Table_Of_Strings_And_Static_Values_G
     (Character, String, "<", "=", Node);

   Files_Table : Res_Files.Table_Type;

   Empty_Buffer : aliased constant Ada.Streams.Stream_Element_Array
     := (1 .. 0 => 0);

   -----------
   -- Close --
   -----------

   procedure Close (Resource : in out File_Tagged) is
      pragma Unreferenced (Resource);
   begin
      null;
   end Close;

   ------------
   -- Create --
   ------------

   procedure Create
     (File   :    out File_Type;
      Buffer : Buffer_Access) is
   begin
      File := new File_Tagged;

      if Buffer = null then
         File_Tagged (File.all).Buffer := Empty_Buffer'Access;
      else
         File_Tagged (File.all).Buffer := Buffer;
      end if;

      File_Tagged (File.all).K := Buffer'First;
   end Create;

   -----------------
   -- End_Of_File --
   -----------------

   function End_Of_File (Resource : File_Tagged) return Boolean is
   begin
      return Resource.K > Resource.Buffer'Last;
   end End_Of_File;

   ------------
   -- Exists --
   ------------

   function Exists (Name : String) return Boolean is
   begin
      return Res_Files.Is_Present (Files_Table, Name);
   end Exists;

   ---------------
   -- File_Size --
   ---------------

   function File_Size
     (Name : String)
      return Ada.Streams.Stream_Element_Offset
   is
      N : Node;
   begin
      if Res_Files.Is_Present (Files_Table, Name) then
         N := Res_Files.Value (Files_Table, Name);
         return N.File_Buffer'Length;
      else
         raise Resource_Error;
      end if;
   end File_Size;

   --------------------
   -- File_Timestamp --
   --------------------

   function File_Timestamp (Name : String) return Ada.Calendar.Time is
      N : Node;
   begin
      if Res_Files.Is_Present (Files_Table, Name) then
         N := Res_Files.Value (Files_Table, Name);
         return N.File_Time;
      else
         raise Resource_Error;
      end if;
   end File_Timestamp;

   ---------------------
   -- Is_Regular_File --
   ---------------------

   function Is_Regular_File (Name : String) return Boolean is
   begin
      return Exists (Name);
   end Is_Regular_File;

   ----------
   -- Open --
   ----------

   procedure Open
     (File :    out File_Type;
      Name : String;
      Form : String    := "")
   is
      pragma Unreferenced (Form);
      N : Node;
   begin
      if Res_Files.Is_Present (Files_Table, Name) then
         N := Res_Files.Value (Files_Table, Name);

         File := new File_Tagged;
         File_Tagged (File.all).Buffer := N.File_Buffer;
         File_Tagged (File.all).K := N.File_Buffer'First;
      else
         File := null;
      end if;
   end Open;

   ----------
   -- Read --
   ----------

   procedure Read
     (Resource : in out File_Tagged;
      Buffer   :    out Stream_Element_Array;
      Last     :    out Stream_Element_Offset)
   is
      K    : Stream_Element_Offset renames Resource.K;
      Size : Stream_Element_Offset; --  Number of byte remaining in buffer
   begin
      if K > Resource.Buffer'Last then
         Last := 0;
      else
         Size := Resource.Buffer'Length - (K - Resource.Buffer'First);

         if Buffer'Length <= Size then
            Buffer := Resource.Buffer (K .. K + Buffer'Length - 1);
            Last := Buffer'Last;
            K := K + Buffer'Length;
         else
            Last := Buffer'First + Size - 1;
            Buffer (Buffer'First .. Last)
              := Resource.Buffer (K .. Resource.Buffer'Last);
            K := Resource.Buffer'Last + 1;
         end if;
      end if;

      Resource.K := K;
   end Read;

   --------------
   -- Register --
   --------------

   procedure Register
     (Name      : String;
      Content   : Buffer_Access;
      File_Time : Calendar.Time) is
   begin
      Res_Files.Insert (Files_Table, Name, (Content, File_Time));
   end Register;

end AWS.Resources.Embedded;

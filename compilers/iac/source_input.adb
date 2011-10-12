------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                               O U T P U T                                --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--           Copyright (C) 2011, Free Software Foundation, Inc.             --
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

with Ada.Containers.Hashed_Maps; use Ada.Containers;
with Ada.Containers.Vectors;

with GNAT.OS_Lib; use GNAT.OS_Lib;

with Errors; use Errors;
with Namet;  use Namet;

package body Source_Input is

   --  Vector of source files in creation order

   package Source_File_Vectors is new Vectors
     (Index_Type => Positive,
      Element_Type => Source_File_Ptr);
   use Source_File_Vectors;

   Source_Files : Source_File_Vectors.Vector;
   --  Contains all source files in creation order. Open_Source appends to
   --  this; elements are never removed. Used by Iterate_Source_Files.

   --  Mapping from file names to source files

   package Source_File_Maps is new Hashed_Maps
     (Key_Type => Name_Id,
      Element_Type => Source_File_Ptr,
      Hash => Hash,
      Equivalent_Keys => "=");
   use Source_File_Maps;

   Source_File_Map : Source_File_Maps.Map;
   --  Contains all source files. Open_Source inserts in this; elements are
   --  never removed. Used by Named_File.

   -----------------------------
   -- Copy_To_Standard_Output --
   -----------------------------

   procedure Copy_To_Standard_Output (Source : Source_File) is
      Ignore : Integer;
      pragma Unreferenced (Ignore);
      Buf : Text_Buffer renames Source.Buffer.all;
   begin
      Ignore := Write (Standout, Buf'Address, Buf'Length - 1);
      --  Deliberately ignore result on output; it's not clear what we could
      --  do about any failure.
      --  "- 1" is to leave out the EOF terminator
   end Copy_To_Standard_Output;

   -------------------
   -- Iterate_Lines --
   -------------------

   procedure Iterate_Lines
     (Source : Source_File;
      Process : not null access procedure (Line : String)) is

      Buf : Text_Buffer renames Source.Buffer.all;
      First : Text_Ptr := 1;
      Last  : Text_Ptr := 0;
   begin
      while Last < Buf'Last loop
         while Buf (Last + 1) /= EOF and then
           Buf (Last + 1) /= ASCII.CR and then
           Buf (Last + 1) /= ASCII.LF
         loop
            Last := Last + 1;
         end loop;

         Process (String (Buf (First .. Last)));

         --  Skip end-of-line characters, which could be any of LF, CR, or
         --  CRLF.  Buf (Last + 1) always exists below, because there's an
         --  extra EOF at the end.

         Last := Last + 1;

         if Buf (Last) = ASCII.CR and then Buf (Last + 1) = ASCII.LF then
            Last := Last + 1;
         end if;

         --  Next line starts after end-of-line characters

         First := Last + 1;
      end loop;
   end Iterate_Lines;

   --------------------------
   -- Iterate_Source_Files --
   --------------------------

   procedure Iterate_Source_Files
     (Process : not null access procedure (Source : Source_File)) is

      procedure Call_Process (Position : Source_File_Vectors.Cursor);
      --  Wrapper just to get the Source_File from the cursor

      ------------------
      -- Call_Process --
      ------------------

      procedure Call_Process (Position : Source_File_Vectors.Cursor) is
      begin
         Process (Element (Position).all);
      end Call_Process;

   begin
      Iterate (Source_Files, Call_Process'Access);
   end Iterate_Source_Files;

   ----------------
   -- Named_File --
   ----------------

   function Named_File (Name : Name_Id) return Source_File_Ptr is
   begin
      return Element (Source_File_Map, Name);
   end Named_File;

   -----------------
   -- Open_Source --
   -----------------

   function Open_Source
     (Name : Name_Id; Kind : Source_Kind) return Source_File_Ptr
   is
      Name_String : aliased constant String :=
        Get_Name_String (Name) & ASCII.NUL;
      FD : constant File_Descriptor := Open_Read (Name_String'Address, Binary);
   begin
      if FD = Invalid_FD then
         case Kind is
            when True_Source =>
               DE ("file not found: %", Get_Name_String (Name));
            when Preprocessed_Source =>
               DE ("preprocessor output file not found: %",
                   Get_Name_String (Name));
         end case;
         raise Fatal_Error;
      end if;

      declare
         Length : Integer := Integer (File_Length (FD));

         Result : constant Source_File_Ptr := new Source_File'
           (Name => Name,
            Buffer => new Text_Buffer (1 .. Text_Ptr (Length) + 1),
            --  "+ 1" is room for the last character to be EOF
            Kind => Kind);

         Buf : Text_Buffer renames Result.Buffer.all;
         Pos : Text_Ptr := 1; -- position in buffer to read into
         Read_RC : Integer; -- return code from Read

      begin
         --  Put the Result in the two containers

         Append (Source_Files, Result);
         Insert (Source_File_Map, Name, Result);

         --  Read the entire contents of the file

         loop
            Read_RC := Read (FD, Buf (Pos)'Address, Length);
            exit when Read_RC = Length;
            if Read_RC <= 0 then
               DE ("cannot read: %", Get_Name_String (Name));
               raise Fatal_Error;
            end if;
            Pos := Pos + Text_Ptr (Read_RC);
            Length  := Length - Read_RC;
         end loop;
         Close (FD);

         --  Terminate buffer with EOF

         Buf (Buf'Length) := EOF;

         return Result;
      end;
   end Open_Source;

end Source_Input;

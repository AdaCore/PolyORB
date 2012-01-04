------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                         I D L A C _ E R R O R S                          --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2001-2012, Free Software Foundation, Inc.          --
--                                                                          --
-- This is free software;  you can redistribute it  and/or modify it  under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for  more details.                                               --
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

with Ada.Strings.Fixed; use Ada.Strings, Ada.Strings.Fixed;
with Ada.Strings.Maps;
with Ada.Text_IO;       use Ada.Text_IO;
with GNAT.Directory_Operations;
with Idlac_Flags;       use Idlac_Flags;
with Idlac_Utils;       use Idlac_Utils;
with Utils;

package body Idlac_Errors is

   -------------------------
   --  Location handling  --
   -------------------------

   procedure Display_Error
     (Message : String;
      Level : Error_Kind;
      Loc : Location);
   --  Display an error

   procedure Pinpoint_Error
     (Message : String;
      Loc     : Location);
   --  Print a full description of the error message with pointers
   --  on the error location.

   function Full_Name (Loc : Location) return String;
   --  Return the full file name (i.e., a usable one), or "<standard input>" if
   --  unknown.

   function Resolve_Tabs (S : String) return String;
   --  Return a string where tabs have been properly expanded

   --------------------------
   --  Location_To_String  --
   --------------------------

   function Location_To_String
     (Loc   : Location;
      Short : Boolean := False)
     return String is

      Path   : constant String := Full_Name (Loc);
      Line   : constant String := Img (Loc.Line);
      Column : constant String := Img (Loc.Col);

   begin
      if Short then
         return Path & ':' & Line & ':' & Column;
      else
         return "line " & Line & ", column " & Column & " of file " & Path;
      end if;
   end Location_To_String;

   ----------------
   --  Set_Path  --
   ----------------

   procedure Set_Path (Loc : in out Location; Filename : String) is
      use Ada.Strings.Maps;

      Separator : constant Natural
        := Index (Filename,
                  To_Set (GNAT.Directory_Operations.Dir_Separator & '/'),
                  Inside,
                  Backward);

   begin
      if Separator /= 0 then
         Loc.Dirname :=
          new String'(Filename (Filename'First .. Separator - 1));
         Loc.Filename :=
          new String'(Filename (Separator + 1 .. Filename'Last));
      else
         Loc.Dirname := null;
         Loc.Filename := new String'(Filename);
      end if;
   end Set_Path;

   ----------------------
   --  Error handling  --
   ----------------------

   --  counters for errors and warnings
   Error_Count : Natural := 0;
   Warning_Count : Natural := 0;

   ---------------------
   --  Display_Error  --
   ---------------------
   procedure Display_Error (Message : String;
                            Level : Error_Kind;
                            Loc : Location) is
   begin
      case Level is
         when Fatal =>
            Pinpoint_Error ("Fatal: " & Message, Loc);
         when Error =>
            Pinpoint_Error ("Error: " & Message, Loc);
         when Warning =>
            Pinpoint_Error ("Warning: " & Message, Loc);
      end case;
   end Display_Error;

   -----------
   -- Error --
   -----------

   procedure Error
     (Message : String;
      Level : Error_Kind;
      Loc : Location) is
   begin
      case Level is
         when Fatal =>
            null;
         when Error =>
            Error_Count := Error_Count + 1;
         when Warning =>
            Warning_Count := Warning_Count + 1;
      end case;
      Display_Error (Message, Level, Loc);
      if Level = Fatal then
         raise Fatal_Error;
      end if;
   end Error;

   ----------------
   --  Is_Error  --
   ----------------

   function Is_Error return Boolean is
   begin
      return Error_Count > 0;
   end Is_Error;

   ------------------
   --  Is_Warning  --
   ------------------

   function Is_Warning return Boolean is
   begin
      return Warning_Count > 0;
   end Is_Warning;

   --------------------
   --  Error_Number  --
   --------------------

   function Error_Number return Natural is
   begin
      return Error_Count;
   end Error_Number;

   ----------------------
   --  Warning_Number  --
   ----------------------

   function Warning_Number return Natural is
   begin
      return Warning_Count;
   end Warning_Number;

   ---------------
   -- Full_Name --
   ---------------

   function Full_Name (Loc : Location) return String is
   begin
      if Loc.Filename = null then
         return "<standard input>";
      elsif Loc.Dirname = null then
         return Loc.Filename.all;
      elsif Utils.Is_Dir_Separator (Loc.Dirname (Loc.Dirname'Last)) then
         return Loc.Dirname.all & Loc.Filename.all;
      else
         return Loc.Dirname.all & GNAT.Directory_Operations.Dir_Separator
           & Loc.Filename.all;
      end if;
   end Full_Name;

   --------------------
   -- PinPoint_Error --
   --------------------

   procedure Pinpoint_Error
     (Message : String;
      Loc     : Location)
   is

      procedure Format (Message : String);
      --  Format an error message so that it fits (hopefully) on a 80
      --  characters screen.

      ------------
      -- Format --
      ------------

      procedure Format (Message : String) is
         Sep : Natural;
      begin
         if Message = "" then
            return;
         elsif Message (Message'First) = ' ' then
            Format (Message (Message'First + 1 .. Message'Last));
            return;
         end if;

         Put (Current_Error, " >>> ");
         if Message'Length <= 75 then
            Put_Line (Current_Error, Message);
         else
            Sep := Index (Message (Message'First .. Message'First + 74),
                          " ", Backward);
            if Sep = 0 then
               Put_Line (Current_Error,
                         Message (Message'First .. Message'First + 74));
               Format (Message (Message'First + 75 .. Message'Last));
            else
               Put_Line (Current_Error, Message (Message'First .. Sep - 1));
               Format (Message (Sep + 1 .. Message'Last));
            end if;
         end if;
      end Format;

   begin
      if Verbose and then Loc.Line > 0 and then Loc.Col > 0 then
         Put_Line (Current_Error, "In file " & Full_Name (Loc));
         New_Line (Current_Error);
         declare
            File_Name : constant String := Full_Name (Loc);
            File      : File_Type;
            Line      : String (1 .. 1024);
            Last      : Natural;
            LN        : constant String := Img (Loc.Line);
            LNN       : constant Positive := LN'Length;
         begin
            Open (File, In_File, File_Name);
            for I in 1 .. Loc.Line loop
               Get_Line (File, Line, Last);
            end loop;
            Put_Line (Current_Error,
                      LN & "   " & Resolve_Tabs (Line (1 .. Last)));
            for I in 1 .. LNN + 3 + Loc.Col loop
               Put (Current_Error, " ");
            end loop;
            Put_Line (Current_Error, "^");
            Format (Message);
            Close (File);
         exception
            when Name_Error =>
               Put_Line (Current_Error, Message);
         end;
         New_Line (Current_Error);
      else
         Put_Line
           (Current_Error,
            Location_To_String (Loc, Short => True) & ' ' & Message);
      end if;
   end Pinpoint_Error;

   ------------------
   -- Resolve_Tabs --
   ------------------

   function Resolve_Tabs (S : String) return String is
      R : String (1 .. S'Length * 8);
      L : Natural := 0;
   begin
      for I in S'Range loop
         if S (I) = ASCII.HT then
            for J in 1 .. 8 - (L mod 8) loop
               L := L + 1;
               R (L) := ' ';
            end loop;
         else
            L := L + 1;
            R (L) := S (I);
         end if;
      end loop;
      return R (1 .. L);
   end Resolve_Tabs;

end Idlac_Errors;

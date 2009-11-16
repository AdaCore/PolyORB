------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                       U P D A T E _ H E A D E R S                        --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2006-2009, Free Software Foundation, Inc.          --
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

with Ada.Calendar; use Ada.Calendar;
with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Command_Line;
with Ada.Directories; use Ada.Directories;
with Ada.Exceptions;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;

with GNAT.Regpat; use GNAT.Regpat;

procedure Update_Headers is

   pragma Style_Checks ("mM100");  --  Allow long lines below

   subtype Line_Type is String (1 .. 256);

   type Kind_Type is (None, Unit_Spec, Unit_Body, Unit_Project);

   Header_Template : constant String :=
   "------------------------------------------------------------------------------" & ASCII.LF &
   "--                                                                          --" & ASCII.LF &
   "--                           POLYORB COMPONENTS                             --" & ASCII.LF &
   "--                                                                          --" & ASCII.LF &
   "@UNIT_NAME@" &
   "--                                                                          --" & ASCII.LF &
   "@COPYRIGHT@" &
   "--                                                                          --" & ASCII.LF &
   "@SECONDARY_HEADER@" &
   "-- PolyORB is free software; you  can  redistribute  it and/or modify it    --" & ASCII.LF &
   "-- under terms of the  GNU General Public License as published by the  Free --" & ASCII.LF &
   "-- Software Foundation;  either version 2,  or (at your option)  any  later --" & ASCII.LF &
   "-- version. PolyORB is distributed  in the hope that it will be  useful,    --" & ASCII.LF &
   "-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --" & ASCII.LF &
   "-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --" & ASCII.LF &
   "-- License  for more details.  You should have received  a copy of the GNU  --" & ASCII.LF &
   "-- General Public License distributed with PolyORB; see file COPYING. If    --" & ASCII.LF &
   "-- not, write to the Free Software Foundation, 51 Franklin Street, Fifth    --" & ASCII.LF &
   "-- Floor, Boston, MA 02111-1301, USA.                                       --" & ASCII.LF &
   "--                                                                          --" & ASCII.LF &
   "-- As a special exception,  if other files  instantiate  generics from this --" & ASCII.LF &
   "-- unit, or you link  this unit with other files  to produce an executable, --" & ASCII.LF &
   "-- this  unit  does not  by itself cause  the resulting  executable  to  be --" & ASCII.LF &
   "-- covered  by the  GNU  General  Public  License.  This exception does not --" & ASCII.LF &
   "-- however invalidate  any other reasons why  the executable file  might be --" & ASCII.LF &
   "-- covered by the  GNU Public License.                                      --" & ASCII.LF &
   "--                                                                          --" & ASCII.LF &
   "--                  PolyORB is maintained by AdaCore                        --" & ASCII.LF &
   "--                     (email: sales@adacore.com)                           --" & ASCII.LF &
   "--                                                                          --" & ASCII.LF &
   "------------------------------------------------------------------------------" & ASCII.LF;

   OMG_Header_Template : constant String :=
   "-- This specification is derived from the CORBA Specification, and adapted  --" & ASCII.LF &
   "-- for use with PolyORB. The copyright notice above, and the license        --" & ASCII.LF &
   "-- provisions that follow apply solely to the contents neither explicitly   --" & ASCII.LF &
   "-- nor implicitly specified by the CORBA Specification defined by the OMG.  --" & ASCII.LF &
   "--                                                                          --" & ASCII.LF;

   -------------------------
   -- Utility subprograms --
   -------------------------

   function Center_Ada (S : String) return String;
   --  Return S centered with comment delimiters of appropriate width

   function Copyright_Line (First_Year, Last_Year : Year_Number) return String;
   --  Return copyright notice for the specified year range

   function Doublespace (S : String) return String;
   --  Return S with double spacing inserted if short enough to fit the header
   --  comment box; otherwise return S unchanged.

   function Has_Prefix (Prefix : String; S : String) return Boolean;
   --  True iff S starts with Prefix

   function Image (Year : Year_Number) return String;
   --  Return the string image of Year (with no leading space)

   procedure Update_Header (Filename : String);
   --  Output the contents of Filename with updated header

   ----------------
   -- Center_Ada --
   ----------------

   function Center_Ada (S : String) return String is
      Line  : String (1 .. 78) := (others => ' ');
      Width : constant := Line'Length;
      Pos   : constant Positive := (Line'Length - (S'Length - 1)) / 2;
   begin
      Line (1 .. 2) := "--";
      Line (Line'Last - 1 .. Line'Last) := "--";
      Line (Pos .. Pos + S'Length - 1) := S;
      return Line;
   end Center_Ada;

   --------------------
   -- Copyright_Line --
   --------------------

   function Copyright_Line
     (First_Year, Last_Year : Year_Number) return String
   is
      Range_Image : constant String :=
        Image (First_Year) & "-" & Image (Last_Year);
      Last : Positive := Range_Image'Last;
   begin
      if First_Year = Last_Year then
         Last := Range_Image'First + 3;
      end if;
      return "Copyright (C) " & Range_Image (Range_Image'First .. Last)
        & ", Free Software Foundation, Inc.";
   end Copyright_Line;

   -----------------
   -- Doublespace --
   -----------------

   function Doublespace (S : String) return String is
   begin
      if S'Length > 35 then
         return S;
      else
         declare
            Res : String (2 * S'First .. 2 * S'Last) := (others => ' ');
         begin
            for J in S'Range loop
               Res (2 * J) := S (J);
            end loop;
            return Res;
         end;
      end if;
   end Doublespace;

   ----------------
   -- Has_Prefix --
   ----------------

   function Has_Prefix (Prefix : String; S : String) return Boolean is
   begin
      return S'Length >= Prefix'Length
        and then S (S'First .. S'First + Prefix'Length - 1) = Prefix;
   end Has_Prefix;

   -----------
   -- Image --
   -----------

   function Image (Year : Year_Number) return String is
      Res : constant String := Year'Img;
   begin
      return Res (Res'First + 1 .. Res'Last);
   end Image;

   -------------------
   -- Update_Header --
   -------------------

   procedure Update_Header (Filename : String) is

      Ofilename : constant String := Filename & ".UHN";

      ----------------------
      -- Global variables --
      ----------------------

      UName : Unbounded_String;
      UKind : Kind_Type;

      Last_Copyright_Year  : Year_Number := Year (Clock);
      First_Copyright_Year : Year_Number := Last_Copyright_Year;

      Require_OMG_Header : Boolean;

      type Substs is (Unit_Name, Copyright, Secondary_Header);

      procedure Output_Header (Outf : File_Type);
      --  Output header templates with appropriate substitutions

      procedure Output_Header (Outf : File_Type) is
         Pattern : Unbounded_String;

         function "+" (S : String) return Unbounded_String is
         begin
            return To_Unbounded_String (Center_Ada (S) & ASCII.LF);
         end "+";

         Subst_Strings : array (Substs) of Unbounded_String :=
           (Unit_Name        =>
              +Doublespace (To_Upper (To_String (UName))),
            Copyright        =>
              +Copyright_Line (First_Copyright_Year, Last_Copyright_Year),
            Secondary_Header =>
              To_Unbounded_String (""));

         Kind_Strings : constant array (Unit_Spec .. Unit_Project)
                          of String (1 .. 4) :=
                            (Unit_Spec    => "Spec",
                             Unit_Body    => "Body",
                             Unit_Project => "Proj");

      begin
         if UKind in Kind_Strings'Range then
            Append (Subst_Strings (Unit_Name), +"");
            Append (Subst_Strings (Unit_Name),
                    +Doublespace (Kind_Strings (Ukind)));
         end if;

         if Require_OMG_Header then
            Subst_Strings (Secondary_Header) :=
              To_Unbounded_String (OMG_Header_Template);
         end if;

         Pattern := To_Unbounded_String ("@(");
         for J in Substs loop
            Append (Pattern, J'Img);
            if J /= Substs'Last then
               Append (Pattern, '|');
            end if;
         end loop;
         Append (Pattern, ")@");

         declare
            Matcher : constant Pattern_Matcher :=
                        Compile (To_String (Pattern), Single_Line);
            Matches : Match_Array (0 .. Paren_Count (Matcher));
            Start   : Positive := Header_Template'First;
         begin
            while Start <= Header_Template'Last loop
               Match (Matcher,
                      Header_Template (Start .. Header_Template'Last), Matches);

               if Matches (0) = No_Match then
                  Put (Outf, Header_Template (Start .. Header_Template'Last));
                  exit;
               end if;

               declare
                  Loc_Token  : Match_Location renames Matches (1);
               begin
                  Put (Outf, Header_Template (Start .. Loc_Token.First - 2));
                  Put (Outf,
                    To_String (Subst_Strings
                      (Substs'Value (Header_Template (Loc_Token.First
                                                   .. Loc_Token.Last)))));
                  Start := Loc_Token.Last + 2;
               end;

            end loop;
         end;
      end Output_Header;

      Line : Line_Type;
      Last : Natural;

      Copyright_Matcher : constant Pattern_Matcher :=
                            Compile ("Copyright \([cC]\) ([0-9]+)");
      Copyright_Matches : Match_Array (0 .. Paren_Count (Copyright_Matcher));

      Unit_Name_Matcher : constant Pattern_Matcher :=
                            Compile ("^(private\s+|separate \(([\w.]+)\)\s+)?"
                                     & "(procedure|function|project|package"
                                     & "(\s+body)?)\s+([\w.]+)\b");
      Unit_Name_Matches : Match_Array (0 .. Paren_Count (Unit_Name_Matcher));

      F    : File_Type;
      Outf : File_Type;

      In_Header : Boolean := True;
      Buf : Unbounded_String;

      Basename : constant String := Base_Name (Filename);

   begin
      Open   (F, In_File, Filename);
      Create (Outf, Out_File, Ofilename, Form => "Text_Translation=No");

      begin
         --  Check for file kind suffix, but omit possible trailing ".in"
         --  for the case of autoconf template files.

         Last := Filename'Last;
         if Last - 2 >= Filename'First
           and then Filename (Last - 2 .. Last) = ".in"
         then
            Last := Last - 3;
         end if;

         if Last - 2 >= Filename'First then
            declare
               Extension : String renames Filename (Last - 2 .. Last);
            begin
               if Extension = "ads" then
                  UKind := Unit_Spec;

               elsif Extension = "adb" then
                  UKind := Unit_Body;

               elsif Extension = "gpr" then
                  UKind := Unit_Project;

               else
                  UKind := None;

               end if;
            end;
         end if;

         Require_OMG_Header := UKind = Unit_Spec
           and then (False
             or else Has_Prefix ("conv_frame", Basename)
             or else Has_Prefix ("corba", Basename)
             or else Has_Prefix ("portableinterceptor", Basename)
             or else Has_Prefix ("portableserver", Basename)
             or else Has_Prefix ("rtcorba", Basename)
             or else Has_Prefix ("rtcosscheduling", Basename)
             or else Has_Prefix ("rtportableserver", Basename));

         loop
            Get_Line (F, Line, Last);
            if Last = Line'Last then
               raise Constraint_Error with "line too long";
            end if;
            if Last < 2 or else Line (1 .. 2) /= "--" then
               In_Header := False;
            end if;

            if In_Header then
               Match (Copyright_Matcher, Line (1 .. Last), Copyright_Matches);
               if Copyright_Matches (0) /= No_Match then
                  First_Copyright_Year :=
                    Year_Number'Value (Line (Copyright_Matches (1).First
                                          .. Copyright_Matches (1).Last));
               end if;
            else
               Append (Buf, Line (1 .. Last));
               Append (Buf, ASCII.LF);
               Match (Unit_Name_Matcher, Line (1 .. Last), Unit_Name_Matches);
               if Unit_Name_Matches (0) /= No_Match then
                  if Unit_Name_Matches (1).First in Line'Range
                       and then
                     Line (Unit_Name_Matches (1).First) = 's'
                  then
                     --  Case of a separate body

                     UName := To_Unbounded_String
                                (Line (Unit_Name_Matches (2).First
                                    .. Unit_Name_Matches (2).Last));
                     Append (Uname, '.');
                  end if;

                  Append (UName, Line (Unit_Name_Matches (5).First
                                    .. Unit_Name_Matches (5).Last));
                  exit;
               end if;
            end if;
         end loop;

         Output_Header (Outf);
         if Slice (Buf, 1, 1) /= (1 => ASCII.LF) then
            New_Line (Outf);
         end if;
         Put (Outf, To_String (Buf));

         while not End_Of_File (F) loop
            Get_Line (F, Line, Last);
            if Last = Line'Last then
               raise Constraint_Error with "line too long";
            end if;
            Put_Line (Outf, Line (1 .. Last));
         end loop;

         Close (F);
         Close (Outf);
         Delete_File (Filename);
         Rename (Ofilename, Filename);
      exception
         when E : others =>
            Put_Line (Standard_Error, "Update of " & Filename & " failed:");
            Put_Line (Ada.Exceptions.Exception_Information (E));
            Delete_File (Ofilename);
      end;

   end Update_Header;

--  Start of processing for Update_Headers

begin
   for J in 1 .. Ada.Command_Line.Argument_Count loop
      Update_Header (Ada.Command_Line.Argument (J));
   end loop;
end Update_Headers;

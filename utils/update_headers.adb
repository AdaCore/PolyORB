------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                       U P D A T E _ H E A D E R S                        --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2006-2012, Free Software Foundation, Inc.          --
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

pragma Ada_2012;

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
   "@OMG_HEADER@" &

   "-- This is free software;  you can redistribute it  and/or modify it  under --" & ASCII.LF &
   "-- terms of the  GNU General Public License as published  by the Free Soft- --" & ASCII.LF &
   "-- ware  Foundation;  either version 3,  or (at your option) any later ver- --" & ASCII.LF &
   "-- sion.  This software is distributed in the hope  that it will be useful, --" & ASCII.LF &
   "-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --" & ASCII.LF &
   "-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --" & ASCII.LF &
   "-- License for  more details.                                               --" & ASCII.LF &
   "--                                                                          --" & ASCII.LF &
   "@RUNTIME_EXCEPTION@" &
   "-- You should have received a copy of the GNU General Public License and    --" & ASCII.LF &
   "-- a copy of the GCC Runtime Library Exception along with this program;     --" & ASCII.LF &
   "-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --" & ASCII.LF &
   "-- <http://www.gnu.org/licenses/>.                                          --" & ASCII.LF &
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

   Runtime_Exception_Template : constant String :=
   "-- As a special exception under Section 7 of GPL version 3, you are granted --" & ASCII.LF &
   "-- additional permissions described in the GCC Runtime Library Exception,   --" & ASCII.LF &
   "-- version 3.1, as published by the Free Software Foundation.               --" & ASCII.LF &
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

      type Substs is (Unit_Name, Copyright, OMG_Header, Runtime_Exception);

      Enable : array (Substs) of Boolean :=
        (OMG_Header | Runtime_Exception => False,
         others                         => True);
      --  By default empty substitution for OMG_Header and Runtime_Exception

      procedure Output_Header (Outf : File_Type);
      --  Output header templates with appropriate substitutions

      -------------------
      -- Output_Header --
      -------------------

      procedure Output_Header (Outf : File_Type) is
         Pattern : Unbounded_String;

         function "+" (S : String) return Unbounded_String is
         begin
            return To_Unbounded_String (Center_Ada (S) & ASCII.LF);
         end "+";

         Subst_Strings : array (Substs) of Unbounded_String :=
           (Unit_Name         =>
              +Doublespace (To_Upper (To_String (UName))),
            Copyright         =>
              +Copyright_Line (First_Copyright_Year, Last_Copyright_Year),
            OMG_Header        =>
              To_Unbounded_String (OMG_Header_Template),
            Runtime_Exception =>
              To_Unbounded_String (Runtime_Exception_Template));

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
                  Loc_Tok : Match_Location renames Matches (1);
                  Subst   : constant Substs :=
                              Substs'Value (Header_Template (Loc_Tok.First
                                                          .. Loc_Tok.Last));
               begin
                  Put (Outf, Header_Template (Start .. Loc_Tok.First - 2));

                  if Enable (Subst) then
                     Put (Outf, To_String (Subst_Strings (Subst)));
                  end if;
                  Start := Loc_Tok.Last + 2;
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

         Enable (OMG_Header) := UKind = Unit_Spec
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

               if Match ("As a special exception", Line (1 .. Last)) then
                  Enable (Runtime_Exception) := True;
               end if;

            else
               if Length (Buf) > 0 then
                  Append (Buf, ASCII.LF);
               end if;
               Append (Buf, Line (1 .. Last));
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
         Put_Line (Outf, To_String (Buf));

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

------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                                  P R J                                   --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision$
--                                                                          --
--             Copyright (C) 2001 Free Software Foundation, Inc.            --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT;  see file COPYING.  If not, write --
-- to  the Free Software Foundation,  59 Temple Place - Suite 330,  Boston, --
-- MA 02111-1307, USA.                                                      --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- It is now maintained by Ada Core Technologies Inc (http://www.gnat.com). --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Characters.Handling; use Ada.Characters.Handling;
with Errout;      use Errout;
with GNAT.OS_Lib; use GNAT.OS_Lib;
with Namet;       use Namet;
with Osint;       use Osint;
with Prj.Attr;
with Prj.Env;
with Scans;       use Scans;
with Scn;
with Stringt;     use Stringt;
with Sinfo.CN;
with Snames;      use Snames;

package body Prj is

   The_Empty_String : String_Id;

   subtype Known_Casing is Casing_Type range All_Upper_Case .. Mixed_Case;

   The_Casing_Images : array (Known_Casing) of String_Access :=
     (All_Lower_Case => new String'("lowercase"),
      All_Upper_Case => new String'("UPPERCASE"),
      Mixed_Case     => new String'("MixedCase"));

   Initialized : Boolean := False;

   Standard_Dot_Replacement      : constant Name_Id :=
     First_Name_Id + Character'Pos ('-');
   Standard_Specification_Append : Name_Id;
   Standard_Body_Append          : Name_Id;

   Std_Naming_Data : Naming_Data :=
     (Dot_Replacement      => Standard_Dot_Replacement,
      Dot_Repl_Loc         => No_Location,
      Casing               => All_Lower_Case,
      Specification_Append => No_Name,
      Spec_Append_Loc      => No_Location,
      Body_Append          => No_Name,
      Body_Append_Loc      => No_Location,
      Separate_Append      => No_Name,
      Sep_Append_Loc       => No_Location,
      Specifications       => No_Array_Element,
      Bodies               => No_Array_Element);

   Project_Empty : Project_Data :=
     (First_Referred_By  => No_Project,
      Name               => No_Name,
      Path_Name          => No_Name,
      Location           => No_Location,
      Directory          => No_Name,
      File_Name          => No_Name,
      Sources            => Nil_String,
      Source_Dirs        => Nil_String,
      Object_Directory   => No_Name,
      Modifies           => No_Project,
      Modified_By        => No_Project,
      Naming             => Std_Naming_Data,
      Decl               => No_Declarations,
      Imported_Projects  => Empty_Project_List,
      Include_Path       => null,
      Objects_Path       => null,
      Gnat_Adc_Generated => False,
      Checked            => False);

   -------------------
   -- Empty_Project --
   -------------------

   function Empty_Project return Project_Data is
   begin
      Initialize;
      return Project_Empty;
   end Empty_Project;

   ------------------
   -- Empty_String --
   ------------------

   function Empty_String return String_Id is
   begin
      return The_Empty_String;
   end Empty_String;

   ------------
   -- Expect --
   ------------

   procedure Expect (The_Token : Token_Type; Token_Image : String) is
   begin
      if Token /= The_Token then
         Error_Msg ("""" & Token_Image & """ expected", Token_Ptr);
      end if;
   end Expect;

   -----------
   -- Image --
   -----------

   function Image (Casing : Casing_Type) return String is
   begin
      return The_Casing_Images (Casing).all;
   end Image;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      if not Initialized then
         Initialized := True;
         Stringt.Initialize;
         Start_String;
         The_Empty_String := End_String;
         Name_Len := 4;
         Name_Buffer (1 .. 4) := ".ads";
         Canonical_Case_File_Name (Name_Buffer (1 .. 4));
         Standard_Specification_Append := Name_Find;
         Name_Buffer (4) := 'b';
         Canonical_Case_File_Name (Name_Buffer (1 .. 4));
         Standard_Body_Append := Name_Find;
         Std_Naming_Data.Specification_Append := Standard_Specification_Append;
         Std_Naming_Data.Body_Append          := Standard_Body_Append;
         Std_Naming_Data.Separate_Append      := Standard_Body_Append;
         Project_Empty.Naming                 := Std_Naming_Data;
         Prj.Env.Initialize;
         Prj.Attr.Initialize;
         Set_Name_Table_Byte (Name_Project,   Token_Type'Pos (Tok_Project));
         Set_Name_Table_Byte (Name_Modifying, Token_Type'Pos (Tok_Modifying));
         Set_Name_Table_Byte (Name_External,  Token_Type'Pos (Tok_External));
      end if;
   end Initialize;

   ------------------------
   -- Same_Naming_Scheme --
   ------------------------

   function Same_Naming_Scheme
     (Left, Right : Naming_Data)
      return        Boolean
   is
   begin
      return Left.Dot_Replacement = Right.Dot_Replacement
        and then Left.Casing = Right.Casing
        and then Left.Specification_Append = Right.Specification_Append
        and then Left.Body_Append = Right.Body_Append
        and then Left.Separate_Append = Right.Separate_Append;
   end Same_Naming_Scheme;

   ----------
   -- Scan --
   ----------

   procedure Scan is
   begin
      Scn.Scan;

      --  Change operator symbol to literal strings, since that's the way
      --  we treat all strings in a project file.

      if Token = Tok_Operator_Symbol then
         Sinfo.CN.Change_Operator_Symbol_To_String_Literal (Token_Node);
         Token := Tok_String_Literal;
      end if;
   end Scan;

   --------------------------
   -- Standard_Naming_Data --
   --------------------------

   function Standard_Naming_Data return Naming_Data is
   begin
      Initialize;
      return Std_Naming_Data;
   end Standard_Naming_Data;

   -----------
   -- Value --
   -----------

   function Value (Image : String) return Casing_Type is
   begin
      for Casing in The_Casing_Images'Range loop
         if To_Lower (Image) = To_Lower (The_Casing_Images (Casing).all) then
            return Casing;
         end if;
      end loop;

      raise Constraint_Error;
   end Value;

end Prj;

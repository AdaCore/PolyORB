------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                                F N A M E                                 --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision$                             --
--                                                                          --
--   Copyright (C) 1992,1993,1994,1995,1996 Free Software Foundation, Inc.  --
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
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- It is now maintained by Ada Core Technologies Inc (http://www.gnat.com). --
--                                                                          --
------------------------------------------------------------------------------

with Alloc;    use Alloc;
with Debug;    use Debug;
with Krunch;
with Namet;    use Namet;
with Opt;      use Opt;
with Table;
with Widechar; use Widechar;

with GNAT.Htable;

package body Fname is

   --------------------------------------------------------
   -- Declarations for Handling Source_File_Name pragmas --
   --------------------------------------------------------

   type SFN_Entry is record
      U : Unit_Name_Type;
      F : File_Name_Type;
   end record;

   package SFN_Table is new Table (
     Table_Component_Type => SFN_Entry,
     Table_Index_Type     => Int,
     Table_Low_Bound      => 0,
     Table_Initial        => Alloc_SFN_Table_Initial,
     Table_Increment      => Alloc_SFN_Table_Increment,
     Table_Name           => "SFN_Table");

   type SFN_Header_Num is range 0 .. 100;

   function SFN_Hash (F : Unit_Name_Type) return SFN_Header_Num;
   --  Compute hash index for use by Simple_Htable

   package SFN_Htable is new GNAT.Htable.Simple_Htable (
     Header_Num => SFN_Header_Num,
     Element    => File_Name_Type,
     No_Element => No_File,
     Key        => Unit_Name_Type,
     Hash       => SFN_Hash,
     Equal      => "=");

   -----------------------
   -- File_Name_Of_Body --
   -----------------------

   function File_Name_Of_Body (Name : Name_Id) return File_Name_Type is
   begin
      Get_Name_String (Name);
      Name_Buffer (Name_Len + 1 .. Name_Len + 2) := "%b";
      Name_Len := Name_Len + 2;
      return Get_File_Name (Name_Enter);
   end File_Name_Of_Body;

   -----------------------
   -- File_Name_Of_Spec --
   -----------------------

   function File_Name_Of_Spec (Name : Name_Id) return File_Name_Type is
   begin
      Get_Name_String (Name);
      Name_Buffer (Name_Len + 1 .. Name_Len + 2) := "%s";
      Name_Len := Name_Len + 2;
      return Get_File_Name (Name_Enter);
   end File_Name_Of_Spec;

   ----------------------------
   -- Get_Expected_Unit_Type --
   ----------------------------

   --  We assume that a file name whose last character is a lower case b is
   --  a body and a file name whose last character is a lower case s is a
   --  spec. If any other character is found (e.g. when we are in syntax
   --  checking only mode, where the file name conventions are not set),
   --  then we return Unknown.

   function Get_Expected_Unit_Type
     (Fname : File_Name_Type)
      return  Expected_Unit_Type
   is
   begin
      Get_Name_String (Fname);

      if Name_Buffer (Name_Len) = 'b' then
         return Expect_Body;
      elsif Name_Buffer (Name_Len) = 's' then
         return Expect_Spec;
      else
         return Unknown;
      end if;
   end Get_Expected_Unit_Type;

   -------------------
   -- Get_File_Name --
   -------------------

   function Get_File_Name (Uname : Unit_Name_Type) return File_Name_Type is
      Unit_Char : Character;
      --  Set to 's' or 'b' for spec or body

      J : Integer;
      N : File_Name_Type;

   begin
      N := SFN_Htable.Get (Uname);

      if N /= No_File then
         return N;
      end if;

      Get_Decoded_Name_String (Uname);

      --  A special fudge, normally we don't have operator symbols present,
      --  since it is always an error to do so. However, if we do, at this
      --  stage it has a leading double quote.

      --  What we do in this case is to go back to the undecoded name, which
      --  is of the form, for example:

      --    Oand%s

      --  and build a file name that looks like:

      --    _and_%s

      --  which is bit peculiar, but we keep it that way

      if Name_Buffer (Name_Len) = '"' then
         Get_Name_String (Uname);
         Name_Len := Name_Len + 1;
         Name_Buffer (Name_Len)     := Name_Buffer (Name_Len - 1);
         Name_Buffer (Name_Len - 1) := Name_Buffer (Name_Len - 2);
         Name_Buffer (Name_Len - 2) := '_';
         Name_Buffer (1)            := '_';
      end if;

      --  Change periods to hyphens, being careful to skip past any
      --  period characters embedded in wide character escape sequences)

      J := 1;

      while J <= Name_Len loop
         if Name_Buffer (J) = '.' then
            Name_Buffer (J) := '-';
            J := J + 1;

         elsif Name_Buffer (J) = Ascii.ESC
           or else (Upper_Half_Encoding
                     and then Name_Buffer (J) in Upper_Half_Character)
         then
            Skip_Wide (Name_Buffer, J);
         else
            J := J + 1;
         end if;
      end loop;

      --  Deal with spec or body suffix

      Unit_Char := Name_Buffer (Name_Len);
      pragma Assert (Unit_Char = 'b' or else Unit_Char = 's');
      pragma Assert (Name_Len >= 3 and then Name_Buffer (Name_Len - 1) = '%');
      Name_Len := Name_Len - 2;

      --  The file name (minus the extension) to be used is stored in
      --  Name_Buffer (1 .. Name_Buffer). If it's too long then crunch it.

      Krunch
        (Name_Buffer,
         Name_Len,
         Integer (Maximum_File_Name_Length),
         Debug_Flag_4);

      --  Here with the file name set and of OK length, add the extension

      Name_Len := Name_Len + 1;
      Name_Buffer (Name_Len) := '.';
      Name_Len := Name_Len + 1;
      Name_Buffer (Name_Len) := 'a';
      Name_Len := Name_Len + 1;
      Name_Buffer (Name_Len) := 'd';
      Name_Len := Name_Len + 1;
      Name_Buffer (Name_Len) := Unit_Char;

      return File_Name_Type (Name_Find);
   end Get_File_Name;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      SFN_Table.Init;
   end Initialize;

   -----------------------------
   -- Is_Predefined_File_Name --
   -----------------------------

   --  This should really be a test of unit name, given the possibility of
   --  pragma Source_File_Name setting arbitrary file names for any files???

   function Is_Predefined_File_Name (Fname : File_Name_Type) return Boolean is
      subtype Str8 is String (1 .. 8);

      Predef_Names : array (1 .. 12) of Str8 :=
         ("ada     ",       -- Ada
          "calendar",       -- Calendar
          "direc_io",       -- Direct_IO
          "gnat    ",       -- GNAT
          "interfac",       -- Interfaces
          "ioexcept",       -- IO_Exceptions
          "machcode",       -- Machine_Code
          "sequenio",       -- Sequential_IO
          "system  ",       -- System
          "text_io ",       -- Text_IO
          "unchconv",       -- Unchecked_Conversion
          "unchdeal");      -- Unchecked_Deallocation

   begin
      --  Get file name, removing the extension (if any)

      Get_Name_String (Fname);

      if Name_Len > 4 and then Name_Buffer (Name_Len - 3) = '.' then
         Name_Len := Name_Len - 4;
      end if;

      --  Definitely false if longer than 12 characters (8.3)

      if Name_Len > 8 then
         return False;

      --  Definitely predefined if prefix is a- g- i- or s-

      elsif Name_Len > 2
        and then Name_Buffer (2) = '-'
        and then (Name_Buffer (1) = 'a' or else
                  Name_Buffer (1) = 'g' or else
                  Name_Buffer (1) = 'i' or else
                  Name_Buffer (1) = 's')
      then
         return True;

      --  Definitely predefined if first character is digit

      elsif Name_Buffer (1) in '0' .. '9' then
         return True;
      end if;

      --  Otherwise check against special list, first padding to 8 characters

      while Name_Len < 8 loop
         Name_Len := Name_Len + 1;
         Name_Buffer (Name_Len) := ' ';
      end loop;

      for J in 1 .. 12 loop
         if Name_Buffer (1 .. 8) = Predef_Names (J) then
            return True;
         end if;
      end loop;

      return False;

   end Is_Predefined_File_Name;

   -------------------
   -- Set_File_Name --
   -------------------

   procedure Set_File_Name (U : Unit_Name_Type; F : File_Name_Type) is
   begin
      SFN_Table.Increment_Last;
      SFN_Table.Table (SFN_Table.Last) := (U, F);
      SFN_Htable.Set (U, F);
   end Set_File_Name;

   --------------
   -- SFN_Hash --
   --------------

   function SFN_Hash (F : Unit_Name_Type) return SFN_Header_Num is
   begin
      return SFN_Header_Num (Int (F) rem SFN_Header_Num'Range_Length);
   end SFN_Hash;

   ---------------
   -- Tree_Read --
   ---------------

   procedure Tree_Read is
   begin
      SFN_Table.Tree_Read;

      --  Reestablish the hash table

      for J in SFN_Table.First .. SFN_Table.Last loop
         SFN_Htable.Set (SFN_Table.Table (J).U, SFN_Table.Table (J).F);
      end loop;
   end Tree_Read;

   ----------------
   -- Tree_Write --
   ----------------

   procedure Tree_Write is
   begin
      --  Note that we do not write out the hash table, instead we
      --  simply reestablish it when we read in the table (see above)

      SFN_Table.Tree_Write;
   end Tree_Write;

end Fname;

------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             F N A M E . U F                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision$
--                                                                          --
--          Copyright (C) 1992-2000, Free Software Foundation, Inc.         --
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

with Alloc;
with Debug;    use Debug;
with Krunch;
with Namet;    use Namet;
with Opt;      use Opt;
with Osint;    use Osint;
with Table;
with Widechar; use Widechar;

with GNAT.HTable;

package body Fname.UF is

   --------------------------------------------------------
   -- Declarations for Handling Source_File_Name pragmas --
   --------------------------------------------------------

   type SFN_Entry is record
      U : Unit_Name_Type; -- Unit name
      F : File_Name_Type; -- Spec/Body file name
   end record;
   --  Record single Unit_Name type call to Set_File_Name

   package SFN_Table is new Table.Table (
     Table_Component_Type => SFN_Entry,
     Table_Index_Type     => Int,
     Table_Low_Bound      => 0,
     Table_Initial        => Alloc.SFN_Table_Initial,
     Table_Increment      => Alloc.SFN_Table_Increment,
     Table_Name           => "SFN_Table");
   --  Table recording all Unit_Name calls to Set_File_Name

   type SFN_Header_Num is range 0 .. 100;

   function SFN_Hash (F : Unit_Name_Type) return SFN_Header_Num;
   --  Compute hash index for use by Simple_HTable

   package SFN_HTable is new GNAT.HTable.Simple_HTable (
     Header_Num => SFN_Header_Num,
     Element    => Int,
     No_Element => -1,
     Key        => Unit_Name_Type,
     Hash       => SFN_Hash,
     Equal      => "=");
   --  Hash table allowing rapid access to SFN_Table, the element value
   --  is an index into this table.

   type SFN_Pattern_Entry is record
      Pat : String_Ptr;   -- File name pattern (with asterisk in it)
      Typ : Character;    -- 'S'/'B'/'U' for spec/body/subunit
      Dot : String_Ptr;   -- Dot_Separator string
      Cas : Casing_Type;  -- Upper/Lower/Mixed
   end record;
   --  Records single call to Set_File_Name_Patterm

   package SFN_Patterns is new Table.Table (
     Table_Component_Type => SFN_Pattern_Entry,
     Table_Index_Type     => Int,
     Table_Low_Bound      => 0,
     Table_Initial        => 10,
     Table_Increment      => 100,
     Table_Name           => "SFN_Patterns");
   --  Table recording all calls to Set_File_Name_Pattern. Note that the
   --  first two entries are set to represent the standard GNAT rules
   --  for file naming.

   -----------------------
   -- File_Name_Of_Body --
   -----------------------

   function File_Name_Of_Body (Name : Name_Id) return File_Name_Type is
   begin
      Get_Name_String (Name);
      Name_Buffer (Name_Len + 1 .. Name_Len + 2) := "%b";
      Name_Len := Name_Len + 2;
      return Get_File_Name (Name_Enter, Subunit => False);
   end File_Name_Of_Body;

   -----------------------
   -- File_Name_Of_Spec --
   -----------------------

   function File_Name_Of_Spec (Name : Name_Id) return File_Name_Type is
   begin
      Get_Name_String (Name);
      Name_Buffer (Name_Len + 1 .. Name_Len + 2) := "%s";
      Name_Len := Name_Len + 2;
      return Get_File_Name (Name_Enter, Subunit => False);
   end File_Name_Of_Spec;

   -------------------
   -- Get_File_Name --
   -------------------

   function Get_File_Name
     (Uname   : Unit_Name_Type;
      Subunit : Boolean)
      return    File_Name_Type
   is
      Unit_Char : Character;
      --  Set to 's' or 'b' for spec or body or to 'u' for a subunit

      N : Int;

   begin
      N := SFN_HTable.Get (Uname);

      if N /= -1 then
         return SFN_Table.Table (N).F;
      end if;

      --  Here for the case where the name was not found in the table

      Get_Decoded_Name_String (Uname);

      --  A special fudge, normally we don't have operator symbols present,
      --  since it is always an error to do so. However, if we do, at this
      --  stage it has a leading double quote.

      --  What we do in this case is to go back to the undecoded name, which
      --  is of the form, for example:

      --    Oand%s

      --  and build a file name that looks like:

      --    _and_.ads

      --  which is bit peculiar, but we keep it that way. This means that
      --  we avoid bombs due to writing a bad file name, and w get expected
      --  error processing downstream, e.g. a compilation following gnatchop.

      if Name_Buffer (1) = '"' then
         Get_Name_String (Uname);
         Name_Len := Name_Len + 1;
         Name_Buffer (Name_Len)     := Name_Buffer (Name_Len - 1);
         Name_Buffer (Name_Len - 1) := Name_Buffer (Name_Len - 2);
         Name_Buffer (Name_Len - 2) := '_';
         Name_Buffer (1)            := '_';
      end if;

      --  Deal with spec or body suffix

      Unit_Char := Name_Buffer (Name_Len);
      pragma Assert (Unit_Char = 'b' or else Unit_Char = 's');
      pragma Assert (Name_Len >= 3 and then Name_Buffer (Name_Len - 1) = '%');
      Name_Len := Name_Len - 2;

      if Subunit then
         Unit_Char := 'u';
      end if;

      --  Now we need to find the proper translation of the name

      declare
         Uname : constant String (1 .. Name_Len) :=
                   Name_Buffer (1 .. Name_Len);

         Pent : Nat;
         Plen : Natural;
         Fnam : File_Name_Type := No_File;
         J    : Natural;
         Dot  : String_Ptr;
         Dotl : Natural;

         function C (N : Natural) return Character;
         --  Return N'th character of pattern

         function C (N : Natural) return Character is
         begin
            return SFN_Patterns.Table (Pent).Pat (N);
         end C;

      --  Start of search through pattern table

      begin
         --  Search pattern table to find a matching entry. We will find
         --  at least one entry, since we set default entries for all
         --  unit types to represent the standard default GNAT names. We
         --  search in reverse order so that these are the last ones to try.

         <<Repeat_Search>>
         --  The search is repeated with Unit_Char set to b, if an initial
         --  search for the subunit case fails to find any match.

         Pent := SFN_Patterns.Last;
         while Pent >= SFN_Patterns.First loop
            if SFN_Patterns.Table (Pent).Typ = Unit_Char then
               Name_Len := 0;

               --  Found a match, execute the pattern

               Name_Len := Uname'Length;
               Name_Buffer (1 .. Name_Len) := Uname;
               Set_Casing (SFN_Patterns.Table (Pent).Cas);

               --  If dot translation required do it, being careful not to
               --  mess with dots that are part of a wide character encoding

               Dot  := SFN_Patterns.Table (Pent).Dot;
               Dotl := Dot.all'Length;

               if Dot.all /= "." then
                  J := 1;

                  while J <= Name_Len loop
                     if Name_Buffer (J) = '.' then

                        if Dotl = 1 then
                           Name_Buffer (J) := Dot (Dot'First);

                        else
                           Name_Buffer (J + Dotl .. Name_Len + Dotl - 1) :=
                             Name_Buffer (J + 1 .. Name_Len);
                           Name_Buffer (J .. J + Dotl - 1) := Dot.all;
                           Name_Len := Name_Len + Dotl - 1;
                        end if;

                        J := J + Dotl;

                     elsif Name_Buffer (J) = ASCII.ESC
                       or else (Upper_Half_Encoding
                                 and then
                                   Name_Buffer (J) in Upper_Half_Character)
                     then
                        Skip_Wide (Name_Buffer, J);
                     else
                        J := J + 1;
                     end if;
                  end loop;
               end if;

               --  Here move result to right if preinsertion before *

               Plen := SFN_Patterns.Table (Pent).Pat'Length;
               for K in 1 .. Plen loop
                  if C (K) = '*' then
                     if K /= 1 then
                        Name_Buffer (1 + K - 1 .. Name_Len + K - 1) :=
                          Name_Buffer (1 .. Name_Len);

                        for L in 1 .. K - 1 loop
                           Name_Buffer (L) := C (L);
                        end loop;

                        Name_Len := Name_Len + K - 1;
                     end if;

                     for L in K + 1 .. Plen loop
                        Name_Len := Name_Len + 1;
                        Name_Buffer (Name_Len) := C (L);
                     end loop;

                     exit;
                  end if;
               end loop;

               --  Execute possible crunch on constructed name. The krunch
               --  operation excludes any extension that may be present.

               J := Name_Len;
               while J > 1 loop
                  exit when Name_Buffer (J) = '.';
                  J := J - 1;
               end loop;

               --  Case of extension present

               if J > 1 then
                  declare
                     Ext : constant String := Name_Buffer (J .. Name_Len);

                  begin
                     --  Remove extension

                     Name_Len := J - 1;

                     --  Krunch what's left

                     Krunch
                       (Name_Buffer,
                        Name_Len,
                        Integer (Maximum_File_Name_Length),
                        Debug_Flag_4);

                     --  Replace extension

                     Name_Buffer
                       (Name_Len + 1 .. Name_Len + Ext'Length) := Ext;
                     Name_Len := Name_Len + Ext'Length;
                  end;

               --  Case of no extension present, straight krunch on whole name

               else
                  Krunch
                    (Name_Buffer,
                     Name_Len,
                     Integer (Maximum_File_Name_Length),
                     Debug_Flag_4);
               end if;

               Fnam := File_Name_Type (Name_Find);

               --  If this is a non-default entry for which we can locate
               --  the file, or if it is one of the default entries, we are
               --  done and can exit from the search.

               if Pent <= 2 or else Find_File (Fnam, Source) /= No_File then
                  return Fnam;
               else
                  Fnam := No_File;
               end if;
            end if;

            Pent := Pent - 1;
         end loop;

         if Fnam = No_Name and then Unit_Char = 'u' then
            Unit_Char := 'b';
            goto Repeat_Search;
         end if;

         --  Something is wrong if search fails for spec or body

         pragma Assert (False);
         raise Program_Error;

      end;
   end Get_File_Name;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      SFN_Table.Init;
      SFN_Patterns.Init;

      --  Add default entries to SFN_Patterns.Table to represent the
      --  standard default GNAT rules for file name translation.

      SFN_Patterns.Set_Last (2);

      SFN_Patterns.Table (1) :=
        (Pat => new String'("*.ads"),
         Typ => 's',
         Dot => new String'("-"),
         Cas => All_Lower_Case);

      SFN_Patterns.Table (2) :=
        (Pat => new String'("*.adb"),
         Typ => 'b',
         Dot => new String'("-"),
         Cas => All_Lower_Case);

   end Initialize;

   ----------
   -- Lock --
   ----------

   procedure Lock is
   begin
      SFN_Table.Locked := True;
      SFN_Table.Release;
   end Lock;

   -------------------
   -- Set_File_Name --
   -------------------

   procedure Set_File_Name (U : Unit_Name_Type; F : File_Name_Type) is
   begin
      SFN_Table.Increment_Last;
      SFN_Table.Table (SFN_Table.Last) := (U, F);
      SFN_HTable.Set (U, SFN_Table.Last);
   end Set_File_Name;

   ---------------------------
   -- Set_File_Name_Pattern --
   ---------------------------

   procedure Set_File_Name_Pattern
     (Pat : String_Ptr;
      Typ : Character;
      Dot : String_Ptr;
      Cas : Casing_Type)
   is
   begin
      SFN_Patterns.Increment_Last;
      SFN_Patterns.Table (SFN_Patterns.Last) := (Pat, Typ, Dot, Cas);
   end Set_File_Name_Pattern;

   --------------
   -- SFN_Hash --
   --------------

   function SFN_Hash (F : Unit_Name_Type) return SFN_Header_Num is
   begin
      return SFN_Header_Num (Int (F) rem SFN_Header_Num'Range_Length);
   end SFN_Hash;

begin

   --  We call the initialization routine from the package body, so that
   --  Fname.Init only needs to be called explicitly to reinitialize.

   Fname.UF.Initialize;
end Fname.UF;

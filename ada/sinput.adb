------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                               S I N P U T                                --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision$
--                                                                          --
--          Copyright (C) 1992-2000 Free Software Foundation, Inc.          --
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

with Debug;   use Debug;
with Namet;   use Namet;
with Output;  use Output;
with Tree_IO; use Tree_IO;
with System;  use System;

with Unchecked_Conversion;
with Unchecked_Deallocation;

package body Sinput is

   use ASCII;
   --  Make control characters visible

   First_Time_Around : Boolean := True;

   -----------------------
   -- Alloc_Lines_Table --
   -----------------------

   procedure Alloc_Lines_Table
     (S       : in out Source_File_Record;
      New_Max : Pos)
   is
      function realloc
        (memblock : Lines_Table_Ptr;
         size     : size_t)
         return     Lines_Table_Ptr;
      pragma Import (C, realloc);

      function malloc
        (size     : size_t)
         return     Lines_Table_Ptr;
      pragma Import (C, malloc);

      New_Table : Lines_Table_Ptr;

      New_Size : constant size_t :=
                   size_t (New_Max * Lines_Table_Type'Component_Size /
                                                             Storage_Unit);

   begin
      if S.Lines_Table = null then
         New_Table := malloc (New_Size);

      else
         New_Table := realloc (memblock => S.Lines_Table, size => New_Size);
      end if;

      if New_Table = null then
         raise Storage_Error;
      else
         S.Lines_Table     := New_Table;
         S.Lines_Table_Max := New_Max;
      end if;
   end Alloc_Lines_Table;

   -----------------
   -- Backup_Line --
   -----------------

   procedure Backup_Line (P : in out Source_Ptr) is
      Sindex : constant Source_File_Index := Get_Source_File_Index (P);
      Src    : constant Source_Buffer_Ptr :=
                 Source_File.Table (Sindex).Source_Text;
      Sfirst : constant Source_Ptr :=
                 Source_File.Table (Sindex).Source_First;

   begin
      P := P - 1;

      if P = Sfirst then
         return;
      end if;

      if Src (P) = CR then
         if Src (P - 1) = LF then
            P := P - 1;
         end if;

      else -- Src (P) = LF
         if Src (P - 1) = CR then
            P := P - 1;
         end if;
      end if;

      --  Now find first character of the previous line

      while P > Sfirst
        and then Src (P - 1) /= LF
        and then Src (P - 1) /= CR
      loop
         P := P - 1;
      end loop;
   end Backup_Line;

   ---------------------------
   -- Build_Location_String --
   ---------------------------

   procedure Build_Location_String (Loc : Source_Ptr) is
      Ptr : Source_Ptr;

   begin
      Name_Len := 0;

      --  Loop through instantiations

      Ptr := Loc;
      loop
         Get_Name_String_And_Append
           (Reference_Name (Get_Source_File_Index (Ptr)));
         Add_Char_To_Name_Buffer (':');
         Add_Nat_To_Name_Buffer (Nat (Get_Line_Number (Ptr)));

         Ptr := Instantiation_Location (Ptr);
         exit when Ptr = No_Location;
         Add_Str_To_Name_Buffer (" instantiated at ");
      end loop;

      Name_Buffer (Name_Len + 1) := NUL;
      return;
   end Build_Location_String;

   -------------------------------
   -- Increment_Line_Table_Size --
   -------------------------------

   procedure Increment_Line_Table_Size
     (S : in out Source_File_Record;
      P : Source_Ptr)
   is
   begin
      --  Reallocate the lines table if it has got too large. Note that
      --  we don't use the normal Table package mechanism because we
      --  have several of these tables, one for each source file.

      if S.Num_Source_Lines = S.Lines_Table_Max then
         Alloc_Lines_Table
           (S, S.Num_Source_Lines * ((100 + Alloc.Lines_Increment) / 100));

         if Debug_Flag_D then
            Write_Str ("--> Reallocating lines table, size = ");
            Write_Int (S.Lines_Table_Max);
            Write_Eol;
         end if;
      end if;

      S.Num_Source_Lines := S.Num_Source_Lines + 1;
      S.Lines_Table (S.Num_Source_Lines) := P;
   end Increment_Line_Table_Size;

   -----------------------
   -- Get_Column_Number --
   -----------------------

   function Get_Column_Number (P : Source_Ptr) return Column_Number is
      S      : Source_Ptr;
      C      : Column_Number;
      Sindex : Source_File_Index;
      Src    : Source_Buffer_Ptr;

   begin
      --  If the input source pointer is not a meaningful value then return
      --  at once with column number 1. This can happen for a file not found
      --  condition for a file loaded indirectly by RTE, and also perhaps on
      --  some unknown internal error conditions. In either case we certainly
      --  don't want to blow up.

      if P < 1 then
         return 1;

      else
         Sindex := Get_Source_File_Index (P);
         Src := Source_File.Table (Sindex).Source_Text;
         S := Line_Start (P);
         C := 1;

         while S < P loop
            if Src (S) = HT then
               C := (C - 1) / 8 * 8 + (8 + 1);
            else
               C := C + 1;
            end if;

            S := S + 1;
         end loop;

         return C;
      end if;
   end Get_Column_Number;

   ---------------------
   -- Get_Line_Number --
   ---------------------

   function Get_Line_Number (P : Source_Ptr) return Logical_Line_Number is
      Sfile : Source_File_Index;
      Table : Lines_Table_Ptr;
      Lo    : Nat;
      Hi    : Nat;
      Mid   : Nat;
      Loc   : Source_Ptr;

   begin
      --  If the input source pointer is not a meaningful value then return
      --  at once with line number 1. This can happen for a file not found
      --  condition for a file loaded indirectly by RTE, and also perhaps on
      --  some unknown internal error conditions. In either case we certainly
      --  don't want to blow up.

      if P < 1 then
         return 1;

      --  Otherwise we can do the binary search

      else
         Sfile := Get_Source_File_Index (P);
         Loc   := P + Source_File.Table (Sfile).Sloc_Adjust;
         Table := Source_File.Table (Sfile).Lines_Table;
         Lo    := 1;
         Hi    := Source_File.Table (Sfile).Num_Source_Lines;

         loop
            Mid := (Lo + Hi) / 2;

            if Loc < Table (Mid) then
               Hi := Mid - 1;

            else -- Loc >= Table (Mid)

               if Mid = Hi or else
                  Loc < Table (Mid + 1)
               then
                  return Logical_Line_Number (Mid + Line_Offset (Sfile));
               else
                  Lo := Mid + 1;
               end if;

            end if;

         end loop;
      end if;
   end Get_Line_Number;

   ---------------------------
   -- Get_Source_File_Index --
   ---------------------------

   Source_Cache_First : Source_Ptr := 1;
   Source_Cache_Last  : Source_Ptr := 0;
   --  Records the First and Last subscript values for the most recently
   --  referenced entry in the source table, to optimize the common case
   --  of repeated references to the same entry. The initial values force
   --  an initial search to set the cache value.

   Source_Cache_Index : Source_File_Index := No_Source_File;
   --  Contains the index of the entry corresponding to Source_Cache

   function Get_Source_File_Index
     (S    : Source_Ptr)
      return Source_File_Index
   is
   begin
      if S in Source_Cache_First .. Source_Cache_Last then
         return Source_Cache_Index;

      else
         for J in 1 .. Source_File.Last loop
            if S in Source_File.Table (J).Source_First ..
                    Source_File.Table (J).Source_Last
            then
               Source_Cache_Index := J;
               Source_Cache_First :=
                 Source_File.Table (Source_Cache_Index).Source_First;
               Source_Cache_Last :=
                 Source_File.Table (Source_Cache_Index).Source_Last;
               return Source_Cache_Index;
            end if;
         end loop;
      end if;

      pragma Assert (False);
      raise Program_Error;
   end Get_Source_File_Index;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      Source_File.Init;
   end Initialize;

   -------------------------
   -- Instantiation_Depth --
   -------------------------

   function Instantiation_Depth (S : Source_Ptr) return Nat is
      Sind  : Source_File_Index;
      Sval  : Source_Ptr;
      Depth : Nat;

   begin
      Sval := S;
      Depth := 0;

      loop
         Sind := Get_Source_File_Index (Sval);
         Sval := Instantiation (Sind);
         exit when Sval = No_Location;
         Depth := Depth + 1;
      end loop;

      return Depth;
   end Instantiation_Depth;

   ----------------------------
   -- Instantiation_Location --
   ----------------------------

   function Instantiation_Location (S : Source_Ptr) return Source_Ptr is
   begin
      return Instantiation (Get_Source_File_Index (S));
   end Instantiation_Location;

   ----------
   -- Lock --
   ----------

   procedure Lock is
   begin
      Source_File.Locked := True;
      Source_File.Release;
   end Lock;

   ----------------------
   -- Last_Source_File --
   ----------------------

   function Last_Source_File return Source_File_Index is
   begin
      return Source_File.Last;
   end Last_Source_File;

   ----------------
   -- Line_Start --
   ----------------

   function Line_Start (P : Source_Ptr) return Source_Ptr is
      Sindex : constant Source_File_Index := Get_Source_File_Index (P);
      Src    : constant Source_Buffer_Ptr :=
                 Source_File.Table (Sindex).Source_Text;
      Sfirst : constant Source_Ptr :=
                 Source_File.Table (Sindex).Source_First;
      S      : Source_Ptr;

   begin
      S := P;

      while S > Sfirst
        and then Src (S - 1) /= CR
        and then Src (S - 1) /= LF
      loop
         S := S - 1;
      end loop;

      return S;
   end Line_Start;

   function Line_Start
     (L    : Logical_Line_Number;
      S    : Source_File_Index)
      return Source_Ptr
   is
   begin
      return Source_File.Table (S).Lines_Table (Logical_To_Physical (L, S));
   end Line_Start;

   -------------------------
   -- Logical_To_Physical --
   -------------------------

   function Logical_To_Physical
     (Line : Logical_Line_Number;
      S    : Source_File_Index)
      return Nat
   is
   begin
      --  Line zero is the source reference pragma itself

      if Line = 0 then
         return 1;
      else
         return Nat (Line) - Line_Offset (S);
      end if;
   end Logical_To_Physical;

   ----------------------
   -- Num_Source_Files --
   ----------------------

   function Num_Source_Files return Nat is
   begin
      return Int (Source_File.Last) - Int (Source_File.First) + 1;
   end Num_Source_Files;

   -----------------------
   -- Original_Location --
   -----------------------

   function Original_Location (S : Source_Ptr) return Source_Ptr is
      Sindex : Source_File_Index;
      Tindex : Source_File_Index;

   begin
      if S <= No_Location then
         return S;

      else
         Sindex := Get_Source_File_Index (S);

         if Instantiation (Sindex) = No_Location then
            return S;

         else
            Tindex := Template (Sindex);
            while Instantiation (Tindex) /= No_Location loop
               Tindex := Template (Tindex);
            end loop;

            return S - Source_First (Sindex) + Source_First (Tindex);
         end if;
      end if;
   end Original_Location;

   -------------------------
   -- Physical_To_Logical --
   -------------------------

   function Physical_To_Logical
     (Line : Nat;
      S    : Source_File_Index)
      return Logical_Line_Number
   is
   begin
      if not Has_Line_Offset (S) then
         return Logical_Line_Number (Line);

      elsif Line = 1 then
         return 0;

      else
         return Logical_Line_Number (Line + Line_Offset (S));
      end if;
   end Physical_To_Logical;

   ---------------------------
   -- Skip_Line_Terminators --
   ---------------------------

   --  There are two distinct concepts of line terminator in GNAT

   --    A logical line terminator is what corresponds to the "end of a line"
   --    as described in RM 2.2 (13). Any of the characters FF, LF, CR or VT
   --    acts as an end of logical line in this sense, and it is essentially
   --    irrelevant whether one or more appears in sequence (since if a
   --    sequence of such characters is regarded as separate ends of line,
   --    then the intervening logical lines are null in any case).

   --    A physical line terminator is a sequence of format effectors that
   --    is treated as ending a physical line. Physical lines have no Ada
   --    semantic significance, but they are significant for error reporting
   --    purposes, since errors are identified by line and column location.

   --  In GNAT, a physical line is ended by any of the sequences LF, CR/LF,
   --  CR or LF/CR. LF is used in typical Unix systems, CR/LF in DOS systems,
   --  and CR alone in System 7. We don't know of any system using LF/CR, but
   --  it seems reasonable to include this case for consistency. In addition,
   --  we recognize any of these sequences in any of the operating systems,
   --  for better behavior in treating foreign files (e.g. a Unix file with
   --  LF terminators transferred to a DOS system).

   procedure Skip_Line_Terminators
     (P        : in out Source_Ptr;
      Physical : out Boolean)
   is
   begin
      pragma Assert (Source (P) in Line_Terminator);

      if Source (P) = CR then
         if Source (P + 1) = LF then
            P := P + 2;
         else
            P := P + 1;
         end if;

      elsif Source (P) = LF then
         if Source (P + 1) = CR then
            P := P + 2;
         else
            P := P + 1;
         end if;

      else -- Source (P) = FF or else Source (P) = VT
         P := P + 1;
         Physical := False;
         return;
      end if;

      --  Fall through in the physical line terminator case. First deal with
      --  making a possible entry into the lines table if one is needed.

      --  Note: we are dealing with a real source file here, this cannot be
      --  the instantiation case, so we need not worry about Sloc adjustment.

      declare
         S : Source_File_Record
               renames Source_File.Table (Current_Source_File);

      begin
         Physical := True;

         --  Make entry in lines table if not already made (in some scan backup
         --  cases, we will be rescanning previously scanned source, so the
         --  entry may have already been made on the previous forward scan).

         if Source (P) /= EOF
           and then P > S.Lines_Table (S.Num_Source_Lines)
         then
            Increment_Line_Table_Size (S, P);
         end if;
      end;
   end Skip_Line_Terminators;

   -------------------
   -- Source_Offset --
   -------------------

   function Source_Offset (S : Source_Ptr) return Nat is
      Sindex : constant Source_File_Index := Get_Source_File_Index (S);
      Sfirst : constant Source_Ptr :=
                 Source_File.Table (Sindex).Source_First;

   begin
      return Nat (S - Sfirst);
   end Source_Offset;

   ------------------------
   -- Top_Level_Location --
   ------------------------

   function Top_Level_Location (S : Source_Ptr) return Source_Ptr is
      Oldloc : Source_Ptr;
      Newloc : Source_Ptr;

   begin
      Newloc := S;
      loop
         Oldloc := Newloc;
         Newloc := Instantiation_Location (Oldloc);
         exit when Newloc = No_Location;
      end loop;

      return Oldloc;
   end Top_Level_Location;

   ---------------
   -- Tree_Read --
   ---------------

   procedure Tree_Read is
   begin
      --  First we must free any old source buffer pointers

      if not First_Time_Around then
         for J in Source_File.First .. Source_File.Last loop
            declare
               S : Source_File_Record renames Source_File.Table (J);

               procedure Free_Ptr is new Unchecked_Deallocation
                 (Big_Source_Buffer, Source_Buffer_Ptr);

               procedure free (X : Lines_Table_Ptr);
               pragma Import (C, free);

               function To_Source_Buffer_Ptr is new
                 Unchecked_Conversion (Address, Source_Buffer_Ptr);

               Tmp1 : Source_Buffer_Ptr;

            begin
               if S.Instantiation /= No_Location then
                  null;

               else
                  --  We have to recreate a proper pointer to the actual array
                  --  from the zero origin pointer stored in the source table.

                  Tmp1 :=
                    To_Source_Buffer_Ptr
                      (S.Source_Text (S.Source_First)'Address);
                  Free_Ptr (Tmp1);

                  if S.Lines_Table /= null then
                     free (S.Lines_Table);
                     S.Lines_Table := null;
                  end if;
               end if;
            end;
         end loop;
      end if;

      --  Reset source cache pointers to force new read

      Source_Cache_First := 1;
      Source_Cache_Last  := 0;

      --  Read in source file table

      Source_File.Tree_Read;

      --  The pointers we read in there for the source buffer and lines
      --  table pointers are junk. We now read in the actual data that
      --  is referenced by these two fields.

      for J in Source_File.First .. Source_File.Last loop
         declare
            S : Source_File_Record renames Source_File.Table (J);

         begin
            --  For the instantiation case, we do not read in any data. Instead
            --  we share the data for the generic template entry. Since the
            --  template always occurs first, we can safetly refer to its data.

            if S.Instantiation /= No_Location then
               declare
                  ST : Source_File_Record renames
                         Source_File.Table (S.Template);

               begin
                  --  The lines table is simply copied from the template entry

                  S.Lines_Table := Source_File.Table (S.Template).Lines_Table;

                  --  In the case of the source table pointer, we share the
                  --  same data as the generic template, but the virtual origin
                  --  is adjusted. For example, if the first subscript of the
                  --  template is 100, and that of the instantiation is 200,
                  --  then the instantiation pointer is obtained by subtracting
                  --  100 from the template pointer.

                  declare
                     pragma Suppress (All_Checks);

                     function To_Source_Buffer_Ptr is new
                       Unchecked_Conversion (Address, Source_Buffer_Ptr);

                  begin
                     S.Source_Text :=
                       To_Source_Buffer_Ptr
                          (ST.Source_Text
                            (ST.Source_First - S.Source_First)'Address);
                  end;
               end;

            --  Normal case (non-instantiation)

            else
               First_Time_Around := False;
               S.Lines_Table := null;
               Alloc_Lines_Table (S, S.Num_Source_Lines);

               for J in 1 .. S.Num_Source_Lines loop
                  Tree_Read_Int (Int (S.Lines_Table (J)));
               end loop;

               --  Allocate source buffer and read in the data and then set the
               --  virtual origin to point to the logical zero'th element. This
               --  address must be computed with subscript checks turned off.

               declare
                  subtype B is Text_Buffer (S.Source_First .. S.Source_Last);
                  type Text_Buffer_Ptr is access B;
                  T : Text_Buffer_Ptr;

                  pragma Suppress (All_Checks);

                  function To_Source_Buffer_Ptr is new
                    Unchecked_Conversion (Address, Source_Buffer_Ptr);

               begin
                  T := new B;

                  Tree_Read_Data (T (S.Source_First)'Address,
                     Int (S.Source_Last) - Int (S.Source_First) + 1);

                  S.Source_Text := To_Source_Buffer_Ptr (T (0)'Address);
               end;
            end if;
         end;
      end loop;
   end Tree_Read;

   ----------------
   -- Tree_Write --
   ----------------

   procedure Tree_Write is
   begin
      Source_File.Tree_Write;

      --  The pointers we wrote out there for the source buffer and lines
      --  table pointers are junk, we now write out the actual data that
      --  is referenced by these two fields.

      for J in Source_File.First .. Source_File.Last loop
         declare
            S : Source_File_Record renames Source_File.Table (J);

         begin
            --  For instantiations, there is nothing to do, since the data is
            --  shared with the generic template. When the tree is read, the
            --  pointers must be set, but no extra data needs to be written.

            if S.Instantiation /= No_Location then
               null;

            --  For the normal case, write out the data of the two tables

            else
               for J in 1 .. S.Num_Source_Lines loop
                  Tree_Write_Int (Int (S.Lines_Table (J)));
               end loop;

               Tree_Write_Data
                 (S.Source_Text (S.Source_First)'Address,
                   Int (S.Source_Last) - Int (S.Source_First) + 1);
            end if;
         end;
      end loop;
   end Tree_Write;

   --------------------
   -- Write_Location --
   --------------------

   procedure Write_Location (P : Source_Ptr) is
   begin
      if P = No_Location then
         Write_Str ("<no location>");

      elsif P <= Standard_Location then
         Write_Str ("<standard location>");

      else
         declare
            SI : constant Source_File_Index := Get_Source_File_Index (P);

         begin
            Write_Name (Debug_Source_Name (SI));
            Write_Char (':');
            Write_Int (Int (Get_Line_Number (P)));
            Write_Char (':');
            Write_Int (Int (Get_Column_Number (P)));

            if Instantiation (SI) /= No_Location then
               Write_Str (" [");
               Write_Location (Instantiation (SI));
               Write_Char (']');
            end if;
         end;
      end if;
   end Write_Location;

   ----------------------
   -- Write_Time_Stamp --
   ----------------------

   procedure Write_Time_Stamp (S : Source_File_Index) is
      T : constant Time_Stamp_Type := Time_Stamp (S);
      P : Natural;

   begin
      if T (1) = '9' then
         Write_Str ("19");
         P := 0;
      else
         Write_Char (T (1));
         Write_Char (T (2));
         P := 2;
      end if;

      Write_Char (T (P + 1));
      Write_Char (T (P + 2));
      Write_Char ('-');

      Write_Char (T (P + 3));
      Write_Char (T (P + 4));
      Write_Char ('-');

      Write_Char (T (P + 5));
      Write_Char (T (P + 6));
      Write_Char (' ');

      Write_Char (T (P + 7));
      Write_Char (T (P + 8));
      Write_Char (':');

      Write_Char (T (P + 9));
      Write_Char (T (P + 10));
      Write_Char (':');

      Write_Char (T (P + 11));
      Write_Char (T (P + 12));
   end Write_Time_Stamp;

   ----------------------------------------------
   -- Access Subprograms for Source File Table --
   ----------------------------------------------

   function File_Name (S : Source_File_Index) return File_Name_Type is
   begin
      return Source_File.Table (S).File_Name;
   end File_Name;

   function Debug_Source_Name (S : Source_File_Index) return File_Name_Type is
   begin
      return Source_File.Table (S).Debug_Source_Name;
   end Debug_Source_Name;

   function Full_File_Name (S : Source_File_Index) return File_Name_Type is
   begin
      return Source_File.Table (S).Full_File_Name;
   end Full_File_Name;

   function Full_Ref_Name (S : Source_File_Index) return File_Name_Type is
   begin
      return Source_File.Table (S).Full_Ref_Name;
   end Full_Ref_Name;

   function Has_Line_Offset (S : Source_File_Index) return Boolean is
   begin
      return Source_File.Table (S).Has_Line_Offset;
   end Has_Line_Offset;

   function Identifier_Casing (S : Source_File_Index) return Casing_Type is
   begin
      return Source_File.Table (S).Identifier_Casing;
   end Identifier_Casing;

   function Instantiation (S : Source_File_Index) return Source_Ptr is
   begin
      return Source_File.Table (S).Instantiation;
   end Instantiation;

   function Keyword_Casing (S : Source_File_Index) return Casing_Type is
   begin
      return Source_File.Table (S).Keyword_Casing;
   end Keyword_Casing;

   function Line_Offset (S : Source_File_Index) return Int is
   begin
      return Source_File.Table (S).Line_Offset;
   end Line_Offset;

   function Num_Source_Lines (S : Source_File_Index) return Nat is
   begin
      return Source_File.Table (S).Num_Source_Lines;
   end Num_Source_Lines;

   function Reference_Name (S : Source_File_Index) return File_Name_Type is
   begin
      return Source_File.Table (S).Reference_Name;
   end Reference_Name;

   function Source_Checksum (S : Source_File_Index) return Word is
   begin
      return Source_File.Table (S).Source_Checksum;
   end Source_Checksum;

   function Source_First (S : Source_File_Index) return Source_Ptr is
   begin
      return Source_File.Table (S).Source_First;
   end Source_First;

   function Source_Last (S : Source_File_Index) return Source_Ptr is
   begin
      return Source_File.Table (S).Source_Last;
   end Source_Last;

   function Source_Text (S : Source_File_Index) return Source_Buffer_Ptr is
   begin
      return Source_File.Table (S).Source_Text;
   end Source_Text;

   function Template (S : Source_File_Index) return Source_File_Index is
   begin
      return Source_File.Table (S).Template;
   end Template;

   function Time_Stamp (S : Source_File_Index) return Time_Stamp_Type is
   begin
      return Source_File.Table (S).Time_Stamp;
   end Time_Stamp;

   ------------------------------------------
   -- Set Procedures for Source File Table --
   ------------------------------------------

   procedure Set_Keyword_Casing (S : Source_File_Index; C : Casing_Type) is
   begin
      Source_File.Table (S).Keyword_Casing := C;
   end Set_Keyword_Casing;

   procedure Set_Has_Line_Offset (S : Source_File_Index; V : Boolean) is
   begin
      Source_File.Table (S).Has_Line_Offset := V;
   end Set_Has_Line_Offset;

   procedure Set_Identifier_Casing (S : Source_File_Index; C : Casing_Type) is
   begin
      Source_File.Table (S).Identifier_Casing := C;
   end Set_Identifier_Casing;

   procedure Set_Line_Offset (S : Source_File_Index; V : Int) is
   begin
      Source_File.Table (S).Line_Offset := V;
   end Set_Line_Offset;

   procedure Set_Reference_Name (S : Source_File_Index; N : Name_Id) is
   begin
      Source_File.Table (S).Reference_Name := N;
   end Set_Reference_Name;

   procedure Set_Full_Ref_Name (S : Source_File_Index; N : Name_Id) is
   begin
      Source_File.Table (S).Full_Ref_Name := N;
   end Set_Full_Ref_Name;

   procedure Set_Debug_Source_Name (S : Source_File_Index; N : Name_Id) is
   begin
      Source_File.Table (S).Debug_Source_Name := N;
   end Set_Debug_Source_Name;

end Sinput;

------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                                N A M E T                                 --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision$                             --
--                                                                          --
--          Copyright (C) 1992-1997 Free Software Foundation, Inc.          --
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

--  WARNING: There is a C version of this package. Any changes to this
--  source file must be properly reflected in the C header file a-namet.h
--  which is created manually from namet.ads and namet.adb.

with Alloc;
with Debug;    use Debug;
with Output;   use Output;
with Table;
with Tree_IO;  use Tree_IO;
with Widechar; use Widechar;

package body Namet is

   --  This table stores the actual string names. Although logically there
   --  is no need for a terminating character (since the length is stored
   --  in the name entry table), we still store a NUL character at the end
   --  of every name (for convenience in interfacing to the C world).

   package Name_Chars is new Table.Table (
     Table_Component_Type => Character,
     Table_Index_Type     => Int,
     Table_Low_Bound      => 0,
     Table_Initial        => Alloc.Name_Chars_Initial,
     Table_Increment      => Alloc.Name_Chars_Increment,
     Table_Name           => "Name_Chars");

   type Name_Entry is record
      Name_Chars_Index : Int;
      --  Starting location of characters in the Name_Chars table minus
      --  one (i.e. pointer to character just before first character). The
      --  reason for the bias of one is that indexes in Name_Buffer are
      --  one's origin, so this avoids unnecessary adds and subtracts of 1.

      Name_Len : Short;
      --  Length of this name in characters

      Byte_Info : Byte;
      --  Byte value associated with this name

      Hash_Link : Name_Id;
      --  Link to next entry in names table for same hash code

      Int_Info : Int;
      --  Int Value associated with this name
   end record;

   --  This is the table that is referenced by Name_Id entries.
   --  It contains one entry for each unique name in the table.

   package Name_Entries is new Table.Table (
     Table_Component_Type => Name_Entry,
     Table_Index_Type     => Name_Id,
     Table_Low_Bound      => First_Name_Id,
     Table_Initial        => Alloc.Names_Initial,
     Table_Increment      => Alloc.Names_Increment,
     Table_Name           => "Name_Entries");

   Name_Chars_Reserve   : constant := 5000;
   Name_Entries_Reserve : constant := 100;
   --  The names table is locked during gigi processing, since gigi assumes
   --  that the table does not move. After returning from gigi, the names
   --  table is unlocked again, since writing library file information needs
   --  to generate some extra names. To avoid the inefficiency of always
   --  reallocating during this second unlocked phase, we reserve a bit of
   --  extra space before doing the release call.

   Hash_Num : constant Int := 2**12;
   --  Number of headers in the hash table. Current hash algorithm is closely
   --  tailored to this choice, so it can only be changed if a corresponding
   --  change is made to the hash alogorithm.

   Hash_Max : constant Int := Hash_Num - 1;
   --  Indexes in the hash header table run from 0 to Hash_Num - 1

   subtype Hash_Index_Type is Int range 0 .. Hash_Max;
   --  Range of hash index values

   Hash_Table : array (Hash_Index_Type) of Name_Id;
   --  The hash table is used to locate existing entries in the names table.
   --  The entries point to the first names table entry whose hash value
   --  matches the hash code. Then subsequent names table entries with the
   --  same hash code value are linked through the Hash_Link fields.

   -----------------------
   -- Local Subprograms --
   -----------------------

   function Hash return Hash_Index_Type;
   pragma Inline (Hash);
   --  Compute hash code for name stored in Name_Buffer (length in Name_Len)

   ----------------------------
   -- Add_Nat_To_Name_Buffer --
   ----------------------------

   procedure Add_Nat_To_Name_Buffer (V : Nat) is
   begin
      if V >= 10 then
         Add_Nat_To_Name_Buffer (V / 10);
      end if;

      Name_Len := Name_Len + 1;
      Name_Buffer (Name_Len) := Character'Val (Character'Pos ('0') + V rem 10);
   end Add_Nat_To_Name_Buffer;

   ----------------------------
   -- Add_Str_To_Name_Buffer --
   ----------------------------

   procedure Add_Str_To_Name_Buffer (S : String) is
   begin
      Name_Buffer (Name_Len + 1 .. Name_Len + S'Length) := S;
      Name_Len := Name_Len + S'Length;
   end Add_Str_To_Name_Buffer;

   ----------
   -- Hash --
   ----------

   function Hash return Hash_Index_Type is
      subtype Int_1_12 is Int range 1 .. 12;
      --  Used to avoid when others on case jump below

      Even_Name_Len : Integer;
      --  Last even numbered position (used for >12 case)

   begin

      --  Special test for 12 (rather than counting on a when others for the
      --  case statement below) avoids some Ada compilers converting the case
      --  statement into successive jumps.

      --  The case of a name longer than 12 characters is handled by taking
      --  the first 6 odd numbered characters and the last 6 even numbered
      --  characters

      if Name_Len > 12 then
         Even_Name_Len := (Name_Len) / 2 * 2;

         return ((((((((((((
           Character'Pos (Name_Buffer (01))) * 2 +
           Character'Pos (Name_Buffer (Even_Name_Len - 10))) * 2 +
           Character'Pos (Name_Buffer (03))) * 2 +
           Character'Pos (Name_Buffer (Even_Name_Len - 08))) * 2 +
           Character'Pos (Name_Buffer (05))) * 2 +
           Character'Pos (Name_Buffer (Even_Name_Len - 06))) * 2 +
           Character'Pos (Name_Buffer (07))) * 2 +
           Character'Pos (Name_Buffer (Even_Name_Len - 04))) * 2 +
           Character'Pos (Name_Buffer (09))) * 2 +
           Character'Pos (Name_Buffer (Even_Name_Len - 02))) * 2 +
           Character'Pos (Name_Buffer (11))) * 2 +
           Character'Pos (Name_Buffer (Even_Name_Len))) mod Hash_Num;
      end if;

      --  For the cases of 1-12 characters, all characters participate in the
      --  hash. The positioning is randomized, with the bias that characters
      --  later on participate fully (i.e. are added towards the right side).

      case Int_1_12 (Name_Len) is

         when 1 =>
            return
               Character'Pos (Name_Buffer (1));

         when 2 =>
            return ((
              Character'Pos (Name_Buffer (1))) * 64 +
              Character'Pos (Name_Buffer (2))) mod Hash_Num;

         when 3 =>
            return (((
              Character'Pos (Name_Buffer (1))) * 16 +
              Character'Pos (Name_Buffer (3))) * 16 +
              Character'Pos (Name_Buffer (2))) mod Hash_Num;

         when 4 =>
            return ((((
              Character'Pos (Name_Buffer (1))) * 8 +
              Character'Pos (Name_Buffer (2))) * 8 +
              Character'Pos (Name_Buffer (3))) * 8 +
              Character'Pos (Name_Buffer (4))) mod Hash_Num;

         when 5 =>
            return (((((
              Character'Pos (Name_Buffer (4))) * 8 +
              Character'Pos (Name_Buffer (1))) * 4 +
              Character'Pos (Name_Buffer (3))) * 4 +
              Character'Pos (Name_Buffer (5))) * 8 +
              Character'Pos (Name_Buffer (2))) mod Hash_Num;

         when 6 =>
            return ((((((
              Character'Pos (Name_Buffer (5))) * 4 +
              Character'Pos (Name_Buffer (1))) * 4 +
              Character'Pos (Name_Buffer (4))) * 4 +
              Character'Pos (Name_Buffer (2))) * 4 +
              Character'Pos (Name_Buffer (6))) * 4 +
              Character'Pos (Name_Buffer (3))) mod Hash_Num;

         when 7 =>
            return (((((((
              Character'Pos (Name_Buffer (4))) * 4 +
              Character'Pos (Name_Buffer (3))) * 4 +
              Character'Pos (Name_Buffer (1))) * 4 +
              Character'Pos (Name_Buffer (2))) * 2 +
              Character'Pos (Name_Buffer (5))) * 2 +
              Character'Pos (Name_Buffer (7))) * 2 +
              Character'Pos (Name_Buffer (6))) mod Hash_Num;

         when 8 =>
            return ((((((((
              Character'Pos (Name_Buffer (2))) * 4 +
              Character'Pos (Name_Buffer (1))) * 4 +
              Character'Pos (Name_Buffer (3))) * 2 +
              Character'Pos (Name_Buffer (5))) * 2 +
              Character'Pos (Name_Buffer (7))) * 2 +
              Character'Pos (Name_Buffer (6))) * 2 +
              Character'Pos (Name_Buffer (4))) * 2 +
              Character'Pos (Name_Buffer (8))) mod Hash_Num;

         when 9 =>
            return (((((((((
              Character'Pos (Name_Buffer (2))) * 4 +
              Character'Pos (Name_Buffer (1))) * 4 +
              Character'Pos (Name_Buffer (3))) * 4 +
              Character'Pos (Name_Buffer (4))) * 2 +
              Character'Pos (Name_Buffer (8))) * 2 +
              Character'Pos (Name_Buffer (7))) * 2 +
              Character'Pos (Name_Buffer (5))) * 2 +
              Character'Pos (Name_Buffer (6))) * 2 +
              Character'Pos (Name_Buffer (9))) mod Hash_Num;

         when 10 =>
            return ((((((((((
              Character'Pos (Name_Buffer (01))) * 2 +
              Character'Pos (Name_Buffer (02))) * 2 +
              Character'Pos (Name_Buffer (08))) * 2 +
              Character'Pos (Name_Buffer (03))) * 2 +
              Character'Pos (Name_Buffer (04))) * 2 +
              Character'Pos (Name_Buffer (09))) * 2 +
              Character'Pos (Name_Buffer (06))) * 2 +
              Character'Pos (Name_Buffer (05))) * 2 +
              Character'Pos (Name_Buffer (07))) * 2 +
              Character'Pos (Name_Buffer (10))) mod Hash_Num;

         when 11 =>
            return (((((((((((
              Character'Pos (Name_Buffer (05))) * 2 +
              Character'Pos (Name_Buffer (01))) * 2 +
              Character'Pos (Name_Buffer (06))) * 2 +
              Character'Pos (Name_Buffer (09))) * 2 +
              Character'Pos (Name_Buffer (07))) * 2 +
              Character'Pos (Name_Buffer (03))) * 2 +
              Character'Pos (Name_Buffer (08))) * 2 +
              Character'Pos (Name_Buffer (02))) * 2 +
              Character'Pos (Name_Buffer (10))) * 2 +
              Character'Pos (Name_Buffer (04))) * 2 +
              Character'Pos (Name_Buffer (11))) mod Hash_Num;

         when 12 =>
            return ((((((((((((
              Character'Pos (Name_Buffer (03))) * 2 +
              Character'Pos (Name_Buffer (02))) * 2 +
              Character'Pos (Name_Buffer (05))) * 2 +
              Character'Pos (Name_Buffer (01))) * 2 +
              Character'Pos (Name_Buffer (06))) * 2 +
              Character'Pos (Name_Buffer (04))) * 2 +
              Character'Pos (Name_Buffer (08))) * 2 +
              Character'Pos (Name_Buffer (11))) * 2 +
              Character'Pos (Name_Buffer (07))) * 2 +
              Character'Pos (Name_Buffer (09))) * 2 +
              Character'Pos (Name_Buffer (10))) * 2 +
              Character'Pos (Name_Buffer (12))) mod Hash_Num;

      end case;
   end Hash;

   --------------
   -- Finalize --
   --------------

   procedure Finalize is
      Max_Chain_Length : constant := 50;
      --  Max length of chains for which specific information is output

      F : array (Int range 0 .. Max_Chain_Length) of Int;
      --  N'th entry is number of chains of length N

      Probes : Int := 0;
      --  Used to compute average number of probes

      Nsyms : Int := 0;
      --  Number of symbols in table

   begin
      if Debug_Flag_H then

         for J in F'Range loop
            F (J) := 0;
         end loop;

         for I in Hash_Index_Type loop
            if Hash_Table (I) = No_Name then
               F (0) := F (0) + 1;

            else
               Write_Str ("Hash_Table (");
               Write_Int (Int (I));
               Write_Str (") has ");

               declare
                  C : Int := 1;
                  N : Name_Id;
                  S : Int;

               begin
                  C := 0;
                  N := Hash_Table (I);

                  while N /= No_Name loop
                     N := Name_Entries.Table (N).Hash_Link;
                     C := C + 1;
                  end loop;

                  Write_Int (C);
                  Write_Str (" entries");
                  Write_Eol;

                  if C < Max_Chain_Length then
                     F (C) := F (C) + 1;
                  else
                     F (Max_Chain_Length) := F (Max_Chain_Length) + 1;
                  end if;

                  N := Hash_Table (I);

                  while N /= No_Name loop
                     S := Name_Entries.Table (N).Name_Chars_Index;
                     Write_Str ("      ");

                     for J in 1 .. Name_Entries.Table (N).Name_Len loop
                        Write_Char (Name_Chars.Table (S + Int (J)));
                     end loop;

                     Write_Eol;
                     N := Name_Entries.Table (N).Hash_Link;
                  end loop;
               end;
            end if;
         end loop;

         Write_Eol;

         for I in Int range 0 .. Max_Chain_Length loop
            if F (I) /= 0 then
               Write_Str ("Number of hash chains of length ");

               if I < 10 then
                  Write_Char (' ');
               end if;

               Write_Int (I);

               if I = Max_Chain_Length then
                  Write_Str (" or greater");
               end if;

               Write_Str (" = ");
               Write_Int (F (I));
               Write_Eol;

               if I /= 0 then
                  Nsyms := Nsyms + F (I);
                  Probes := Probes + F (I) * (1 + I) * 100;
               end if;
            end if;
         end loop;

         Write_Eol;
         Write_Str ("Average number of probes for lookup = ");
         Probes := Probes / Nsyms;
         Write_Int (Probes / 200);
         Write_Char ('.');
         Probes := (Probes mod 200) / 2;
         Write_Char (Character'Val (48 + Probes / 10));
         Write_Char (Character'Val (48 + Probes mod 10));
         Write_Eol;
         Write_Eol;
      end if;
   end Finalize;

   ---------------------
   -- Get_Name_String --
   ---------------------

   procedure Get_Name_String (Id : Name_Id) is
      S : Int;

   begin
      pragma Assert (Id in Name_Entries.First .. Name_Entries.Last);

      S := Name_Entries.Table (Id).Name_Chars_Index;
      Name_Len := Natural (Name_Entries.Table (Id).Name_Len);

      for J in 1 .. Name_Len loop
         Name_Buffer (J) := Name_Chars.Table (S + Int (J));
      end loop;
   end Get_Name_String;

   --------------------------------
   -- Get_Name_String_And_Append --
   --------------------------------

   procedure Get_Name_String_And_Append (Id : Name_Id) is
      S : Int;

   begin
      pragma Assert (Id in Name_Entries.First .. Name_Entries.Last);

      S := Name_Entries.Table (Id).Name_Chars_Index;

      for J in 1 .. Natural (Name_Entries.Table (Id).Name_Len) loop
         Name_Len := Name_Len + 1;
         Name_Buffer (Name_Len) := Name_Chars.Table (S + Int (J));
      end loop;
   end Get_Name_String_And_Append;

   -----------------------------
   -- Get_Decoded_Name_String --
   -----------------------------

   procedure Get_Decoded_Name_String (Id : Name_Id) is
   begin
      Get_Name_String (Id);

      --  Case of operator name

      if Name_Buffer (1) = 'O' then
         Name_Buffer (1) := '"';

         declare
            --  This table maps the 2nd and 3rd characters of the name into
            --  the required output. Two blanks means leave the name alone

            Map : constant String :=
               "ab  " &                   --  Oabs         => "abs"
               "ad+ " &                   --  Oadd         => "+"
               "an  " &                   --  Oand         => "and"
               "co& " &                   --  Oconcat      => "&"
               "di/ " &                   --  Odivide      => "/"
               "eq= " &                   --  Oeq          => "="
               "ex**" &                   --  Oexpon       => "**"
               "gt> " &                   --  Ogt          => ">"
               "ge>=" &                   --  Oge          => ">="
               "le<=" &                   --  Ole          => "<="
               "lt< " &                   --  Olt          => "<"
               "mo  " &                   --  Omod         => "mod"
               "mu* " &                   --  Omutliply    => "*"
               "ne/=" &                   --  One          => "/="
               "no  " &                   --  Onot         => "not"
               "or  " &                   --  Oor          => "or"
               "re  " &                   --  Orem         => "rem"
               "su- " &                   --  Osubtract    => "-"
               "xo  ";                    --  Oxor         => "xor"

            J : Integer;

         begin
            J := Map'First;

            --  Note that this loop must terminate, if not we have some kind
            --  of internal error, and a constraint error will be raised.

            loop
               exit when Name_Buffer (2) = Map (J)
                 and then Name_Buffer (3) = Map (J + 1);
               J := J + 4;
            end loop;

            --  Special operator name

            if Map (J + 2) /= ' ' then
               Name_Buffer (2) := Map (J + 2);
               Name_Len := 3;

               if Map (J + 3) /= ' ' then
                  Name_Buffer (3) := Map (J + 3);
                  Name_Len := 4;
               end if;

               Name_Buffer (Name_Len) := '"';
               return;

            --  For other operator names, leave them in lower case,
            --  surrounded by apostrophes

            else
               Name_Len := Name_Len + 1;
               Name_Buffer (Name_Len) := '"';
               return;
            end if;
         end;
      end if;

      --  For character literals, put apostrophes around, and then fall into
      --  the remaining circuit for possible decoding of Uhh/Whhhh sequence.

      if Name_Buffer (1) = 'Q' then
         Name_Buffer (1) := ''';
         Name_Len := Name_Len + 1;
         Name_Buffer (Name_Len) := ''';
      end if;

      --  Only remaining task is to decode Uhh and Whhhh sequences. First
      --  a quick check to see if there are any such sequences in the name

      for J in 1 .. Name_Len loop
         if Name_Buffer (J) = 'U' or else Name_Buffer (J) = 'W' then
            goto Do_Decode;
         end if;
      end loop;

      return;

      --  Here we have to decode one or more Uhh or Whhhh sequences

      <<Do_Decode>> declare
         New_Len : Natural;
         Old     : Positive;
         New_Buf : String (1 .. Hostparm.Max_Name_Length);

         function Hex (N : Natural) return Natural;
         --  Scans past N digits using Old pointer and returns hex value

         function Hex (N : Natural) return Natural is
            T : Natural := 0;
            C : Character;

         begin
            for J in 1 .. N loop
               C := Name_Buffer (Old);
               Old := Old + 1;

               pragma Assert (C in '0' .. '9' or else C in 'a' .. 'f');

               if C <= '9' then
                  T := 16 * T + Character'Pos (C) - Character'Pos ('0');
               else -- C in 'a' .. 'f'
                  T := 16 * T + Character'Pos (C) - (Character'Pos ('a') - 10);
               end if;
            end loop;

            return T;
         end Hex;

      begin
         New_Len := 0;
         Old := 1;

         while Old <= Name_Len loop
            if Name_Buffer (Old) = 'U' then
               Old := Old + 1;
               New_Len := New_Len + 1;
               New_Buf (New_Len) := Character'Val (Hex (2));

            elsif Name_Buffer (Old) = 'W' then
               Old := Old + 1;
               Widechar.Set_Wide (Char_Code (Hex (4)), New_Buf, New_Len);

            else
               New_Len := New_Len + 1;
               New_Buf (New_Len) := Name_Buffer (Old);
               Old := Old + 1;
            end if;
         end loop;

         Name_Len := New_Len;
         Name_Buffer (1 .. New_Len) := New_Buf (1 .. New_Len);
      end;
   end Get_Decoded_Name_String;

   -------------------------
   -- Get_Name_Table_Byte --
   -------------------------

   function Get_Name_Table_Byte (Id : Name_Id) return Byte is
   begin
      pragma Assert (Id in Name_Entries.First .. Name_Entries.Last);
      return Name_Entries.Table (Id).Byte_Info;
   end Get_Name_Table_Byte;

   -------------------------
   -- Get_Name_Table_Info --
   -------------------------

   function Get_Name_Table_Info (Id : Name_Id) return Int is
   begin
      pragma Assert (Id in Name_Entries.First .. Name_Entries.Last);
      return Name_Entries.Table (Id).Int_Info;
   end Get_Name_Table_Info;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is

   begin
      Name_Chars.Init;
      Name_Entries.Init;

      --  Initialize entries for one character names

      for C in Character loop
         Name_Entries.Increment_Last;
         Name_Entries.Table (Name_Entries.Last).Name_Chars_Index :=
           Name_Chars.Last;
         Name_Entries.Table (Name_Entries.Last).Name_Len  := 1;
         Name_Entries.Table (Name_Entries.Last).Hash_Link := No_Name;
         Name_Entries.Table (Name_Entries.Last).Int_Info  := 0;
         Name_Entries.Table (Name_Entries.Last).Byte_Info := 0;
         Name_Chars.Increment_Last;
         Name_Chars.Table (Name_Chars.Last) := C;
         Name_Chars.Increment_Last;
         Name_Chars.Table (Name_Chars.Last) := Ascii.NUL;
      end loop;

      --  Clear hash table

      for J in Hash_Index_Type loop
         Hash_Table (J) := No_Name;
      end loop;
   end Initialize;

   ----------------------
   -- Is_Internal_Name --
   ----------------------

   function Is_Internal_Name (Id : Name_Id) return Boolean is
   begin
      Get_Name_String (Id);

      for J in 1 .. Name_Len loop
         if Is_OK_Internal_Letter (Name_Buffer (J)) then
            return True;

         elsif Name_Buffer (J) = '_'
           and then (J = 1
                      or else J = Name_Len
                      or else Name_Buffer (J + 1) = '_')
         then
            return True;
         end if;
      end loop;

      return False;
   end Is_Internal_Name;

   ---------------------------
   -- Is_OK_Internal_Letter --
   ---------------------------

   function Is_OK_Internal_Letter (C : Character) return Boolean is
   begin
      return C in 'A' .. 'Z'
        and then C /= 'O'
        and then C /= 'Q'
        and then C /= 'U'
        and then C /= 'W'
        and then C /= 'X';
   end Is_OK_Internal_Letter;

   --------------------
   -- Length_Of_Name --
   --------------------

   function Length_Of_Name (Id : Name_Id) return Nat is
   begin
      return Int (Name_Entries.Table (Id).Name_Len);
   end Length_Of_Name;

   ----------
   -- Lock --
   ----------

   procedure Lock is
   begin
      Name_Chars.Set_Last (Name_Chars.Last + Name_Chars_Reserve);
      Name_Entries.Set_Last (Name_Entries.Last + Name_Entries_Reserve);
      Name_Chars.Locked := True;
      Name_Entries.Locked := True;
      Name_Chars.Release;
      Name_Entries.Release;
   end Lock;

   ------------------------
   -- Name_Chars_Address --
   ------------------------

   function Name_Chars_Address return System.Address is
   begin
      return Name_Chars.Table (0)'Address;
   end Name_Chars_Address;

   ----------------
   -- Name_Enter --
   ----------------

   function Name_Enter return Name_Id is
   begin

      Name_Entries.Increment_Last;
      Name_Entries.Table (Name_Entries.Last).Name_Chars_Index :=
        Name_Chars.Last;
      Name_Entries.Table (Name_Entries.Last).Name_Len  := Short (Name_Len);
      Name_Entries.Table (Name_Entries.Last).Hash_Link := No_Name;
      Name_Entries.Table (Name_Entries.Last).Int_Info  := 0;
      Name_Entries.Table (Name_Entries.Last).Byte_Info := 0;

      --  Set corresponding string entry in the Name_Chars table

      for J in 1 .. Name_Len loop
         Name_Chars.Increment_Last;
         Name_Chars.Table (Name_Chars.Last) := Name_Buffer (J);
      end loop;

      Name_Chars.Increment_Last;
      Name_Chars.Table (Name_Chars.Last) := Ascii.NUL;

      return Name_Entries.Last;
   end Name_Enter;

   --------------------------
   -- Name_Entries_Address --
   --------------------------

   function Name_Entries_Address return System.Address is
   begin
      return Name_Entries.Table (First_Name_Id)'Address;
   end Name_Entries_Address;

   ------------------------
   -- Name_Entries_Count --
   ------------------------

   function Name_Entries_Count return Nat is
   begin
      return Int (Name_Entries.Last - Name_Entries.First + 1);
   end Name_Entries_Count;

   ---------------
   -- Name_Find --
   ---------------

   function Name_Find return Name_Id is
      New_Id : Name_Id;
      --  Id of entry in hash search, and value to be returned

      S : Int;
      --  Pointer into string table

      Hash_Index : Hash_Index_Type;
      --  Computed hash index

   begin
      --  Quick handling for one character names

      if Name_Len = 1 then
         return Name_Id (First_Name_Id + Character'Pos (Name_Buffer (1)));

      --  Otherwise search hash table for existing matching entry

      else
         Hash_Index := Namet.Hash;
         New_Id := Hash_Table (Hash_Index);

         if New_Id = No_Name then
            Hash_Table (Hash_Index) := Name_Entries.Last + 1;

         else
            Search : loop
               if Name_Len /=
                 Integer (Name_Entries.Table (New_Id).Name_Len)
               then
                  goto No_Match;
               end if;

               S := Name_Entries.Table (New_Id).Name_Chars_Index;

               for I in 1 .. Name_Len loop
                  if Name_Chars.Table (S + Int (I)) /= Name_Buffer (I) then
                     goto No_Match;
                  end if;
               end loop;

               return New_Id;

               --  Current entry in hash chain does not match

               <<No_Match>>
                  if Name_Entries.Table (New_Id).Hash_Link /= No_Name then
                     New_Id := Name_Entries.Table (New_Id).Hash_Link;
                  else
                     Name_Entries.Table (New_Id).Hash_Link :=
                       Name_Entries.Last + 1;
                     exit Search;
                  end if;

            end loop Search;
         end if;

         --  We fall through here only if a matching entry was not found in the
         --  hash table. We now create a new entry in the names table. The hash
         --  link pointing to the new entry (Name_Entries.Last+1) has been set.

         Name_Entries.Increment_Last;
         Name_Entries.Table (Name_Entries.Last).Name_Chars_Index :=
           Name_Chars.Last;
         Name_Entries.Table (Name_Entries.Last).Name_Len  := Short (Name_Len);
         Name_Entries.Table (Name_Entries.Last).Hash_Link := No_Name;
         Name_Entries.Table (Name_Entries.Last).Int_Info  := 0;
         Name_Entries.Table (Name_Entries.Last).Byte_Info := 0;

         --  Set corresponding string entry in the Name_Chars table

         for I in 1 .. Name_Len loop
            Name_Chars.Increment_Last;
            Name_Chars.Table (Name_Chars.Last) := Name_Buffer (I);
         end loop;

         Name_Chars.Increment_Last;
         Name_Chars.Table (Name_Chars.Last) := Ascii.NUL;

         return Name_Entries.Last;
      end if;
   end Name_Find;

   ----------------------
   -- Reset_Name_Table --
   ----------------------

   procedure Reset_Name_Table is
   begin
      for J in First_Name_Id .. Name_Entries.Last loop
         Name_Entries.Table (J).Int_Info  := 0;
         Name_Entries.Table (J).Byte_Info := 0;
      end loop;
   end Reset_Name_Table;

   --------------------------------
   -- Set_Character_Literal_Name --
   --------------------------------

   procedure Set_Character_Literal_Name (C : Char_Code) is
   begin
      Name_Buffer (1) := 'Q';
      Name_Len := 1;
      Store_Encoded_Character (C);
   end Set_Character_Literal_Name;

   -------------------------
   -- Set_Name_Table_Info --
   -------------------------

   procedure Set_Name_Table_Info (Id : Name_Id; Val : Int) is
   begin
      pragma Assert (Id in Name_Entries.First .. Name_Entries.Last);
      Name_Entries.Table (Id).Int_Info := Val;
   end Set_Name_Table_Info;

   -------------------------
   -- Set_Name_Table_Byte --
   -------------------------

   procedure Set_Name_Table_Byte (Id : Name_Id; Val : Byte) is
   begin
      pragma Assert (Id in Name_Entries.First .. Name_Entries.Last);
      Name_Entries.Table (Id).Byte_Info := Val;
   end Set_Name_Table_Byte;

   -----------------------------
   -- Store_Encoded_Character --
   -----------------------------

   procedure Store_Encoded_Character (C : Char_Code) is

      procedure Set_Hex_Chars (N : Natural);
      --  Stores given value, which is in the range 0 .. 255, as two hex
      --  digits (using lower case a-f) in Name_Buffer, incrementing Name_Len

      procedure Set_Hex_Chars (N : Natural) is
         Hexd : constant String := "0123456789abcdef";

      begin
         Name_Buffer (Name_Len + 1) := Hexd (N / 16 + 1);
         Name_Buffer (Name_Len + 2) := Hexd (N mod 16 + 1);
         Name_Len := Name_Len + 2;
      end Set_Hex_Chars;

   begin
      Name_Len := Name_Len + 1;

      if In_Character_Range (C) then
         declare
            CC : constant Character := Get_Character (C);

         begin
            if CC in 'a' .. 'z' or else CC in '0' .. '9' then
               Name_Buffer (Name_Len) := CC;

            else
               Name_Buffer (Name_Len) := 'U';
               Set_Hex_Chars (Natural (C));
            end if;
         end;

      else
         Name_Buffer (Name_Len) := 'W';
         Set_Hex_Chars (Natural (C) / 256);
         Set_Hex_Chars (Natural (C) mod 256);
      end if;

   end Store_Encoded_Character;

   ---------------
   -- Tree_Read --
   ---------------

   procedure Tree_Read is
   begin
      Name_Chars.Tree_Read;
      Name_Entries.Tree_Read;

      Tree_Read_Data
        (Hash_Table'Address,
         Hash_Table'Length * (Hash_Table'Component_Size / Storage_Unit));
   end Tree_Read;

   ----------------
   -- Tree_Write --
   ----------------

   procedure Tree_Write is
   begin
      Name_Chars.Tree_Write;
      Name_Entries.Tree_Write;

      Tree_Write_Data
        (Hash_Table'Address,
         Hash_Table'Length * (Hash_Table'Component_Size / Storage_Unit));
   end Tree_Write;

   ------------
   -- Unlock --
   ------------

   procedure Unlock is
   begin
      Name_Chars.Set_Last (Name_Chars.Last - Name_Chars_Reserve);
      Name_Entries.Set_Last (Name_Entries.Last - Name_Entries_Reserve);
      Name_Chars.Locked := False;
      Name_Entries.Locked := False;
      Name_Chars.Release;
      Name_Entries.Release;
   end Unlock;

   --------
   -- wn --
   --------

   procedure wn (Id : Name_Id) is
   begin
      Write_Name (Id);
      Write_Eol;
   end wn;

   ----------------
   -- Write_Name --
   ----------------

   procedure Write_Name (Id : Name_Id) is
   begin
      if Id >= First_Name_Id then
         Get_Name_String (Id);
         Write_Str (Name_Buffer (1 .. Name_Len));
      end if;
   end Write_Name;

   ------------------------
   -- Write_Name_Decoded --
   ------------------------

   procedure Write_Name_Decoded (Id : Name_Id) is
   begin
      if Id >= First_Name_Id then
         Get_Decoded_Name_String (Id);
         Write_Str (Name_Buffer (1 .. Name_Len));
      end if;
   end Write_Name_Decoded;

end Namet;

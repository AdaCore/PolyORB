------------------------------------------------------------------------------
--                                                                          --
--                            GLADE COMPONENTS                              --
--                                                                          --
--                  S Y S T E M . G A R L I C . U T I L S                   --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision$
--                                                                          --
--         Copyright (C) 1996-2000 Free Software Foundation, Inc.           --
--                                                                          --
-- GARLIC is free software;  you can redistribute it and/or modify it under --
-- terms of the  GNU General Public License  as published by the Free Soft- --
-- ware Foundation;  either version 2,  or (at your option)  any later ver- --
-- sion.  GARLIC is distributed  in the hope that  it will be  useful,  but --
-- WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHANTABI- --
-- LITY or  FITNESS FOR A PARTICULAR PURPOSE.  See the  GNU General Public  --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License  distributed with GARLIC;  see file COPYING.  If  --
-- not, write to the Free Software Foundation, 59 Temple Place - Suite 330, --
-- Boston, MA 02111-1307, USA.                                              --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
--               GLADE  is maintained by ACT Europe.                        --
--               (email: glade-report@act-europe.fr)                        --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Exceptions;
with Ada.Unchecked_Deallocation;
with System.Garlic.Debug; use System.Garlic.Debug;

package body System.Garlic.Utils is

   Private_Debug_Key : constant Debug_Key :=
     Debug_Initialize ("S_GARUTI", "(s-garuti): ");
   procedure D
     (Message : in String;
      Key     : in Debug_Key := Private_Debug_Key)
     renames Print_Debug_Info;

   Version_Id_Window : constant Version_Id := Version_Id'Last / 2;

   procedure Free is
     new Ada.Unchecked_Deallocation (String, Error_Type);

   procedure Free is
     new Ada.Unchecked_Deallocation (String, String_Access);

   procedure Free is
     new Ada.Unchecked_Deallocation (String_Array, String_Array_Access);

   procedure Next_Separator
     (S : in String;
      I : in out Natural;
      C : in Character);

   procedure Skip_Separator
     (S : in String;
      I : in out Natural;
      C : in Character);

   ---------
   -- "<" --
   ---------

   function "<" (L, R : Version_Id) return Boolean is
   begin
      return Integer (R - L) < Integer (Version_Id_Window);
   end "<";

   ----------------------
   -- Access_To_String --
   ----------------------

   function Access_To_String (S : String_Access) return String is
   begin
      return S.all;
   end Access_To_String;

   -----------
   -- Catch --
   -----------

   procedure Catch (Error : in out Error_Type)
   is
   begin
      if Error /= null then
         pragma Debug (D ("*** Catch *** " & Error.all));
         Free (Error);
      end if;
   end Catch;

   -------------
   -- Content --
   -------------

   function Content (Error : access Error_Type) return String is
      pragma Assert (Error.all /= null);
      Error_Message : constant String := Error.all.all;
   begin
      Free (Error.all);
      return Error_Message;
   end Content;

   ----------
   -- Copy --
   ----------

   function Copy (S : String_Array_Access) return String_Array_Access is
      R : String_Array_Access;
   begin
      if S /= null then
         R := new String_Array'(S.all);
         for I in S'Range loop
            R (I) := new String'(S (I).all);
         end loop;
      end if;
      return R;
   end Copy;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (S : in out String_Access) is
   begin
      if S /= null then
         Free (S);
      end if;
   end Destroy;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (S : in out String_Array_Access) is
   begin
      if S /= null then
         for I in S'Range loop
            Destroy (S (I));
         end loop;
         Free (S);
      end if;
   end Destroy;

   -----------
   -- Found --
   -----------

   function Found (Error : Error_Type) return Boolean is
   begin
      return Error /= null;
   end Found;

   ------------------
   -- Merge_String --
   ------------------

   function Merge_String
     (S : String_Array_Access;
      C : Character := Location_Separator)
     return String
   is
      N_Separator : Natural := 0;
      Full_Length : Natural := 0;
   begin
      if S'Length = 0 then
         return "";
      end if;

      --  Count number of separators needed and the size of all the
      --  string concatenated.

      for I in S'Range loop
         if S (I) /= null then
            N_Separator := N_Separator + 1;
            Full_Length := Full_Length + S (I)'Length;
         end if;
      end loop;
      if N_Separator = 0 then
         return "";
      end if;
      N_Separator := N_Separator - 1;

      --  Concatenate and add separators.

      declare
         R : String (1 .. Full_Length + N_Separator);
         F : Natural := 1;
      begin
         for I in S'Range loop
            if S (I) /= null then
               R (F .. F + S (I)'Length - 1) := S (I).all;
               if N_Separator /= 0 then
                  R (F + S (I)'Length) := C;
                  N_Separator := N_Separator - 1;
               end if;
               F := F + S (I)'Length + 1;
            end if;
         end loop;
         return R;
      end;
   end Merge_String;

   -------------
   -- Missing --
   -------------

   function Missing
     (Elt : String;
      Set : String_Array)
     return Boolean
   is
   begin
      for N in Set'Range loop
         if Set (N) /= null
           and then Elt = Set (N).all
         then
            return False;
         end if;
      end loop;
      return True;
   end Missing;

   --------------------
   -- Next_Seperator --
   --------------------

   procedure Next_Separator
     (S : in String;
      I : in out Natural;
      C : in Character) is
   begin
      while I <= S'Last
        and then S (I) /= C
      loop
         I := I + 1;
      end loop;
   end Next_Separator;

   -------------------------------
   -- Raise_Communication_Error --
   -------------------------------

   procedure Raise_Communication_Error (Error : in out Error_Type) is
      pragma Assert (Error /= null);
      Error_Message : constant String := Error.all;
   begin
      Free (Error);
      Ada.Exceptions.Raise_Exception
        (Communication_Error'Identity, Error_Message);
   end Raise_Communication_Error;

   --------------------
   -- Skip_Separator --
   --------------------

   procedure Skip_Separator
     (S : in String;
      I : in out Natural;
      C : in Character) is
   begin
      while I <= S'Last
        and then S (I) = C
      loop
         I := I + 1;
      end loop;
   end Skip_Separator;

   ------------------
   -- Split_String --
   ------------------

   function Split_String
     (S : String;
      C : Character := Location_Separator)
     return String_Array_Access
   is
      N : Natural := 0;
      A : String_Array_Access;
      F : Natural;
      L : Natural;
   begin
      if S'Length = 0 then
         return null;
      end if;

      --  Count number of substrings.

      F := S'First;
      L := S'Last;
      loop
         Skip_Separator (S, F, C);
         exit when F > L;
         N := N + 1;
         Next_Separator (S, F, C);
         exit when F > L;
      end loop;
      if N = 0 then
         return null;
      end if;

      --  Fill each slot of array.

      A := new String_Array (1 .. N);
      F := S'First;
      for I in A'Range loop
         Skip_Separator (S, F, C);
         L := F;
         Next_Separator (S, L, C);
         A (I) := new String'(S (F .. L - 1));
         F := L;
      end loop;
      return A;
   end Split_String;

   ----------------------
   -- String_To_Access --
   ----------------------

   function String_To_Access (S : String) return String_Access is
   begin
      return new String'(S);
   end String_To_Access;

   -----------
   -- Throw --
   -----------

   procedure Throw (Error : in out Error_Type; Message : in String)
   is
   begin
      if Error /= null then
         pragma Debug (D ("*** Abort *** " & Error.all));

         Free (Error);
      end if;

      Error := new String'(Message);

      pragma Debug (D ("*** Throw *** " & Error.all));
   end Throw;

   ----------------
   -- To_Address --
   ----------------

   function To_Address (Addr : Portable_Address) return Address is
   begin
      return Address (Addr);
   end To_Address;

   --------------
   -- To_Lower --
   --------------

   procedure To_Lower (Item : in out String) is
   begin
      for I in Item'Range loop
         if Item (I) in 'A' .. 'Z' then
            Item (I) :=
               Character'Val (Character'Pos (Item (I)) -
                              Character'Pos ('A') +
                              Character'Pos ('a'));
         end if;
      end loop;
   end To_Lower;

   -------------------------
   -- To_Portable_Address --
   -------------------------

   function To_Portable_Address (Addr : Address) return Portable_Address is
   begin
      return Portable_Address (Addr);
   end To_Portable_Address;

   -------------
   -- Unquote --
   -------------

   function Unquote (S : String) return String is
   begin
      if S (S'First) = '"' and then S (S'Last) = '"' then
         return S (S'First + 1 .. S'Last - 1);
      elsif S (S'First) = ''' and then S (S'Last) = ''' then
         return S (S'First + 1 .. S'Last - 1);
      else
         return S;
      end if;
   end Unquote;

end System.Garlic.Utils;

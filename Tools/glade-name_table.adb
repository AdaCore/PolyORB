------------------------------------------------------------------------------
--                                                                          --
--                              GLADE TOOLS                                 --
--                                                                          --
--                     G L A D E . N A M E _ T A B L E                      --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision$                             --
--                                                                          --
--         Copyright (C) 1996-2000 Free Software Foundation, Inc.           --
--                                                                          --
-- GLADE  is free software;  you can redistribute it and/or modify it under --
-- terms of the  GNU General Public License  as published by the Free Soft- --
-- ware Foundation;  either version 2,  or (at your option)  any later ver- --
-- sion.  GLADE  is distributed  in the hope that  it will be  useful,  but --
-- WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHANTABI- --
-- LITY or  FITNESS FOR A PARTICULAR PURPOSE.  See the  GNU General Public  --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License  distributed  with GLADE;  see file COPYING.  If  --
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

with Ada.Unchecked_Deallocation;
with GLADE.Types; use GLADE.Types;
with GNAT.Table;

package body GLADE.Name_Table is

   Size  : constant := 2 ** 9;
   --  Size of actual string names table

   Min   : constant Natural := 1;
   Max   : Natural := Size;
   Last  : Natural := 0;

   Table : String_Access := new String (Min .. Max);

   Hash_Max : constant Name_Id := 2 ** 8;
   --  Indexes in the hash header table run from 0 to Hash_Max

   subtype Hash_Id is Name_Id range 0 .. Hash_Max;
   --  Range of hash index values

   Hash_Nodes : array (1 .. Hash_Max) of Name_Id := (others => Null_Name);
   --  The hash table is used to locate existing entries in the names table.
   --  The entries point to the first names table entry whose hash value
   --  matches the hash code. Then subsequent names table entries with the
   --  same hash code value are linked through the Next fields.

   function Hash (Name : String) return Hash_Id;
   --  Return an hash entry which corresponds to the first entry in the
   --  hash table.

   type Node_Type is record
      First  : Name_Id;
      --  Starting location of characters in Table

      Length : Natural;
      --  Length of this name in characters

      Info   : Integer;
      --  Int Value associated with this name

      Next   : Name_Id;
      --  Next entry in names table for same hash code

   end record;

   Null_Node : constant Node_Type :=
     (First  => Null_Name,
      Length => 0,
      Info   => 0,
      Next   => Null_Name);

   package Nodes is new GNAT.Table
     (Table_Component_Type => Node_Type,
      Table_Index_Type     => Name_Id,
      Table_Low_Bound      => First_Name,
      Table_Initial        => Hash_Max,
      Table_Increment      => 100);
   --  This is the table that is referenced by Name_Id entries. It
   --  contains one entry for each unique name in the table.

   ----------
   -- Hash --
   ----------

   function Hash (Name : String) return Hash_Id is
      type T is mod 2 ** 8;
      X : T := 0;
   begin
      for J in Name'Range loop
         X := 2 * X + Character'Pos (Name (J));
      end loop;

      return Hash_Id (X) + 1;
   end Hash;

   ---------
   -- Get --
   ---------

   function Get (S : String) return Name_Id is
      L : constant Natural := S'Length;
      H : constant Hash_Id := Hash (S);
      I : Name_Id;
      N : Node_Type;
   begin
      --  Set to the first entry whose hash value matches S hash code

      I := Hash_Nodes (H);

      while I /= Null_Name loop
         N := Nodes.Table (I);

         --  Check whether the string of this entry matches with S

         if N.Length = L and then Table (N.First .. N.First + L - 1) = S then
            return I;
         end if;

         I := N.Next;
      end loop;

      --  Check whether the table is large enough or not

      if Max < Last + L then
         declare
            Old : String_Access := Table;
         begin
            Max := Max + Size;
            Table := new String (Min .. Max);
            for Index in Min .. Last loop
               Table (Index) := Old (Index);
            end loop;
            for Index in Last + 1 .. Max loop
               Table (Index) := Ascii.Nul;
            end loop;
            Free (Old);
         end;
      end if;

      --  Enter the name in the first entry whose hash code matches S

      I := Nodes.Allocate;

      Nodes.Table (I).Next   := Hash_Nodes (H);
      Hash_Nodes (H)         := I;

      Nodes.Table (I).Length := L;
      Nodes.Table (I).First  := Last + 1;
      Table (Last + 1 .. Last + L) := S;
      Last := Last + L;

      return I;
   end Get;

   ---------
   -- Get --
   ---------

   function Get (N : Name_Id) return String is
      F : constant Natural := Nodes.Table (N).First;
      L : constant Natural := Nodes.Table (N).Length;

   begin
      return Table (F .. F + L - 1);
   end Get;

   --------------
   -- Get_Info --
   --------------

   function Get_Info (N : Name_Id) return Integer is
   begin
      return Nodes.Table (N).Info;
   end Get_Info;

   --------------
   -- Set_Info --
   --------------

   procedure Set_Info (N : Name_Id; I : Integer) is
   begin
      Nodes.Table (N).Info := I;
   end Set_Info;

end GLADE.Name_Table;

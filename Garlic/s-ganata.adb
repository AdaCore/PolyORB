------------------------------------------------------------------------------
--                                                                          --
--                            GLADE COMPONENTS                              --
--                                                                          --
--             S Y S T E M . G A R L I C . N A M E _ T A B L E              --
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

with System.Garlic.Debug;      use System.Garlic.Debug;
with System.Garlic.Soft_Links; use System.Garlic.Soft_Links;
with System.Garlic.Table;
with System.Garlic.Utils;      use System.Garlic.Utils;

package body System.Garlic.Name_Table is

   Private_Debug_Key : constant Debug_Key :=
     Debug_Initialize ("S_GANATA", "(s-ganata): ");
   procedure D
     (Message : in String;
      Key     : in Debug_Key := Private_Debug_Key)
     renames Print_Debug_Info;

   use ASCII, Ada.Streams;

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
      Info   => Empty_Info,
      Next   => Null_Name);

   package Nodes is new System.Garlic.Table.Simple
     (Index_Type     => Name_Id,
      Null_Index     => Null_Name,
      First_Index    => First_Name,
      Initial_Size   => Natural (Hash_Max),
      Increment_Size => Natural (Hash_Max),
      Component_Type => Node_Type,
      Null_Component => Null_Node);
   --  This is the table that is referenced by Name_Id entries. It
   --  contains one entry for each unique name in the table.

   Mutex : Mutex_Access;

   ---------
   -- Get --
   ---------

   function Get (S : String) return Name_Id is
      L : constant Natural := S'Length;
      H : constant Hash_Id := Hash (S);
      I : Name_Id;
      N : Node_Type;
   begin
      Enter (Mutex);

      --  Set to the first entry whose hash value matches S hash code

      I := Hash_Nodes (H);

      while I /= Null_Name loop
         N := Nodes.Table (I);

         --  Check whether the string of this entry matches with S

         if N.Length = L
           and then Table (Natural (N.First) .. Natural (N.First) + L - 1) = S
         then
            Leave (Mutex);

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
            Table (Min .. Last) := Old (Min .. Last);
            Table (Last + 1 .. Max) := (others => ASCII.NUL);
            Destroy (Old);
         end;
      end if;

      --  Enter the name in the first entry whose hash code matches S
      I := Nodes.Allocate;

      Nodes.Table (I).Next   := Hash_Nodes (H);
      Hash_Nodes (H)         := I;

      Nodes.Table (I).Length := L;
      Nodes.Table (I).First  := Name_Id (Last + 1);
      Table (Last + 1 .. Last + L) := S;
      Last := Last + L;

      Leave (Mutex);

      return I;
   end Get;

   ---------
   -- Get --
   ---------

   function Get (N : Name_Id) return String is
   begin
      Enter (Mutex);
      declare
         F : constant Natural := Natural (Nodes.Table (N).First);
         L : constant Natural := Nodes.Table (N).Length;
         S : constant String  := Table (F .. F + L - 1);
      begin
         Leave (Mutex);

         return S;
      end;
   end Get;

   --------------
   -- Get_Info --
   --------------

   function Get_Info (N : Name_Id) return Integer
   is
      Info : Integer;
   begin
      Enter (Mutex);
      Info := Nodes.Table (N).Info;
      Leave (Mutex);
      return Info;
   end Get_Info;

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

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      Create (Mutex);
      Nodes.Initialize;
   end Initialize;

   ----------
   -- Read --
   ----------

   procedure Read (S : access Root_Stream_Type'Class; N : out Name_Id) is
   begin
      N := Get (String'Input (S));
   end Read;

   --------------
   -- Set_Info --
   --------------

   procedure Set_Info (N : Name_Id; I : Integer) is
   begin
      Enter (Mutex);
      Nodes.Table (N).Info := I;
      Leave (Mutex);
   end Set_Info;

   ----------------
   -- To_Name_Id --
   ----------------

   function To_Name_Id (N : Natural) return Name_Id is
   begin
      return Name_Id (N);
   end To_Name_Id;

   ----------------
   -- To_Natural --
   ----------------

   function To_Natural (N : Name_Id) return Natural is
   begin
      return Natural (N);
   end To_Natural;

   -----------
   -- Write --
   -----------

   procedure Write (S : access Root_Stream_Type'Class; N : in Name_Id) is
   begin
      String'Output (S, Get (N));
   end Write;

end System.Garlic.Name_Table;

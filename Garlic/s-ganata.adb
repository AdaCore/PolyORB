with Ada.Unchecked_Deallocation;
with System.Garlic.Utils; use System.Garlic.Utils;
with System.Garlic.Table;

package body System.Garlic.Name_Table is

   use Ascii;

   Size  : constant := 2 ** 9;
   --  Size of actual string names table.

   Min   : constant Natural := 1;
   Max   : Natural := Size;
   Last  : Natural := 0;

   Table : String_Access := new String (Min .. Max);

   Hash_Max : constant Name_Id := 2 ** 8;
   --  Indexes in the hash header table run from 0 to Hash_Max.

   subtype Hash_Id is Name_Id range 0 .. Hash_Max;
   --  Range of hash index values.

   Hash_Nodes : array (1 .. Hash_Max) of Name_Id;
   --  The hash table is used to locate existing entries in the names table.
   --  The entries point to the first names table entry whose hash value
   --  matches the hash code. Then subsequent names table entries with the
   --  same hash code value are linked through the Next fields.

   function Hash (Name : String) return Hash_Id;
   --  Return an hash entry which corresponds to the first entry in the
   --  hash table.

   type Node_Type is
      record
         First  : Name_Id;
         --  Starting location of characters in Table.

         Length : Natural;
         --  Length of this name in characters.

         Info   : Integer;
         --  Int Value associated with this name.

         Next   : Name_Id;
         --  Next entry in names table for same hash Code.

      end record;
   Null_Node : constant Node_Type := (Null_Name, 0, 0, Null_Name);

   --  This is the table that is referenced by Name_Id entries.
   --  It contains one entry for each unique name in the table.

   package Nodes is new System.Garlic.Table.Sequential
     (Name_Id, Hash_Max, Hash_Max, Node_Type, Null_Node);

   procedure Reallocate;

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
      Len  : constant Natural := S'Length;
      HId  : constant Hash_Id := Hash (S);
      Id   : Name_Id;
      Node : Node_Type;

      Found : Boolean;
      --  The string name is already there.

   begin

      --  Handles the case where the hash entry is still empty.

      if Hash_Nodes (HId) = Null_Name then
         Id := Nodes.Allocate;
         Hash_Nodes (HId) := Id;
      end if;

      --  Set to the first entry whose hash value matches S hash code.

      Id   := Hash_Nodes (HId);
      Node := Nodes.Get (Id);

      if Node.Length = 0 then
         Found := False;

      else
         loop

            --  Check whether the string of this entry matches with S.

            if Node.Length = Len then
               Found := True;

               for I in 0 .. Len - 1 loop
                  if S (S'First + I) /= Table (Node.First + I) then
                     Found := False;
                     exit;
                  end if;
               end loop;

               exit when Found;
            end if;

            --  When no more entry available, create a new one.

            if Node.Next = Null_Name then
               Found := False;

               Id        := Nodes.Allocate;
               Node.Next := Id;
               Node      := Nodes.Get (Id);
               exit;
            end if;

            --  Try with next entry.

            Id   := Node.Next;
            Node := Nodes.Get (Id);

         end loop;
      end if;

      --  Initialize the new entry.

      if not Found then
         Node.Next   := Null_Name;
         Node.Length := Len;
         Node.First  := Last + 1;
         Last        := Last + Len;

         for I in 0 .. Len - 1 loop
            Table (Node.First + I) := S (S'First + I);
         end loop;

         Nodes.Set (Id, Node);
      end if;

      return Id;
   end Get;

   ---------
   -- Get --
   ---------

   function Get (N : Name_Id) return String is
      Node : Node_Type;

   begin
      Node := Nodes.Get (N);
      declare
         S : String (1 .. Node.Length);
         F : Natural := Node.First - 1;
      begin
         for I in S'Range loop
            S (I) := Table (F + I);
         end loop;

         return S;
      end;
   end Get;

   --------------
   -- Get_Info --
   --------------

   function Get_Info (N : Name_Id) return Integer is
   begin
      return Nodes.Get (N).Info;
   end Get_Info;

   ----------------
   -- Reallocate --
   ----------------

   procedure Reallocate is
      New_Max   : Name_Id       := Max + Size;
      New_Table : String_Access := new String (Min .. New_Max);

      begin
         for Index in Min .. Last loop
            New_Table (Index) := Table (Index);
         end loop;
         for Index in Last + 1 .. Max loop
            New_Table (Index) := Ascii.Nul;
         end loop;
         Free (Table);
         Table := New_Table;
      end Reallocate;

   --------------
   -- Set_Info --
   --------------

   procedure Set_Info (N : Name_Id; I : Integer) is
      Node : Node_Type;
   begin
      Node := Nodes.Get (N);
      Node.Info := I;
      Nodes.Set (N, Node);
   end Set_Info;

end System.Garlic.Name_Table;

--  idlac: IDL to Ada compiler.
--  Copyright (C) 1999 Tristan Gingold.
--
--  emails: gingold@enst.fr
--          adabroker@adabroker.eu.org
--
--  IDLAC is free software;  you can  redistribute it and/or modify it under
--  terms of the  GNU General Public License as published  by the Free Software
--  Foundation;  either version 2,  or (at your option) any later version.
--  IDLAC is distributed in the hope that it will be useful, but WITHOUT ANY
--  WARRANTY;  without even the  implied warranty of MERCHANTABILITY
--  or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
--  for  more details.  You should have  received  a copy of the GNU General
--  Public License  distributed with IDLAC;  see file COPYING.  If not, write
--  to  the Free Software Foundation,  59 Temple Place - Suite 330,  Boston,
--  MA 02111-1307, USA.
--

with Ada.Unchecked_Deallocation;
with Ada.Text_IO;
with Gnat.Table;
with Gnat.Case_Util;
with Tokens;
with Errors;

package body Types is
   --  Primitives of n_root.
   procedure Set_Loc (N : in out N_Root'Class; Loc : Errors.Location) is
   begin
      N.Loc := Loc;
   end Set_Loc;

   function Get_Loc (N : N_Root'Class) return Errors.Location is
   begin
      return N.Loc;
   end Get_Loc;

   procedure Set_Back_End (N : in out N_Root'Class;
                           Be : access N_Back_End'Class) is
   begin
      if N.Back_End /= null then
         raise Errors.Internal_Error;
      end if;
      N.Back_End := N_Back_End_Acc (Be);
   end Set_Back_End;

   function Get_Back_End (N : N_Root'Class) return N_Back_End_Acc is
   begin
      return N.Back_End;
   end Get_Back_End;

   --  These define the stack of the scopes.
   --  The top of the stack is keep in CURRENT_SCOPE.
   --  Each cell of the stack points to a scope node and to the cell defining
   --  the enclosing scope.
   --  Of course, the global scope as a NULL parent.
   type Scope_Slot;
   type Scope_Slot_Acc is access Scope_Slot;
   type Scope_Slot is record
      Parent : Scope_Slot_Acc;
      Scope : N_Scope_Acc;
   end record;
   Current_Scope : Scope_Slot_Acc := null;
   Root_Scope : Scope_Slot_Acc := null;

   procedure Unchecked_Deallocation is new Ada.Unchecked_Deallocation
     (Object => Scope_Slot, Name => Scope_Slot_Acc);

   function Get_Root_Scope return N_Scope_Acc is
   begin
      return Root_Scope.scope;
   end Get_Root_Scope;

   function Get_Current_Scope return N_Scope_Acc is
   begin
      return Current_Scope.Scope;
   end Get_Current_Scope;

   --  Hash table.
   type Hash_Value_Type is mod 2**32;
   Hash_Mod : constant Hash_Value_Type := 2053;
   Nil_Uniq_Id : constant Uniq_Id := 0;
   type Hash_Table_Type is array (0 .. Hash_Mod - 1) of Uniq_Id;
   Hash_Table : Hash_Table_Type := (others => Nil_Uniq_Id);

   type Hash_Entry is record
      Str : String_Cacc := null;
      Next : Uniq_Id;
      Cell : Scope_Cell_Acc := null;
      List : Scope_Cell_Acc := null;
   end record;

   package Id_Table is new Gnat.Table
     (Table_Component_Type => Hash_Entry, Table_Index_Type => Uniq_Id,
      Table_Low_Bound => Nil_Uniq_Id + 1, Table_Initial => 256,
      Table_Increment => 100);

   --  type Hash_Entry_Acc is access Hash_Entry;

   procedure Push_Scope (Scope : access N_Scope'Class) is
      Slot : Scope_Slot_Acc;
   begin
      Slot := new Scope_Slot;
      Slot.Parent := Current_Scope;
      Slot.Scope := N_Scope_Acc (Scope);
      if Current_Scope = null then
         Root_Scope := Slot;
      end if;
      Current_Scope := Slot;
   end Push_Scope;

   procedure Pop_Scope (Scope : access N_Scope'Class) is
      Old : Scope_Slot_Acc;
      Cell : Scope_Cell_Acc;
   begin
      if Current_Scope = null
        or else Current_Scope.Scope /= N_Scope_Acc (Scope) then
         raise Errors.Internal_Error;
      end if;
      Old := Current_Scope;
      Current_Scope := Old.Parent;
      if Root_Scope = Old then
         if Current_Scope /= null then
            raise Errors.Internal_Error;
         end if;
         Root_Scope := Old.Parent;
      end if;
      --  Remove all definition of scope from the hash table, and
      --  replace them by the previous one.
      Cell := Old.Scope.Identifier_List;
      while Cell /= null loop
         --  Just a check.
         if Id_Table.Table (Cell.Identifier).Cell /= Cell then
            raise Errors.Internal_Error;
         end if;
         Id_Table.Table (Cell.Identifier).Cell := Cell.Old;
         Cell := Cell.Next;
      end loop;
      Unchecked_Deallocation (Old);
   end Pop_Scope;

   function Hash (Str : in String) return Hash_Value_Type is
      Res : Hash_Value_Type := 0;
   begin
      for I in Str'Range loop
         Res := ((Res and 16#0fffffff#) * 16) xor
           Character'Pos (Gnat.Case_Util.To_Lower (Str (I)));
      end loop;
      return Res;
   end Hash;

   function Add_Identifier (Id : String_Cacc) return Uniq_Id is
      use Tokens;
      Hash_Index : Hash_Value_Type := Hash (Id.all) mod Hash_Mod;
      Index : Uniq_Id := Hash_Table (Hash_Index);
   begin
      if Index = Nil_Uniq_Id then
         Id_Table.Increment_Last;
         Index := Id_Table.Last;
         Hash_Table (Hash_Index) := Index;
      else
         while Id_Table.Table (Index).Str /= null loop
            if Idl_Identifier_Equal (Id_Table.Table (Index).Str.all,
                                     Id.all) /= Differ
            then
               return Index;
            end if;
            if Id_Table.Table (Index).Next = Nil_Uniq_Id then
               Id_Table.Increment_Last;
               Id_Table.Table (Index).Next := Id_Table.Last;
               Index := Id_Table.Last;
               exit;
            end if;
            Index := Id_Table.Table (Index).Next;
         end loop;
      end if;
      --  Add an entry in INDEX.
      Id_Table.Table (Index) := (Str => Id,
                                 Next => Nil_Uniq_Id,
                                 Cell => null,
                                 List => null);
      return Index;
   end Add_Identifier;

   function Get_Identifier (Id : String) return Uniq_Id is
      use Tokens;
      Hash_Index : Hash_Value_Type := Hash (Id) mod Hash_Mod;
      Index : Uniq_Id := Hash_Table (Hash_Index);
   begin
      if Index = Nil_Uniq_Id then
         Id_Table.Increment_Last;
         Index := Id_Table.Last;
         Hash_Table (Hash_Index) := Index;
      else
         while Id_Table.Table (Index).Str /= null loop
            if Idl_Identifier_Equal (Id_Table.Table (Index).Str.all, Id)
              /= Differ
            then
               return Index;
            end if;
            if Id_Table.Table (Index).Next = Nil_Uniq_Id then
               Id_Table.Increment_Last;
               Id_Table.Table (Index).Next := Id_Table.Last;
               Index := Id_Table.Last;
               exit;
            end if;
            Index := Id_Table.Table (Index).Next;
         end loop;
      end if;
      --  Add an entry in INDEX.
      Id_Table.Table (Index) := (Str => new String'(Id),
                                 Next => Nil_Uniq_Id,
                                 Cell => null,
                                 List => null);
      return Index;
   end Get_Identifier;

   function Find_Identifier return Named_Node_Acc is
      Cell : Scope_Cell_Acc;
      Index : Uniq_Id;
   begin
      Index := Get_Identifier (Tokens.Get_Identifier);
      Cell := Id_Table.Table (Index).Cell;
      if Cell = null then
         return null;
      else
         return Cell.Node;
      end if;
   end Find_Identifier;

   function Find_Identifier return Scope_Cell_Acc is
      Index : Uniq_Id;
   begin
      Index := Get_Identifier (Tokens.Get_Identifier);
      return Id_Table.Table (Index).Cell;
   end Find_Identifier;

   function Get_Node (Cell : Scope_Cell_Acc) return Named_Node_Acc is
   begin
      return Cell.Node;
   end Get_Node;

   procedure Redefine_Identifier
     (Cell : Scope_Cell_Acc; Node : access Named_Node'Class) is
   begin
      if Cell.Node = null or else Node.Cell /= null then
         raise Errors.Internal_Error;
      end if;
      Cell.Node.Cell := null;
      Cell.Node := Named_Node_Acc (Node);
      Node.Cell := Cell;
   end Redefine_Identifier;

   function Add_Identifier (Index : Uniq_Id;
                            Node : access Named_Node'Class)
                            return Scope_Cell_Acc is
      Cell : Scope_Cell_Acc;
   begin
      --  There must be a current scope.
      if Current_Scope = null then
         raise Errors.Internal_Error;
      end if;
      Cell := Id_Table.Table (Index).Cell;
      --  Check if the identifier is not being redefined in the same scope.
      if Cell /= null and then Cell.Parent = Current_Scope.Scope then
         return null;
      end if;
      --  Create a cell.
      Cell := new Scope_Cell;
      Cell.Identifier := Index;
      Cell.Node := Named_Node_Acc (Node);
      Cell.Old := Id_Table.Table (Index).Cell;
      Id_Table.Table (Index).Cell := Cell;
      Cell.Next := Current_Scope.Scope.Identifier_List;
      Current_Scope.Scope.Identifier_List := Cell;
      Cell.Link := Id_Table.Table (Index).List;
      Id_Table.Table (Index).List := Cell.Link;
      Cell.Parent := Current_Scope.Scope;
      return Cell;
   end Add_Identifier;

   function Get_Name (Node : in Named_Node'Class) return String is
   begin
      if Node.Cell /= null then
         return Id_Table.Table (Node.Cell.Identifier).Str.all;
      else
         return "*nil*";
      end if;
   end Get_Name;

   procedure Add_Identifier (Node : access Named_Node'Class) is
      Index : Uniq_Id;
      Cell : Scope_Cell_Acc;
   begin
      Index := Get_Identifier (Tokens.Get_Identifier);
      Cell := Add_Identifier (Index, Node);
      if Cell = null then
         raise Identifier_Redefined;
      end if;
      Node.Cell := Cell;
   end Add_Identifier;

   function Find_Identifier_In_Scope (Scope : N_Scope_Acc)
                                      return Named_Node_Acc is
      Cell : Scope_Cell_Acc;
      Index : Uniq_Id;
   begin
      Index := Get_Identifier (Tokens.Get_Identifier);
      Cell := Id_Table.Table (Index).Cell;
      loop
         if Cell = null then
            return null;
         end if;
         if Cell.Parent = Scope then
            return Cell.Node;
         else
            Cell := Cell.Link;
         end if;
      end loop;
   end Find_Identifier_In_Scope;

   function Import_Uniq_Identifier (Node : Named_Node_Acc) return Boolean is
   begin
      return Add_Identifier (Node.Cell.Identifier, Node) /= null;
   end Import_Uniq_Identifier;

   function Is_Identifier_Imported (Cell : Scope_Cell_Acc) return Boolean is
   begin
      return Cell.Parent = Current_Scope.Scope;
   end Is_Identifier_Imported;

   procedure Import_Identifier (Node : Named_Node_Acc) is
   begin
      raise Errors.Internal_Error;
   end Import_Identifier;

   procedure Disp_Id_Table is
      use Ada.Text_IO;
   begin
      for I in Id_Table.First .. Id_Table.Last loop
         Put_Line (Uniq_Id'Image (I) & "str: `" & Id_Table.Table (I).Str.all
                   & "', next: " & Uniq_Id'Image (Id_Table.Table (I).Next));
      end loop;
   end Disp_Id_Table;
end Types;

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
with GNAT.Case_Util;
with Tokens;
with Errors;

package body Types is


   --------------------------------------------
   --  Root of the tree parsed from the idl  --
   --------------------------------------------

   --------------------
   --  Set_Location  --
   --------------------
   procedure Set_Location (N : in out N_Root'Class; Loc : Errors.Location) is
   begin
      N.Loc := Loc;
   end Set_Location;

   --------------------
   --  Get_Location  --
   --------------------
   function Get_Location (N : N_Root'Class) return Errors.Location is
   begin
      return N.Loc;
   end Get_Location;



   ---------------------------------------------------
   --  Named nodes in the tree parsed from the idl  --
   ---------------------------------------------------

   ----------------
   --  Get_Name  --
   ----------------
   function Get_Name (Node : in N_Named'Class) return String is
   begin
      if Node.Definition /= null then
         return Node.Definition.Name.all;
      else
         return "*null*";
      end if;
   end Get_Name;



   -----------------------------
   --  Identifier definition  --
   -----------------------------

   ----------------
   --  Get_Node  --
   ----------------
   function Get_Node (Definition : Identifier_Definition_Acc)
                      return N_Named_Acc is
   begin
      if Definition /= null then
         return Definition.Node;
      else
         raise Errors.Fatal_Error;
      end if;
   end Get_Node;

   --  To deallocate an identifier_definition_list
   procedure Unchecked_Deallocation is new Ada.Unchecked_Deallocation
     (Object => Identifier_Definition_Cell,
      Name => Identifier_Definition_List);

   ---------------------------------
   --  Add_Identifier_Definition  --
   ---------------------------------
   procedure Add_Identifier_Definition (Scope : in out N_Scope'Class;
                                        Identifier : in Identifier_Definition)
   is
      List : Identifier_Definition_List;
   begin
      List := new Identifier_Definition_Cell'(Definition => Identifier,
                                              Next => Scope.Identifier_List);
      Scope.Identifier_List := List;
   end Add_Identifier_Definition;



   ----------------------------
   --  scope handling types  --
   ----------------------------

   --  Definition of a stack of scopes.
   type Scope_Stack;
   type Scope_Stack_Acc is access Scope_Stack;
   type Scope_Stack is record
      Parent : Scope_Stack_Acc;
      Scope : N_Scope_Acc;
   end record;

   --  To deallocate a Scope_Stack
   procedure Unchecked_Deallocation is new Ada.Unchecked_Deallocation
     (Object => Scope_Stack,
      Name => Scope_Stack_Acc);

   --  The top of the stack is kept in Current_Scope,
   --  the bottom in Root_Scope
   Current_Scope : Scope_Stack_Acc := null;
   Root_Scope : Scope_Stack_Acc := null;




   ----------------------------------
   --  identifiers handling types  --
   ----------------------------------

   --  Each identifier is given a unique id number. This number is
   --  its location in the table of all the identifiers definitions :
   --  the id_table.
   --  In order to find easily a given identifier in this id_table,
   --  an hashtable of the position of the identifiers in the
   --  id_table is maintained : the Hash_table. This one keeps the
   --  position in the id_table of the first identifier defined for
   --  each possible hash value. All the identifiers having the same
   --  hash_value are then linked : each one has a pointer on the
   --  next defined.

   --  dimension of the hashtable
   Hash_Mod : constant Hash_Value_Type := 2053;

   --  The hash table of the location of the identifiers in the
   --  id_table
   type Hash_Table_Type is array (0 .. Hash_Mod - 1) of Uniq_Id;
   Hash_Table : Hash_Table_Type := (others => Nil_Uniq_Id);

   --  Type of an entry in the id_table.
   --  it contains the following :
   --    - the identifier_definition,
   --    - a pointer on the entry correponding to the definition
   --  of an identifier with the same hash value.
   type Hash_Entry is record
      Definition : Identifier_Definition_Acc := null;
      Next : Uniq_Id;
   end record;

   --  The id_table. It is actually an variable size table. If it
   --  becomes to little, it grows automatically.
   package Id_Table is new GNAT.Table
     (Table_Component_Type => Hash_Entry, Table_Index_Type => Uniq_Id,
      Table_Low_Bound => Nil_Uniq_Id + 1, Table_Initial => 256,
      Table_Increment => 100);

   ------------
   --  Hash  --
   ------------
   function Hash (Str : in String) return Hash_Value_Type is
      Res : Hash_Value_Type := 0;
   begin
      for I in Str'Range loop
         Res := ((Res and 16#0fffffff#) * 16) xor
           Character'Pos (GNAT.Case_Util.To_Lower (Str (I)));
      end loop;
      return Res;
   end Hash;



   -------------------------------------
   --  scope handling types  methods  --
   -------------------------------------

   ----------------------
   --  Get_Root_Scope  --
   ----------------------
   function Get_Root_Scope return N_Scope_Acc is
   begin
      return Root_Scope.Scope;
   end Get_Root_Scope;

   -------------------------
   --  Get_Current_Scope  --
   -------------------------
   function Get_Current_Scope return N_Scope_Acc is
   begin
      return Current_Scope.Scope;
   end Get_Current_Scope;

   ------------------
   --  Push_Scope  --
   ------------------
   procedure Push_Scope (Scope : access N_Scope'Class) is
      Stack : Scope_Stack_Acc;
   begin
      Stack := new Scope_Stack;
      Stack.Parent := Current_Scope;
      Stack.Scope := N_Scope_Acc (Scope);
      if Current_Scope = null then
         Root_Scope := Stack;
      end if;
      Current_Scope := Stack;
   end Push_Scope;

   -----------------
   --  Pop_Scope  --
   -----------------
   procedure Pop_Scope is
      Old_Scope : Scope_Stack_Acc;
      Definition_List : Identifier_Definition_List;
      Old_Definition_List : Identifier_Definition_List;
   begin
      Old_Scope := Current_Scope;
      Current_Scope := Old_Scope.Parent;
      --  Remove all definition of scope from the hash table, and
      --  replace them by the previous one.
      Definition_List := Old_Scope.Scope.Identifier_List;
      while Definition_List /= null loop
         Id_Table.Table (Definition_List.Definition.Id).Definition :=
           Definition_List.Definition.Previous_Definition;
         Old_Definition_List := Definition_List;
         Definition_List := Definition_List.Next;
         Unchecked_Deallocation (Old_Definition_List);
      end loop;
      Unchecked_Deallocation (Old_Scope);
   end Pop_Scope;



   ------------------------------------
   --  identifiers handling methods  --
   ------------------------------------

   -----------------------------------
   --  Check_identifier_Identifier  --
   -----------------------------------
   function Check_Identifier_Index (Identifier : String) return Uniq_Id is
      use Tokens;
      Hash_Index : Hash_Value_Type := Hash (Identifier) mod Hash_Mod;
      Index : Uniq_Id := Hash_Table (Hash_Index);
   begin
      if Index /= Nil_Uniq_Id then
         while Id_Table.Table (Index).Definition.Name /= null loop
            if Idl_Identifier_Equal
              (Id_Table.Table (Index).Definition.Name.all,
               Identifier) /= Differ
            then
               return Index;
            end if;
            if Id_Table.Table (Index).Next = Nil_Uniq_Id then
               exit;
            end if;
            Index := Id_Table.Table (Index).Next;
         end loop;
      end if;
      --  return  Nil_Uniq_Id
      return Index;
   end Check_Identifier_Index;

   ----------------------------------
   --  Create_Indentifier_Index    --
   ----------------------------------
   function Create_Identifier_Index (Identifier : String) return Uniq_Id is
      use Tokens;
      Hash_Index : Hash_Value_Type := Hash (Identifier) mod Hash_Mod;
      Index : Uniq_Id := Hash_Table (Hash_Index);
   begin
      if Index = Nil_Uniq_Id then
         Id_Table.Increment_Last;
         Index := Id_Table.Last;
         Hash_Table (Hash_Index) := Index;
      else
         while Id_Table.Table (Index).Definition.Name /= null loop
            if Idl_Identifier_Equal
              (Id_Table.Table (Index).Definition.Name.all,
               Identifier) /= Differ
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
      Id_Table.Table (Index) := (Definition => null,
                                 Next => Nil_Uniq_Id);
      return Index;
   end Create_Identifier_Index;

   ----------------------------------
   --  Find_Identifier_Definition  --
   ----------------------------------
   function Find_Identifier_Definition return Identifier_Definition_Acc is
      Index : Uniq_Id;
   begin
      Index := Check_Identifier_Index (Tokens.Get_Lexer_String);
      if Index /= Nil_Uniq_Id then
         return Id_Table.Table (Index).Definition;
      else
         return null;
      end if;
   end Find_Identifier_Definition;

   ----------------------------
   --  Find_Identifier_Node  --
   ----------------------------
   function Find_Identifier_Node return N_Named_Acc is
      Definition : Identifier_Definition_Acc;
   begin
      Definition := Find_Identifier_Definition;
      if Definition = null then
         return null;
      else
         return Definition.Node;
      end if;
   end Find_Identifier_Node;

   ---------------------------
   --  Redefine_Identifier  --
   ---------------------------
   procedure Redefine_Identifier
     (Definition : Identifier_Definition_Acc; Node : access N_Named'Class) is
   begin
      if Definition.Node = null or else Node.Definition /= null then
         raise Errors.Internal_Error;
      end if;
      Definition.Node.Definition := null;
      --  free????????
      Definition.Node := N_Named_Acc (Node);
      Node.Definition := Definition;
   end Redefine_Identifier;

   ----------------------
   --  Add_Identifier  --
   ----------------------
   function Add_Identifier (Node : access N_Named'Class)
                            return Boolean is
      Definition : Identifier_Definition_Acc;
      Index : Uniq_Id;
   begin
      Index := Create_Identifier_Index (Tokens.Get_Lexer_String);
      Definition := Id_Table.Table (Index).Definition;
      --  Checks if the identifier is not being redefined in the same
      --  scope.
      if Definition /= null
        and then Definition.Parent_Scope = Current_Scope.Scope then
         return False;
      end if;
      --  Creates a new definition.
      Definition := new Identifier_Definition;
      Definition.Name := new String'(Tokens.Get_Lexer_String);
      Definition.Id := Index;
      Definition.Node := N_Named_Acc (Node);
      Definition.Previous_Definition := Id_Table.Table (Index).Definition;
      Definition.Parent_Scope := Current_Scope.Scope;
      Id_Table.Table (Index).Definition := Definition;
      Add_Identifier_Definition (Current_Scope.Scope.all, Definition.all);
      Node.Definition := Definition;
      return True;
   end Add_Identifier;



--  INUTILE ???

--    procedure Set_Back_End (N : in out N_Root'Class;
--                            Be : access N_Back_End'Class) is
--    begin
--       if N.Back_End /= null then
--          raise Errors.Internal_Error;
--       end if;
--       N.Back_End := N_Back_End_Acc (Be);
--    end Set_Back_End;

--    function Get_Back_End (N : N_Root'Class) return N_Back_End_Acc is
--    begin
--       return N.Back_End;
--    end Get_Back_End;
--
--
--
--
--   function Add_Identifier (Id : String_Cacc) return Uniq_Id is
--      use Tokens;
--      Hash_Index : Hash_Value_Type := Hash (Id.all) mod Hash_Mod;
--      Index : Uniq_Id := Hash_Table (Hash_Index);
--   begin
--      if Index = Nil_Uniq_Id then
--         Id_Table.Increment_Last;
--         Index := Id_Table.Last;
--         Hash_Table (Hash_Index) := Index;
--      else
--         while Id_Table.Table (Index).Str /= null loop
--            if Idl_Identifier_Equal (Id_Table.Table (Index).Str.all,
--                                      Id.all) /= Differ
--            then
--               return Index;
--            end if;
--            if Id_Table.Table (Index).Next = Nil_Uniq_Id then
--               Id_Table.Increment_Last;
--               Id_Table.Table (Index).Next := Id_Table.Last;
--               Index := Id_Table.Last;
--               exit;
--            end if;
--            Index := Id_Table.Table (Index).Next;
--         end loop;
--      end if;
--      --         Add an entry in INDEX.
--      Id_Table.Table (Index) := (Str => Id,
--                                 Next => Nil_Uniq_Id,
--                                 Cell => null,
--                                 List => null);
--      return Index;
--   end Add_Identifier;

--
--    function Is_Identifier_Imported (Cell : Identifier_Definition_Acc)
--                                     return Boolean is
--    begin
--       return Cell.Parent = Current_Scope.Scope;
--    end Is_Identifier_Imported;
--
--
--    procedure Disp_Id_Table is
--       use Ada.Text_IO;
--    begin
--       for I in Id_Table.First .. Id_Table.Last loop
--          Put_Line (Uniq_Id'Image (I) & "str: `" & Id_Table.Table (I).Str.all
--                    & "', next: " & Uniq_Id'Image (Id_Table.Table (I).Next));
--       end loop;
--    end Disp_Id_Table;
--
--
--    procedure Import_Identifier (Node : N_Named_Acc) is
--    begin
--       raise Errors.Internal_Error;
--    end Import_Identifier;
--
--
--    function Import_Uniq_Identifier (Node : N_Named_Acc) return Boolean is
--    begin
--       return Add_Node_To_Id_Table (Node.Cell.Identifier, Node) /= null;
--    end Import_Uniq_Identifier;

--  FIXME : to be uncomment when used
--    function Find_Identifier_Node (Scope : N_Scope_Acc)
--                                   return N_Named_Acc is
--       Definition : Identifier_Definition_Acc;
--    begin
--       Definition := Find_Identifier_Definition;
--       loop
--          if Definition = null then
--             return null;
--          end if;
--          if Definition.Parent_Scope = Scope then
--             return Definition.Node;
--          else
--             Definition := Definition.Previous_Definition;
--          end if;
--       end loop;
--    end Find_Identifier_Node;


end Types;

------------------------------------------------------------------------------
--                                                                          --
--                          ADABROKER COMPONENTS                            --
--                                                                          --
--                         I D L _ F E . T Y P E S                          --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1999-2000 ENST Paris University, France.          --
--                                                                          --
-- AdaBroker is free software; you  can  redistribute  it and/or modify it  --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. AdaBroker  is distributed  in the hope that it will be  useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with AdaBroker; see file COPYING. If  --
-- not, write to the Free Software Foundation, 59 Temple Place - Suite 330, --
-- Boston, MA 02111-1307, USA.                                              --
--                                                                          --
--             AdaBroker is maintained by ENST Paris University.            --
--                     (email: broker@inf.enst.fr)                          --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Unchecked_Deallocation;
with System;
with GNAT.Case_Util;
with Idl_Fe.Errors;
with Idl_Fe.Lexer;
with Idl_Fe.Tree; use Idl_Fe.Tree;
with Idl_Fe.Tree.Synthetic; use Idl_Fe.Tree.Synthetic;
with Idl_Fe.Debug;
pragma Elaborate (Idl_Fe.Debug);

with Utils; use Utils;

package body Idl_Fe.Types is

   -----------
   -- Debug --
   -----------

   Flag : constant Natural
     := Idl_Fe.Debug.Is_Active ("idl_fe.types");
   procedure O is new Idl_Fe.Debug.Output (Flag);

   Flag2 : constant Natural
     := Idl_Fe.Debug.Is_Active ("idl_fe.types_method_trace");
   procedure O2 is new Idl_Fe.Debug.Output (Flag2);

   --------------------------------------
   -- Root of the tree parsed from IDL --
   --------------------------------------

   ------------------
   -- Set_Location --
   ------------------

   procedure Set_Location
     (N : Node_Id;
      Loc : Idl_Fe.Errors.Location) is
      Loc2 : Idl_Fe.Errors.Location;
      use Idl_Fe.Errors;
   begin
      Loc2.Col := Loc.Col;
      Loc2.Line := Loc.Line;
      pragma Assert (Loc.Filename /= null);
      Loc2.Filename := new String'(Loc.Filename.all);
      if (Loc.Dirname = null) then
         Loc2.Dirname := null;
      else
         Loc2.Dirname := new String'(Loc.Dirname.all);
      end if;
      Set_Loc (N, Loc2);
   end Set_Location;

   ------------------
   -- Get_Location --
   ------------------

   function Get_Location
     (N : Node_Id)
     return Idl_Fe.Errors.Location is
   begin
      return Loc (N);
   end Get_Location;


   ----------------------------------
   --  Management of const values  --
   ----------------------------------

   -----------------
   --  Duplicate  --
   -----------------

   function Duplicate (C : in Constant_Value_Ptr)
                       return Constant_Value_Ptr is
      Result : Constant_Value_Ptr := new Constant_Value (Kind => C.Kind);
   begin
      case C.Kind is
         when C_Octet
           | C_Short
           | C_Long
           | C_LongLong
           | C_UShort
           | C_ULong
           | C_ULongLong
           | C_General_Integer =>
            Result.Integer_Value := C.Integer_Value;
         when C_Char =>
            Result.Char_Value := C.Char_Value;
         when C_WChar =>
            Result.WChar_Value := C.WChar_Value;
         when C_Boolean =>
            Result.Boolean_Value := C.Boolean_Value;
         when C_Float
           | C_Double
           | C_LongDouble
           | C_General_Float =>
            Result.Float_Value := C.Float_Value;
         when C_String =>
            Result.String_Value := C.String_Value;
         when C_WString =>
            Result.WString_Value := C.WString_Value;
         when C_Fixed
           | C_General_Fixed =>
            Result.Fixed_Value := C.Fixed_Value;
            Result.Digits_Nb := C.Digits_Nb;
            Result.Scale := C.Scale;
         when C_Enum =>
            Result.Enum_Value := C.Enum_Value;
            Result.Enum_Name := C.Enum_Name;
         when C_No_Kind =>
            null;
      end case;
      return Result;
   end Duplicate;

   ------------
   --  Free  --
   ------------

   procedure Free (C : in out Constant_Value_Ptr) is
      procedure Real_Free is new Ada.Unchecked_Deallocation
        (Constant_Value, Constant_Value_Ptr);
   begin
      case C.Kind is
         when C_String =>
            Free_Idl_String (C.String_Value);
         when C_WString =>
            Free_Idl_Wide_String (C.WString_Value);
         when others =>
            null;
      end case;
      Real_Free (C);
   end Free;


   ---------------------
   -- A list of nodes --
   ---------------------

   ------------
   --  Head  --
   ------------

   function Head
     (NL : Node_List)
     return Node_Id is
   begin
      pragma Assert (NL /= Nil_List);
      return NL.Car;
   end Head;

   ----------------
   --  Is_Empty  --
   ----------------

   function Is_Empty
     (NL : Node_List)
     return Boolean is
   begin
      return NL = Nil_List;
   end Is_Empty;

   --------------
   --  Length  --
   --------------

   function Length
     (NL : Node_List)
     return Natural
   is
      Current : Node_List
        := NL;
      Count : Natural
        := 0;
   begin
      while not Is_Empty (Current) loop
         Count := Count + 1;
         Current := Current.Cdr;
      end loop;

      return Count;
   end Length;

   ----------
   -- Init --
   ----------

   procedure Init (It : out Node_Iterator; List : Node_List) is
   begin
      It := Node_Iterator (List);
   end Init;

   -------------------
   -- Get_Next_Node --
   -------------------

   procedure Get_Next_Node
     (It : in out Node_Iterator;
      Node : out Node_Id) is
   begin
      pragma Debug (O ("Getting head of list at "
                       & Img (It.all'Address)));
      Node := It.Car;
      It := Node_Iterator (It.Cdr);
   end Get_Next_Node;

   --------------
   --  Is_End  --
   --------------
   function Is_End (It : Node_Iterator) return Boolean is
   begin
      return Is_Empty (Node_List (It));
   end Is_End;

   -------------------
   --  Append_Node  --
   -------------------

   procedure Append_Node
     (List : in out Node_List;
      Node : in Node_Id) is
   begin
      List := Append_Node (List, Node);
   end Append_Node;

   -------------------
   --  Append_Node  --
   -------------------

   function Append_Node
     (List : Node_List;
      Node : Node_Id) return Node_List
   is
      Cell, Last : Node_List;
   begin
      Cell := new Node_List_Cell'(Car => Node, Cdr => null);
      if List = null then
         return Cell;
      else
         Last := List;
         while Last.Cdr /= null loop
            Last := Last.Cdr;
         end loop;
         Last.Cdr := Cell;
         return List;
      end if;
   end Append_Node;

   ---------------------
   --  Insert_Before  --
   ---------------------

   procedure Insert_Before
     (List : in out Node_List;
      Node : Node_Id;
      Before : Node_Id)
   is
      Cell : Node_List;
   begin
      pragma Assert (List /= Nil_List);

      if List.Car = Before then
         Cell := new Node_List_Cell'
           (Car => Node,
            Cdr => List);
         List := Cell;
      else
         Insert_Before (List.Cdr, Node, Before);
      end if;
   end Insert_Before;

   --------------------
   --  Insert_After  --
   --------------------

   procedure Insert_After
     (List : in Node_List;
      Node : Node_Id;
      After : Node_Id)
   is
      Cell : Node_List;
   begin
      pragma Assert (List /= Nil_List);

      if List.Car = After then
         Cell := new Node_List_Cell'
           (Car => Node,
            Cdr => List.Cdr);
         List.Cdr := Cell;
      else
         Insert_After (List.Cdr, Node, After);
      end if;
   end Insert_After;

   ------------------
   --  Is_In_List  --
   ------------------

   function Is_In_List
     (List : Node_List;
      Node : Node_Id)
     return Boolean is
   begin
      pragma Debug (O2 ("Is_In_List : enter"));
      if List = Nil_List then
         pragma Debug (O2 ("Is_In_List : enter"));
         pragma Debug (O ("Is_In_List : nil_list"));
         return False;
      end if;
      if List.Car = Node then
         pragma Debug (O2 ("Is_In_List : enter"));
         pragma Debug (O ("Is_In_List : found"));
         return True;
      else
         pragma Debug (O2 ("Is_In_List : enter"));
         pragma Debug (O ("Is_In_List : searching further"));
         return Is_In_List (List.Cdr, Node);
      end if;
   end Is_In_List;

   -------------------------
   --  Is_In_Parent_List  --
   -------------------------

   function Is_In_Parent_List
     (List : Node_List;
      Node : Node_Id)
      return Boolean is
   begin
      if List = Nil_List then
         return False;
      end if;
      pragma Assert (Kind (List.Car) = K_Scoped_Name);
      if Value (List.Car) = Value (Node) then
         return True;
      else
         return Is_In_Parent_List (List.Cdr, Node);
      end if;
   end Is_In_Parent_List;

   -------------------
   --  Remove_Node  --
   -------------------

   procedure Unchecked_Deallocation is
      new Ada.Unchecked_Deallocation
     (Node_List_Cell, Node_List);

   -------------------
   --  Remove_Node  --
   -------------------

   function Remove_Node
     (List : Node_List;
      Node : Node_Id)
     return Node_List is
   begin
      if List /= Nil_List then
         if List.Car = Node then
            declare
               Old_List : Node_List := List;
               Old_Cdr : constant Node_List
                 := List.Cdr;
            begin
               pragma Debug (O ("Deallocating list cell at "
                                & Img (Old_List.all'Address)));
               Unchecked_Deallocation (Old_List);
               return Old_Cdr;
            end;
         else
            List.Cdr := Remove_Node (List.Cdr, Node);
         end if;
      end if;
      return List;
   end Remove_Node;

   procedure Remove_Node
     (List : in out Node_List;
      Node : Node_Id) is
   begin
      List := Remove_Node (List, Node);
   end Remove_Node;

   ------------
   --  Free  --
   ------------

   procedure Free
     (List : in out Node_List)
   is
      Old_List : Node_List;
   begin
      while List /= null loop
         Old_List := List;
         List := List.Cdr;
         Unchecked_Deallocation (Old_List);
      end loop;
   end Free;

   ------------------
   --  Get_Length  --
   ------------------

   function Get_Length
     (List : Node_List)
     return Integer
   is
      Temp_List : Node_List := List;
      Result : Integer := 0;
   begin
      while Temp_List /= Nil_List loop
         Result := Result + 1;
         Temp_List := Temp_List.Cdr;
      end loop;
      return Result;
   end Get_Length;

   --------------------------
   --  Simplify node list  --
   --------------------------

   function Simplify_Node_List
     (In_List : Node_List)
     return Node_List
   is
      Result_List : Node_List := null;
      It : Node_Iterator;
      Node : Node_Id;
   begin
      Init (It, In_List);
      while not Is_End (It) loop
         Get_Next_Node (It, Node);

         if not Is_In_List (Result_List, Node) then
            Append_Node (Result_List, Node);
         end if;
      end loop;
      return Result_List;
   end Simplify_Node_List;

   procedure Merge_List
     (Into : in out Node_List;
      From : in Node_List)
   is
      It : Node_Iterator;
      N : Node_Id;
   begin
      Init (It, From);
      while not Is_End (It) loop
         Get_Next_Node (It, N);

         if not Is_In_List (Into, N) then
            Append_Node (Into, N);
         end if;
      end loop;
   end Merge_List;

   -----------------------------
   --  Identifier definition  --
   -----------------------------

   ----------------
   --  Get_Node  --
   ----------------

   function Get_Node
     (Definition : Identifier_Definition_Acc)
     return Node_Id is
   begin
      if Definition  /= null then
         return Definition.Node;
      else
         raise Idl_Fe.Errors.Fatal_Error;
      end if;
   end Get_Node;

   --  To deallocate an identifier_definition_list
   procedure Unchecked_Deallocation is
      new Ada.Unchecked_Deallocation
     (Object => Identifier_Definition_Cell,
      Name   => Identifier_Definition_List);

   ---------------------------------
   --  Add_Identifier_Definition  --
   ---------------------------------
   procedure Add_Identifier_Definition
     (Scope : Node_Id;
      Identifier : in Identifier_Definition_Acc)
   is
      List : Identifier_Definition_List;
   begin
      pragma Debug (O2 ("Add_Identifier_Definition : enter"));
      List := new Identifier_Definition_Cell'
        (Definition => Identifier,
         Next => Identifier_List (Scope));
      Set_Identifier_List (Scope, List);
      Add_Definition_To_Storage (Identifier);
      pragma Debug (O2 ("Add_Identifier_Definition : end"));
   end Add_Identifier_Definition;

   ----------------------------
   --  scope handling types  --
   ----------------------------

   --  Definition of a stack of scopes.
   type Scope_Stack;
   type Scope_Stack_Acc is access Scope_Stack;
   type Scope_Stack is record
      Parent : Scope_Stack_Acc;
      Scope : Node_Id;
   end record;

   --  To deallocate a Scope_Stack
   procedure Unchecked_Deallocation is new Ada.Unchecked_Deallocation
     (Object => Scope_Stack,
      Name => Scope_Stack_Acc);

   --  The top of the stack is kept in Current_Scope,
   --  the bottom in Root_Scope
   Current_Scope : Scope_Stack_Acc := null;
   Root_Scope : Scope_Stack_Acc := null;

   ------------------------------------------------
   --  The Gnat_Table type implemented functions --
   ------------------------------------------------

   Initial : constant Positive := 37;
   --  The size allocated at the creation of the table

   Min : constant Integer := Integer (Nil_Uniq_Id + 1);
   --  Subscript of the minimum entry in the currently allocated table

   type size_t is new Integer;

   procedure Reallocate (T : in out Table);
   --  Reallocate the existing table according to the current value stored
   --  in Max. Works correctly to do an initial allocation if the table
   --  is currently null.

   procedure Allocate (T : in out Table;
                       Num : in Integer := 1;
                       Result : out Uniq_Id) is
      Old_Last : constant Integer := T.Last_Val;

   begin
      T.Last_Val := T.Last_Val + Num;

      if T.Last_Val > T.Max then
         Reallocate (T);
      end if;

      Result := Uniq_Id (Old_Last + 1);
      return;
   end Allocate;

   procedure Decrement_Last (T : in out Table) is
   begin
      T.Last_Val := T.Last_Val - 1;
   end Decrement_Last;

   procedure Increment_Last (T : in out Table) is
   begin
      T.Last_Val := T.Last_Val + 1;

      if T.Last_Val > T.Max then
         Reallocate (T);
      end if;
   end Increment_Last;

   procedure Init (T : in out Table) is
      Old_Length : Integer := T.Length;

   begin
      T.Last_Val := Min - 1;
      T.Max      := Min + Initial - 1;
      T.Length   := T.Max - Min + 1;

      --  If table is same size as before (happens when table is never
      --  expanded which is a common case), then simply reuse it. Note
      --  that this also means that an explicit Init call right after
      --  the implicit one in the package body is harmless.

      if Old_Length = T.Length then
         return;

      --  Otherwise we can use Reallocate to get a table of the right size.
      --  Note that Reallocate works fine to allocate a table of the right
      --  initial size when it is first allocated.

      else
         Reallocate (T);
      end if;
   end Init;

   function Last (T : Table) return Uniq_Id is
   begin
      return Uniq_Id (T.Last_Val);
   end Last;

   procedure Reallocate (T : in out Table)  is

      function realloc
        (memblock : Table_Ptr;
         size     : size_t)
         return     Table_Ptr;
      pragma Import (C, realloc);

      function malloc
        (size     : size_t)
         return     Table_Ptr;
      pragma Import (C, malloc);

      New_Size : size_t;

   begin
      if T.Max < T.Last_Val then
         if T.Length = 0 and T.Max = Min - 1 then
            T.Max := Min + Initial - 1;
            T.Length :=  T.Max - Min + 1;
         else
            pragma Assert (not Locked);
            while T.Max < T.Last_Val loop
               T.Length := T.Length * 2;
               T.Max := Min + T.Length - 1;
            end loop;
         end if;
      end if;

      New_Size :=
        size_t ((T.Max - Min + 1) *
                (Table_Type'Component_Size / System.Storage_Unit));

      if T.Table = null then
         T.Table := malloc (New_Size);

      elsif New_Size > 0 then
         T.Table :=
           realloc
             (memblock => T.Table,
              size     => New_Size);
      end if;

      if T.Length /= 0 and then T.Table = null then
         raise Storage_Error;
      end if;

   end Reallocate;

   procedure Release (T : in out Table) is
   begin
      T.Length := T.Last_Val - Min + 1;
      T.Max    := T.Last_Val;
      Reallocate (T);
   end Release;

   procedure Set_Last (T : in out Table; New_Val : in Uniq_Id) is
      Old_Last : Integer;

   begin
      if Integer (New_Val) < T.Last_Val then
         T.Last_Val := Integer (New_Val);
      else
         Old_Last := T.Last_Val;
         T.Last_Val := Integer (New_Val);

         if T.Last_Val > T.Max then
            Reallocate (T);
         end if;
      end if;
   end Set_Last;

   --------------------------------
   -- Identifiers handling types --
   --------------------------------

   Id_Table : Table;

   -----------
   -- Hash  --
   -----------

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

   ---------------------------
   --  Add_Int_Val_Forward  --
   ---------------------------

   procedure Add_Int_Val_Forward
     (Node : in Node_Id)
   is
      UF : Node_List
        := Unimplemented_Forwards (Current_Scope.Scope);
   begin
      Append_Node (UF, Node);
      Set_Unimplemented_Forwards
        (Current_Scope.Scope, UF);
   end Add_Int_Val_Forward;

   ------------------------------
   --  Add_Int_Val_Definition  --
   ------------------------------
   procedure Add_Int_Val_Definition
     (Node : in Node_Id)
   is
      UF : Node_List
        := Unimplemented_Forwards (Current_Scope.Scope);
   begin
      Remove_Node (UF, Node);
      Set_Unimplemented_Forwards
        (Current_Scope.Scope, UF);
   end Add_Int_Val_Definition;

   ----------------------
   --  Get_Root_Scope  --
   ----------------------

   function Get_Root_Scope
     return Node_Id is
   begin
      return Root_Scope.Scope;
   end Get_Root_Scope;

   -------------------------
   --  Get_Current_Scope  --
   -------------------------

   function Get_Current_Scope
     return Node_Id is
   begin
      return Current_Scope.Scope;
   end Get_Current_Scope;

   ---------------------------
   -- Get_Current_Gen_Scope --
   ---------------------------

   function Get_Current_Gen_Scope
     return Node_Id
   is
      Scope : Scope_Stack_Acc
        := Current_Scope;
   begin
      while Scope /= null
        and then not Is_Gen_Scope (Scope.Scope) loop
         Scope := Scope.Parent;
      end loop;
      if Scope /= null then
         return Scope.Scope;
      else
         return No_Node;
      end if;
   end Get_Current_Gen_Scope;

   -------------------------
   --  Get_Previous_Scope  --
   -------------------------

   function Get_Previous_Scope
     return Node_Id is
   begin
      return Current_Scope.Parent.Scope;
   end Get_Previous_Scope;

   --------------------------
   -- Identifiers handling --
   --------------------------

   function Identifier_Index
     (Identifier : String;
      Hash_Table : Hash_Table_Type;
      Table : Idl_Fe.Types.Table)
     return Uniq_Id;
   --  Return the Uniq_id of Identifier if it is defined in
   --  Table, or else return Nil_Uniq_Id.

   function Imported_Identifier_Index
     (Identifier : String)
     return Uniq_Id;
   --  Check if the  uniq_id from an identifier is already defined
   --  in the imported table.
   --  return it or Nil_Unqiq_Id

   procedure Create_Identifier_Index
     (Identifier : String;
      Hash_Table : in out Hash_Table_Type;
      Table : in out Idl_Fe.Types.Table;
      Index : out Uniq_Id);
   --  Create the uniq_id entry for an identifier if it doesn't exist
   --  return it

   function Create_Identifier_In_Storage
     (Identifier : String)
     return Uniq_Id;
   --  Create the uniq_id entry for an identifier in the storage table
   --  at the end of the scope parsing
   --  return it

   function Create_Identifier_In_Imported
     (Identifier : String;
      Scope : Node_Id)
     return Uniq_Id;
   --  Create the uniq_id entry for an identifier in the imported table of
   --  the given scope
   --  return it

   ------------------
   --  Push_Scope  --
   ------------------

   procedure Push_Scope
     (Scope : Node_Id)
   is
      Stack : Scope_Stack_Acc;
      Index : Uniq_Id;

      procedure Reenter_Definition_List
        (Definition_List : Identifier_Definition_List);
      --  Enter all identifiers in Definition_List into
      --  the global Id_Table and Hash_Table, in reverse
      --  order.

      procedure Reenter_Definition_List
        (Definition_List : Identifier_Definition_List) is
      begin
         --  Definitions must be re-added in the order they
         --  were first created, i. e. stating at the tail
         --  of Definition_List.

         if Definition_List = null then
            return;
         end if;

         pragma Debug (O2 ("Reenter_Definition_List: enter"));

         Reenter_Definition_List (Definition_List.Next);

         --  first find a place for this identifier in the table
         Create_Identifier_Index
           (Definition_List.Definition.Name.all,
            Hash_Table,
            Id_Table,
            Index);
         --  set its fields previous_definition and id that were
         --  not used in the scope table
         Definition_List.Definition.Previous_Definition :=
           Id_Table.Table (Index).Definition;
         Definition_List.Definition.Id := Index;
         --  And finally add it to the scope
         Id_Table.Table (Index).Definition := Definition_List.Definition;
         pragma Debug (O2 ("Reenter_Definition_List: leave"));
      end Reenter_Definition_List;

   begin
      pragma Debug (O2 ("Push_Scope : enter"));
      pragma Assert (Is_Scope (Scope));

      Stack := new Scope_Stack;
      Stack.Parent := Current_Scope;
      Stack.Scope := Scope;
      if Current_Scope = null then
         pragma Debug (O ("Push_Scope : current_scope is null."));
         pragma Debug (O ("Push_Scope : root scope is defined at " &
                          Idl_Fe.Errors.Display_Location
                          (Get_Location (Scope))));
         Root_Scope := Stack;
      end if;
      Current_Scope := Stack;

      pragma Debug (O ("Push_Scope : putting the old definition " &
                       "in the id_table."));
      --  Add all definition of the new scope into the hash table,
      --  in case there are some. Useful when a scoped is reopened.
      Reenter_Definition_List (Identifier_List (Scope));
      pragma Debug (O2 ("Push_Scope : end"));
   end Push_Scope;

   -----------------
   --  Pop_Scope  --
   -----------------

   procedure Pop_Scope is
      Old_Scope : Scope_Stack_Acc;
      Definition_List : Identifier_Definition_List;
      Old_Definition_List : Identifier_Definition_List;
      Old_Definition : Identifier_Definition_Acc;
      Forward_Defs : Node_Iterator;
      Forward_Def : Node_Id;
      Hash_Index : Hash_Value_Type;
      Index : Uniq_Id;
   begin
      pragma Debug (O2 ("Pop_Scope : enter"));
      --  Remove all definition of scope from the hash table, and
      --  replace them by the previous one.
      --  Add these definition to the identifier_table of the current_scope
      Definition_List := Identifier_List (Current_Scope.Scope);
      while Definition_List /= null loop
         Old_Definition := Definition_List.Definition;
         --  remove the identifier from the id_table
         Id_Table.Table (Definition_List.Definition.Id).Definition :=
           Definition_List.Definition.Previous_Definition;
         if Definition_List.Definition.Previous_Definition = null then
            pragma Debug (O ("Pop_Scope: removing "
                             & Definition_List.Definition.Name.all));

            Hash_Index := Hash (Definition_List.Definition.Name.all)
              mod Hash_Mod;

            if Id_Table.Table (Hash_Table (Hash_Index)).Definition = null then
               Hash_Table (Hash_Index) := Nil_Uniq_Id;
            else
               Index := Hash_Table (Hash_Index);
               while Id_Table.Table (Id_Table.Table (Index).Next).Definition
                 /= null loop
                  Index := Id_Table.Table (Index).Next;
               end loop;
               Id_Table.Table (Index).Next := Nil_Uniq_Id;
            end if;
         end if;
         --  once the definition is no more in the id table but
         --  only in the table of the scope, the previous_definition
         --  field has no more interest, as well as the Id field
         Old_Definition.Previous_Definition := null;
         Old_Definition.Id := Nil_Uniq_Id;

         Old_Definition_List := Definition_List;
         Definition_List := Definition_List.Next;
      end loop;

      Old_Scope := Current_Scope;
      Current_Scope := Old_Scope.Parent;
      --  Test if all forward definitions were implemented
      if False
        or else Kind (Old_Scope.Scope) = K_Repository
        or else Kind (Old_Scope.Scope) = K_Module
      then
         Init (Forward_Defs,
               Unimplemented_Forwards (Old_Scope.Scope));

         while not Is_End (Forward_Defs) loop
            Get_Next_Node (Forward_Defs, Forward_Def);

            Idl_Fe.Errors.Parser_Error
              ("The forward declaration " &
               Idl_Fe.Errors.Display_Location
               (Get_Location (Forward_Def)) &
               " is not implemented.",
               Idl_Fe.Errors.Error,
               Get_Location (Old_Scope.Scope));
         end loop;
      end if;

      --  Free the forward definition list

      if Kind (Old_Scope.Scope) = K_Repository
        or else Kind (Old_Scope.Scope) = K_Module then
         declare
            UF : Node_List
              := Unimplemented_Forwards (Old_Scope.Scope);
         begin
            Set_Unimplemented_Forwards (Old_Scope.Scope, Nil_List);
            Free (UF);
         end;
      end if;

      Unchecked_Deallocation (Old_Scope);
      pragma Debug (O2 ("Pop_Scope : end"));
   end Pop_Scope;

   --------------------
   -- Is_Redefinable --
   --------------------

   function Is_Redefinable (Name : String) return Boolean is
      A_Definition : Identifier_Definition_Acc;
   begin
      pragma Debug (O2 ("Is_Redefinable : enter"));
      pragma Debug (O ("Is_Redefinable : the identifier is : " & Name));
      --  Checks if the identifier is already imported
         if Imported_Identifier_Index (Name) /= Nil_Uniq_Id then
            pragma Debug (O2 ("Is_Redefinable : already imported"));
            pragma Debug (O2 ("Is_Redefinable : end"));
            return False;
         end if;
      A_Definition := Find_Identifier_Definition (Name);
      if A_Definition /= null then
         pragma Debug (O ("Is_Redefinable : " &
                          "Definition found is" &
                          Node_Kind'Image
                          (Kind (A_Definition.Node))));
         --  Checks if the identifier is not being redefined in the same
         --  scope.
         if A_Definition.Parent_Scope = Current_Scope.Scope then
            pragma Debug (O2 ("Is_Redefinable : end"));
            return False;
         end if;
         --  An attribute or operation may not be redefined
         if Kind (A_Definition.Node) = K_Operation or
           (Kind (A_Definition.Node) = K_Declarator and then
            Kind (Parent (A_Definition.Node)) = K_Attribute) then
            pragma Debug (O ("Is_Redefinable : cannot redefine an op, attr"));
            pragma Debug (O2 ("Is_Redefinable : end"));
            return False;
         end if;
         --  Ckecks if identifier found is the current scope name:
         --  it is not allowed except for the operation
         if Kind (Current_Scope.Scope) /= K_Operation
           and then A_Definition = Definition (Current_Scope.Scope) then
            pragma Debug (O2 ("Is_Redefinable : end"));
            return False;
         end if;
      end if;
      pragma Debug (O2 ("Is_Redefinable : end"));
      return True;
   end Is_Redefinable;

   ----------------------
   -- Identifier_Index --
   ----------------------

   function Identifier_Index
     (Identifier : String;
      Hash_Table : Hash_Table_Type;
      Table : Idl_Fe.Types.Table)
     return Uniq_Id is
      use Idl_Fe.Lexer;

      Index : Uniq_Id
        := Hash_Table (Hash (Identifier) mod Hash_Mod);

   begin
      pragma Debug (O2 ("Identifier_Index : enter"));
      pragma Debug (O ("Identifier_Index : the identifier is " & Identifier));
      while Index /= Nil_Uniq_Id
        and then Table.Table (Index).Definition.Name /= null
        and then Idl_Identifier_Equal
          (Table.Table (Index).Definition.Name.all,
           Identifier) = Differ
      loop
         pragma Debug (O ("Identifier_Index : identifier is " &
                          Identifier &
                          ", in the table, we have " &
                          Table.Table (Index).Definition.Name.all));
         Index := Table.Table (Index).Next;
      end loop;
      pragma Debug (O2 ("Identifier_Index : end"));
      return Index;
   end Identifier_Index;

   function Imported_Identifier_Index
     (Identifier : String)
     return Uniq_Id
   is
      use Idl_Fe.Lexer;

      Scope : Node_Id;
   begin
      pragma Debug (O2 ("Imported_Identifier_Index : enter"));
      if not Is_Imports (Current_Scope.Scope) then
         pragma Debug (O2 ("Imported_Identifier_Index : end"));
         return Nil_Uniq_Id;
      end if;
      Scope := Current_Scope.Scope;
      pragma Debug (O ("Imported_Identifier_Index : scope type is " &
                       Node_Kind'Image (Kind (Scope)) & "."));

      pragma Debug (O2 ("Imported_Identifier_Index : end"));
      return Identifier_Index
        (Identifier,
         Imported_Table (Scope).Hash_Table,
         Imported_Table (Scope).Content_Table);

   end Imported_Identifier_Index;

   ------------------------------
   -- Create_Indentifier_Index --
   ------------------------------

   procedure Create_Identifier_Index
     (Identifier : String;
      Hash_Table : in out Hash_Table_Type;
      Table : in out Idl_Fe.Types.Table;
      Index : out Uniq_Id)
   is
      use Idl_Fe.Lexer;

      Hash_Value : constant Hash_Value_Type
        := Hash (Identifier) mod Hash_Mod;
      Result : Uniq_Id
        := Hash_Table (Hash_Value);
      Previous : Uniq_Id
        := Nil_Uniq_Id;
   begin
      pragma Debug (O2 ("Create_Identifier_Index : enter, id = "
                        & Identifier & ", hash = "
                        & Img (Integer (Hash_Value))));
      if Result = Nil_Uniq_Id then
         --  No identifier in this slot yet.

         Increment_Last (Table);
         Result := Uniq_Id (Table.Last_Val);
         Hash_Table (Hash_Value) := Result;
      else
         --  This slot already has identifiers.

         while True
           and then Result /= Nil_Uniq_Id
           and then Table.Table (Result).Definition.Name /= null
           and then Idl_Identifier_Equal
             (Table.Table (Result).Definition.Name.all,
              Identifier) = Differ
         loop
            Previous := Result;
            Result := Table.Table (Result).Next;
         end loop;

         if Result /= Nil_Uniq_Id then
            --  This identifier is already in the table.

            Index := Result;
            pragma Debug
              (O2 ("Create_Identifier_Index : end (already in table)"));
            return;
         end if;

         pragma Assert (Previous /= Nil_Uniq_Id);

         Increment_Last (Table);
         Result := Uniq_Id (Table.Last_Val);
         Table.Table (Previous).Next := Uniq_Id (Result);
      end if;

      --  Add an entry in INDEX.
      Table.Table (Result) := (Definition => null,
                              Next => Nil_Uniq_Id);
      Index := Result;
      pragma Debug (O2 ("Create_Identifier_Index : end"));
      return;
   end Create_Identifier_Index;

   --------------------------------
   -- Find_Identifier_Definition --
   --------------------------------

   function Find_Identifier_Definition (Name : String)
                                        return Identifier_Definition_Acc is
      Index : Uniq_Id;
      Imported_Definition, Inherited_Definition : Identifier_Definition_Acc;
      Definition : Identifier_Definition_Acc := null;
   begin
      pragma Debug (O2 ("Find_Identifier_Definition : enter"));
      pragma Debug (O ("Find_Identifier_Definition : " &
                       "the identifier is " & Name));
      Index := Identifier_Index (Name, Hash_Table, Id_Table);
      pragma Debug (O ("Find_Identifier_Definition : " &
                       "check_identifier_index done"));
      if Index /= Nil_Uniq_Id then
         pragma Debug (O ("Find_Identifier_Definition : " &
                          "there is a definition in the id table"));
         Definition := Id_Table.Table (Index).Definition;
         --  is the definition in the scope
         if Definition.Parent_Scope = Current_Scope.Scope then
            pragma Debug (O2 ("Find_Identifier_Definition : end"));
            return Definition;
         end if;
      end if;

      Imported_Definition := Find_Imported_Identifier_Definition (Name);
      --  is the definition imported
      if Imported_Definition /= null then
         pragma Debug (O2 ("Find_Identifier_Definition : end"));
         return Imported_Definition;
      end if;
      --  is the definition inherited
      Inherited_Definition
        := Find_Inherited_Identifier_Definition (Name);
      if Inherited_Definition /= null then
         pragma Debug (O ("Find_Identifier_Definition : " &
                          "Inherited definition is of type " &
                          Node_Kind'Image
                          (Kind (Inherited_Definition.Node))));
         pragma Debug (O2 ("Find_Identifier_Definition : end"));
         return Inherited_Definition;
      end if;
      --  the definition is in a upper scope
      pragma Debug (O2 ("Find_Identifier_Definition : end"));
      return Definition;
   end Find_Identifier_Definition;

   --------------------------
   -- Find_Identifier_Node --
   --------------------------

   function Find_Identifier_Node (Name : String) return Node_Id is
      Definition : Identifier_Definition_Acc;
   begin
      Definition := Find_Identifier_Definition (Name);
      if Definition = null then
         return No_Node;
      else
         return Definition.Node;
      end if;
   end Find_Identifier_Node;

   -------------------------
   -- Redefine_Identifier --
   -------------------------

   procedure Redefine_Identifier
     (A_Definition : Identifier_Definition_Acc;
      Node : Node_Id) is
   begin
      if A_Definition.Node = No_Node
        or else Definition (Node) /= null then
         raise Idl_Fe.Errors.Internal_Error;
      end if;

      Set_Definition (A_Definition.Node, null);
      --  free????????
      A_Definition.Node := Node;
      Set_Definition (Node, A_Definition);
   end Redefine_Identifier;

   --------------------
   -- Add_Identifier --
   --------------------

   function Add_Identifier
     (Node : Node_Id;
      Name : String)
     return Boolean
   is
      Definition : Identifier_Definition_Acc;
      Index : Uniq_Id;
   begin
      pragma Debug (O2 ("Add_Identifier : enter"));
      pragma Debug (O ("Add_Identifier : the identifier is " & Name));
      --  Checks if the identifier is redefinable
      if not Is_Redefinable (Name) then
         pragma Debug (O ("Add_Identifier : identifier not redefinable"));
         pragma Debug (O2 ("Add_Identifier : end"));
         return False;
      end if;
      pragma Debug (O ("Add_Identifier : creating a definition"));
      --  Creates a new definition.
      Create_Identifier_Index
        (Name, Hash_Table, Id_Table, Index);
      Definition := new Identifier_Definition;
      Definition.Name := new String'(Name);
      Definition.Id := Index;
      Definition.Node := Node;
      Definition.Previous_Definition := Id_Table.Table (Index).Definition;
      Definition.Parent_Scope := Current_Scope.Scope;
      Id_Table.Table (Index).Definition := Definition;
      pragma Debug (O ("Add_Identifier : adding definition to " &
                       "the current scope"));
      Add_Identifier_Definition (Current_Scope.Scope, Definition);
      pragma Debug (O ("Add_Identifier : puting the definition in its node"));
      Set_Definition (Node, Definition);
      pragma Debug (O2 ("Add_Identifier : end"));
      return True;
   end Add_Identifier;

   --------------------------------
   -- Find_Identifier_In_Storage --
   --------------------------------

   function Find_Identifier_In_Storage
     (Scope : Node_Id;
      Name : String)
     return Identifier_Definition_Acc
   is

      function Stored_Identifier_Index
        (Scope : Node_Id;
         Identifier : String)
        return Uniq_Id;
      --  Check if the  uniq_id from an identifier is already defined
      --  in the scope and return it or Nil_Uniq_Id

      function Stored_Identifier_Index
        (Scope : Node_Id;
         Identifier : String)
        return Uniq_Id
      is
         use Idl_Fe.Lexer;

      begin
         pragma Debug (O2 ("Stored_Identifier_Index : enter & end"));

         return Identifier_Index
           (Identifier,
            Identifier_Table (Scope).Hash_Table,
            Identifier_Table (Scope).Content_Table);
      end Stored_Identifier_Index;

      Index : Uniq_Id;
   begin
      pragma Debug (O2 ("Find_Identifier_In_Storage : enter"));
      Index := Stored_Identifier_Index (Scope, Name);
      if Index /= Nil_Uniq_Id then
         pragma Debug (O2 ("Find_Identifier_In_Storage : end"));
         return Identifier_Table (Scope).
           Content_Table.Table (Index).Definition;
      else
         pragma Debug (O2 ("Find_Identifier_In_Storage : end"));
         return null;
      end if;
   end Find_Identifier_In_Storage;

   -----------------------------------
   -- Create_Indentifier_In_Storage --
   -----------------------------------

   function Create_Identifier_In_Storage
     (Identifier : String)
     return Uniq_Id
   is
      use Idl_Fe.Lexer;

      IT : Storage
        := Identifier_Table (Current_Scope.Scope);
      Index : Uniq_Id;
   begin
      Create_Identifier_Index
        (Identifier, IT.Hash_Table, IT.Content_Table, Index);
      Set_Identifier_Table (Current_Scope.Scope, IT);
      return Index;
   end Create_Identifier_In_Storage;

   -------------------------------
   -- Add_Definition_To_Storage --
   -------------------------------

   procedure Add_Definition_To_Storage
     (Definition : in Identifier_Definition_Acc)
   is
      Index : Uniq_Id;
   begin
      pragma Debug (O2 ("Add_Definition_To_Storage : enter"));
      Index := Create_Identifier_In_Storage (Definition.Name.all);
      --  their shouldn't be any redefinition
      pragma Assert (Identifier_Table
                     (Current_Scope.Scope).Content_Table.
                     Table (Index).Definition = null);
      Identifier_Table (Current_Scope.Scope).Content_Table.
        Table (Index).Definition := Definition;
      pragma Debug (O2 ("Add_Definition_To_Storage : end"));
   end Add_Definition_To_Storage;


   -----------------------------------------
   -- Find_Imported_Identifier_Definition --
   -----------------------------------------

   function Find_Imported_Identifier_Definition
     (Name : String)
     return Identifier_Definition_Acc
   is

      Index : Uniq_Id;
      Scope : Node_Id;

   begin
      pragma Debug (O2 ("Find_Imported_Identifier_Definition : enter"));
      Scope := Current_Scope.Scope;
      Index := Imported_Identifier_Index (Name);
      if Index /= Nil_Uniq_Id then
         pragma Debug (O2 ("Find_Imported_Identifier_Definition : end"));
         return Imported_Table (Scope).Content_Table.Table (Index).Definition;
      else
         pragma Debug (O2 ("Find_Imported_Identifier_Definition : end"));
         return null;
      end if;
   end Find_Imported_Identifier_Definition;

   ------------------------------------
   -- Create_Indentifier_In_Imported --
   ------------------------------------

   function Create_Identifier_In_Imported
     (Identifier : String;
      Scope : Node_Id)
     return Uniq_Id
   is
      use Idl_Fe.Lexer;

      Index : Uniq_Id;
      IT : Storage := Imported_Table (Scope);
   begin
      Create_Identifier_Index
        (Identifier, IT.Hash_Table, IT.Content_Table, Index);

      Set_Imported_Table (Scope, IT);
      return Index;
   end Create_Identifier_In_Imported;

   --------------------------------
   -- Add_Definition_To_Imported --
   --------------------------------

   procedure Add_Definition_To_Imported
     (Definition : in Identifier_Definition_Acc;
      Scope : in Node_Id)
   is
      Index : Uniq_Id;
      Definition_Test : Identifier_Definition_Acc;
   begin
      --  check if we are in value type or interfaces (we should be);
      if not Is_Imports (Scope) then
         return;
      end if;
      pragma Assert (Kind (Scope) = K_Interface
             or else Kind (Scope) = K_ValueType);

      Index := Create_Identifier_In_Imported (Definition.Name.all, Scope);
      Definition_Test :=
        Imported_Table (Scope).Content_Table.Table (Index).Definition;
      if Definition_Test /= null then
         if Definition_Test /= Definition then
            raise Idl_Fe.Errors.Internal_Error;
         end if;
      end if;
      Imported_Table (Scope).Content_Table.
        Table (Index).Definition := Definition;
   end Add_Definition_To_Imported;

   ------------------------------------
   -- Find_Identifier_In_Inheritance --
   ------------------------------------

   procedure Find_Identifier_In_Inheritance
     (Name : in String;
      Scope : in Node_Id;
      List : in out Node_List)
   is
      It : Node_Iterator;
      Node : Node_Id;
      Definition : Identifier_Definition_Acc;
      Parent : Node_List;
   begin
      pragma Debug (O2 ("Find_Identifier_In_Inheritance : enter"));
      Parent :=  Parents (Scope);
      Init (It, Parent);
      --  loop for all the Parent of the scope
      while not Is_End (It) loop
         Get_Next_Node (It, Node);
         pragma Assert (Kind (Node) = K_Scoped_Name);
         pragma Debug (O ("Find_Identifier_In_Inheritance : " &
                          Node_Kind'Image (Kind (Node))));
         Definition := Find_Identifier_In_Storage
           (Value (Node), Name);
         if Definition /= null then
            Append_Node (List, Definition.Node);
         else
            Find_Identifier_In_Inheritance
              (Name,
               Value (Node),
               List);
         end if;
      end loop;
      pragma Debug (O2 ("Find_Identifier_In_Inheritance : end"));
      return;
   end Find_Identifier_In_Inheritance;

   --------------------------------------------
   --  Find_Inherited_Identifier_Definition  --
   --------------------------------------------

   function Find_Inherited_Identifier_Definition
     (Name : String)
     return Identifier_Definition_Acc
   is
      Result_List : Node_List := Nil_List;
      First_List : Node_List := Nil_List;
   begin
      pragma Debug (O2 ("Find_Inherited_Identifier_Definition : enter"));
      --  There are no imports in modules.
      if not (False
        or else Kind (Current_Scope.Scope) = K_Interface
        or else Kind (Current_Scope.Scope) = K_ValueType)
      then
         pragma Debug (O2 ("Find_Inherited_Identifier_Definition : end"));
         return null;
      end if;

      Find_Identifier_In_Inheritance
        (Name,
         Current_Scope.Scope,
         First_List);
      Result_List := Simplify_Node_List (First_List);
      Free (First_List);
      if Get_Length (Result_List) = 0 then
         pragma Debug (O ("Find_Inherited_Identifier_Definition : " &
                          "Nothing found in inheritance"));
         pragma Debug (O2 ("Find_Inherited_Identifier_Definition : end"));
         return null;
      elsif  Get_Length (Result_List) = 1 then
         declare
            Node : Node_Id;
         begin
            pragma Debug (O ("Find_Inherited_Identifier_Definition : " &
                             "One definition found in inheritance"));
            Node := Head (Result_List);
            pragma Debug (O2 ("Find_Inherited_Identifier_Definition : end"));
            return Definition (Node);
         end;
      else
         declare
            Node : Node_Id;
         begin
            --  there is multiple definition
            pragma Debug (O ("Find_Inherited_Identifier_Definition : " &
                             "Many definitions found in inheritance"));

            Idl_Fe.Errors.Parser_Error ("Multiple definitions found" &
                                        " in inheritance : ambiguous " &
                                        "referance",
                                        Idl_Fe.Errors.Error,
                                        Idl_Fe.Lexer.Get_Lexer_Location);

            Node := Head (Result_List);
            Free (Result_List);
            pragma Debug (O2 ("Find_Inherited_Identifier_Definition : end"));
            return Definition (Node);
         end;
      end if;
   end Find_Inherited_Identifier_Definition;

end Idl_Fe.Types;

with Ada.Unchecked_Deallocation;
with System;
with GNAT.Case_Util;
with GNAT.Table;
with Idl_Fe.Errors;
with Idl_Fe.Lexer;
with Idl_Fe.Tree; use Idl_Fe.Tree;
with Idl_Fe.Tree.Synthetic; use Idl_Fe.Tree.Synthetic;
with Idl_Fe.Debug;
pragma Elaborate (Idl_Fe.Debug);

package body Idl_Fe.Types is

   -----------
   -- Debug --
   -----------

   Flag : constant Natural := Idl_Fe.Debug.Is_Active ("idl_fe.types");
   procedure O is new Idl_Fe.Debug.Output (Flag);

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

   ---------------------
   -- A list of nodes --
   ---------------------

   ----------
   -- Init --
   ----------

   procedure Init (It : out Node_Iterator; List : Node_List) is
   begin
      It := Node_Iterator (List);
   end Init;

   ----------------
   --  Get_Node  --
   ----------------
   function Get_Node (It : Node_Iterator) return Node_Id is
   begin
      return It.Car;
   end Get_Node;

   ------------
   --  Next  --
   ------------
   procedure Next (It : in out Node_Iterator) is
   begin
      It := Node_Iterator (It.Cdr);
   end Next;

   --------------
   --  Is_End  --
   --------------
   function Is_End (It : Node_Iterator) return Boolean is
   begin
      return It = null;
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
      if List = Nil_List then
         return False;
      end if;
      if List.Car = Node then
         return True;
      else
         return Is_In_List (List.Cdr, Node);
      end if;
   end Is_In_List;

   -------------------
   --  Remove_Node  --
   -------------------

   procedure Unchecked_Deallocation is
      new Ada.Unchecked_Deallocation
     (Node_List_Cell, Node_List);

   procedure Remove_Node
     (List : in out Node_List;
      Node : Node_Id) is
      Old_List : Node_List;
   begin
      if List = null then
         return;
      end if;
      if List.Car = Node then
         Old_List := List;
         List := List.Cdr;
         Unchecked_Deallocation (Old_List);
      else
         while List.Cdr /= null loop
            if List.Cdr.Car = Node then
               Old_List := List.Cdr;
               List.Cdr := List.Cdr.Cdr;
               Unchecked_Deallocation (Old_List);
               return;
            end if;
            List := List.Cdr;
         end loop;
      end if;
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
      while Temp_List /= null loop
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
         Node := Get_Node (It);
         if not Is_In_List (Result_List, Node) then
            Append_Node (Result_List, Node);
         end if;
         Next (It);
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
         N := Get_Node (It);
         Next (It);

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
      List := new Identifier_Definition_Cell'
        (Definition => Identifier.all,
         Next => Identifier_List (Scope));
      Set_Identifier_List (Scope, List);
      Add_Definition_To_Storage (Identifier);
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


   ----------------------------------
   --  identifiers handling types  --
   ----------------------------------
   package Id_Table is new GNAT.Table
     (Table_Component_Type => Hash_Entry, Table_Index_Type => Uniq_Id,
      Table_Low_Bound => Nil_Uniq_Id + 1, Table_Initial => 256,
      Table_Increment => 100);


   ------------
   --  Hash  --
   -------------
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

   ------------------
   --  Push_Scope  --
   ------------------

   procedure Push_Scope
     (Scope : Node_Id)
   is
      Stack : Scope_Stack_Acc;
   begin
      pragma Debug (O ("Push_scope : enter"));
      Stack := new Scope_Stack;
      Stack.Parent := Current_Scope;
      Stack.Scope := Scope;
      if Current_Scope = null then
         pragma Debug (O ("Push_scope : current_scope is null"));
         pragma Debug (O ("Push_scope : root scope is"
                          & Name (Scope)));
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
      Forward_Defs : Node_Iterator;
      Forward_Def : Node_Id;
      Hash_Index : Hash_Value_Type;
      Index : Uniq_Id;
   begin
      pragma Debug (O ("pop_scope : end"));
      --  Remove all definition of scope from the hash table, and
      --  replace them by the previous one.
      --  Add these definition to the identifier_table of the current_scope
      Definition_List := Identifier_List (Current_Scope.Scope);
      while Definition_List /= null loop
         Id_Table.Table (Definition_List.Definition.Id).Definition :=
           Definition_List.Definition.Previous_Definition;
         if Definition_List.Definition.Previous_Definition = null then
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

         Old_Definition_List := Definition_List;
         Definition_List := Definition_List.Next;
         Unchecked_Deallocation (Old_Definition_List);
      end loop;

      Old_Scope := Current_Scope;
      Current_Scope := Old_Scope.Parent;
      --  Test if all forward definitions were implemented
      if Kind (Old_Scope.Scope) = K_Repository or
        Kind (Old_Scope.Scope) = K_Module then
         Init (Forward_Defs,
               Unimplemented_Forwards (Old_Scope.Scope));
      end if;
      while not Is_End (Forward_Defs) loop
         Forward_Def := Get_Node (Forward_Defs);
         Idl_Fe.Errors.Parser_Error
           ("The forward declaration " &
            Idl_Fe.Errors.Display_Location
            (Get_Location (Forward_Def)) &
            " is not implemented.",
            Idl_Fe.Errors.Error,
            Get_Location (Old_Scope.Scope));
         Next (Forward_Defs);
      end loop;

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
      pragma Debug (O ("pop_scope : end"));
   end Pop_Scope;



   ------------------------------------
   --  identifiers handling methods  --
   ------------------------------------

   ----------------------
   --  Is_Redefinable  --
   ----------------------
   function Is_Redefinable (Name : String) return Boolean is
      A_Definition : Identifier_Definition_Acc;
   begin
      pragma Debug (O ("Is_Redefinable : enter"));
      --  Checks if the identifier is already imported
         if Check_Imported_Identifier_Index (Name) /= Nil_Uniq_Id then
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
            return False;
         end if;
         --  A attribute or operation may not be redefined
         if Kind (A_Definition.Node) = K_Operation or
           (Kind (A_Definition.Node) = K_Declarator and then
            Kind (Parent (A_Definition.Node)) = K_Attribute) then
            pragma Debug (O ("Is_Redefinable : cannot redefine an op, attr"));
            return False;
         end if;
         --  Ckecks if identifier found is the current scope name:
         --  it is not allowed except for the operation
         if Kind (Current_Scope.Scope) /= K_Operation
           and then A_Definition = Definition (Current_Scope.Scope) then
            return False;
         end if;
      end if;
      return True;
   end Is_Redefinable;

   ------------------------------
   --  Check_Identifier_Index  --
   ------------------------------
   function Check_Identifier_Index (Identifier : String) return Uniq_Id is
      use Idl_Fe.Lexer;
      Hash_Index : Hash_Value_Type := Hash (Identifier) mod Hash_Mod;
      Index : Uniq_Id := Hash_Table (Hash_Index);
   begin
      pragma Debug (O ("Check_Identifier_Index : enter"));
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
      pragma Debug (O ("Check_Identifier_Index : end"));
      return Nil_Uniq_Id;
   end Check_Identifier_Index;

   ----------------------------------
   --  Create_Indentifier_Index    --
   ----------------------------------
   function Create_Identifier_Index (Identifier : String) return Uniq_Id is
      use Idl_Fe.Lexer;
      Hash_Index : Hash_Value_Type := Hash (Identifier) mod Hash_Mod;
      Index : Uniq_Id := Hash_Table (Hash_Index);
   begin
      pragma Debug (O ("Create_Identifier_Index : enter"));
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
   function Find_Identifier_Definition (Name : String)
                                        return Identifier_Definition_Acc is
      Index : Uniq_Id;
      Imported_Definition, Inherited_Definition : Identifier_Definition_Acc;
      Definition : Identifier_Definition_Acc := null;
   begin
      pragma Debug (O ("Find_Identifier_Definition : enter"));
      Index := Check_Identifier_Index (Name);
      pragma Debug (O ("Find_Identifier_Definition : " &
                       "check_identifier_index done"));
      if Index /= Nil_Uniq_Id then
         Definition := Id_Table.Table (Index).Definition;
         --  is the definition in the scope
         if Definition.Parent_Scope = Current_Scope.Scope then
            return Definition;
         end if;
      end if;

      Imported_Definition := Find_Imported_Identifier_Definition (Name);
      --  is the definition imported
      if Imported_Definition /= null then
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
         return Inherited_Definition;
      end if;
      --  the definition is in a upper scope
      return Definition;
   end Find_Identifier_Definition;

   ----------------------------
   --  Find_Identifier_Node  --
   ----------------------------
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

   ---------------------------
   --  Redefine_Identifier  --
   ---------------------------

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

   ----------------------
   --  Add_Identifier  --
   ----------------------

   function Add_Identifier
     (Node : Node_Id;
      Name : String)
     return Boolean
   is
      Definition : Identifier_Definition_Acc;
      Index : Uniq_Id;
   begin
      pragma Debug (O ("Add_Identifier : enter"));
      pragma Debug (O ("Add_Identifier : identifier is " & Name));
      --  Checks if the identifier is redefinable
      if not Is_Redefinable (Name) then
         return False;
      end if;
      --  Creates a new definition.
      Index := Create_Identifier_Index (Name);
      Definition := new Identifier_Definition;
      Definition.Name := new String'(Name);
      Definition.Id := Index;
      Definition.Node := Node;
      Definition.Previous_Definition := Id_Table.Table (Index).Definition;
      Definition.Parent_Scope := Current_Scope.Scope;
      Id_Table.Table (Index).Definition := Definition;
      Add_Identifier_Definition (Current_Scope.Scope, Definition);
      Set_Definition (Node, Definition);
      pragma Debug (O ("Add_Identifier : end"));
      return True;
   end Add_Identifier;

   -----------------------------------
   --  Check_Identifier_In_Storage  --
   -----------------------------------

   function Check_Identifier_In_Storage
     (Scope : Node_Id;
      Identifier : String)
     return Uniq_Id
   is
      use Idl_Fe.Lexer;

      Hash_Index : Hash_Value_Type
        := Hash (Identifier) mod Hash_Mod;
      Index : Uniq_Id;
   begin
      pragma Debug (O ("Check_Identifier_In_Storage : enter"));
      Index := Identifier_Table (Scope).Hash_Table (Hash_Index);
      if Index /= Nil_Uniq_Id then
         while Identifier_Table (Scope).Content_Table.Table (Index).
           Definition.Name /= null loop
            if Idl_Identifier_Equal
              (Identifier_Table (Scope).Content_Table.Table (Index).
               Definition.Name.all, Identifier) /= Differ
            then
               pragma Debug (O ("Check_Identifier_In_Storage : end"));
               return Index;
            end if;
            if Identifier_Table (Scope).Content_Table.Table (Index).Next =
              Nil_Uniq_Id then
               exit;
            end if;
            Index := Identifier_Table (Scope).Content_Table.Table (Index).Next;
         end loop;
      end if;
      pragma Debug (O ("Check_Identifier_In_Storage : end"));
      return  Nil_Uniq_Id;
   end Check_Identifier_In_Storage;

   ----------------------------------
   --  Find_Identifier_In_Storage  --
   ----------------------------------

   function Find_Identifier_In_Storage
     (Scope : Node_Id;
      Name : String)
     return Identifier_Definition_Acc
   is
      Index : Uniq_Id;
   begin
      pragma Debug (O ("Find_Identifier_In_Storage : enter"));
      Index := Check_Identifier_In_Storage (Scope, Name);
      if Index /= Nil_Uniq_Id then
         return Identifier_Table (Scope).
           Content_Table.Table (Index).Definition;
      else
         return null;
      end if;
   end Find_Identifier_In_Storage;

   -------------------------------------
   --  Create_Indentifier_In_Storage  --
   -------------------------------------

   function Create_Identifier_In_Storage
     (Identifier : String)
     return Uniq_Id
   is
      use Idl_Fe.Lexer;

      Hash_Index : Hash_Value_Type := Hash (Identifier) mod Hash_Mod;
      IT : Storage := Identifier_Table (Current_Scope.Scope);
      Index : Uniq_Id :=
        IT.Hash_Table (Hash_Index);
   begin
      if Index = Nil_Uniq_Id then
         Increment_Last (IT.Content_Table);
         Index := Last (IT.Content_Table);
         IT.Hash_Table (Hash_Index) := Index;
      else
         while IT.Content_Table.
           Table (Index).Definition.Name /= null loop
            if Idl_Identifier_Equal
              (IT.Content_Table.
               Table (Index).Definition.Name.all, Identifier) /= Differ
            then
               return Index;
            end if;
            if  IT.Content_Table.
              Table (Index).Next = Nil_Uniq_Id then
               Increment_Last (IT.
                               Content_Table);
               IT.Content_Table.
                 Table (Index).Next :=
                 Last (IT.Content_Table);
               Index := Last (IT.
                              Content_Table);
               exit;
            end if;
            Index :=
              IT.Content_Table.
              Table (Index).Next;
         end loop;
      end if;
      --  Add an entry in INDEX.
      IT.Content_Table.Table (Index) :=
        (Definition => null, Next => Nil_Uniq_Id);
      Set_Identifier_Table (Current_Scope.Scope, IT);
      return Index;
   end Create_Identifier_In_Storage;

   --------------------------------
   --  Add_Definition_To_Storage --
   --------------------------------

   procedure Add_Definition_To_Storage
     (Definition : in Identifier_Definition_Acc)
   is
      Index : Uniq_Id;
      Definition_Test : Identifier_Definition_Acc;
   begin
      Index := Create_Identifier_In_Storage (Definition.Name.all);
      Definition_Test :=
        Identifier_Table (Current_Scope.Scope).Content_Table.
        Table (Index).Definition;
      --  their shouldn't be any redefinition
      if Definition_Test /= null then
         raise Idl_Fe.Errors.Internal_Error;
         return;
      end if;
      Identifier_Table (Current_Scope.Scope).Content_Table.
        Table (Index).Definition := Definition;
   end Add_Definition_To_Storage;

   ---------------------------------------
   --  Check_Imported_Identifier_Index  --
   ---------------------------------------

   function Check_Imported_Identifier_Index
     (Identifier : String)
     return Uniq_Id
   is
      use Idl_Fe.Lexer;

      Hash_Index : Hash_Value_Type
        := Hash (Identifier) mod Hash_Mod;
      Index : Uniq_Id;
      Scope : Node_Id;
   begin
      pragma Debug (O ("Check_Imported_Identifier : enter"));
      if not Is_Imports (Current_Scope.Scope) then
         return Nil_Uniq_Id;
         pragma Debug (O ("Check_Imported_Identifier : is imports "));
      end if;
      Scope := Current_Scope.Scope;
      pragma Debug (O ("Check_Imported_Identifier : scope type is " &
                       Node_Kind'Image (Kind (Scope)) & "."));
      Index := Imported_Table (Scope).Hash_Table (Hash_Index);
      if Index /= Nil_Uniq_Id then
         while Imported_Table (Scope).
           Content_Table.Table (Index).Definition.Name
           /= null loop
            if Idl_Identifier_Equal
              (Imported_Table (Scope).Content_Table.Table (Index).
               Definition.Name.all, Identifier) /= Differ
            then
               return Index;
            end if;
            if Imported_Table (Scope).Content_Table.Table (Index).Next =
              Nil_Uniq_Id then
               exit;
            end if;
            Index := Imported_Table (Scope).Content_Table.Table
              (Index).Next;
         end loop;
      end if;
      return  Nil_Uniq_Id;
   end Check_Imported_Identifier_Index;


   -------------------------------------------
   --  Find_Imported_Identifier_Definition  --
   -------------------------------------------

   function Find_Imported_Identifier_Definition
     (Name : String)
     return Identifier_Definition_Acc
   is
      Index : Uniq_Id;
      Scope : Node_Id;
   begin
      pragma Debug (O ("Find_Imported_Identifier_Definition : enter"));
      Scope := Current_Scope.Scope;
      Index := Check_Imported_Identifier_Index (Name);
      if Index /= Nil_Uniq_Id then
         return Imported_Table (Scope).Content_Table.Table (Index).Definition;
      else
         return null;
      end if;
   end Find_Imported_Identifier_Definition;

   --------------------------------------
   --  Create_Indentifier_In_Imported  --
   --------------------------------------

   function Create_Identifier_In_Imported
     (Identifier : String;
      Scope : Node_Id)
     return Uniq_Id
   is
      use Idl_Fe.Lexer;

      Hash_Index : Hash_Value_Type
        := Hash (Identifier) mod Hash_Mod;
      Index : Uniq_Id;
      IT : Storage := Imported_Table (Scope);
   begin
      Index := IT.Hash_Table (Hash_Index);
      if Index = Nil_Uniq_Id then
         Increment_Last (IT.Content_Table);
         Index := Last (IT.Content_Table);
         IT.Hash_Table (Hash_Index) := Index;
      else
         while IT.Content_Table.
           Table (Index).Definition.Name /= null loop
            if Idl_Identifier_Equal (IT.Content_Table.
               Table (Index).Definition.Name.all, Identifier) /= Differ
            then
               return Index;
            end if;
            if  IT.Content_Table.
              Table (Index).Next = Nil_Uniq_Id then
               Increment_Last (IT.Content_Table);
               IT.Content_Table.Table (Index).Next :=
                 Last (IT.Content_Table);
               Index := Last (IT.Content_Table);
               exit;
            end if;
            Index :=
              IT.Content_Table.Table (Index).Next;
         end loop;
      end if;
      --  Add an entry in INDEX.
      IT.Content_Table.Table (Index) :=
        (Definition => null, Next => Nil_Uniq_Id);
      Set_Imported_Table (Scope, IT);
      return Index;
   end Create_Identifier_In_Imported;

   ---------------------------------
   --  Add_Definition_To_Imported --
   ---------------------------------

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

   --------------------------------
   --  Find_Identifier_In_Scope  --
   --------------------------------

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

      Parent :=  Parents (Scope);
      Init (It, Parent);
      --  loop for all the Parent of the scope
      while not Is_End (It) loop
         Node := Get_Node (It);
         if Kind (Node) /= K_Scoped_Name then
            raise Idl_Fe.Errors.Internal_Error;
         end if;
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
         Next (It);
      end loop;
      return;
   end Find_Identifier_In_Inheritance;

   --------------------------------------------
   --  Find_Inherited_Identifier_Definition  --
   --------------------------------------------

   function Find_Inherited_Identifier_Definition
     (Name : String)
     return Identifier_Definition_Acc
   is
      Result_List : Node_List := null;
      First_List : Node_List := null;
   begin
      --  there is no imports in moduls types
      if not (Kind (Current_Scope.Scope) = K_Interface
        or else Kind (Current_Scope.Scope) = K_ValueType) then
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
         return null;
      elsif  Get_Length (Result_List) = 1 then
         declare
            It : Node_Iterator;
            Node : Node_Id;
         begin
            pragma Debug (O ("Find_Inherited_Identifier_Definition : " &
                             "One definition found in inheritance"));
            Init (It, Result_List);
            Node := Get_Node (It);
            Free (Result_List);
            return Definition (Node);
         end;
      else
         declare
            It : Node_Iterator;
            Node : Node_Id;
         begin
            --  there is multiple definition
            pragma Debug (O ("Find_Inherited_Identifier_Definition : " &
                             "Many definitions found in inheritance"));

            Idl_Fe.Errors.Parser_Error ("Multiple definitions found" &
                                        " in inheritance.",
                                        Idl_Fe.Errors.Error,
                                        Idl_Fe.Lexer.Get_Lexer_Location);

            Init (It, Result_List);
            Node := Get_Node (It);
            Free (Result_List);
            return Definition (Node);
         end;
      end if;
   end Find_Inherited_Identifier_Definition;

end Idl_Fe.Types;

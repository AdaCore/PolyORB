with Ada.Text_IO;

with Idl_Fe.Types;          use Idl_Fe.Types;
with Idl_Fe.Tree;           use Idl_Fe.Tree;
with Idl_Fe.Tree.Synthetic; use Idl_Fe.Tree.Synthetic;
with Idl_Fe.Tree.Low_Level; use Idl_Fe.Tree.Low_Level;
with Idl_Fe.Errors;         use Idl_Fe.Errors;

with Ada_Be.Identifiers;    use Ada_Be.Identifiers;
with Ada_Be.Debug;
pragma Elaborate (Ada_Be.Debug);

with Utils;                 use Utils;

with GNAT.HTable;

package body Ada_Be.Expansion is

   -----------
   -- Debug --
   -----------

   Flag : constant Natural := Ada_Be.Debug.Is_Active ("ada_be.expansion");
   procedure O is new Ada_Be.Debug.Output (Flag);

   ------------------------------------
   -- Internal expansion subprograms --
   ------------------------------------

   procedure Expand_Node (Node : in Node_Id);
   --  Generic function that calls the more specific ones
   --  according to the type of the node

   --  Specific expansion subprograms

   procedure Expand_Module
     (Node : in Node_Id);
   procedure Expand_Ben_Idl_File
     (Node : in Node_Id);
   --  Expand all subnodes.

   procedure Expand_Interface
     (Node : in Node_Id);
   --  First expand all subnodes,
   --  then copy inherited methods and attributes
   --  from ancestors.

   procedure Expand_Attribute
     (Node : in Node_Id);
   --  Expand an attribute into the corresponding _get_
   --  and _set_ operations.

   procedure Expand_Operation
     (Node : Node_Id);
   --  Expand each formal parameter.
   --  FIXME: Should expand non-void function with inout or out
   --    formals into void function with a supplementary
   --    Returns out formal.
   procedure Expand_Param
     (Node : Node_Id);
   --  Expand Param_Type.

   procedure Expand_Exception
     (Node : in Node_Id);
   --  Expand an exception into a _Members struct.

   procedure Expand_Type_Declarator
     (Node : Node_Id);
   --  Expand the denoted type.

   procedure Expand_Struct
     (Node : Node_Id);
   procedure Expand_Member
     (Node : Node_Id);
   --  Expand struct members: for each member,
   --  isolate array declarators, then expand M_Type.

   procedure Expand_Union
     (Node : Node_Id);
   procedure Expand_Case
     (Node : Node_Id);
   --  Expand union: for each case, isolate array declarators,
   --  then expand Case_Type.

   procedure Expand_Sequence
     (Node : Node_Id);
   --  Replace a Sequence node by a reference to
   --  a Sequence_Instance node. The Sequence Instance
   --  node is also inserted as a declaration in the current
   --  scope.

   procedure Expand_Constructed_Type
     (Node : Node_Id;
      Replacement_Node : out Node_Id);
   --  Expand a constructed type (enum, struct, or union)
   --  occurring in a type_spec.
   --  If the node is expanded, the type_spec must set to
   --  with Replacement_Node, which is a valid node id, else
   --  Replacement_Node is No_Node.

   procedure Expand_Array_Declarators
     (Node : Node_Id);
   --  Expand all the array declarators in a member.
   procedure Expand_Array_Declarator
     (Node : Node_Id);
   --  Expand on array declarator into a simple declarator
   --  whose parent member or case has a reference to
   --  an array typedef as type.
   --  Precondition: The declarator must be the only one
   --  in the parent member or case declarator list.
   --  (This precondition is guaranteed by Expand_Array_Declarators).

   ----------------------
   -- Utility routines --
   ----------------------

   Current_Position_In_List : Node_Id
     := No_Node;
   procedure Expand_Node_List
     (List : in Node_List;
      Set_Current_Position : in Boolean);
   --  Expand a whole list of nodes
   --  The global variable Current_Position_In_List is set
   --  before each node is expanded.

   procedure Append_Node_To_Contents
     (Parent : Node_Id;
      Child : Node_Id);
   --  Append a node to the contents node_list of parent

   function Sequence_Type_Name
     (Node : Node_Id)
     return String;
   --  The name corresponding to type Node as used
   --  to construct the name of an instance of
   --  CORBA.Sequences.Bounded or CORBA.Sequences.Unbounded.

   procedure Add_Identifier_With_Renaming
     (Node : Node_Id;
      Identifier : String);
   --  Assign Identifier to Node in the current scope,
   --  possibly appending a numeric prefix if a conflict
   --  would otherwise be introduced.

   -----------------
   -- Expand_Node --
   -----------------

   procedure Expand_Node (Node : in Node_Id) is
   begin
      pragma Debug (O ("Expanding node : "
                       & Node_Kind'Image (Kind (Node))));
      case (Kind (Node)) is
         when K_Repository =>
            Expand_Repository (Node);
         when K_Module =>
            Expand_Module (Node);
         when K_Interface =>
            Expand_Interface (Node);
         when K_Attribute =>
            Expand_Attribute (Node);
         when K_Operation =>
            Expand_Operation (Node);
         when K_Exception =>
            Expand_Exception (Node);
         when K_Ben_Idl_File =>
            Expand_Ben_Idl_File (Node);

         when K_Type_Declarator =>
            Expand_Type_Declarator (Node);
         when K_Struct =>
            Expand_Struct (Node);
         when K_Member =>
            Expand_Member (Node);
         when K_Union =>
            Expand_Union (Node);
         when K_Case =>
            Expand_Case (Node);

         when K_Sequence =>
            Expand_Sequence (Node);

         when others =>
            null;
      end case;
   end Expand_Node;


   -------------------------------------------
   --  and now one procedure per node type  --
   -------------------------------------------

   ------------------------
   --  Expand_Repository --
   ------------------------

   procedure  Expand_Repository (Node : in Node_Id) is

      Iterator : Node_Iterator;

      type Header_Num is range 0 .. 1024;
      function Hash is new GNAT.HTable.Hash (Header_Num);
      function Hash (A : Idl_Fe.Errors.File_Name_Ptr) return Header_Num;
      function Hash (A : Idl_Fe.Errors.File_Name_Ptr) return Header_Num is
      begin
         return Hash (A.all);
      end Hash;
      function Equals (A, B : Idl_Fe.Errors.File_Name_Ptr) return Boolean;
      function Equals (A, B : Idl_Fe.Errors.File_Name_Ptr) return Boolean is
      begin
         return A.all = B.all;
      end Equals;
      package Idlnodes is new GNAT.HTable.Simple_HTable
        (Header_Num,
         Node_Id,
         No_Node,
         Idl_Fe.Errors.File_Name_Ptr,
         Hash,
         Equals);

      Old_Repository : Node_Id := Node;
      New_Repository : Node_Id;
   begin
      pragma Assert (Kind (Old_Repository) = K_Repository);

      --  create a new node Ben_Idl_File with the same ID as the old one
      --  and replace the ID of the old one
      New_Repository := Make_Repository;
      Replace_Node (Old_Repository, New_Repository);

      --  to prevent idl_fe-types to crash
      --  is it correct for name clashes ?
      Push_Scope (New_Repository);

      --  loop over the nodes of the old list
      Init (Iterator, Contents (Old_Repository));
      while not Is_End (Iterator) loop
         declare
            Current : Node_Id;
            Loc : Idl_Fe.Errors.Location;
            Filename : Idl_Fe.Errors.File_Name_Ptr;

            Idl_File_Node : Node_Id;
            Success : Boolean;
         begin
            Get_Next_Node (Iterator, Current);
            Loc := Get_Location (Current);
            Filename := Loc.Filename;

            pragma Assert (Filename /= null);
            pragma Debug (O ("node "
                             & Node_Kind'Image (Kind (Current))
                             & " in file "
                             & Filename.all));

            Idl_File_Node := Idlnodes.Get (Filename);

            --  if this is the first node of this file
            if (Idl_File_Node = No_Node) then

               --  create a new node Ben_Idl_File
               Idl_File_Node := Make_Ben_Idl_File;

               --  set its name
               --  is it correct when conflict ?
               Success := Add_Identifier
                 (Idl_File_Node,
                  Filename.all & "_IDL_File");
               if not Success then
                  --  conflicts in filenames, not implemented yet
                  --  *** NIY ***
                  raise Program_Error;
               end if;

               --  add the new node to the hashtable
               Idlnodes.Set (Filename, Idl_File_Node);
               --  add the new node to the repository
               Append_Node_To_Contents (New_Repository, Idl_File_Node);
            end if;

            --  add the current node to the correct Ben_Idl_File
            Append_Node_To_Contents (Idl_File_Node, Current);
         end;
      end loop;

      Expand_Node_List (Contents (New_Repository), True);
      --  Pop_Scope;
   end Expand_Repository;


   --------------------
   --  Expand_Module --
   --------------------

   procedure Expand_Module (Node : in Node_Id) is
   begin
      pragma Assert (Kind (Node) = K_Module);
      Push_Scope (Node);
      Expand_Node_List (Contents (Node), True);
      --  Pop_Scope;
   end Expand_Module;

   --------------------------
   --  Expand_Ben_Idl_File --
   --------------------------

   procedure Expand_Ben_Idl_File (Node : in Node_Id) is
   begin
      pragma Assert (Kind (Node) = K_Ben_Idl_File);
      Push_Scope (Node);
      Expand_Node_List (Contents (Node), True);
      --  Pop_Scope;
   end Expand_Ben_Idl_File;

   -----------------------
   --  Expand_Interface --
   -----------------------

   procedure Recursive_Copy_Operations
     (Into : in out Node_List;
      Parent : in Node_Id;
      From : Node_Id;
      Implicit_Inherited : Boolean;
      Parents_Seen : in out Node_List);
   --  Recursively copy all operations from K_Interface
   --  node From and all its ancestors into Into.
   --  Ancestors are appended to the Parents_Seen list
   --  as they are explored, and will not be explored twice.

   --  The Parent_Scope for the copies is set to Parent,
   --  and the Is_Implicit_Inherited attribute is set
   --  to Implicit_Inherited.

   procedure Recursive_Copy_Operations
     (Into : in out Node_List;
      Parent : in Node_Id;
      From : Node_Id;
      Implicit_Inherited : Boolean;
      Parents_Seen : in out Node_List)
   is
      Ops_It : Node_Iterator;
      O_Node : Node_Id;
      New_O_Node : Node_Id;

      Inh_It : Node_Iterator;
      I_Node : Node_Id;
   begin
      pragma Assert (Kind (From) = K_Interface);

      if Is_In_List (Parents_Seen, From) then
         return;
      end if;

      Init (Ops_It, Contents (From));
      while not Is_End (Ops_It) loop
         Get_Next_Node (Ops_It, O_Node);

         if Kind (O_Node) = K_Operation
           and then From = Original_Parent_Scope (O_Node)
         then
            New_O_Node := Copy_Node (O_Node);
            Set_Parent_Scope (New_O_Node, Parent);
            Set_Is_Implicit_Inherited
              (New_O_Node, Implicit_Inherited);
            Append_Node (Into, New_O_Node);
         end if;
      end loop;

      Append_Node (Parents_Seen, From);

      Init (Inh_It, Parents (From));
      while not Is_End (Inh_It) loop
         Get_Next_Node (Inh_It, I_Node);

         Recursive_Copy_Operations
           (Into, Parent, Value (I_Node),
            Implicit_Inherited, Parents_Seen);
      end loop;
   end Recursive_Copy_Operations;

   procedure Expand_Interface
     (Node : in Node_Id)
   is
      Export_List : Node_List;

      It : Node_Iterator;
      I_Node : Node_Id;
      First : Boolean := True;

      Parents_Seen : Node_List
        := Nil_List;
   begin
      pragma Assert (Kind (Node) = K_Interface);
      Push_Scope (Node);

      Export_List := Contents (Node);
      Expand_Node_List (Export_List, True);
      --  First expand the interface's exports
      --  (eg, attributes are expanded into operations.)

      Init (It, Parents (Node));
      while not Is_End (It) loop
         Get_Next_Node (It, I_Node);

         Recursive_Copy_Operations
           (Into => Export_List,
            Parent => Node,
            From => Value (I_Node),
            Implicit_Inherited => First,
            Parents_Seen => Parents_Seen);

         First := False;
      end loop;

      Set_Contents (Node, Export_List);

      --  Pop_Scope;
   end Expand_Interface;

   -----------------------
   --  Expand_Attribute --
   -----------------------

   procedure Expand_Attribute
     (Node : in Node_Id)
   is
      Exports_List : Node_List
        := Nil_List;
      --  The exports list of the interface
      --  containing these attributes, wherein we insert
      --  _get_Attribute and _set_Attribute operations.

      Position : Node_Id
        := Node;
      Iterator : Node_Iterator;
      Current_Declarator : Node_Id;
   begin
      pragma Assert (Kind (Node) = K_Attribute);

      Init (Iterator, Declarators (Node));

      while not Is_End (Iterator) loop

         Get_Next_Node (Iterator, Current_Declarator);

         if Exports_List = Nil_List then
            Exports_List := Contents (Parent_Scope (Current_Declarator));
            pragma Assert (Exports_List /= Nil_List);
         end if;

         pragma Debug (O ("Expanding attribute declarator "
                          & Ada_Name (Current_Declarator)));

         --  create the get_method
         declare
            Get_Method : constant Node_Id
              := Make_Operation;
            Success : Boolean;
         begin
            Success := Add_Identifier
              (Get_Method, "_get_"
               & Ada_Name (Current_Declarator));
            pragma Assert (Success);
            Push_Scope (Get_Method);
            Set_Is_Oneway (Get_Method, False);
            Set_Operation_Type (Get_Method, A_Type (Node));
            --  parameters
            Set_Parameters (Get_Method, Nil_List);
            Set_Raises (Get_Method, Nil_List);
            Set_Contexts (Get_Method, Nil_List);

            --  add the node to the node list
            --  Append_Node_To_Contents (New_Node, Get_Method);
            Insert_After
              (List => Exports_List,
               Node => Get_Method,
               After => Position);
            Position := Get_Method;
            Pop_Scope;
         end;


         --  create the Set method
         if not Is_Readonly (Node) then
            declare
               Set_Method : constant Node_Id
                 := Make_Operation;
               Void_Node : constant Node_Id
                 := Make_Void;
               Success : Boolean;
            begin
               Success := Add_Identifier
                 (Set_Method, "_set_"
                  & Ada_Name (Current_Declarator));
               pragma Assert (Success);
               Push_Scope (Set_Method);
               Set_Is_Oneway (Set_Method, False);
               Set_Operation_Type (Set_Method, Void_Node);
               --  parameters
               declare
                  Param : Node_Id := Make_Param;
                  Decl : Node_Id := Make_Declarator;
                  Params : Node_List := Nil_List;
               begin
                  --  new value parameter
                  Set_Mode (Param, Mode_In);
                  Set_Param_Type (Param, A_Type (Node));
                  Success := Add_Identifier (Decl, "To");
                  pragma Assert (Success = True);
                  Set_Array_Bounds (Decl, Nil_List);
                  Set_Parent (Decl, Param);
                  Set_Declarator (Param, Decl);
                  Append_Node (Params, Param);

                  Set_Parameters (Set_Method, Params);
               end;
               Set_Raises (Set_Method, Nil_List);
               Set_Contexts (Set_Method, Nil_List);

               --  add the node to the node list
               Insert_After
                 (List => Exports_List,
                  Node => Set_Method,
                  After => Position);
               Position := Set_Method;
               Pop_Scope;
            end;
         end if;
      end loop;
   end Expand_Attribute;

   procedure Expand_Operation
     (Node : in Node_Id) is
   begin
      Expand_Node_List (Parameters (Node), False);
   end Expand_Operation;

   procedure Expand_Param
     (Node : in Node_Id) is
   begin
      Expand_Node (Param_Type (Node));
   end Expand_Param;

   procedure Expand_Exception
     (Node : in Node_Id) is
   begin
      pragma Assert (Kind (Node) = K_Exception);

      declare
         Members_Struct : constant Node_Id
           := Make_Struct;
         Enclosing_Scope : constant Node_Id
           := Parent_Scope (Node);
         Enclosing_List : Node_List
           := Contents (Enclosing_Scope);
         Success : Boolean;
      begin
         pragma Debug (O ("Expand_Exception: "
                          & Ada_Name (Node)));

         Success := Add_Identifier
           (Members_Struct, Ada_Name (Node) & "_Members");
         pragma Assert (Success);

         Set_Members (Members_Struct, Members (Node));
         Set_Is_Exception_Members (Members_Struct, True);
         Expand_Struct (Members_Struct);

         Insert_Before
           (List => Enclosing_List,
            Node => Members_Struct,
            Before => Node);
         Set_Members_Type (Node, Members_Struct);

         Set_Contents (Enclosing_Scope, Enclosing_List);
      end;
   end Expand_Exception;

   procedure Expand_Type_Declarator
     (Node : Node_Id)
   is
      R_Node : Node_Id;
   begin
      Expand_Constructed_Type (T_Type (Node), R_Node);
      if R_Node /= No_Node then
         Set_T_Type (Node, R_Node);
      end if;
      Expand_Node (T_Type (Node));
   end Expand_Type_Declarator;

   procedure Expand_Struct
     (Node : Node_Id) is
   begin
      --  Push_Scope (Node);
      Expand_Node_List (Members (Node), False);
      --  Pop_Scope;
   end Expand_Struct;

   procedure Expand_Member
     (Node : Node_Id)
   is
      R_Node : Node_Id;
   begin
      Expand_Constructed_Type (M_Type (Node), R_Node);
      if R_Node /= No_Node then
         Set_M_Type (Node, R_Node);
      end if;
      Expand_Node (M_Type (Node));
      Expand_Array_Declarators (Node);
   end Expand_Member;

   procedure Expand_Union
     (Node : Node_Id)
   is
      R_Node : Node_Id;
   begin
      --  Push_Scope (Node);
      Expand_Constructed_Type (Switch_Type (Node), R_Node);
      if R_Node /= No_Node then
         Set_Switch_Type (Node, R_Node);
      end if;
      Expand_Node_List (Cases (Node), False);
      --  Pop_Scope;
   end Expand_Union;

   procedure Expand_Case
     (Node : Node_Id)
   is
      R_Node : Node_Id;
   begin
      Expand_Constructed_Type (Case_Type (Node), R_Node);
      if R_Node /= No_Node then
         Set_Case_Type (Node, R_Node);
      end if;
      Expand_Node (Case_Type (Node));
      Expand_Array_Declarator (Case_Decl (Node));
   end Expand_Case;

   procedure Expand_Sequence
     (Node : Node_Id)
   is
      Sequence_Node : Node_Id
        := Node;
      Seq_Ref_Node : Node_Id
        := Make_Scoped_Name;
      Seq_Inst_Node : Node_Id
        := Make_Sequence_Instance;
   begin
      Add_Identifier_With_Renaming
        (Seq_Inst_Node,
         "IDL_" & Sequence_Type_Name (Node));
      --  FIXME: The Add_Identifier should be performed
      --    in the Current_Gen_Scope, not in the Current_Scope.
      --  FIXME: If the identifier is not available
      --     in the current gen scope, that may mean that the
      --     correct sequence type has already been created.
      --     If it is the case, maybe we should reuse it.

      Expand_Node (Sequence_Type (Node));

      declare
         Current_Gen_Scope : constant Node_Id
           := Get_Current_Gen_Scope;
         Current_Scope_Contents : Node_List;
      begin
         pragma Assert (Is_Gen_Scope (Current_Gen_Scope));
         Current_Scope_Contents
           := Contents (Current_Gen_Scope);
         Insert_Before
           (Current_Scope_Contents,
            Seq_Inst_Node,
            Before => Current_Position_In_List);
         Set_Contents (Current_Gen_Scope, Current_Scope_Contents);
      end;

      Set_Value (Seq_Ref_Node, Seq_Inst_Node);
      Set_S_Type (Seq_Ref_Node, Seq_Inst_Node);
      Replace_Node (Sequence_Node, Seq_Ref_Node);
      Set_Sequence (Seq_Inst_Node, Sequence_Node);
   end Expand_Sequence;

   procedure Expand_Constructed_Type
     (Node : Node_Id;
      Replacement_Node : out Node_Id)
   is
      NK : constant Node_Kind
        := Kind (Node);

   begin
      Replacement_Node := No_Node;

      if not (False
        or else NK = K_Enum
        or else NK = K_Union
        or else NK = K_Struct) then
         return;
      end if;

      Expand_Node (Node);

      pragma Debug (O ("Expand_Constructed_Type: enter"));

      declare
         Current_Gen_Scope : constant Node_Id
           := Get_Current_Gen_Scope;
         Current_Scope_Contents : Node_List;

         Constr_Type_Node : Node_Id := Node;
         Constr_Type_Ref_Node : Node_Id
           := Make_Scoped_Name;

      begin
         pragma Assert (Is_Gen_Scope (Current_Gen_Scope));
         Current_Scope_Contents
           := Contents (Current_Gen_Scope);

         Insert_Before
           (Current_Scope_Contents,
            Constr_Type_Node,
            Before => Current_Position_In_List);

         if Parent_Scope (Constr_Type_Node) /= Current_Gen_Scope then
            Add_Identifier_With_Renaming
              (Constr_Type_Node, Name (Constr_Type_Node));
            --  FIXME: We change the parent scope of Constr_Type_Node
            --    (it becomes Current_Scope). Actually this should
            --    be Current_Gen_Scope.
         end if;

         if Kind (Constr_Type_Node) = K_Enum then
            --  Also reparent all enumerators
            declare
               It : Node_Iterator;
               E_Node : Node_Id;
            begin
               Init (It, Enumerators (Constr_Type_Node));

               while not Is_End (It) loop
                  Get_Next_Node (It, E_Node);

                  Add_Identifier_With_Renaming
                    (E_Node, Name (E_Node));
                  --   FIXME: See above.
               end loop;
            end;
         end if;

         Set_Contents (Current_Gen_Scope, Current_Scope_Contents);

         Set_Value (Constr_Type_Ref_Node, Constr_Type_Node);
         Set_S_Type (Constr_Type_Ref_Node, Constr_Type_Node);
         Replacement_Node := Constr_Type_Ref_Node;
      end;
   end Expand_Constructed_Type;

   procedure Expand_Array_Declarators
     (Node : Node_Id)
   is
      It : Node_Iterator;
      D_Node : Node_Id;
      Position : Node_Id := Node;
      First : Boolean := True;
   begin
      pragma Assert (Kind (Node) = K_Member);
      pragma Debug (O ("Expand_Array_Declarators : enter"));
      Init (It, Decl (Node));
      while not Is_End (It) loop
         Get_Next_Node (It, D_Node);

         if Kind (D_Node) /= K_Declarator then
            Ada.Text_IO.Put_Line
              ("ERROR: Unexpected " & Kind (D_Node)'Img);
            raise Program_Error;
         end if;

         if not Is_Empty (Array_Bounds (D_Node)) then
            if not (First and then Is_End (It)) then
               declare
                  New_Node : constant Node_Id
                    := Copy_Node (Node);
               begin
                  Set_Decl
                    (Node, Remove_Node (Decl (Node), D_Node));
                  Set_Decl
                    (New_Node, Append_Node (Nil_List, D_Node));
                  Set_Parent (D_Node, New_Node);
                  Insert_After
                    (Members (Parent_Scope (D_Node)), New_Node, Position);
                  --  The new member will be processed again later.
                  Position := New_Node;
               end;
            else
               Expand_Array_Declarator (D_Node);
            end if;
         end if;
         First := False;
      end loop;
      pragma Debug (O ("Expand_Array_Declarators : leave"));
   end Expand_Array_Declarators;

   procedure Expand_Array_Declarator
     (Node : Node_Id)
   is
   begin
      if Is_Empty (Array_Bounds (Node)) then
         return;
      end if;

      declare
         Current_Gen_Scope : constant Node_Id
           := Get_Current_Gen_Scope;
         Current_Scope_Contents : Node_List;

         Parent_Node : constant Node_Id
           := Parent (Node);

         Array_Node : constant Node_Id := Make_Declarator;
         Array_Type_Node : constant Node_Id
           := Make_Type_Declarator;

         Element_Type_Node : Node_Id;
         Array_Ref_Node : Node_Id
        := Make_Scoped_Name;

         Success : Boolean;
      begin
         pragma Debug (O ("Expand_Array_Declarator: enter"));
         pragma Assert (Is_Gen_Scope (Current_Gen_Scope));

         Current_Scope_Contents
           := Contents (Current_Gen_Scope);

         case Kind (Parent_Node) is
            when K_Member =>
               Element_Type_Node := M_Type (Parent_Node);
            when K_Case =>
               Element_Type_Node := Case_Type (Parent_Node);
            when others =>
               pragma Assert (False);
               null;
         end case;
         Set_Value (Array_Ref_Node, Array_Node);
         Set_S_Type (Array_Ref_Node, Array_Node);

         Replace_Node (Element_Type_Node, Array_Ref_Node);

         Set_T_Type (Array_Type_Node, Element_Type_Node);
         Set_Declarators
           (Array_Type_Node,
            Append_Node (Nil_List, Array_Node));
         Success := Add_Identifier
           (Array_Node, Name (Node) & "_Array");
         pragma Assert (Success);
         --  FIXME: In current /gen/scope (see comment in Expand_Sequence).

         Insert_Before
           (Current_Scope_Contents,
            Array_Type_Node,
            Before => Current_Position_In_List);
         Set_Contents (Current_Gen_Scope, Current_Scope_Contents);

         Set_Array_Bounds (Array_Node, Array_Bounds (Node));
         Set_Array_Bounds (Node, Nil_List);
      end;
   end Expand_Array_Declarator;

   -----------------------------------------
   --          private utilities          --
   -----------------------------------------

   -----------------------
   --  Expand_Node_List --
   -----------------------

   procedure Expand_Node_List
     (List : in Node_List;
      Set_Current_Position : in Boolean)
   is
      It : Node_Iterator;
      Node : Node_Id;
   begin
      Init (It, List);

      while not Is_End (It) loop
         Get_Next_Node (It, Node);

         if Set_Current_Position then
            Current_Position_In_List := Node;
            pragma Debug
              (O ("Current_Position_In_List = "
                  & Img (Integer (Node))));
         end if;
         Expand_Node (Node);

      end loop;

      if Set_Current_Position then
         Current_Position_In_List := No_Node;
      end if;
   end Expand_Node_List;

   ------------------------------
   --  Append_Node_To_Contents --
   ------------------------------

   procedure Append_Node_To_Contents
     (Parent : Node_Id;
      Child : Node_Id) is
   begin
      Set_Contents
        (Parent, Append_Node (Contents (Parent), Child));
   end Append_Node_To_Contents;

   function Sequence_Type_Name
     (Node : Node_Id)
     return String
   is
      NK : constant Node_Kind
        := Kind (Node);
   begin
      case NK is

         when K_Sequence =>
            if Bound (Node) = No_Node then
               return "SEQUENCE_"
                 & Sequence_Type_Name (Sequence_Type (Node));
            else
               return "SEQUENCE_"
                 & Sequence_Type_Name (Sequence_Type (Node))
                 & Img (Integer_Value (Bound (Node)));
            end if;

         when K_Scoped_Name =>
            declare
               Full_Name : String
                 := Ada_Full_Name (Value (Node));
            begin
               for I in Full_Name'Range loop
                  if Full_Name (I) = '.' then
                     Full_Name (I) := '_';
                  end if;
               end loop;
               return Full_Name;
            end;

         when K_Short =>
            return "short";

         when K_Long =>
            return "long";

         when K_Long_Long =>
            return "long_long";

         when K_Unsigned_Short =>
            return "unsigned_short";

         when K_Unsigned_Long =>
            return "unsigned_long";

         when K_Unsigned_Long_Long =>
            return "unsigned_long_long";

         when K_Char =>
            return "char";

         when K_Wide_Char =>
            return "wide_char";

         when K_Boolean =>
            return "boolean";

         when K_Float =>
            return "float";

         when K_Double =>
            return "double";

         when K_Long_Double =>
            return "long_double";

         when K_String =>
            return "string";

         when K_Wide_String =>
            return "wide_string";

         when K_Octet =>
            return "octet";

         when others =>
            --  Improper use: node N is not
            --  mapped to an Ada type.

            Ada.Text_IO.Put_Line ("Error: A "
                                  & NK'Img
                                  & " cannot be used in a sequence.");
            raise Program_Error;
      end case;
   end Sequence_Type_Name;

   procedure Add_Identifier_With_Renaming
     (Node : Node_Id;
      Identifier : String)
   is
      Suffix : Integer := 1;
   begin
      if not Add_Identifier (Node, Identifier) then
         while not Add_Identifier
           (Node, Identifier & "_" & Img (Suffix)) loop
            Suffix := Suffix + 1;
         end loop;
      end if;
   end Add_Identifier_With_Renaming;

end Ada_Be.Expansion;

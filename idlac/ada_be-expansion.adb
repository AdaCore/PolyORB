with Idl_Fe.Types; use Idl_Fe.Types;
with Idl_Fe.Tree; use Idl_Fe.Tree;
with Idl_Fe.Tree.Synthetic; use Idl_Fe.Tree.Synthetic;
with Idl_Fe.Errors; use Idl_Fe.Errors;

with Ada_Be.Identifiers; use Ada_Be.Identifiers;
with Ada_Be.Debug;
pragma Elaborate (Ada_Be.Debug);

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
   procedure Expand_Interface
     (Node : in Node_Id);
   procedure Expand_Attribute
     (Node : in Node_Id);
   procedure Expand_Exception
     (Node : in Node_Id);

   ----------------------
   -- Utility routines --
   ----------------------

   procedure Expand_Node_List
     (List : in Node_List);
   --  Expand a whole list of nodes

   procedure Append_Node_To_Contents
     (Parent : Node_Id;
      Child : Node_Id);
   --  Append a node to the contents node_list of parent

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
         when K_Exception =>
            Expand_Exception (Node);
         when K_Ben_Idl_File =>
            Expand_Ben_Idl_File (Node);
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
            Current : Node_Id := Get_Node (Iterator);
            Loc : Idl_Fe.Errors.Location
              := Get_Location (Current);
            Filename : Idl_Fe.Errors.File_Name_Ptr := Loc.Filename;
            Idl_File_Node : Node_Id;
            Success : Boolean;
         begin
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
               Success := Add_Identifier (Idl_File_Node,
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

            --  next node
            Next (Iterator);
         end;
      end loop;

      Expand_Node_List (Contents (New_Repository));
      --  Pop_Scope;
   end Expand_Repository;


   --------------------
   --  Expand_Module --
   --------------------

   procedure Expand_Module (Node : in Node_Id) is
   begin
      pragma Assert (Kind (Node) = K_Module);
      Push_Scope (Node);
      Expand_Node_List (Contents (Node));
      --  Pop_Scope;
   end Expand_Module;

   --------------------------
   --  Expand_Ben_Idl_File --
   --------------------------

   procedure Expand_Ben_Idl_File (Node : in Node_Id) is
   begin
      pragma Assert (Kind (Node) = K_Ben_Idl_File);
      Push_Scope (Node);
      Expand_Node_List (Contents (Node));
      --  Pop_Scope;
   end Expand_Ben_Idl_File;

   -----------------------
   --  Expand_Interface --
   -----------------------

   function All_Secondary_Ancestors
     (Node : Node_Id)
     return Node_List;
   --  List all the ancestors of K_Interface/K_ValueType Node,
   --  except its first parent and the ancestors of its first
   --  parent. It is up to the caller to Free the returned
   --  Node_List after use.

   function All_Operations
     (Node : Node_Id)
     return Node_List;
   --  Return the operations defined within K_Interface Node.
   --  It is up to the caller to Free the returned Node_List
   --  after use.

   function All_Secondary_Ancestors
     (Node : Node_Id)
     return Node_List
   is
      Parents_List : constant Node_List
        := Parents (Node);
      Exclude_List : Node_List
        := Nil_List;
      Secondary_Ancestors_List : Node_List
        := Nil_List;
   begin
      pragma Assert (False
        or else Kind (Node) = K_Interface
        or else Kind (Node) = K_ValueType);

      if not Is_Empty (Parents_List) then
         Append_Node (Exclude_List, Value (Head (Parents_List)));
         Merge_List (Exclude_List,
                     All_Ancestors (Value (Head (Parents_List))));
         Secondary_Ancestors_List := All_Ancestors (Node, Exclude_List);
         Free (Exclude_List);
      end if;
      return Secondary_Ancestors_List;
   end All_Secondary_Ancestors;

   function All_Operations
     (Node : Node_Id)
     return Node_List
   is
      Ops : Node_List
        := Nil_List;
      It : Node_Iterator;
      E_Node : Node_Id;
   begin
      pragma Assert (Kind (Node) = K_Interface);

      Init (It, Contents (Node));
      while not Is_End (It) loop
         E_Node := Get_Node (It);
         Next (It);

         if Kind (E_Node) = K_Operation then
            pragma Assert (not Is_In_List (Ops, E_Node));
            Append_Node (Ops, E_Node);
         end if;
      end loop;

      return Ops;
   end All_Operations;

   procedure Expand_Interface
     (Node : in Node_Id)
   is
      Export_List : Node_List;

      Secondary_Ancestors : Node_List
        := All_Secondary_Ancestors (Node);
      It : Node_Iterator;
      A_Node : Node_Id;
   begin
      pragma Assert (Kind (Node) = K_Interface);
      Push_Scope (Node);

      Export_List := Contents (Node);
      Expand_Node_List (Export_List);
      --  First expand the interface's exports
      --  (eg, attributes are expanded into operations.)

      Init (It, Secondary_Ancestors);
      while not Is_End (It) loop
         A_Node := Get_Node (It);
         Next (It);

         declare
            Ops : Node_List
              := All_Operations (A_Node);
         begin
            Merge_List
              (Into => Export_List,
               From => Ops);
            Free (Ops);
         end;
      end loop;
      Free (Secondary_Ancestors);

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

         Current_Declarator := Get_Node (Iterator);
         Next (Iterator);

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
         Success := Add_Identifier
           (Members_Struct, Ada_Name (Node) & "_Members");
         pragma Assert (Success);

         Set_Members (Members_Struct, Members (Node));
         Set_Is_Exception_Members (Members_Struct, True);
         Insert_Before
           (List => Enclosing_List,
            Node => Members_Struct,
            Before => Node);
         Set_Members_Type (Node, Members_Struct);

         Set_Contents (Enclosing_Scope, Enclosing_List);
      end;
   end Expand_Exception;

   -----------------------------------------
   --          private utilities          --
   -----------------------------------------

   -----------------------
   --  Expand_Node_List --
   -----------------------

   procedure Expand_Node_List (List : in Node_List) is
   begin
      if List /= Nil_List then
         Expand_Node (List.Car);
         Expand_Node_List (List.Cdr);
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

end Ada_Be.Expansion;

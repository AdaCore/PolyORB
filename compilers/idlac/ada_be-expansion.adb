------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                     A D A _ B E . E X P A N S I O N                      --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2001-2006, Free Software Foundation, Inc.          --
--                                                                          --
-- PolyORB is free software; you  can  redistribute  it and/or modify it    --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. PolyORB is distributed  in the hope that it will be  useful,    --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with PolyORB; see file COPYING. If    --
-- not, write to the Free Software Foundation, 51 Franklin Street, Fifth    --
-- Floor, Boston, MA 02111-1301, USA.                                       --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
--                  PolyORB is maintained by AdaCore                        --
--                     (email: sales@adacore.com)                           --
--                                                                          --
------------------------------------------------------------------------------

with Idl_Fe.Tree;           use Idl_Fe.Tree;
with Idl_Fe.Tree.Synthetic; use Idl_Fe.Tree.Synthetic;
with Idl_Fe.Tree.Low_Level; use Idl_Fe.Tree.Low_Level;
with Idl_Fe.Utils;          use Idl_Fe.Utils;

with Ada_Be.Identifiers;    use Ada_Be.Identifiers;
with Ada_Be.Debug;
pragma Elaborate_All (Ada_Be.Debug);

with Errors;                use Errors;
with Utils;                 use Utils;

with Ada.Characters.Handling;
with GNAT.HTable;

package body Ada_Be.Expansion is

   -----------
   -- Debug --
   -----------

   Flag : constant Natural := Ada_Be.Debug.Is_Active ("ada_be.expansion");
   procedure O is new Ada_Be.Debug.Output (Flag);

   ------------------
   -- Shared state --
   ------------------

   In_Sequence_Type : Boolean := False;
   --  Set in Expand_Sequence during expansion of the sequence type

   ------------------------------------
   -- Internal expansion subprograms --
   ------------------------------------

   procedure Expand_Node (Node : Node_Id);
   --  Expand node according to its type (using the type-specific routines
   --  below).

   --  Specific expansion subprograms

   procedure Expand_Module (Node : Node_Id);
   procedure Expand_Ben_Idl_File (Node : Node_Id);
   --  Expand all subnodes

   procedure Expand_Interface
     (Node : Node_Id);
   --  First expand all subnodes, then copy inherited methods and attributes
   --  from ancestors

   procedure Expand_ValueType
     (Node : Node_Id);
   --  First expand all subnodes, then copy inherited methods and attributes
   --  from ancestors and supported interfaces

   procedure Expand_Boxed_ValueType
     (Node : Node_Id);
   --  Expand the type of a boxed value (sequence for example)

   procedure Expand_Attribute
     (Node : Node_Id);
   --  Expand an attribute into the corresponding _get_ and _set_ operations

   procedure Expand_State_Member
     (Node : Node_Id);
   --  Expand a State_Member :
   --  1. Split it if there are several declarators;
   --  2. Creates the corresponding _get_ and _set_ operations.

   procedure Expand_Attribute_Or_State_Member
     (Node        : Node_Id;
      The_Type    : Node_Id;
      Declarators : Node_List;
      Is_Readable : Boolean;
      Is_Writable : Boolean);
   --  Code factorization for the two subprograms above

   procedure Expand_Operation
     (Node : Node_Id);
   --  Expand each formal parameter, then replace
   --  the operation if it has a non-void return
   --  type and out formal parameters with a void
   --  operation having a supplementary "Returns"
   --  out formal parameter.

   procedure Expand_Param
     (Node : Node_Id);
   --  Expand Param_Type.

   procedure Expand_Exception
     (Node : Node_Id);
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

   procedure Expand_Constant
     (Node : Node_Id);
   --  Expand the constant's type.

   procedure Expand_Union
     (Node : Node_Id);
   procedure Expand_Case
     (Node : Node_Id);
   --  Expand union: for each case, isolate array declarators,
   --  then expand Case_Type.

   procedure Expand_Enum
     (Node : Node_Id);
   --  Expand enum (used to handle renaming of Ada keywords)

   procedure Expand_Sequence
     (Node : Node_Id);
   --  Replace a Sequence node with a reference to
   --  a Sequence_Instance node. The Sequence Instance
   --  node is also inserted as a declaration in the current
   --  scope.

   procedure Expand_String
     (Node : Node_Id);
   --  Replace a bounded string or bounded wide string node
   --  with a reference to a Bounded_String_Instance node.

   procedure Expand_Fixed
     (Node : Node_Id);
   --  Expand a fixed-point node.

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

   procedure Expand_Scoped_Name (Node : Node_Id);
   --  If Node is a reference to an interface or valuetype
   --  within the current scope for which a forward declaration
   --  exists, then reference the forward declaration instead
   --  of the interface or valuetype itself.

   ----------------------
   -- Utility routines --
   ----------------------

   subtype Location is Errors.Location;

   Current_Position_In_List : Node_Id := No_Node;
   procedure Expand_Node_List
     (List : Node_List;
      Set_Current_Position : Boolean);
   --  Expand a whole list of nodes
   --  The global variable Current_Position_In_List is set
   --  before each node is expanded.

   function Sequence_Type_Name
     (Node : Node_Id)
     return String;
   --  The name corresponding to type Node as used
   --  to construct the name of an instance of
   --  CORBA.Sequences.Bounded or CORBA.Sequences.Unbounded.

   procedure Insert_Before_Current
     (Node : Node_Id);
   --  Insert node in Current_Gen_Scope immediately before
   --  Current_Position_In_List.

   function Has_Out_Formals
     (Node : Node_Id)
     return Boolean;
   --  True if Node (K_Operation) has "out" or "in out" formal parameters

   function Is_Ada_Keyword (Name : String) return Boolean;
   --  Check whether Name is an Ada 95 keyword

   procedure Recursive_Copy_Operations
     (Into                        : in out Node_List;
      Parent                      : Node_Id;
      From                        : Node_Id;
      Implicit_Inherited          : Boolean;
      Directly_Supported          : Boolean;
      Oldest_Supporting_ValueType : Node_Id;
      Parents_Seen                : in out Node_List);
   --  Recursively copy all operations from K_Interface or K_ValueType node
   --  From and all its ancestors into Into. Ancestors are appended to the
   --  Parents_Seen list as they are explored, and will not be explored twice.

   --  The Parent_Scope for the copies is set to Parent, and the
   --  Is_Implicit_Inherited attribute is set to Implicit_Inherited.

   --  Directly_Supported and Oldest_Supporting_ValueType are valuetype
   --  attributes. See nodes.txt for their meaning.

   function Is_CORBA_PolicyList (Node : Node_Id) return Boolean;
   --  Return True iff Node denotes CORBA::PolicyList

   function Is_CORBA_IR_Entity (Node : Node_Id) return Boolean;
   --  Return True iff Node denotes one entity from CORBA Interface Repository.

   function Is_CORBA_Sequence (Node : Node_Id) return Boolean;
   --  Return True iff Node is a sequence type declared in the CORBA module,
   --  in which case its mapped type is reparented under the
   --  CORBA.IDL_SEQUENCES package.

   --  Predefined CORBA entities requiring specific processing

   CORBA_TypeCode_Node : Node_Id := No_Node;
   --  Declaration of CORBA::TypeCode
   --  CORBA::TypeCode interface don't have instantiated forward declaration
   --  package, thus we always must use full declaration node, independed
   --  of existence of forward declaration in used orb.idl or TypeCode.idl
   --  file.

   -----------------------
   -- Subprogram bodies --
   -----------------------

   -----------------
   -- Expand_Node --
   -----------------

   procedure Expand_Node (Node : Node_Id) is
      NK : constant Node_Kind := Kind (Node);
   begin
      if Expanded (Node) then
         return;
      end if;

      pragma Debug (O ("Expanding node : " & Node_Kind'Image (NK)));

      --  Set node expanded early to catch infinite loops as well

      Set_Expanded (Node, True);

      if Is_Named (Node) then
         if Is_Ada_Keyword (Name (Node)) then

            --  Rename nodes whose name collide with Ada
            --  reserved words.

            pragma Debug
              (O ("Renaming node" & Node_Id'Image (Node)
                  & " with kind " & Node_Kind'Image (Kind (Node))
                  & " to IDL_" & Name (Node)));
            Add_Identifier_With_Renaming
              (Node, "IDL_" & Name (Node),
               Is_Inheritable => False);
         end if;

         --  Allocate a name for the node's repository ID

         if Kind (Node) /= K_Repository
           and then Kind (Node) /= K_Ben_Idl_File
         then
            declare
               RID_Name_Node : constant Node_Id
                 := Make_Named (Loc (Node));
            begin
               Set_Repository_Id_Identifier (Node, RID_Name_Node);
               if Is_Gen_Scope (Node) then
                  Push_Scope (Node);
                  Add_Identifier_With_Renaming
                    (RID_Name_Node, "Repository_Id", Is_Inheritable => False);
                  Pop_Scope;
               else
                  Add_Identifier_With_Renaming
                    (RID_Name_Node, Name (Node) & "_Repository_Id",
                     Is_Inheritable => False);
               end if;
               pragma Debug (O ("Allocated RID name:"
                                & Name (RID_Name_Node)));
            end;
         end if;
      end if;

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
         when K_ValueType =>
            Expand_ValueType (Node);
         when K_State_Member =>
            Expand_State_Member (Node);

         when K_Type_Declarator =>
            Expand_Type_Declarator (Node);
         when K_Const_Dcl =>
            Expand_Constant (Node);
         when K_Struct =>
            Expand_Struct (Node);
         when K_Member =>
            Expand_Member (Node);
         when K_Union =>
            Expand_Union (Node);
         when K_Case =>
            Expand_Case (Node);
         when K_Fixed =>
            Expand_Fixed (Node);

         when K_Sequence =>
            Expand_Sequence (Node);

         when K_Enum =>
            Expand_Enum (Node);

         when K_Param =>
            Expand_Param (Node);

         when
           K_String      |
           K_Wide_String =>
            Expand_String (Node);

         when K_Boxed_ValueType =>
            Expand_Boxed_ValueType (Node);

         when K_Scoped_Name =>
            Expand_Scoped_Name (Node);

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

   Unknown_Filename : constant Errors.String_Ptr
     := new String'("<unknown file>.idl");

   procedure  Expand_Repository (Node : Node_Id) is

      Iterator : Node_Iterator;

      type Header_Num is range 0 .. 1024;
      function Hash is new GNAT.HTable.Hash (Header_Num);
      function Hash (A : Errors.String_Ptr) return Header_Num;
      function Hash (A : Errors.String_Ptr) return Header_Num is
      begin
         return Hash (A.all);
      end Hash;
      function Equals (A, B : Errors.String_Ptr) return Boolean;
      function Equals (A, B : Errors.String_Ptr) return Boolean is
      begin
         return A.all = B.all;
      end Equals;
      package Idlnodes is new GNAT.HTable.Simple_HTable
        (Header_Num,
         Node_Id,
         No_Node,
         Errors.String_Ptr,
         Hash,
         Equals);

      Repository_Contents : constant Node_List
        := Contents (Node);
      New_Repository_Contents : Node_List
        := Nil_List;
      Is_Unknown_File : Boolean;
   begin
      Push_Scope (Node);

      Init (Iterator, Repository_Contents);
      while not Is_End (Iterator) loop
         declare
            Current : Node_Id;
            Loc : Errors.Location;
            Filename : Errors.String_Ptr;

            Idl_File_Node : Node_Id;
            Success : Boolean;

            Has_Named_Subnodes : Boolean := False;
            Named_Subnodes : Node_Iterator;
         begin
            Get_Next_Node (Iterator, Current);
            Loc := Get_Location (Current);
            Filename := Loc.Filename;

            if Filename = null then
               Is_Unknown_File := True;
               Filename := Unknown_Filename;
            else
               Is_Unknown_File := False;
            end if;
            pragma Debug (O ("node "
                             & Node_Kind'Image (Kind (Current))
                             & " in file "
                             & Filename.all));

            Idl_File_Node := Idlnodes.Get (Filename);

            --  if this is the first node of this file
            if Idl_File_Node = No_Node then

               --  create a new node Ben_Idl_File
               Idl_File_Node := Make_Ben_Idl_File (Loc);
               Set_Is_Unknown (Idl_File_Node, Is_Unknown_File);

               --  set its name
               --  is it correct when conflict ?
               if Get_Current_Scope /= Node then
                  Pop_Scope;
                  Push_Scope (Node);
               end if;
               declare
                  Base_Name : constant String
                     := Filename.all (Filename'First .. Filename'Last - 4);
                  --  Omit ".idl"
               begin
                  Success := Add_Identifier
                    (Idl_File_Node, Base_Name  & "_IDL_File");
               end;
               pragma Assert (Success);

               --  add the new node to the hashtable.
               Idlnodes.Set (Filename, Idl_File_Node);
               --  add the new node to the repository.
               Append_Node (New_Repository_Contents, Idl_File_Node);
            end if;

            pragma Assert (Idl_File_Node /= No_Node);

            if Get_Current_Scope /= Idl_File_Node then
               --  Entering a new file.
               Pop_Scope;
               Push_Scope (Idl_File_Node);
            end if;

            Append_Node_To_Contents (Idl_File_Node, Current);
            if Is_Named (Current) then

               --  Reparent current node.

               if Definition (Current) /= null then
                  Success := Add_Identifier (Current, Name (Current));
                  pragma Assert (Success);
               end if;

               if Is_Enum (Current) then
                  Has_Named_Subnodes := True;
                  Init (Named_Subnodes, Enumerators (Current));
                  --  Reparent all enumerators of current node.
               end if;

               --  Enable code generation for Idl_File only if its
               --  content is not imported to another file

               if Kind (Current) /= K_Forward_Interface
                 and then Kind (Current) /= K_Forward_ValueType
                 and then Imported (Current)
               then
                  Set_Generate_Code (Idl_File_Node, False);
               end if;

            elsif Is_Type_Declarator (Current) then
               Has_Named_Subnodes := True;
               Init (Named_Subnodes, Declarators (Current));
               --  Reparent all declarators of current node.

            end if;

            --  If the current node has named subnodes, reparent
            --  them now.

            if Has_Named_Subnodes then
               declare
                  Dcl_Node : Node_Id;
               begin
                  while not Is_End (Named_Subnodes) loop
                     Get_Next_Node (Named_Subnodes, Dcl_Node);
                     Success := Add_Identifier
                       (Dcl_Node, Name (Dcl_Node));
                     pragma Assert (Success);
                  end loop;
               end;
            end if;
         end;
      end loop;

      Set_Contents (Node, New_Repository_Contents);

      Pop_Scope;
      Expand_Node_List (Contents (Node), True);
   end Expand_Repository;

   -------------------
   -- Expand_Module --
   -------------------

   procedure Expand_Module (Node : Node_Id) is

      procedure Relocate (Parent : Node_Id; Node : Node_Id);
      --  Reparent Node and its named subnodes to the new Parent

      procedure Relocate (Parent : Node_Id; Node : Node_Id) is
         Has_Named_Subnodes : Boolean;
         Named_Subnodes     : Node_Iterator;
         Success            : Boolean;

      begin
         Push_Scope (Parent);

         Append_Node_To_Contents (Parent, Node);

         if Is_Named (Node) then

            --  Rattach current node

            if Definition (Node) /= null then
               Success := Add_Identifier (Node, Name (Node));
               pragma Assert (Success);
            end if;

            if Is_Enum (Node) then
               Has_Named_Subnodes := True;
               Init (Named_Subnodes, Enumerators (Node));
               --  Attach all enumerators of the current node

            end if;

         elsif Is_Type_Declarator (Node) then
            Has_Named_Subnodes := True;
            Init (Named_Subnodes, Declarators (Node));
            --  Attach all declarators of the current node

         end if;

         --  If the current node has named subnodes, rattach
         --  them now.

         if Has_Named_Subnodes then
            declare
               Dcl_Node : Node_Id;

            begin
               while not Is_End (Named_Subnodes) loop
                  Get_Next_Node (Named_Subnodes, Dcl_Node);
                  Success := Add_Identifier (Dcl_Node, Name (Dcl_Node));
                  pragma Assert (Success);
               end loop;
            end;
         end if;

         Pop_Scope;
      end Relocate;

      CORBA_IR_Root_Node   : Node_Id;
      CORBA_Sequences_Node : Node_Id;
      CORBA_Policy_Node    : Node_Id;
      Success              : Boolean;

   begin
      pragma Assert (Kind (Node) = K_Module);

      Push_Scope (Node);

      if Name (Node) = "CORBA" then
         --  Allocate CORBA.Repository_Root node for rattachment all entities
         --  of the Interface Repository to it

         CORBA_IR_Root_Node := Make_Module (No_Location);
         Set_Default_Repository_Id (CORBA_IR_Root_Node);
         Set_Initial_Current_Prefix (CORBA_IR_Root_Node);

         Success := Add_Identifier (CORBA_IR_Root_Node, "Repository_Root");
         pragma Assert (Success);

         Append_Node_To_Contents (Node, CORBA_IR_Root_Node);

         --  Allocate CORBA.IDL_SEQUENCES node for rattach all sequences to it

         CORBA_Sequences_Node := Make_Module (No_Location);
         Set_Default_Repository_Id (CORBA_Sequences_Node);
         Set_Initial_Current_Prefix (CORBA_Sequences_Node);

         Success := Add_Identifier (CORBA_Sequences_Node, "IDL_SEQUENCES");
         pragma Assert (Success);

         Append_Node_To_Contents (Node, CORBA_Sequences_Node);

         declare
            CORBA_Contents     : constant Node_List := Contents (Node);
            New_CORBA_Contents : Node_List;
            Iterator           : Node_Iterator;
            Current            : Node_Id;

         begin
            Init (Iterator, CORBA_Contents);

            while not Is_End (Iterator) loop
               Get_Next_Node (Iterator, Current);

               --  Detect and store Nodes for the CORBA entities that
               --  require special processing

               if Kind (Current) = K_Interface
                 and then Ada_Name (Current) = "TypeCode"
               then
                  CORBA_TypeCode_Node := Current;
               end if;

               if Kind (Current) = K_Interface
                 and then Ada_Name (Current) = "Policy"
               then
                  CORBA_Policy_Node := Current;
               end if;

               --  Relocate CORBA Interface Repository entities

               if Is_CORBA_IR_Entity (Current) then
                  Relocate (CORBA_IR_Root_Node, Current);

               elsif Is_CORBA_Sequence (Current) then
                  Relocate (CORBA_Sequences_Node, Current);

               elsif Is_CORBA_PolicyList (Current) then
                  Relocate (CORBA_Policy_Node, Current);

               else
                  Append_Node (New_CORBA_Contents, Current);
               end if;
            end loop;

            Set_Contents (Node, New_CORBA_Contents);
         end;
      end if;

      Expand_Node_List (Contents (Node), True);
      Pop_Scope;
   end Expand_Module;

   --------------------------
   --  Expand_Ben_Idl_File --
   --------------------------

   procedure Expand_Ben_Idl_File (Node : Node_Id) is
   begin
      pragma Assert (Kind (Node) = K_Ben_Idl_File);
      Push_Scope (Node);
      Expand_Node_List (Contents (Node), True);
      Pop_Scope;
   end Expand_Ben_Idl_File;

   ---------------------------------
   --  Recursive_Copy_Operations  --
   ---------------------------------

   procedure Recursive_Copy_Operations
     (Into                        : in out Node_List;
      Parent                      : Node_Id;
      From                        : Node_Id;
      Implicit_Inherited          : Boolean;
      Directly_Supported          : Boolean;
      Oldest_Supporting_ValueType : Node_Id;
      Parents_Seen                : in out Node_List)
   is
      Ops_It : Node_Iterator;
      O_Node : Node_Id;
      New_O_Node : Node_Id;

      Inh_It : Node_Iterator;
      I_Node : Node_Id;
   begin
      pragma Assert (False
        or else Kind (From) = K_Interface
        or else Kind (From) = K_ValueType);

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
            if not Implicit_Inherited then
               Set_Has_Non_Implicit_Inherited_Operations (Parent, True);
            end if;

            Set_Is_Directly_Supported
              (New_O_Node, Directly_Supported);
            if Oldest_Supporting_ValueType /= No_Node then
               Set_Oldest_Supporting_ValueType
                 (New_O_Node, Oldest_Supporting_ValueType);
            end if;

            Append_Node (Into, New_O_Node);
         end if;
      end loop;

      Append_Node (Parents_Seen, From);

      Init (Inh_It, Parents (From));
      while not Is_End (Inh_It) loop
         Get_Next_Node (Inh_It, I_Node);

         Recursive_Copy_Operations
           (Into, Parent, Value (I_Node),
            Implicit_Inherited,
            Directly_Supported,
            Oldest_Supporting_ValueType,
            Parents_Seen);
      end loop;
   end Recursive_Copy_Operations;

   ----------------------
   -- Expand_Interface --
   ----------------------

   procedure Expand_Interface
     (Node : Node_Id)
   is
      Export_List : Node_List;

      It : Node_Iterator;
      I_Node : Node_Id;
      Parents_Seen : Node_List
        := Nil_List;
      Primary_Parent : constant Node_Id
        := Idl_Fe.Tree.Synthetic.Primary_Parent (Node);
   begin
      pragma Assert (Kind (Node) = K_Interface);
      Push_Scope (Node);

      Export_List := Contents (Node);
      Expand_Node_List (Export_List, True);
      --  First expand the interface's exports
      --  (eg, attributes are expanded into operations.)

      Export_List := Contents (Node);
      --  Expand_Node_List may have inserted new nodes
      --  in Contents.

      --  copy the operations of the primary parent
      if  Primary_Parent /= No_Node then
         Recursive_Copy_Operations
           (Into => Export_List,
            Parent => Node,
            From => Value (Primary_Parent),
            Implicit_Inherited => True,
            Directly_Supported => False,
            Oldest_Supporting_ValueType => No_Node,
            Parents_Seen => Parents_Seen);
      end if;

      Init (It, Parents (Node));
      while not Is_End (It) loop
         Get_Next_Node (It, I_Node);

         Recursive_Copy_Operations
           (Into => Export_List,
            Parent => Node,
            From => Value (I_Node),
            Implicit_Inherited => False,
            Directly_Supported => False,
            Oldest_Supporting_ValueType => No_Node,
            Parents_Seen => Parents_Seen);
      end loop;

      Set_Contents (Node, Export_List);

      Pop_Scope;
   end Expand_Interface;

   ----------------------
   -- Expand_ValueType --
   ----------------------

   procedure Expand_ValueType (Node : Node_Id) is
      Export_List  : Node_List;
      It           : Node_Iterator;
      I_Node       : Node_Id;
      Parents_Seen : Node_List
        := Nil_List;
      Interfaces_Seen : Node_List
        := Nil_List;
      Primary_Parent  : constant Node_Id
        := Idl_Fe.Tree.Synthetic.Primary_Parent (Node);
   begin
      pragma Assert (Kind (Node) = K_ValueType);
      Push_Scope (Node);

      Export_List := Contents (Node);
      Expand_Node_List (Export_List, True);
      --  First expand the valuetype's exports
      --  (eg, attributes are expanded into operations.)

      Export_List := Contents (Node);
      --  Expand_Node_List may have inserted new nodes
      --  in Contents.

      --  Copy the operations of the primary parent

      if  Primary_Parent /= No_Node then
         Recursive_Copy_Operations
           (Into   => Export_List,
            Parent => Node,
            From   => Value (Primary_Parent),
            Implicit_Inherited
                   => True,
            Directly_Supported
                   => False,
            Oldest_Supporting_ValueType
                   => No_Node,
            Parents_Seen => Parents_Seen);
      end if;

      --  Copy the operations of the secondary parents

      Init (It, Parents (Node));
      while not Is_End (It) loop
         Get_Next_Node (It, I_Node);

         Recursive_Copy_Operations
           (Into => Export_List,
            Parent => Node,
            From => Value (I_Node),
            Implicit_Inherited => False,
            Directly_Supported => False,
            Oldest_Supporting_ValueType => No_Node,
            Parents_Seen => Parents_Seen);
      end loop;

      --  Copy the operations of the supported interfaces

      Init (It, Supports (Node));
      while not Is_End (It) loop
         Get_Next_Node (It, I_Node);

         Recursive_Copy_Operations
           (Into => Export_List,
            Parent => Node,
            From => Value (I_Node),
            Implicit_Inherited => False,
            Directly_Supported => True,
            Oldest_Supporting_ValueType => Node,
            Parents_Seen => Interfaces_Seen);
      end loop;

      Set_Contents (Node, Export_List);

      Pop_Scope;
   end Expand_ValueType;

   ----------------------
   -- Expand_Attribute --
   ----------------------

   procedure Expand_Attribute (Node : Node_Id) is
   begin
      pragma Assert (Kind (Node) = K_Attribute);

      Expand_Node (A_Type (Node));
      Expand_Attribute_Or_State_Member
        (Node,
         A_Type (Node),
         Declarators (Node),
         Is_Readable => True,
         Is_Writable => not Is_Readonly (Node));
   end Expand_Attribute;

   -------------------------
   -- Expand_State_Member --
   -------------------------

   procedure Expand_State_Member
     (Node : Node_Id) is
      Declarators : Node_List;
      It : Node_Iterator;
      Current_Decl : Node_Id;
      Parent_List : Node_List;
   begin
      pragma Assert (Kind (Node) = K_State_Member);
      Declarators := State_Declarators (Node);

      --  expand the type
      Expand_Node (State_Type (Node));

      Init (It, Declarators);
      pragma Assert (not Is_End (It));
      Get_Next_Node (It, Current_Decl);
      Parent_List := Contents (Parent_Scope (Current_Decl));

      while not Is_End (It) loop
         pragma Debug (O ("Expand_State_Member:"));
         declare
            New_State_Member : constant Node_Id
              := Make_State_Member (Get_Location (Current_Decl));
            Decls : Node_List := Nil_List;
         begin
            Get_Next_Node (It, Current_Decl);
            Append_Node (Decls, Current_Decl);
            --  create node State_Member
            Set_Original_Node (New_State_Member, Node);
            Set_State_Type (New_State_Member, State_Type (Node));
            Set_Is_Public (New_State_Member, Is_Public (Node));
            Set_State_Declarators (New_State_Member, Decls);
            Insert_After
              (List => Parent_List,
               Node => New_State_Member,
               After => Node);
            Remove_Node (Declarators, Current_Decl);

            Expand_Attribute_Or_State_Member
              (Node => New_State_Member,
               The_Type => State_Type (New_State_Member),
               Declarators => State_Declarators (New_State_Member),
               Is_Readable => Is_Public (New_State_Member),
               Is_Writable => Is_Public (New_State_Member));
         end;
      end loop;

      --  and now expand the first declarator
      Expand_Attribute_Or_State_Member
        (Node => Node,
         The_Type => State_Type (Node),
         Declarators => State_Declarators (Node),
         Is_Readable => Is_Public (Node),
         Is_Writable => Is_Public (Node));
   end Expand_State_Member;

   ----------------------------------------
   --  Expand_Attribute_Or_State_Member --
   ----------------------------------------

   procedure Expand_Attribute_Or_State_Member
     (Node : Node_Id;
      The_Type : Node_Id;
      Declarators : Node_List;
      Is_Readable : Boolean;
      Is_Writable : Boolean)
   is
      Exports_List : Node_List := Nil_List;
      --  The exports list of the interface
      --  containing these attributes, wherein we insert
      --  _get_Attribute and _set_Attribute operations.

      Loc : Location;

      Position : Node_Id := Node;
      Iterator : Node_Iterator;
      Current_Declarator : Node_Id;
   begin
      pragma Assert (Kind (Node) = K_Attribute
                     or else Kind (Node) = K_State_Member);

      Init (Iterator, Declarators);

      while not Is_End (Iterator) loop
         Get_Next_Node (Iterator, Current_Declarator);

         if Exports_List = Nil_List then
            Exports_List := Contents (Parent_Scope (Current_Declarator));
            pragma Assert (Exports_List /= Nil_List);
         end if;

         pragma Debug (O ("Expanding attribute or state member "
                          & "declarator with name : "
                          & Ada_Name (Current_Declarator)));

         Loc := Get_Location (Current_Declarator);

         if Is_Readable then
            --  create the get_method
            declare
               Get_Method : constant Node_Id
                 := Make_Operation (Loc);
               Success : Boolean;
            begin
               pragma Debug (O ("Creating _get_ method"));
               Success := Add_Identifier
                 (Get_Method, "_get_"
                  & Ada_Name (Current_Declarator));
               pragma Assert (Success);
               Push_Scope (Get_Method);
               Set_Is_Oneway (Get_Method, False);
               Set_Operation_Type (Get_Method, The_Type);
               --  parameters
               Set_Parameters (Get_Method, Nil_List);
               if Kind (Node) = K_Attribute then
                  if Is_Readonly (Node) then
                     Set_Raises (Get_Method, Raises (Node));
                  else
                     Set_Raises (Get_Method, Get_Raises (Node));
                  end if;
               else
                  Set_Raises (Get_Method, Nil_List);
               end if;
               Set_Contexts (Get_Method, Nil_List);
               Set_Original_Node (Get_Method, Node);

               if Kind (Node) = K_State_Member then
                  Set_Oldest_Supporting_ValueType
                    (Get_Method, Parent_Scope (Current_Declarator));
               end if;

               Insert_After
                 (List => Exports_List,
                  Node => Get_Method,
                  After => Position);
               Position := Get_Method;
               Pop_Scope;
            end;
         end if;

         --  create the Set method
         if Is_Writable then
            declare
               Set_Method : constant Node_Id
                 := Make_Operation (Loc);
               Void_Node : constant Node_Id
                 := Make_Void (Loc);
               Success : Boolean;
            begin
               pragma Debug (O ("Creating _set_ method"));
               Success := Add_Identifier
                 (Set_Method, "_set_"
                  & Ada_Name (Current_Declarator));
               pragma Assert (Success);
               Push_Scope (Set_Method);
               Set_Is_Oneway (Set_Method, False);
               Set_Operation_Type (Set_Method, Void_Node);
               --  parameters
               declare
                  Param : constant Node_Id := Make_Param (Loc);
                  Decl  : constant Node_Id := Make_Declarator (Loc);
                  Params : Node_List := Nil_List;
               begin
                  --  new value parameter
                  Set_Mode (Param, Mode_In);
                  Set_Param_Type (Param, The_Type);
                  Success := Add_Identifier (Decl, "To");
                  pragma Assert (Success);
                  Set_Array_Bounds (Decl, Nil_List);
                  Set_Parent (Decl, Param);
                  Set_Declarator (Param, Decl);
                  Append_Node (Params, Param);

                  Set_Parameters (Set_Method, Params);
               end;
               if Kind (Node) = K_Attribute then
                  Set_Raises (Set_Method, Set_Raises (Node));
               else
                  Set_Raises (Set_Method, Nil_List);
               end if;
               Set_Contexts (Set_Method, Nil_List);
               Set_Original_Node (Set_Method, Node);

               if Kind (Node) = K_State_Member then
                  Set_Oldest_Supporting_ValueType
                    (Set_Method, Parent_Scope (Current_Declarator));
               end if;
               --  add the node to the node list
               Insert_After
                 (List => Exports_List,
                  Node => Set_Method,
                  After => Position);
               Position := Set_Method;
               Pop_Scope;
            end;
         end if;
         Expand_Node (Current_Declarator);
      end loop;
   end Expand_Attribute_Or_State_Member;

   procedure Expand_Operation
     (Node : Node_Id)
   is
      Loc : constant Location := Get_Location (Node);
   begin
      Expand_Node_List (Parameters (Node), False);
      Expand_Node (Operation_Type (Node));
      --  First expand all formals and return type.

      if Kind (Operation_Type (Node)) /= K_Void
        and then Has_Out_Formals (Node) then
         declare
            Operation_Type_Node : Node_Id
              := Operation_Type (Node);

            Void_Node : Node_Id
              := Make_Void (Loc);

            Param_Node : constant Node_Id
              := Make_Param (Loc);
            Decl_Node : constant Node_Id
              := Make_Declarator (Loc);

            Success : Boolean;
         begin

            --  Create an identifier in the operation's scope

            Push_Scope (Node);
            Success := Add_Identifier (Decl_Node, "Returns");
            pragma Assert (Success);
            Pop_Scope;

            --  Make the operation void. The actual operation type can be
            --  retrieved from the Void_Node's Original_Node attribute.

            Replace_Node (Operation_Type_Node, Void_Node);

            --  Create a new parameter node

            Set_Mode (Param_Node, Mode_Out);
            Set_Param_Type (Param_Node, Operation_Type_Node);
            Set_Declarator (Param_Node, Decl_Node);
            Set_Is_Returns (Param_Node, True);
            Set_Parent (Decl_Node, Param_Node);

            --  Insert it in the operation parameter list

            Set_Parameters (Node, Append_Node (Parameters (Node), Param_Node));
         end;
      end if;

      --  If this operation is defined in a valuetype, set its
      --  "Oldest_Supporting_ValueType" attribute

      if Kind (Parent_Scope (Node)) = K_ValueType then
         Set_Oldest_Supporting_ValueType
           (Node, Parent_Scope (Node));
      end if;

      --  If this operation is not implicitly inherited, note it in the
      --  enclosing interface.

      if Kind (Parent_Scope (Node)) = K_Interface
        and then not Is_Implicit_Inherited (Node)
      then
         Set_Has_Non_Implicit_Inherited_Operations (Parent_Scope (Node), True);
      end if;
   end Expand_Operation;

   procedure Expand_Param
     (Node : Node_Id) is
   begin
      Expand_Node (Param_Type (Node));
      Expand_Node (Declarator (Node));
   end Expand_Param;

   procedure Expand_Exception
     (Node : Node_Id)
   is
      Loc : constant Location := Get_Location (Node);
   begin
      pragma Assert (Kind (Node) = K_Exception);

      declare
         Members_Struct : constant Node_Id
           := Make_Struct (Loc);
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

         Set_Default_Repository_Id (Members_Struct);
         Set_Initial_Current_Prefix (Members_Struct);
         Set_Members (Members_Struct, Members (Node));
         Set_Is_Exception_Members (Members_Struct, True);
         Set_Members_Type (Node, Members_Struct);

         --  Members_Struct must be expanded as though it was
         --  encountered during the traversal of the Enclosing_List,
         --  for the necessary ancillary types (arrays, sequences...)
         --  to be declared correctly before it. We thus need to
         --  insert it at the proper position, and then temporarily
         --  fake Current_Position_In_List.

         Insert_Before
           (List => Enclosing_List,
            Node => Members_Struct,
            Before => Node);
         Set_Contents (Enclosing_Scope, Enclosing_List);

         pragma Assert (Current_Position_In_List = Node);
         --  If this were not the case we would need to save
         --  Current_Position_In_List in a temporary variable
         --  so we can restore it after expanding Member_Struct.

         Current_Position_In_List := Members_Struct;
         Expand_Node (Members_Struct);
         Current_Position_In_List := Node;
      end;
   end Expand_Exception;

   ----------------------------
   -- Expand_Type_Declarator --
   ----------------------------

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
      Expand_Node_List (Declarators (Node), False);
   end Expand_Type_Declarator;

   procedure Expand_Struct
     (Node : Node_Id) is
   begin
      Push_Scope (Node);
      Expand_Node_List (Members (Node), False);
      Pop_Scope;
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
      Expand_Node_List (Decl (Node), False);
   end Expand_Member;

   procedure Expand_Constant
     (Node : Node_Id) is
   begin
      Expand_Node (Constant_Type (Node));
   end Expand_Constant;

   procedure Expand_Union
     (Node : Node_Id)
   is
      R_Node : Node_Id;
   begin
      Push_Scope (Node);
      Expand_Constructed_Type (Switch_Type (Node), R_Node);
      if R_Node /= No_Node then
         Set_Switch_Type (Node, R_Node);
      end if;
      Expand_Node_List (Cases (Node), False);
      Pop_Scope;
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
      Expand_Node (Case_Decl (Node));
   end Expand_Case;

   procedure Expand_Enum
     (Node : Node_Id)
   is
   begin
      Expand_Node_List (Enumerators (Node), False);
   end Expand_Enum;

   -----------------------
   --  Expand_Sequence  --
   -----------------------

   procedure Expand_Sequence
     (Node : Node_Id)
   is
      Loc : constant Location := Get_Location (Node);
      Sequence_Node : Node_Id
        := Node;
      Seq_Ref_Node : Node_Id
        := Make_Scoped_Name (Loc);
      Seq_Inst_Node : constant Node_Id
        := Make_Sequence_Instance (Loc);
      Prev_In_Sequence_Type : constant Boolean := In_Sequence_Type;
   begin
      In_Sequence_Type := True;
      Expand_Node (Sequence_Type (Node));
      In_Sequence_Type := Prev_In_Sequence_Type;

      Add_Identifier_With_Renaming
        (Seq_Inst_Node,
         "IDL_" & Sequence_Type_Name (Node),
         Get_Current_Gen_Scope);
      --  FIXME: If the identifier is not available
      --     in the current gen scope, that may mean that the
      --     correct sequence type has already been created.
      --     If it is the case, maybe we should reuse it.

      Insert_Before_Current (Seq_Inst_Node);

      Set_Value (Seq_Ref_Node, Seq_Inst_Node);

      Replace_Node (Sequence_Node, Seq_Ref_Node);
      Set_Sequence (Seq_Inst_Node, Sequence_Node);
   end Expand_Sequence;

   Prefix : constant array (Boolean) of String_Cacc
     := (False => new String'("Bounded_String"),
         True  => new String'("Bounded_Wide_String"));

   procedure Expand_String
     (Node : Node_Id)
   is
      Loc : constant Location := Get_Location (Node);
   begin
      if Bound (Node) = No_Node then
         --  This string is not bounded.
         return;
      end if;

      declare
         Is_Wide_String : constant Boolean
           := Kind (Node) = K_Wide_String;

         String_Node : Node_Id
           := Node;
         String_Inst_Node : constant Node_Id
           := Make_String_Instance (Loc);
         String_Ref_Node : Node_Id
           := Make_Scoped_Name (Loc);

      begin
         Add_Identifier_With_Renaming
           (String_Inst_Node,
            Prefix (Is_Wide_String).all
            & "_" & Img (Integer_Value (Bound (Node))));

         Set_Is_Wide
           (String_Inst_Node,
            Is_Wide_String);
         Set_Bound (String_Inst_Node, Bound (Node));

         Insert_Before_Current (String_Inst_Node);

         Set_Value (String_Ref_Node, String_Inst_Node);

         Replace_Node (String_Node, String_Ref_Node);
      end;
   end Expand_String;

   procedure Expand_Fixed
     (Node : Node_Id)
   is
      Loc : constant Location := Get_Location (Node);

      Fixed_Node : Node_Id
        := Node;
      Fixed_Ref_Node : Node_Id
        := Make_Scoped_Name (Loc);

      Identifier : constant String
        := "Fixed_" & Img (Integer_Value (Digits_Nb (Node)))
        & "_" & Img (Integer_Value (Scale (Node)));

      Definition : constant Identifier_Definition_Acc
        := Find_Identifier_Definition (Identifier, Loc);
      Success : Boolean;

   begin
      pragma Assert (Kind (Node) = K_Fixed);

      Replace_Node (Fixed_Node, Fixed_Ref_Node);

      --  If the identifier already exists in the current scope,
      --  and resolves to denote a K_Fixed typedef, then it
      --  is guaranteed that it is a node created by expansion,
      --  and we can reuse it.

      if Definition = null then
         declare
            Typedef_Node : constant Node_Id
              := Make_Type_Declarator (Loc);
            Declarator_Node : constant Node_Id
              := Make_Declarator (Loc);
         begin
            Success := Add_Identifier (Declarator_Node, Identifier,
                                       Get_Current_Gen_Scope);
            pragma Assert (Success);

            Set_Original_Node (Declarator_Node, Fixed_Node);

            Set_Value (Fixed_Ref_Node, Declarator_Node);

            Insert_Before_Current (Typedef_Node);

            Set_T_Type (Typedef_Node, Fixed_Node);
            Set_Declarators
              (Typedef_Node, Append_Node (Nil_List, Declarator_Node));
            Set_Parent (Declarator_Node, Typedef_Node);
         end;
      else
         Set_Value (Fixed_Ref_Node, Definition.Node);
      end if;
   end Expand_Fixed;

   ------------------------------
   --  Expand_Boxed_ValueType  --
   ------------------------------

   procedure Expand_Boxed_ValueType
     (Node : Node_Id) is
   begin
      Expand_Node (Boxed_Type (Node));
   end Expand_Boxed_ValueType;

   -------------------------------
   --  Expand_Constructed_Type  --
   -------------------------------

   procedure Expand_Constructed_Type
     (Node : Node_Id;
      Replacement_Node : out Node_Id)
   is
      Loc : constant Location  := Get_Location (Node);
      NK  : constant Node_Kind := Kind (Node);
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
         Current_Gen_Scope : constant Node_Id := Get_Current_Gen_Scope;
         Constr_Type_Ref_Node : constant Node_Id := Make_Scoped_Name (Loc);

      begin
         Insert_Before_Current (Node);
         --  Pull up the constructed type node into a declaration
         --  by itself.

         if Parent_Scope (Node) /= Current_Gen_Scope then
            Add_Identifier_With_Renaming
              (Node, Name (Node),
               Current_Gen_Scope);

            if Kind (Node) = K_Enum then
               --  Also reparent all enumerators
               declare
                  It : Node_Iterator;
                  E_Node : Node_Id;
               begin
                  Init (It, Enumerators (Node));

                  while not Is_End (It) loop
                     Get_Next_Node (It, E_Node);

                     Add_Identifier_With_Renaming
                       (E_Node, Name (E_Node), Current_Gen_Scope);
                  end loop;
               end;
            end if;
         end if;

         Set_Value (Constr_Type_Ref_Node, Node);
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
            Error
              ("Unexpected " & Node_Kind'Image (Kind (D_Node)),
               Fatal, Get_Location (D_Node));
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
                    (Members (Parent_Scope (D_Node)),
                     New_Node,
                     After => Position);

                  Expand_Array_Declarator (D_Node);
                  Expand_Node_List (Decl (New_Node), False);
                  --  The new member would not be processed by
                  --  Expand_Struct, because the iterator in that
                  --  procedure is already pointing to the next one.
                  --  Also note that Expand_Member should not be
                  --  called on New_Node, because the M_Type
                  --  has already been expanded; only the declarators
                  --  of New_Node have not been expanded yet.

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
      Loc : constant Location := Get_Location (Node);
   begin
      if Is_Empty (Array_Bounds (Node)) then
         return;
      end if;

      declare
         Parent_Node       : constant Node_Id := Parent (Node);
         Array_Node        : constant Node_Id := Make_Declarator (Loc);
         Array_Type_Node   : constant Node_Id := Make_Type_Declarator (Loc);
         Array_Ref_Node    : constant Node_Id := Make_Scoped_Name (Loc);

         Element_Type_Node : Node_Id;
         Success           : Boolean;
      begin
         pragma Debug (O ("Expand_Array_Declarator: enter"));

         case Kind (Parent_Node) is
            when K_Member =>
               Element_Type_Node := M_Type (Parent_Node);
            when K_Case =>
               Element_Type_Node := Case_Type (Parent_Node);
            when others =>
               pragma Assert (False);
               null;
         end case;

         Set_Original_Node (Array_Node, Node);

         Set_Original_Node (Array_Ref_Node, Element_Type_Node);
         Set_Original_Node (Array_Type_Node, Element_Type_Node);

         Set_Value (Array_Ref_Node, Array_Node);

         case Kind (Parent_Node) is
            when K_Member =>
               Set_M_Type (Parent_Node, Array_Ref_Node);
            when K_Case =>
               Set_Case_Type (Parent_Node, Array_Ref_Node);
            when others =>
               pragma Assert (False);
               null;
         end case;

         Set_T_Type (Array_Type_Node, Element_Type_Node);
         Set_Declarators
           (Array_Type_Node,
            Append_Node (Nil_List, Array_Node));
         Set_Parent (Array_Node, Array_Type_Node);
         pragma Debug (O ("Expand_Array_Declarator : "
                          & "about to call add_identifier"));
         Success := Add_Identifier
           (Array_Node, Name (Node) & "_Array", Get_Current_Gen_Scope);
         pragma Assert (Success);
         pragma Debug (O ("Expand_Array_Declarator : "
                          & "add_identifier successfully called"));

         Insert_Before_Current (Array_Type_Node);
         Set_Array_Bounds (Array_Node, Array_Bounds (Node));
         Set_Array_Bounds (Node, Nil_List);
      end;
   end Expand_Array_Declarator;

   -----------------------
   -- Expand_Scope_Name --
   -----------------------

   procedure Expand_Scoped_Name (Node : Node_Id) is
      V              : constant Node_Id := Value (Node);
      V_Scope        : Node_Id;
      This_Gen_Scope : constant Node_Id := Get_Current_Gen_Scope;

      Forward_Declaration : Node_Id;

      function Create_Forward_Declaration (Node : Node_Id) return Node_Id;
      --  Create forward decalaration node for specified declaration

      --------------------------------
      -- Create_Forward_Declaration --
      --------------------------------

      function Create_Forward_Declaration (Node : Node_Id) return Node_Id is
         Result : Node_Id;
      begin
         pragma Assert (Kind (Node) = K_Interface);

         Result := Make_Forward_Interface (No_Location);
         Set_Abst (Result, Abst (Node));
         Set_Local (Result, Local (Node));
         Set_Forward (Result, Node);
         Set_Repository_Id (Result, Repository_Id (Node));

         Set_Forward (Node, Result);

         return Result;
      end Create_Forward_Declaration;

   begin

      --  Special processing of CORBA::TypeCode: we always refer to
      --  the full interface declaration

      if V = CORBA_TypeCode_Node then
         return;

      elsif Kind (V) = K_Forward_Interface
        and then Forward (V) = CORBA_TypeCode_Node
      then
         Set_Value (Node, Forward (V));
         return;

      elsif Kind (V) /= K_Interface and then Kind (V) /= K_ValueType then
         return;
      end if;

      --  Check whether the value of this scoped name is within a child
      --  scope of the current scope.

      V_Scope := Parent_Scope (V);
      loop
         exit when V_Scope = No_Node or else V_Scope = This_Gen_Scope;
         V_Scope := Parent_Scope (V_Scope);
      end loop;

      if (Kind (This_Gen_Scope) = K_Interface
          and then Has_Interface_Component (V, This_Gen_Scope)
          and then In_Sequence_Type)
         or else V_Scope = This_Gen_Scope
      then

         --  If the value of this scoped name is within a child scope of
         --  the current scope, a forward declaration is necessary. Also,
         --  if the value of this scoped name denotes the current interface,
         --  or has a component whose type is the current interface, a
         --  forward declaration is necessary if it is used as the item type
         --  for a sequence, because the instantiation of the sequences
         --  generic would otherwise cause freezing.

         Forward_Declaration := Forward (V);
         if Forward_Declaration = No_Node then

            --  If there is no explicit forward declaration, create one to
            --  avoid a circular dependency between Ada units, and insert
            --  it immediately before the complete interface declaration.

            Forward_Declaration := Create_Forward_Declaration (V);

            declare
               Enclosing_Scope : constant Node_Id := Parent_Scope (V);
               Enclosing_List  : Node_List := Contents (Enclosing_Scope);
            begin
               Insert_Before
                 (List   => Enclosing_List,
                  Node   => Forward_Declaration,
                  Before => V);
               Set_Contents (Enclosing_Scope, Enclosing_List);
            end;
         end if;

         --  Now we are assured that a forward declaration exists: fix up
         --  the scoped name to denote the forward instead of the complete
         --  declaration.

         Set_Value (Node, Forward_Declaration);

      end if;
   end Expand_Scoped_Name;

   -----------------------
   -- Private utilities --
   -----------------------

   --------------------
   -- Is_Ada_Keyword --
   --------------------

   function Is_Ada_Keyword (Name : String) return Boolean is
      Lower : constant String := Ada.Characters.Handling.To_Lower (Name);
   begin
      return    Lower = "abort"
        or else Lower = "abs"
        or else Lower = "abstract"
        or else Lower = "accept"
        or else Lower = "access"
        or else Lower = "aliased"
        or else Lower = "all"
        or else Lower = "and"
        or else Lower = "array"
        or else Lower = "at"
        or else Lower = "begin"
        or else Lower = "body"
        or else Lower = "case"
        or else Lower = "constant"
        or else Lower = "declare"
        or else Lower = "delay"
        or else Lower = "delta"
        or else Lower = "digits"
        or else Lower = "do"
        or else Lower = "else"
        or else Lower = "elsif"
        or else Lower = "end"
        or else Lower = "entry"
        or else Lower = "exception"
        or else Lower = "exit"
        or else Lower = "for"
        or else Lower = "function"
        or else Lower = "generic"
        or else Lower = "goto"
        or else Lower = "if"
        or else Lower = "in"
        or else Lower = "is"
        or else Lower = "limited"
        or else Lower = "loop"
        or else Lower = "mod"
        or else Lower = "new"
        or else Lower = "not"
        or else Lower = "null"
        or else Lower = "of"
        or else Lower = "or"
        or else Lower = "others"
        or else Lower = "out"
        or else Lower = "package"
        or else Lower = "pragma"
        or else Lower = "private"
        or else Lower = "procedure"
        or else Lower = "protected"
        or else Lower = "raise"
        or else Lower = "range"
        or else Lower = "record"
        or else Lower = "rem"
        or else Lower = "renames"
        or else Lower = "requeue"
        or else Lower = "return"
        or else Lower = "reverse"
        or else Lower = "select"
        or else Lower = "separate"
        or else Lower = "subtype"
        or else Lower = "tagged"
        or else Lower = "task"
        or else Lower = "terminate"
        or else Lower = "then"
        or else Lower = "type"
        or else Lower = "until"
        or else Lower = "use"
        or else Lower = "when"
        or else Lower = "while"
        or else Lower = "with"
        or else Lower = "xor";
   end Is_Ada_Keyword;

   ----------------------
   -- Expand_Node_List --
   ----------------------

   procedure Expand_Node_List
     (List : Node_List;
      Set_Current_Position : Boolean)
   is
      It : Node_Iterator;
      Node : Node_Id;
   begin
      Init (It, List);

      while not Is_End (It) loop
         Node := Get_Node (It);

         if Set_Current_Position then
            Current_Position_In_List := Node;
            pragma Debug
              (O ("Current_Position_In_List = " & Img (Integer (Node))));
         end if;
         Expand_Node (Node);

         --  Go to the next position only after Node has been expanded,
         --  as this expansion may have inserted new nodes in List.

         Next (It);
      end loop;

      if Set_Current_Position then
         Current_Position_In_List := No_Node;
      end if;
   end Expand_Node_List;

   ------------------------
   -- Sequence_Type_Name --
   -------------------------

   function Sequence_Type_Name (Node : Node_Id) return String is
      NK : constant Node_Kind := Kind (Node);
   begin
      case NK is
         when K_Sequence =>
            if Bound (Node) = No_Node then
               return "SEQUENCE_"
                 & Sequence_Type_Name (Sequence_Type (Node));
            else
               return "SEQUENCE_"
                 & Img (Integer_Value (Bound (Node)))
                 & "_" & Sequence_Type_Name (Sequence_Type (Node));
            end if;

         when K_Scoped_Name =>
            declare
               P : constant String := "CORBA_Repository_Root";
               N : String          := Ada_Full_Name (Value (Node));
            begin
               for I in N'Range loop
                  if N (I) = '.' then
                     N (I) := '_';
                  end if;
               end loop;

               if N'Length > P'Length
                 and then N (N'First .. N'First + P'Length - 1) = P
               then
                  return "CORBA" & N (N'First + P'Length .. N'Last);

               else
                  return N;
               end if;
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

         when K_Any =>
            return "any";

         when others =>
            --  Improper use: node N is not mapped to an Ada type.

            Error ("A " & Node_Kind'Image (NK)
                   & " cannot be used in a sequence.",
                   Fatal,
                   Get_Location (Node));

            --  Keep the compiler happy

            raise Program_Error;

      end case;
   end Sequence_Type_Name;

   ---------------------------
   -- Insert_Before_Current --
   ---------------------------

   procedure Insert_Before_Current
     (Node : Node_Id)
   is
      Current_Gen_Scope : constant Node_Id
        := Get_Current_Gen_Scope;
      Current_Scope_Contents : Node_List;
   begin
      pragma Assert (Is_Gen_Scope (Current_Gen_Scope));

      Current_Scope_Contents
        := Contents (Current_Gen_Scope);

      Insert_Before
        (Current_Scope_Contents,
         Node,
         Before => Current_Position_In_List);

      Set_Contents
        (Current_Gen_Scope, Current_Scope_Contents);

   end Insert_Before_Current;

   function Has_Out_Formals
     (Node : Node_Id)
     return Boolean
   is
      It : Node_Iterator;
      N : Node_Id;
   begin
      Init (It, Parameters (Node));
      while not Is_End (It) loop
         Get_Next_Node (It, N);

         if Mode (N) = Mode_Out
           or else Mode (N) = Mode_Inout then
            return True;
         end if;
      end loop;
      return False;
   end Has_Out_Formals;

   ------------------------
   -- Is_CORBA_IR_Entity --
   ------------------------

   type String_Access is access all String;

   --  CORBA 3.0 Interface Repository entities

   CORBA_IR_Names : constant array (Positive range <>) of String_Access
     := (new String'("CORBA.AbstractInterfaceDef"),        --  interface
         new String'("CORBA.AbstractInterfaceDefSeq"),     --  typedef/sequence
         new String'("CORBA.AliasDef"),                    --  interface
         new String'("CORBA.ArrayDef"),                    --  interface
         new String'("CORBA.AttrDescriptionSeq"),          --  typedef/sequence
         new String'("CORBA.AttributeDef"),                --  interface
         new String'("CORBA.AttributeDescription"),        --  struct
         new String'("CORBA.AttributeMode"),               --  enum
         new String'("CORBA.ComponentIR"),                 --  module
         new String'("CORBA.ConstantDef"),                 --  interface
         new String'("CORBA.ConstantDescription"),         --  struct
         new String'("CORBA.Contained"),                   --  interface
         new String'("CORBA.ContainedSeq"),                --  typedef/sequence
         new String'("CORBA.Container"),                   --  interface
         new String'("CORBA.ContextIdentifier"),           --  typedef
         new String'("CORBA.ContextIdSeq"),                --  typedef/sequence
         new String'("CORBA.DefinitionKind"),              --  enum
         new String'("CORBA.EnumDef"),                     --  interface
         new String'("CORBA.EnumMemberSeq"),               --  typedef/sequence
         new String'("CORBA.ExcDescriptionSeq"),           --  typedef/sequence
         new String'("CORBA.ExceptionDef"),                --  interface
         new String'("CORBA.ExceptionDefSeq"),             --  typedef/sequence
         new String'("CORBA.ExceptionDescription"),        --  struct
         new String'("CORBA.ExtAttrDescriptionSeq"),       --  typedef/sequence
         new String'("CORBA.ExtAttributeDef"),             --  interface
         new String'("CORBA.ExtAttributeDescription"),     --  struct
         new String'("CORBA.ExtAbstractInterfaceDef"),     --  interface
         new String'("CORBA.ExtAbstractInterfaceDefSeq"),  --  typedef/sequence
         new String'("CORBA.ExtInterfaceDef"),             --  interface
         new String'("CORBA.ExtInterfaceDefSeq"),          --  typedef/sequence
         new String'("CORBA.ExtInitializer"),              --  struct
         new String'("CORBA.ExtInitializerSeq"),           --  typedef/sequence
         new String'("CORBA.ExtLocalInterfaceDef"),        --  interface
         new String'("CORBA.ExtLocalInterfaceDefSeq"),     --  typedef/sequence
         new String'("CORBA.ExtValueDef"),                 --  interface
         new String'("CORBA.ExtValueDefSeq"),              --  typedef/sequence
         new String'("CORBA.FixedDef"),                    --  interface
         new String'("CORBA.IDLType"),                     --  interface
         new String'("CORBA.InterfaceAttrExtension"),      --  interface
         new String'("CORBA.InterfaceDef"),                --  interface
         new String'("CORBA.InterfaceDefSeq"),             --  typedef/sequence
         new String'("CORBA.InterfaceDescription"),        --  struct
         new String'("CORBA.Initializer"),                 --  struct
         new String'("CORBA.InitializerSeq"),              --  typedef/sequence
         new String'("CORBA.IRObject"),                    --  interface
         new String'("CORBA.LocalInterfaceDef"),           --  interface
         new String'("CORBA.LocalInterfaceDefSeq"),        --  typedef/sequence
         new String'("CORBA.ModuleDef"),                   --  interface
         new String'("CORBA.ModuleDescription"),           --  struct
         new String'("CORBA.NativeDef"),                   --  interface
         new String'("CORBA.OpDescriptionSeq"),            --  typedef/sequence
         new String'("CORBA.OperationDef"),                --  interface
         new String'("CORBA.OperationDescription"),        --  struct
         new String'("CORBA.OperationMode"),               --  enum
         new String'("CORBA.ParameterDescription"),        --  struct
         new String'("CORBA.ParameterMode"),               --  enum
         new String'("CORBA.ParDescriptionSeq"),           --  typedef/sequence
         new String'("CORBA.PrimitiveDef"),                --  interface
         new String'("CORBA.PrimitiveKind"),               --  enum
         new String'("CORBA.Repository"),                  --  interface
         --  new String'("CORBA.RepositoryId");            --  typedef
         new String'("CORBA.RepositoryIdSeq"),             --  typedef/sequence
         --  new String'("CORBA.ScopedName");              --  typedef
         new String'("CORBA.SequenceDef"),                 --  interface
         new String'("CORBA.StringDef"),                   --  interface
         new String'("CORBA.StructDef"),                   --  interface
         new String'("CORBA.StructMember"),                --  struct
         new String'("CORBA.StructMemberSeq"),             --  typedef/sequence
         new String'("CORBA.TypedefDef"),                  --  interface
         new String'("CORBA.TypeDescription"),             --  struct
         new String'("CORBA.UnionDef"),                    --  interface
         new String'("CORBA.UnionMember"),                 --  struct
         new String'("CORBA.UnionMemberSeq"),              --  typedef/sequence
         new String'("CORBA.ValueBoxDef"),                 --  interface
         new String'("CORBA.ValueDef"),                    --  interface
         new String'("CORBA.ValueDefSeq"),                 --  typedef/sequence
         new String'("CORBA.ValueDescription"),            --  struct
         new String'("CORBA.ValueMember"),                 --  struct
         new String'("CORBA.ValueMemberSeq"),              --  typedef/sequence
         new String'("CORBA.ValueMemberDef"),              --  interface
         new String'("CORBA.VersionSpec"),                 --  typedef
         --  new String'("CORBA.Visibility"),              --  typedef
         new String'("CORBA.WstringDef"));                 --  interface

   function Is_CORBA_IR_Entity (Node : Node_Id) return Boolean is
      NK : constant Node_Kind := Kind (Node);

      N  : Node_Id := Node;

   begin
      if NK /= K_Interface
        and then NK /= K_Forward_Interface
        and then NK /= K_Declarator
        and then NK /= K_Type_Declarator
        and then NK /= K_Enum
        and then NK /= K_Struct
      then
         return False;
      end if;

      if NK = K_Type_Declarator then
         declare
            List : constant Node_List := Declarators (Node);
            Iter : Node_Iterator;
         begin
            Init (Iter, List);
            Get_Next_Node (Iter, N);
         end;

      elsif Kind (Node) = K_Forward_Interface then
         N := Forward (Node);
      end if;

      declare
         Name : constant String := Ada_Full_Name (N);
      begin
         for J in CORBA_IR_Names'Range loop
            if CORBA_IR_Names (J).all = Name then
               return True;
            end if;
         end loop;
      end;

      return False;
   end Is_CORBA_IR_Entity;

   -------------------------
   -- Is_CORBA_PolicyList --
   -------------------------

   --  CORBA::PolicyList relocated to CORBA.Policy package

   CORBA_PolicyList_Names : constant array (Positive range <>) of String_Access
     := (1 => new String'("CORBA.PolicyList"));

   function Is_CORBA_PolicyList (Node : Node_Id) return Boolean is
      N : Node_Id;

   begin
      if Kind (Node) /= K_Type_Declarator then
         return False;
      end if;

      declare
         List : constant Node_List := Declarators (Node);
         Iter : Node_Iterator;

      begin
         Init (Iter, List);
         Get_Next_Node (Iter, N);
      end;

      declare
         Name : constant String := Ada_Full_Name (N);

      begin
         for J in CORBA_PolicyList_Names'Range loop
            if CORBA_PolicyList_Names (J).all = Name then
               return True;
            end if;
         end loop;
      end;

      return False;
   end Is_CORBA_PolicyList;

   -----------------------
   -- Is_CORBA_Sequence --
   -----------------------

   --  CORBA 3.0 sequences relocated to CORBA.IDL_SEQUENCES package

   CORBA_Sequences_Names : constant array (Positive range <>) of String_Access
     := (new String'("CORBA.AnySeq"),
         new String'("CORBA.BooleanSeq"),
         new String'("CORBA.CharSeq"),
         new String'("CORBA.WCharSeq"),
         new String'("CORBA.OctetSeq"),
         new String'("CORBA.ShortSeq"),
         new String'("CORBA.UShortSeq"),
         new String'("CORBA.LongSeq"),
         new String'("CORBA.ULongSeq"),
         new String'("CORBA.LongLongSeq"),
         new String'("CORBA.ULongLongSeq"),
         new String'("CORBA.FloatSeq"),
         new String'("CORBA.DoubleSeq"),
         new String'("CORBA.LongDoubleSeq"),
         new String'("CORBA.StringSeq"),
         new String'("CORBA.WStringSeq"));

   function Is_CORBA_Sequence (Node : Node_Id) return Boolean is
      N : Node_Id;

   begin
      if Kind (Node) /= K_Type_Declarator then
         return False;
      end if;

      declare
         List : constant Node_List := Declarators (Node);
         Iter : Node_Iterator;

      begin
         Init (Iter, List);
         Get_Next_Node (Iter, N);
      end;

      declare
         Name : constant String := Ada_Full_Name (N);

      begin
         for J in CORBA_Sequences_Names'Range loop
            if CORBA_Sequences_Names (J).all = Name then
               return True;
            end if;
         end loop;
      end;

      return False;
   end Is_CORBA_Sequence;

end Ada_Be.Expansion;

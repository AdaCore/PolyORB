with Idl_Fe.Tree.Accessors; use Idl_Fe.Tree.Accessors;
with Idl_Fe.Tree.Constructors;
with Idl_Fe.Types; use Idl_Fe.Types;
with Idl_Fe.Tree; use Idl_Fe.Tree;
with Ada_Be.Expansion.Types; use Ada_Be.Expansion.Types;
with Idl_Fe.Errors;

with GNAT.HTable;

package body Ada_Be.Expansion is

   ------------------------
   --  Expand_Repository --
   ------------------------

   function Expand_Repository (Node : in N_Root_Acc)
                               return N_Root_Acc is
      --  Named_Nodes_In_Scope : Node_List;
      Result : N_Repository_Acc;
      Iterator : Node_Iterator;
      type Header_Num is range 0 .. 1024;
      function Hash is new GNAT.HTable.Hash (Header_Num);
      function Hash (A : String_Ptr) return Header_Num;
      function Hash (A : String_Ptr) return Header_Num is
      begin
         return Hash (A.all);
      end Hash;
      function Equals (A, B : String_Ptr) return Boolean;
      function Equals (A, B : String_Ptr) return Boolean is
      begin
         return A.all = B.all;
      end Equals;
      package Idlnodes is new GNAT.HTable.Simple_HTable
        (Header_Num,
         Ben_Idl_File_Acc,
         null,
         String_Ptr,
         Hash,
         Equals);


   begin
      pragma Assert (Get_Kind (Node.all) = K_Repository);

      --  create the new node
      Result := Idl_Fe.Tree.Constructors.New_N_Repository (Node);

      --  loop over the nodes of the old list
      Init (Iterator, Contents (Node));
      while not Is_End (Iterator) loop
         declare
            Current : N_Root_Acc := Get_Node (Iterator);
            Loc : Idl_Fe.Errors.Location
              := Get_Location (Current.all);
            Filename : String_Ptr := String_Ptr (Loc.Filename);
            Idl_File_Node : Ben_Idl_File_Acc;
         begin
            Idl_File_Node := Idlnodes.Get (Filename);
            if (N_Root_Acc (Idl_File_Node) = Nil_Node) then
               Idl_File_Node := New_Ben_Idl_File (Filename);
               Idlnodes.Set (Filename, Idl_File_Node);
            end if;

            --  add a new node to the N_Idl_File
            Append_Node (Idl_File_Node.all.Elements,
                         Current);

            --  next node
            Next (Iterator);
         end;
      end loop;
      --  create N_Idl_File nodes to put together declarations
      --  that were in the same file

      --  rename these N_Idl_File nodes

      --  expand these N_Idl_File nodes
      return Node;
   end Expand_Repository;


   -----------------
   --  Expand_Node --
   ------------------

   function Expand_Node (Node : in N_Root_Acc;
                         Named_Nodes_In_Scope : in Node_List)
                         return N_Root_Acc is
   begin
      case (Get_Kind (Node.all)) is
         when K_Repository =>
            return Expand_Repository (Node);
         when K_Unknown =>
            declare
               Br : Ben_Root'Class := Ben_Root'Class (Node.all);
            begin
               case (Get_Be_Kind (Br)) is
                  when others =>
                     return Node;
               end case;
            end;
         when others =>
            return Node;
      end case;
   end Expand_Node;


end Ada_Be.Expansion;

with Idl_Fe.Tree.Accessors; use Idl_Fe.Tree.Accessors;
with Idl_Fe.Tree.Constructors;
with Idl_Fe.Types; use Idl_Fe.Types;
with Idl_Fe.Tree; use Idl_Fe.Tree;
with Ada_Be.Expansion.Types; use Ada_Be.Expansion.Types;
with Idl_Fe.Errors;
with Ada_Be.Debug;

with GNAT.HTable;

package body Ada_Be.Expansion is

   --------------
   --   Debug  --
   --------------

   Flag : constant Natural := Ada_Be.Debug.Is_Active ("ada_be.expansion");
   procedure O is new Ada_Be.Debug.Output (Flag);


   ------------------------
   --  Expand_Repository --
   ------------------------

   procedure Expand_Repository (Node : in out N_Repository_Acc) is
   begin
      Expand_Repository (N_Root_Acc (Node));
   end Expand_Repository;

   ------------------------
   --  Expand_Repository --
   ------------------------

   procedure  Expand_Repository (Node : in out N_Root_Acc) is
      Named_Nodes_In_Scope : Node_List;
      Result : N_Repository_Acc;
      Iterator : Node_Iterator;
      Temp_Node_List : Node_List;
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
            pragma Debug (O ("node "
                             & " in file "
                             & Filename.all));
            Idl_File_Node := Idlnodes.Get (Filename);
            if (N_Root_Acc (Idl_File_Node) = Nil_Node) then
               Idl_File_Node := New_Ben_Idl_File (Filename);
               Idlnodes.Set (Filename, Idl_File_Node);
               Append_Node (Temp_Node_List, N_Root_Acc (Idl_File_Node));
            end if;

            --  add a new node to the N_Idl_File
            Append_Node (Idl_File_Node.all.Elements,
                         Current);

            --  next node
            Next (Iterator);
         end;
      end loop;

      --  create the new node
      Result := Idl_Fe.Tree.Constructors.New_N_Repository (Node);

      --  expand the newly created Ben_Idl_File nodes
      Init (Iterator, Temp_Node_List);
      while not Is_End (Iterator) loop
         declare
            Temp_Node : N_Root_Acc;
         begin
            Temp_Node := Get_Node (Iterator);
            Expand_Node (Temp_Node, Named_Nodes_In_Scope);
            Append_Node (Result.Contents, Temp_Node);
            Next (Iterator);
         end;
      end loop;

      --  return the newly created repository
      Node := N_Root_Acc (Result);
   end Expand_Repository;


   -----------------
   --  Expand_Node --
   ------------------

   procedure Expand_Node (Node : in out N_Root_Acc;
                          Named_Nodes_In_Scope : in Node_List) is
   begin
      case (Get_Kind (Node.all)) is
         when K_Repository =>
            Expand_Repository (Node);
         when K_Unknown =>
            declare
               Br : Ben_Root'Class := Ben_Root'Class (Node.all);
            begin
               case (Get_Be_Kind (Br)) is
                  when others =>
                     null;
               end case;
            end;
         when others =>
            null;
      end case;
   end Expand_Node;


end Ada_Be.Expansion;

with Idl_Fe.Types; use Idl_Fe.Types;
with Idl_Fe.Tree; use Idl_Fe.Tree;
with Idl_Fe.Errors; use Idl_Fe.Errors;
with Ada_Be.Debug;

--  with Idl_Fe.Display_Tree;

with GNAT.HTable;

package body Ada_Be.Expansion is

   --------------
   --   Debug  --
   --------------

   Flag : constant Natural := Ada_Be.Debug.Is_Active ("ada_be.expansion");
   procedure O is new Ada_Be.Debug.Output (Flag);


   -----------------
   --  Expand_Node --
   ------------------

   procedure Expand_Node (Node : in Node_Id) is
   begin
      case (Kind (Node)) is
         when K_Repository =>
            Expand_Repository (Node);
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
                  --  conflicts in filenames not implemented yet
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

      Expand_Node_List (Contents (Node));
   end Expand_Repository;


   --------------------
   --  Expand_Module --
   --------------------
   procedure Expand_Module (Node : in Node_Id) is
   begin
      Expand_Node_List (Contents (Node));
   end Expand_Module;


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

   -----------------------------
   --  Append_Node_To_Content --
   -----------------------------
   procedure Append_Node_To_Contents (Parent : Node_Id;
                                      Child : Node_Id) is
      Temp : Node_List := Contents (Parent);
   begin
      Append_Node (Temp, Child);
      Set_Contents (Parent, Temp);
   end Append_Node_To_Contents;

end Ada_Be.Expansion;

--  Some code in this package is taken from GNAT's Atree.
--  Copyright (C) 1992-1998 Free Software Fundation, Inc.

----------------------------------------
--                                    --
--       ----  ---     --  ----       --
--       -      -     - -  -  -       --
--       -      -    ----  -  -       --
--       ----  ---  -   -  ----       --
--                                    --
----------------------------------------
--  CORBA                             --
--  Interface for                     --
--  Ada'95 distributed systems annex  --
--  Objects                           --
----------------------------------------
--  Copyright (c) 1999                --
--  École nationale supérieure des    --
--  télécommunications                --
----------------------------------------

--  IDL syntax tree management.
--  $Id: //depot/ciao/main/ciao-idl_tree.adb#8 $

with CIAO.Nlists; use CIAO.Nlists;

package body CIAO.IDL_Tree is

   use IDL_Tree_Private_Part;
   --  We are allowed to see our own private data structures.

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
      Dummy : Node_Id;

   begin
      --  Allocate Empty and Error nodes

      Dummy := New_Node (N_Empty);
      -- XXX Set_Name1 (Empty, No_Name);
      Dummy := New_Node (N_Error);
      -- XXX Set_Name1 (Error, Error_Name);

   end Initialize;

   ---------------
   -- Allocator --
   ---------------

   Default_Node : Node := (
      In_List => False,
      Link    => Empty_List_Or_Node,

      Kind    => N_Empty,
      Origin  => Asis.Nil_Element,

      Field1  => Empty_List_Or_Node,
      Field2  => Empty_List_Or_Node,
      Field3  => Empty_List_Or_Node,
      Field4  => Empty_List_Or_Node,

      Flag1   => False);

   function New_Node (New_Node_Kind : Nkind)
     return    Node_Id
   is

-- XXX |      procedure New_Node_Debugging_Output;
-- XXX |      --  Debugging routine for debug flag N
-- XXX |
-- XXX |      procedure New_Node_Debugging_Output is
-- XXX |      begin
-- XXX |         if Debug_Flag_N then
-- XXX |            Write_Str ("Allocate node, Id = ");
-- XXX |            Write_Int (Int (Nodes.Last));
-- XXX |            Write_Str ("  ");
-- XXX |            Write_Location (New_Sloc);
-- XXX |            Write_Str ("  ");
-- XXX |            Write_Str (Node_Kind'Image (New_Node_Kind));
-- XXX |            Write_Eol;
-- XXX |         end if;
-- XXX |      end New_Node_Debugging_Output;

-- XXX |      pragma Inline (New_Node_Debugging_Output);

   --  Start of processing for New_Node

   begin
      -- XXX pragma Assert (New_Node_Kind not in N_Entity);
      Nodes.Increment_Last;
      Nodes.Table (Nodes.Last)      := Default_Node;
      Nodes.Table (Nodes.Last).Kind := New_Node_Kind;

      -- XXX pragma Debug (New_Node_Debugging_Output);
      -- XXX Fatal_Error_Node := Nodes.Last;
      -- XXX Node_Count := Node_Count + 1;

      -- XXX Orig_Nodes.Increment_Last;
      Allocate_List_Tables (Nodes.Last);
      -- XXX Orig_Nodes.Table (Nodes.Last) := Nodes.Last;
      return Nodes.Last;
   end New_Node;

   ------------------------------
   -- Typed accessor functions --
   ------------------------------

   function Node_Kind (N : Node_Id) return Nkind is
   begin
      pragma Assert (N in Nodes.First .. Nodes.Last);
      return Nodes.Table (N).Kind;
   end Node_Kind;

   function Origin (N : Node_Id) return Asis.Element is
   begin
      pragma Assert (N in Nodes.First .. Nodes.Last);
      return Nodes.Table (N).Origin;
   end Origin;

   function Parent (N : Node_Id) return Node_Id is
   begin
      pragma Assert (N in Nodes.First .. Nodes.Last);
      if Is_List_Member (N) then
         return Parent (List_Containing (N));
      else
         return Node_Id (Nodes.Table (N).Link);
      end if;
   end Parent;

   function No (N : Node_Id) return Boolean is
   begin
      return N = Empty;
   end No;

   function Present (N : Node_Id) return Boolean is
   begin
      return N /= Empty;
   end Present;

   function Node1 (N : Node_Id) return Node_Id is
   begin
      pragma Assert (N in Nodes.First .. Nodes.Last);
      return Node_Id (Nodes.Table (N).Field1);
   end Node1;

   function Node2 (N : Node_Id) return Node_Id is
   begin
      pragma Assert (N in Nodes.First .. Nodes.Last);
      return Node_Id (Nodes.Table (N).Field2);
   end Node2;

   function Node3 (N : Node_Id) return Node_Id is
   begin
      pragma Assert (N in Nodes.First .. Nodes.Last);
      return Node_Id (Nodes.Table (N).Field3);
   end Node3;

   function Node4 (N : Node_Id) return Node_Id is
   begin
      pragma Assert (N in Nodes.First .. Nodes.Last);
      return Node_Id (Nodes.Table (N).Field4);
   end Node4;

   function List1 (N : Node_Id) return List_Id is
   begin
      pragma Assert (N in Nodes.First .. Nodes.Last);
      return List_Id (Nodes.Table (N).Field1);
   end List1;

   function List2 (N : Node_Id) return List_Id is
   begin
      pragma Assert (N in Nodes.First .. Nodes.Last);
      return List_Id (Nodes.Table (N).Field2);
   end List2;

   function List3 (N : Node_Id) return List_Id is
   begin
      pragma Assert (N in Nodes.First .. Nodes.Last);
      return List_Id (Nodes.Table (N).Field3);
   end List3;

   function List4 (N : Node_Id) return List_Id is
   begin
      pragma Assert (N in Nodes.First .. Nodes.Last);
      return List_Id (Nodes.Table (N).Field4);
   end List4;

   function Name1 (N : Node_Id) return Name_Id is
   begin
      pragma Assert (N in Nodes.First .. Nodes.Last);
      return Name_Id (Nodes.Table (N).Field1);
   end Name1;

   function Uint1 (N : Node_Id) return Uint is
   begin
      pragma Assert (N in Nodes.First .. Nodes.Last);
      return Uint (Nodes.Table (N).Field1);
   end Uint1;

   function Flag1 (N : Node_Id) return Boolean is
   begin
      pragma Assert (N in Nodes.First .. Nodes.Last);
      return Nodes.Table (N).Flag1;
   end Flag1;

   procedure Set_Origin (N : Node_Id; Val : Asis.Element) is
   begin
      pragma Assert (N in Nodes.First .. Nodes.Last);
      Nodes.Table (N).Origin := Val;
   end Set_Origin;

   procedure Set_Parent (N : Node_Id; Val : Node_Id) is
   begin
      pragma Assert (N in Nodes.First .. Nodes.Last);
      Nodes.Table (N).Link := Union_Id (Val);
   end Set_Parent;

   procedure Set_Node1 (N : Node_Id; Val : Node_Id) is
   begin
      pragma Assert (N in Nodes.First .. Nodes.Last);
      Nodes.Table (N).Field1 := Union_Id (Val);
   end Set_Node1;

   procedure Set_Node2 (N : Node_Id; Val : Node_Id) is
   begin
      pragma Assert (N in Nodes.First .. Nodes.Last);
      Nodes.Table (N).Field2 := Union_Id (Val);
   end Set_Node2;

   procedure Set_Node3 (N : Node_Id; Val : Node_Id) is
   begin
      pragma Assert (N in Nodes.First .. Nodes.Last);
      Nodes.Table (N).Field3 := Union_Id (Val);
   end Set_Node3;

   procedure Set_Node4 (N : Node_Id; Val : Node_Id) is
   begin
      pragma Assert (N in Nodes.First .. Nodes.Last);
      Nodes.Table (N).Field4 := Union_Id (Val);
   end Set_Node4;

   procedure Set_List1 (N : Node_Id; Val : List_Id) is
   begin
      pragma Assert (N in Nodes.First .. Nodes.Last);
      Nodes.Table (N).Field1 := Union_Id (Val);
   end Set_List1;

   procedure Set_List2 (N : Node_Id; Val : List_Id) is
   begin
      pragma Assert (N in Nodes.First .. Nodes.Last);
      Nodes.Table (N).Field2 := Union_Id (Val);
   end Set_List2;

   procedure Set_List3 (N : Node_Id; Val : List_Id) is
   begin
      pragma Assert (N in Nodes.First .. Nodes.Last);
      Nodes.Table (N).Field3 := Union_Id (Val);
   end Set_List3;

   procedure Set_List4 (N : Node_Id; Val : List_Id) is
   begin
      pragma Assert (N in Nodes.First .. Nodes.Last);
      Nodes.Table (N).Field4 := Union_Id (Val);
   end Set_List4;

   procedure Set_Name1 (N : Node_Id; Val : Name_Id) is
   begin
      pragma Assert (N in Nodes.First .. Nodes.Last);
      Nodes.Table (N).Field1 := Union_Id (Val);
   end Set_Name1;

   procedure Set_Uint1 (N : Node_Id; Val : Uint) is
   begin
      pragma Assert (N in Nodes.First .. Nodes.Last);
      Nodes.Table (N).Field1 := Union_Id (Val);
   end Set_Uint1;

   procedure Set_Flag1 (N : Node_Id; Val : Boolean) is
   begin
      pragma Assert (N in Nodes.First .. Nodes.Last);
      Nodes.Table (N).Flag1 := Val;
   end Set_Flag1;

   generic
      with function List (N : Node_Id) return List_Id is <>;
      with procedure Set_List (N : Node_Id ; Val : List_Id) is <>;
   procedure Add_List (N : Node_Id; Val : Node_Id);

   procedure Add_List (N : Node_Id; Val : Node_Id) is
   begin
      if No (List (N)) then
         declare
            L : List_Id := New_List;
         begin
            Set_Parent (L, N);
            Set_List (N, L);
         end;
      end if;
      Append_To (List (N), Val);
   end Add_List;

   procedure Add_List1_I is new Add_List (List1, Set_List1);
   procedure Add_List1 (N : Node_Id; Val : Node_Id) renames Add_List1_I;
   procedure Add_List2_I is new Add_List (List2, Set_List2);
   procedure Add_List2 (N : Node_Id; Val : Node_Id) renames Add_List2_I;
   procedure Add_List3_I is new Add_List (List3, Set_List3);
   procedure Add_List3 (N : Node_Id; Val : Node_Id) renames Add_List3_I;
   procedure Add_List4_I is new Add_List (List4, Set_List4);
   procedure Add_List4 (N : Node_Id; Val : Node_Id) renames Add_List4_I;

end CIAO.IDL_Tree;

--  The package Nlists is a part of the GNAT compiler.
--  Copyright (C) 1992-1998 Free Software Foundation, Inc.

--  This modified version is a part of the CIAO project.
--  Copyright (C) 1999 École nationale supérieure des télécommunications.

------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                               N L I S T S                                --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.2 $                             --
--                                                                          --
--          Copyright (C) 1992-1998 Free Software Foundation, Inc.          --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT;  see file COPYING.  If not, write --
-- to  the Free Software Foundation,  59 Temple Place - Suite 330,  Boston, --
-- MA 02111-1307, USA.                                                      --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- It is now maintained by Ada Core Technologies Inc (http://www.gnat.com). --
--                                                                          --
------------------------------------------------------------------------------

--  WARNING: There is a C version of this package. Any changes to this source
--  file must be properly reflected in the corresponding C header a-nlists.h

with CIAO.Alloc;
-- XXX | with Debug;  use Debug;
-- XXX | with Output; use Output;
--  with Atree;  use Atree;
--  with Sinfo;  use Sinfo;
with CIAO.IDL_Tree;   use CIAO.IDL_Tree;
with CIAO.IDL_Syntax;
with GNAT.Table;

package body CIAO.Nlists is

   use IDL_Tree_Private_Part;
   --  Get access to Nodes table

   ----------------------------------
   -- Implementation of Node Lists --
   ----------------------------------

   --  A node list is represented by a list header which contains
   --  three fields:

   type List_Header is record
      First : Node_Id;
      --  Pointer to first node in list. Empty if list is empty

      Last  : Node_Id;
      --  Pointer to last node in list. Empty if list is empty

      Parent : Node_Id;
      --  Pointer to parent of list. Empty if list has no parent
   end record;

   --  The node lists are stored in a table indexed by List_Id values

   package Lists is new GNAT.Table (
     Table_Component_Type => List_Header,
     Table_Index_Type     => List_Id,
     Table_Low_Bound      => First_List_Id,
     Table_Initial        => CIAO.Alloc.Lists_Initial,
     Table_Increment      => CIAO.Alloc.Lists_Increment);

   --  The nodes in the list all have the In_List flag set, and their Link
   --  fields (which otherwise point to the parent) contain the List_Id of
   --  the list header giving immediate access to the list containing the
   --  node, and its parent and first and last elements.

   --  Two auxiliary tables, indexed by Node_Id values and built in parallel
   --  with the main nodes table and always having the same size contain the
   --  list link values that allow locating the previous and next node in a
   --  list. The entries in these tables are valid only if the In_List flag
   --  is set in the corresponding node. Next_Node is Empty at the end of a
   --  list and Prev_Node is Empty at the start of a list.

   package Next_Node is new GNAT.Table (
      Table_Component_Type => Node_Id,
      Table_Index_Type     => Node_Id,
      Table_Low_Bound      => First_Node_Id,
      Table_Initial        => CIAO.Alloc.Orig_Nodes_Initial,
      Table_Increment      => CIAO.Alloc.Orig_Nodes_Increment);

   package Prev_Node is new GNAT.Table (
      Table_Component_Type => Node_Id,
      Table_Index_Type     => Node_Id,
      Table_Low_Bound      => First_Node_Id,
      Table_Initial        => CIAO.Alloc.Orig_Nodes_Initial,
      Table_Increment      => CIAO.Alloc.Orig_Nodes_Increment);

   -----------------------
   -- Local Subprograms --
   -----------------------

   procedure Set_First (List : List_Id; To : Node_Id);
   pragma Inline (Set_First);
   --  Sets First field of list header List to reference To

   procedure Set_Last (List : List_Id; To : Node_Id);
   pragma Inline (Set_Last);
   --  Sets Last field of list header List to reference To

   procedure Set_List_Link (Node : Node_Id; To : List_Id);
   pragma Inline (Set_List_Link);
   --  Sets list link of Node to list header To

   procedure Set_Next (Node : Node_Id; To : Node_Id);
   pragma Inline (Set_Next);
   --  Sets the Next_Node pointer for Node to reference To

   procedure Set_Prev (Node : Node_Id; To : Node_Id);
   pragma Inline (Set_Prev);
   --  Sets the Prev_Node pointer for Node to reference To

   -----------
   -- First --
   -----------

   --  This subprogram is deliberately placed early on, out of alphabetical
   --  order, so that it can be properly inlined from within this unit.

   function First (List : List_Id) return Node_Id is
   begin
      if List = No_List then
         return Empty;
      else
         pragma Assert (List in First_List_Id .. Lists.Last);
         return Lists.Table (List).First;
      end if;
   end First;

   ----------
   -- Last --
   ----------

   --  This subprogram is deliberately placed early on, out of alphabetical
   --  order, so that it can be properly inlined from within this unit.

   function Last (List : List_Id) return Node_Id is
   begin
      pragma Assert (List in First_List_Id .. Lists.Last);
      return Lists.Table (List).Last;
   end Last;

   ----------
   -- Next --
   ----------

   --  This subprogram is deliberately placed early on, out of alphabetical
   --  order, so that it can be properly inlined from within this unit.

   function Next (Node : Node_Id) return Node_Id is
   begin
      pragma Assert (Is_List_Member (Node));
      return Next_Node.Table (Node);
   end Next;

   procedure Next (Node : in out Node_Id) is
   begin
      Node := Next (Node);
   end Next;

   --------
   -- No --
   --------

   --  This subprogram is deliberately placed early on, out of alphabetical
   --  order, so that it can be properly inlined from within this unit.

   function No (List : List_Id) return Boolean is
   begin
      return List = No_List;
   end No;

   -------
   -- p --
   -------

   function p (U : Union_Id) return Node_Id is
   begin
      if U in Node_Range then
         return Parent (Node_Id (U));

      elsif U in List_Range then
         return Parent (List_Id (U));

      else
         return 99_999_999;
      end if;
   end p;

   ----------
   -- Prev --
   ----------

   --  This subprogram is deliberately placed early on, out of alphabetical
   --  order, so that it can be properly inlined from within this unit.

   function Prev (Node : Node_Id) return Node_Id is
   begin
      pragma Assert (Is_List_Member (Node));
      return Prev_Node.Table (Node);
   end Prev;

   procedure Prev (Node : in out Node_Id) is
   begin
      Node := Prev (Node);
   end Prev;

   ---------------
   -- Set_First --
   ---------------

   --  This subprogram is deliberately placed early on, out of alphabetical
   --  order, so that it can be properly inlined from within this unit.

   procedure Set_First (List : List_Id; To : Node_Id) is
   begin
      Lists.Table (List).First := To;
   end Set_First;

   --------------
   -- Set_Last --
   --------------

   --  This subprogram is deliberately placed early on, out of alphabetical
   --  order, so that it can be properly inlined from within this unit.

   procedure Set_Last (List : List_Id; To : Node_Id) is
   begin
      Lists.Table (List).Last := To;
   end Set_Last;

   -------------------
   -- Set_List_Link --
   -------------------

   --  This subprogram is deliberately placed early on, out of alphabetical
   --  order, so that it can be properly inlined from within this unit.

   procedure Set_List_Link (Node : Node_Id; To : List_Id) is
   begin
      Nodes.Table (Node).Link := Union_Id (To);
   end Set_List_Link;

   --------------
   -- Set_Next --
   --------------

   --  This subprogram is deliberately placed early on, out of alphabetical
   --  order, so that it can be properly inlined from within this unit.

   procedure Set_Next (Node : Node_Id; To : Node_Id) is
   begin
      Next_Node.Table (Node) := To;
   end Set_Next;

   --------------
   -- Set_Prev --
   --------------

   --  This subprogram is deliberately placed early on, out of alphabetical
   --  order, so that it can be properly inlined from within this unit.

   procedure Set_Prev (Node : Node_Id; To : Node_Id) is
   begin
      Prev_Node.Table (Node) := To;
   end Set_Prev;

   --------------------------
   -- Allocate_List_Tables --
   --------------------------

   procedure Allocate_List_Tables (N : Node_Id) is
   begin
      Next_Node.Set_Last (N);
      Prev_Node.Set_Last (N);
   end Allocate_List_Tables;

   ------------
   -- Append --
   ------------

   procedure Append (Node : Node_Id; To : List_Id) is
      L : constant Node_Id := Last (To);

-- XXX |      procedure Append_Debug;
-- XXX |      pragma Inline (Append_Debug);
-- XXX |      --  Output debug information if Debug_Flag_N set
-- XXX |
-- XXX |      procedure Append_Debug is
-- XXX |      begin
-- XXX |         if Debug_Flag_N then
-- XXX |            Write_Str ("Append node ");
-- XXX |            Write_Int (Int (Node));
-- XXX |            Write_Str (" to list ");
-- XXX |            Write_Int (Int (To));
-- XXX |            Write_Eol;
-- XXX |         end if;
-- XXX |      end Append_Debug;
-- XXX |
-- XXX |   --  Start of processing for Append

   begin
      pragma Assert (not Is_List_Member (Node));

      if Node = Error then
         return;
      end if;

-- XXX |      pragma Debug (Append_Debug);

      if No (L) then
         Set_First (To, Node);
      else
         Set_Next (L, Node);
      end if;

      Set_Last (To, Node);

      Nodes.Table (Node).In_List := True;

      Set_Next      (Node, Empty);
      Set_Prev      (Node, L);
      Set_List_Link (Node, To);
   end Append;

   ---------------
   -- Append_To --
   ---------------

   procedure Append_To (To : List_Id; Node : Node_Id) is
   begin
      Append (Node, To);
   end Append_To;

   -----------------
   -- Append_List --
   -----------------

   procedure Append_List (List : List_Id; To : List_Id) is

-- XXX |      procedure Append_List_Debug;
-- XXX |      pragma Inline (Append_List_Debug);
-- XXX |      --  Output debug information if Debug_Flag_N set
-- XXX |
-- XXX |      procedure Append_List_Debug is
-- XXX |      begin
-- XXX |         if Debug_Flag_N then
-- XXX |            Write_Str ("Append list ");
-- XXX |            Write_Int (Int (List));
-- XXX |            Write_Str (" to list ");
-- XXX |            Write_Int (Int (To));
-- XXX |            Write_Eol;
-- XXX |         end if;
-- XXX |      end Append_List_Debug;

   --  Start of processing for Append_List

   begin
      if Is_Empty_List (List) then
         return;

      else
         declare
            L : constant Node_Id := Last (To);
            F : constant Node_Id := First (List);
            N : Node_Id;

         begin
-- XXX |            pragma Debug (Append_List_Debug);

            N := F;
            loop
               Set_List_Link (N, To);
               N := Next (N);
               exit when No (N);
            end loop;

            if No (L) then
               Set_First (To, F);
            else
               Set_Next (L, F);
            end if;

            Set_Prev (F, L);
            Set_Last (To, Last (List));

            Set_First (List, Empty);
            Set_Last  (List, Empty);
         end;
      end if;
   end Append_List;

   --------------------
   -- Append_List_To --
   --------------------

   procedure Append_List_To (To : List_Id; List : List_Id) is
   begin
      Append_List (List, To);
   end Append_List_To;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
      E : constant List_Id := Error_List;

   begin
      Lists.Init;
      Next_Node.Init;
      Prev_Node.Init;

      --  Allocate Error_List list header

      Lists.Increment_Last;
      Set_Parent (E, Empty);
      Set_First  (E, Empty);
      Set_Last   (E, Empty);
   end Initialize;

   ------------------
   -- Insert_After --
   ------------------

   procedure Insert_After (After : Node_Id; Node : Node_Id) is

-- XXX |      procedure Insert_After_Debug;
-- XXX |      pragma Inline (Insert_After_Debug);
-- XXX |      --  Output debug information if Debug_Flag_N set
-- XXX |
-- XXX |      procedure Insert_After_Debug is
-- XXX |      begin
-- XXX |         if Debug_Flag_N then
-- XXX |            Write_Str ("Insert node");
-- XXX |            Write_Int (Int (Node));
-- XXX |            Write_Str (" after node ");
-- XXX |            Write_Int (Int (After));
-- XXX |            Write_Eol;
-- XXX |         end if;
-- XXX |      end Insert_After_Debug;

   --  Start of processing for Insert_After

   begin
      pragma Assert
        (Is_List_Member (After) and then not Is_List_Member (Node));

      if Node = Error then
         return;
      end if;

      declare
         Before : constant Node_Id := Next (After);
         LC     : constant List_Id := List_Containing (After);

      begin
         if Present (Before) then
            Set_Prev (Before, Node);
         else
            Set_Last (LC, Node);
         end if;

         Set_Next (After, Node);

         Nodes.Table (Node).In_List := True;

         Set_Prev      (Node, After);
         Set_Next      (Node, Before);
         Set_List_Link (Node, LC);
      end;
   end Insert_After;

   -------------------
   -- Insert_Before --
   -------------------

   procedure Insert_Before (Before : Node_Id; Node : Node_Id) is

-- XXX |      procedure Insert_Before_Debug;
-- XXX |      pragma Inline (Insert_Before_Debug);
-- XXX |      --  Output debug information if Debug_Flag_N set
-- XXX |
-- XXX |      procedure Insert_Before_Debug is
-- XXX |      begin
-- XXX |         if Debug_Flag_N then
-- XXX |            Write_Str ("Insert node");
-- XXX |            Write_Int (Int (Node));
-- XXX |            Write_Str (" before node ");
-- XXX |            Write_Int (Int (Before));
-- XXX |            Write_Eol;
-- XXX |         end if;
-- XXX |      end Insert_Before_Debug;

   --  Start of processing for Insert_Before

   begin
      pragma Assert
        (Is_List_Member (Before) and then not Is_List_Member (Node));

      if Node = Error then
         return;
      end if;

-- XXX |      pragma Debug (Insert_Before_Debug);

      declare
         After : constant Node_Id := Prev (Before);
         LC    : constant List_Id := List_Containing (Before);

      begin
         if Present (After) then
            Set_Next (After, Node);
         else
            Set_First (LC, Node);
         end if;

         Set_Prev (Before, Node);

         Nodes.Table (Node).In_List := True;

         Set_Prev      (Node, After);
         Set_Next      (Node, Before);
         Set_List_Link (Node, LC);
      end;
   end Insert_Before;

   -----------------------
   -- Insert_List_After --
   -----------------------

   procedure Insert_List_After (After : Node_Id; List : List_Id) is

-- XXX |      procedure Insert_List_After_Debug;
-- XXX |      pragma Inline (Insert_List_After_Debug);
-- XXX |      --  Output debug information if Debug_Flag_N set
-- XXX |
-- XXX |      procedure Insert_List_After_Debug is
-- XXX |      begin
-- XXX |         if Debug_Flag_N then
-- XXX |            Write_Str ("Insert list ");
-- XXX |            Write_Int (Int (List));
-- XXX |            Write_Str (" after node ");
-- XXX |            Write_Int (Int (After));
-- XXX |            Write_Eol;
-- XXX |         end if;
-- XXX |      end Insert_List_After_Debug;

   --  Start of processing for Insert_List_After

   begin
      pragma Assert (Is_List_Member (After));

      if Is_Empty_List (List) then
         return;

      else
         declare
            Before : constant Node_Id := Next (After);
            LC     : constant List_Id := List_Containing (After);
            F      : constant Node_Id := First (List);
            L      : constant Node_Id := Last (List);
            N      : Node_Id;

         begin
-- XXX |            pragma Debug (Insert_List_After_Debug);

            N := F;
            loop
               Set_List_Link (N, LC);
               exit when N = L;
               N := Next (N);
            end loop;

            if Present (Before) then
               Set_Prev (Before, L);
            else
               Set_Last (LC, L);
            end if;

            Set_Next (After, F);
            Set_Prev (F, After);
            Set_Next (L, Before);

            Set_First (List, Empty);
            Set_Last  (List, Empty);
         end;
      end if;
   end Insert_List_After;

   ------------------------
   -- Insert_List_Before --
   ------------------------

   procedure Insert_List_Before (Before : Node_Id; List : List_Id) is

-- XXX |      procedure Insert_List_Before_Debug;
-- XXX |      pragma Inline (Insert_List_Before_Debug);
-- XXX |      --  Output debug information if Debug_Flag_N set
-- XXX |
-- XXX |      procedure Insert_List_Before_Debug is
-- XXX |      begin
-- XXX |         if Debug_Flag_N then
-- XXX |            Write_Str ("Insert list ");
-- XXX |            Write_Int (Int (List));
-- XXX |            Write_Str (" before node ");
-- XXX |            Write_Int (Int (Before));
-- XXX |            Write_Eol;
-- XXX |         end if;
-- XXX |      end Insert_List_Before_Debug;

   --  Start of prodcessing for Insert_List_Before

   begin
      pragma Assert (Is_List_Member (Before));

      if Is_Empty_List (List) then
         return;

      else
         declare
            After : constant Node_Id := Prev (Before);
            LC    : constant List_Id := List_Containing (Before);
            F     : constant Node_Id := First (List);
            L     : constant Node_Id := Last (List);
            N     : Node_Id;

         begin
-- XXX |            pragma Debug (Insert_List_Before_Debug);

            N := F;
            loop
               Set_List_Link (N, LC);
               exit when N = L;
               N := Next (N);
            end loop;

            if Present (After) then
               Set_Next (After, F);
            else
               Set_First (LC, F);
            end if;

            Set_Prev (Before, L);
            Set_Prev (F, After);
            Set_Next (L, Before);

            Set_First (List, Empty);
            Set_Last  (List, Empty);
         end;
      end if;
   end Insert_List_Before;

   -------------------
   -- Is_Empty_List --
   -------------------

   function Is_Empty_List (List : List_Id) return Boolean is
   begin
      return First (List) = Empty;
   end Is_Empty_List;

   --------------------
   -- Is_List_Member --
   --------------------

   function Is_List_Member (Node : Node_Id) return Boolean is
   begin
      return Nodes.Table (Node).In_List;
   end Is_List_Member;

   -----------------------
   -- Is_Non_Empty_List --
   -----------------------

   function Is_Non_Empty_List (List : List_Id) return Boolean is
   begin
      return List /= No_List and then First (List) /= Empty;
   end Is_Non_Empty_List;

   ------------------
   -- Last_List_Id --
   ------------------

   function Last_List_Id return List_Id is
   begin
      return Lists.Last;
   end Last_List_Id;

   ---------------------
   -- List_Containing --
   ---------------------

   function List_Containing (Node : Node_Id) return List_Id is
   begin
      pragma Assert (Is_List_Member (Node));
      return List_Id (Nodes.Table (Node).Link);
   end List_Containing;

   -----------------
   -- List_Length --
   -----------------

   function List_Length (List : List_Id) return Nat is
      Result : Nat;
      Node   : Node_Id;

   begin
      Result := 0;
      Node := First (List);
      while Present (Node) loop
         Result := Result + 1;
         Node := Next (Node);
      end loop;

      return Result;
   end List_Length;

   -------------------
   -- Lists_Address --
   -------------------

   function Lists_Address return System.Address is
   begin
      return Lists.Table (First_List_Id)'Address;
   end Lists_Address;

   ----------
   -- Lock --
   ----------

   procedure Lock is
   begin
      Lists.Locked := True;
      Lists.Release;

      Prev_Node.Locked := True;
      Next_Node.Locked := True;

      Prev_Node.Release;
      Next_Node.Release;
   end Lock;

   -----------------------
   -- Next_Node_Address --
   -----------------------

   function Next_Node_Address return System.Address is
   begin
      return Next_Node.Table (First_Node_Id)'Address;
   end Next_Node_Address;

   --------------
   -- New_List --
   --------------

   function New_List return List_Id is

-- XXX |      procedure New_List_Debug;
-- XXX |      pragma Inline (New_List_Debug);
-- XXX |      --  Output debugging information if Debug_Flag_N is set
-- XXX |
-- XXX |      procedure New_List_Debug is
-- XXX |      begin
-- XXX |         if Debug_Flag_N then
-- XXX |            Write_Str ("Allocate new list, returned ID = ");
-- XXX |            Write_Int (Int (Lists.Last));
-- XXX |            Write_Eol;
-- XXX |         end if;
-- XXX |      end New_List_Debug;

   --  Start of processing for New_List

   begin
      Lists.Increment_Last;

      declare
         List : constant List_Id := Lists.Last;

      begin
         Set_Parent (List, Empty);
         Set_First  (List, Empty);
         Set_Last   (List, Empty);

-- XXX |         pragma Debug (New_List_Debug);
         return (List);
      end;
   end New_List;

   --  Since the one argument case is common, we optimize to build the right
   --  list directly, rather than first building an empty list and then doing
   --  the insertion, which results in some unnecessary work.

   function New_List (Node : Node_Id) return List_Id is

-- XXX |      procedure New_List_Debug;
-- XXX |      pragma Inline (New_List_Debug);
-- XXX |      --  Output debugging information if Debug_Flag_N is set
-- XXX |
-- XXX |      procedure New_List_Debug is
-- XXX |      begin
-- XXX |         if Debug_Flag_N then
-- XXX |            Write_Str ("Allocate new list, returned ID = ");
-- XXX |            Write_Int (Int (Lists.Last));
-- XXX |            Write_Eol;
-- XXX |         end if;
-- XXX |      end New_List_Debug;

   --  Start of processing for New_List

   begin
      if Node = Error then
         return New_List;

      else
         Lists.Increment_Last;

         declare
            List : constant List_Id := Lists.Last;

         begin
            Set_Parent (List, Empty);
            Set_First  (List, Node);
            Set_Last   (List, Node);

            Nodes.Table (Node).In_List := True;
            Set_List_Link (Node, List);
            Set_Prev (Node, Empty);
            Set_Next (Node, Empty);
-- XXX |            pragma Debug (New_List_Debug);
            return List;
         end;
      end if;
   end New_List;

   function New_List (Node1, Node2 : Node_Id) return List_Id is
      L : constant List_Id := New_List (Node1);

   begin
      Append (Node2, L);
      return L;
   end New_List;

   function New_List (Node1, Node2, Node3 : Node_Id) return List_Id is
      L : constant List_Id := New_List (Node1);

   begin
      Append (Node2, L);
      Append (Node3, L);
      return L;
   end New_List;

   function New_List (Node1, Node2, Node3, Node4 : Node_Id) return List_Id is
      L : constant List_Id := New_List (Node1);

   begin
      Append (Node2, L);
      Append (Node3, L);
      Append (Node4, L);
      return L;
   end New_List;

   function New_List
     (Node1 : Node_Id;
      Node2 : Node_Id;
      Node3 : Node_Id;
      Node4 : Node_Id;
      Node5 : Node_Id)
      return  List_Id
   is
      L : constant List_Id := New_List (Node1);

   begin
      Append (Node2, L);
      Append (Node3, L);
      Append (Node4, L);
      Append (Node5, L);
      return L;
   end New_List;

   function New_List
     (Node1 : Node_Id;
      Node2 : Node_Id;
      Node3 : Node_Id;
      Node4 : Node_Id;
      Node5 : Node_Id;
      Node6 : Node_Id)
      return  List_Id
   is
      L : constant List_Id := New_List (Node1);

   begin
      Append (Node2, L);
      Append (Node3, L);
      Append (Node4, L);
      Append (Node5, L);
      Append (Node6, L);
      return L;
   end New_List;

-- XXX |   -------------------
-- XXX |   -- New_List_Copy --
-- XXX |   -------------------
-- XXX |
-- XXX |   function New_List_Copy (List : List_Id) return List_Id is
-- XXX |      NL : List_Id;
-- XXX |      E  : Node_Id;
-- XXX |
-- XXX |   begin
-- XXX |      if List = No_List then
-- XXX |         return No_List;
-- XXX |
-- XXX |      else
-- XXX |         NL := New_List;
-- XXX |         E := First (List);
-- XXX |
-- XXX |         while Present (E) loop
-- XXX |            Append (New_Copy (E), NL);
-- XXX |            E := Next (E);
-- XXX |         end loop;
-- XXX |
-- XXX |         return NL;
-- XXX |      end if;
-- XXX |   end New_List_Copy;
-- XXX |
-- XXX |   ----------------------------
-- XXX |   -- New_List_Copy_Original --
-- XXX |   ----------------------------
-- XXX |
-- XXX |   function New_List_Copy_Original (List : List_Id) return List_Id is
-- XXX |      NL : List_Id;
-- XXX |      E  : Node_Id;
-- XXX |
-- XXX |   begin
-- XXX |      if List = No_List then
-- XXX |         return No_List;
-- XXX |
-- XXX |      else
-- XXX |         NL := New_List;
-- XXX |         E := First (List);
-- XXX |
-- XXX |         while Present (E) loop
-- XXX |            if Comes_From_Source (E) then
-- XXX |               Append (New_Copy (E), NL);
-- XXX |            end if;
-- XXX |
-- XXX |            E := Next (E);
-- XXX |         end loop;
-- XXX |
-- XXX |         return NL;
-- XXX |      end if;
-- XXX |   end New_List_Copy_Original;
-- XXX |
-- XXX |   ------------------------
-- XXX |   -- New_List_Copy_Tree --
-- XXX |   ------------------------
-- XXX |
-- XXX |   function New_List_Copy_Tree (List : List_Id) return List_Id is
-- XXX |      NL : List_Id;
-- XXX |      E  : Node_Id;
-- XXX |
-- XXX |   begin
-- XXX |      if List = No_List then
-- XXX |         return No_List;
-- XXX |
-- XXX |      else
-- XXX |         NL := New_List;
-- XXX |         E := First (List);
-- XXX |
-- XXX |         while Present (E) loop
-- XXX |            Append (New_Copy_Tree (E), NL);
-- XXX |            E := Next (E);
-- XXX |         end loop;
-- XXX |
-- XXX |         return NL;
-- XXX |      end if;
-- XXX |   end New_List_Copy_Tree;

   ---------------
   -- Num_Lists --
   ---------------

   function Num_Lists return Nat is
   begin
      return Int (Lists.Last) - Int (Lists.First) + 1;
   end Num_Lists;

   ------------
   -- Parent --
   ------------

   function Parent (List : List_Id) return Node_Id is
   begin
      pragma Assert (List in First_List_Id .. Lists.Last);
      return Lists.Table (List).Parent;
   end Parent;

   -------------
   -- Prepend --
   -------------

   procedure Prepend (Node : Node_Id; To : List_Id) is
      F : constant Node_Id := First (To);

-- XXX |      procedure Prepend_Debug;
-- XXX |      pragma Inline (Prepend_Debug);
-- XXX |      --  Output debug information if Debug_Flag_N set
-- XXX |
-- XXX |      procedure Prepend_Debug is
-- XXX |      begin
-- XXX |         if Debug_Flag_N then
-- XXX |            Write_Str ("Prepend node ");
-- XXX |            Write_Int (Int (Node));
-- XXX |            Write_Str (" to list ");
-- XXX |            Write_Int (Int (To));
-- XXX |            Write_Eol;
-- XXX |         end if;
-- XXX |      end Prepend_Debug;

   --  Start of processing for Prepend


   begin
      pragma Assert (not Is_List_Member (Node));

      if Node = Error then
         return;
      end if;

-- XXX |      pragma Debug (Prepend_Debug);

      if No (F) then
         Set_Last (To, Node);
      else
         Set_Prev (F, Node);
      end if;

      Set_First (To, Node);

      Nodes.Table (Node).In_List := True;

      Set_Next      (Node, F);
      Set_Prev      (Node, Empty);
      Set_List_Link (Node, To);
   end Prepend;

   ----------------
   -- Prepend_To --
   ----------------

   procedure Prepend_To (To : List_Id; Node : Node_Id) is
   begin
      Prepend (Node, To);
   end Prepend_To;

   -------------
   -- Present --
   -------------

   function Present (List : List_Id) return Boolean is
   begin
      return List /= No_List;
   end Present;

   -----------------------
   -- Prev_Node_Address --
   -----------------------

   function Prev_Node_Address return System.Address is
   begin
      return Prev_Node.Table (First_Node_Id)'Address;
   end Prev_Node_Address;

   ------------
   -- Remove --
   ------------

   procedure Remove (Node : Node_Id) is
      Lst : constant List_Id := List_Containing (Node);
      Prv : constant Node_Id := Prev (Node);
      Nxt : constant Node_Id := Next (Node);

-- XXX |      procedure Remove_Debug;
-- XXX |      pragma Inline (Remove_Debug);
-- XXX |      --  Output debug information if Debug_Flag_N set
-- XXX |
-- XXX |      procedure Remove_Debug is
-- XXX |      begin
-- XXX |         if Debug_Flag_N then
-- XXX |            Write_Str ("Remove node ");
-- XXX |            Write_Int (Int (Node));
-- XXX |            Write_Eol;
-- XXX |         end if;
-- XXX |      end Remove_Debug;

   --  Start of processing for Remove

   begin
-- XXX |      pragma Debug (Remove_Debug);

      if No (Prv) then
         Set_First (Lst, Nxt);
      else
         Set_Next (Prv, Nxt);
      end if;

      if No (Nxt) then
         Set_Last (Lst, Prv);
      else
         Set_Prev (Nxt, Prv);
      end if;

      Nodes.Table (Node).In_List := False;
      Set_Parent (Node, Empty);
   end Remove;

   -----------------
   -- Remove_Head --
   -----------------

   function Remove_Head (List : List_Id) return Node_Id is
      Frst : constant Node_Id := First (List);

-- XXX |      procedure Remove_Head_Debug;
-- XXX |      pragma Inline (Remove_Head_Debug);
-- XXX |      --  Output debug information if Debug_Flag_N set
-- XXX |
-- XXX |      procedure Remove_Head_Debug is
-- XXX |      begin
-- XXX |         if Debug_Flag_N then
-- XXX |            Write_Str ("Remove head of list ");
-- XXX |            Write_Int (Int (List));
-- XXX |            Write_Eol;
-- XXX |         end if;
-- XXX |      end Remove_Head_Debug;

   --  Start of processing for Remove_Head

   begin
-- XXX |      pragma Debug (Remove_Head_Debug);

      if Frst = Empty then
         return Empty;

      else
         declare
            Nxt : constant Node_Id := Next (Frst);

         begin
            Set_First (List, Nxt);

            if No (Nxt) then
               Set_Last (List, Empty);
            else
               Set_Prev (Nxt, Empty);
            end if;

            Nodes.Table (Frst).In_List := False;
            Set_Parent (Frst, Empty);
            return Frst;
         end;
      end if;
   end Remove_Head;

   -----------------
   -- Remove_Next --
   -----------------

   function Remove_Next (Node : Node_Id) return Node_Id is
      Nxt : constant Node_Id := Next (Node);

-- XXX |      procedure Remove_Next_Debug;
-- XXX |      pragma Inline (Remove_Next_Debug);
-- XXX |      --  Output debug information if Debug_Flag_N set
-- XXX |
-- XXX |      procedure Remove_Next_Debug is
-- XXX |      begin
-- XXX |         if Debug_Flag_N then
-- XXX |            Write_Str ("Remove next node after ");
-- XXX |            Write_Int (Int (Node));
-- XXX |            Write_Eol;
-- XXX |         end if;
-- XXX |      end Remove_Next_Debug;

   --  Start of processing for Remove_Next

   begin
      if Present (Nxt) then
         declare
            Nxt2 : constant Node_Id := Next (Nxt);
            LC   : constant List_Id := List_Containing (Node);

         begin
-- XXX |            pragma Debug (Remove_Next_Debug);
            Set_Next (Node, Nxt2);

            if No (Nxt2) then
               Set_Last (LC, Node);
            else
               Set_Prev (Nxt2, Node);
            end if;

            Nodes.Table (Nxt).In_List := False;
            Set_Parent (Nxt, Empty);
         end;
      end if;

      return Nxt;
   end Remove_Next;

   ----------------
   -- Set_Parent --
   ----------------

   procedure Set_Parent (List : List_Id; Node : Node_Id) is
   begin
      pragma Assert (List in First_List_Id .. Lists.Last);
      Lists.Table (List).Parent := Node;
   end Set_Parent;

end CIAO.Nlists;

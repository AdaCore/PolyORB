with Lexer;    use Lexer;

package body Nutils is

   -------------------------
   -- Append_List_To_List --
   -------------------------

   procedure Append_List_To_List (S : List_Id; D : in out List_Id) is
   begin
      if Present (D) then
         Append_Node_To_List (Node_Id (S), D);
      else
         D := S;
      end if;
   end Append_List_To_List;

   -------------------------
   -- Append_Node_To_List --
   -------------------------

   procedure Append_Node_To_List (E : Node_Id; L : List_Id) is
      Last : Node_Id;
   begin
      Last := Last_Node (L);
      if No (Last) then
         Set_First_Node (L, E);
      else
         Set_Next_Node (Last, E);
      end if;

      Last := E;
      while Present (Last) loop
         Set_Last_Node (L, Last);
         Last := Next_Node (Last);
      end loop;
   end Append_Node_To_List;

   --------------
   -- Is_Empty --
   --------------

   function Is_Empty (L : List_Id) return Boolean is
   begin
      return L = No_List or else No (First_Node (L));
   end Is_Empty;

   -----------------------------
   -- Make_Current_Identifier --
   -----------------------------

   function Make_Current_Identifier return Node_Id
   is
      Node : constant Node_Id := New_Node (K_Identifier, Token_Location);

   begin
      Set_Name (Node, Token_Name);
      Set_Display_Name (Node, Token_Display_Name);
      return Node;
   end Make_Current_Identifier;

   ---------------------
   -- Make_Identifier --
   ---------------------

   function Make_Identifier (Loc          : Location;
                             Name         : Name_Id;
                             Display_Name : Name_Id) return Node_Id
   is
      Node : constant Node_Id := New_Node (K_Identifier, Loc);

   begin
      Set_Name (Node, Name);
      Set_Display_Name (Node, Display_Name);
      return Node;
   end Make_Identifier;

   --------------
   -- New_Copy --
   --------------

   function New_Copy (N : Node_Id) return Node_Id is
      L : Node_Id;
   begin
      Entries.Increment_Last;
      L := Entries.Last;
      Entries.Table (L) := Entries.Table (N);
      Set_Loc       (L, No_Location);
      Set_Next_Node (L, No_Node);
--      if Kind (L) = K_Identifier then
--         Set_Homonym (L, No_Node);
--      end if;
      return L;
   end New_Copy;

   --------------
   -- New_List --
   --------------

   function New_List (Kind : Node_Kind; Loc : Location) return List_Id is
   begin
      return List_Id (New_Node (Kind, Loc));
   end New_List;

   --------------
   -- New_Node --
   --------------

   function New_Node (Kind : Node_Kind; Loc : Location) return Node_Id is
      N : Node_Id;
   begin
      Entries.Increment_Last;
      N := Entries.Last;
      Entries.Table (N) := Default_Node;
      Set_Kind (N, Kind);
      Set_Loc  (N, Loc);

      return N;
   end New_Node;

   ---------------------------
   -- Remove_Node_From_List --
   ---------------------------

   procedure Remove_Node_From_List (E : Node_Id; L : List_Id) is
      C : Node_Id;
   begin
      C := First_Node (L);
      if C = E then
         Set_First_Node (L, Next_Node (E));
         if Last_Node (L) = E then
            Set_Last_Node (L, No_Node);
         end if;
      else
         while Present (C) loop
            if Next_Node (C) = E then
               Set_Next_Node (C, Next_Node (E));
               if Last_Node (L) = E then
                  Set_Last_Node (L, C);
               end if;
               exit;
            end if;
            C := Next_Node (C);
         end loop;
      end if;
   end Remove_Node_From_List;

end Nutils;

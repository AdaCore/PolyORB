with GNAT.Table;

with Debug;     use Debug;
pragma Warnings (Off, Debug);
with Errors;    use Errors;
with Flags;     use Flags;
with Names;     use Names;
with Namet;     use Namet;
with Nodes;     use Nodes;
with Nutils;    use Nutils;
with Scopes;    use Scopes;
with Types;     use Types;

package body Scopes is

   Verbose : Boolean renames Flags.V_Scopes;

   use Scope_Stack;

   procedure Remove_From_Homonyms (N : Node_Id);

   -----------------------------
   -- Current_Entity_In_Scope --
   -----------------------------

   function Current_Entity_In_Scope (N : Node_Id) return Entity_Id is
   begin
      return Entity_In_Scope (N, Current_Scope);
   end Current_Entity_In_Scope;

   -------------------
   -- Current_Scope --
   -------------------

   function Current_Scope return Entity_Id is
   begin
      if Last = No_Scope_Depth then
         return No_Entity;
      else
         return Table (Last).Entity;
      end if;
   end Current_Scope;

   -------------------------
   -- Current_Scope_Depth --
   -------------------------

   function Current_Scope_Depth return Int is
   begin
      return Last;
   end Current_Scope_Depth;

   ----------------------------
   -- Current_Visible_Entity --
   ----------------------------

   function Current_Visible_Entity (N : Node_Id) return Entity_Id
   is
      T : Natural   := 0;
      C : Entity_Id := No_Entity;
      H : Node_Id   := First_Homonym (N);
      E : Entity_Id;
   begin
      while Present (H) loop
         E := Entity (H);
         if Is_Visible (E) then
            if No (C) then
               C := E;
            else
               if T = 0 then
                  Error_Loc  (1)  := Loc      (N);
                  Error_Name (1)  := IDL_Name (N);
                  DE ("multiple#declarations");
                  Error_Loc  (1)  := Loc (N);
                  Error_Loc  (2)  := Loc (Identifier (C));
                  DE ("found declaration!", K_None);
               end if;

               Error_Loc  (1)  := Loc (N);
               Error_Loc  (2)  := Loc (Identifier (E));
               DE ("found declaration!", K_None);
               T := T + 1;
            end if;
         end if;
         H := Homonym (H);
      end loop;

      if No (C) then
         Error_Loc  (1) := Loc      (N);
         Error_Name (1) := IDL_Name (N);
         DE ("#is undefined");
         return No_Entity;
      end if;

      return C;
   end Current_Visible_Entity;

   -----------------------
   -- Enclosed_Entities --
   -----------------------

   function Enclosed_Entities (E : Entity_Id) return List_Id is
   begin
      case Kind (E) is
         when K_Module =>
            return Definitions (E);
         when K_Interface_Declaration =>
            return Interface_Body (E);
         when K_Operation_Declaration
           | K_Initializer_Declaration =>
            return Parameters (E);
         when K_Value_Declaration
           | K_Abstract_Value_Declaration =>
            return Value_Body (E);
         when K_Exception_Declaration
           | K_Structure_Type =>
            return Members (E);
         when others =>
            raise Program_Error;
      end case;
   end Enclosed_Entities;

   ---------------------
   -- Entity_In_Scope --
   ---------------------

   function Entity_In_Scope (N : Node_Id; S : Entity_Id) return Entity_Id
   is
      H : Node_Id := First_Homonym (N);
   begin
      while Present (H) loop
         if Scope (H) = S then
            return Entity (H);
         end if;
         H := Homonym (H);
      end loop;

      return No_Entity;
   end Entity_In_Scope;

   -------------------------
   -- Enter_Name_In_Scope --
   -------------------------

   procedure Enter_Name_In_Scope (N : Node_Id)
   is
      C : Entity_Id := Current_Entity_In_Scope (N);
      E : constant Entity_Id := Entity (N);
      S : constant Entity_Id := Current_Scope;
      D : constant Int       := Current_Scope_Depth;
      H : Node_Id;
   begin
      if Present (C) then
         H := Identifier (C);

         if C = E then
            return;

         elsif Kind (C) = K_Module
           and then Kind (E) = K_Module
         then
            null;

         elsif Kind (C) = K_Scoped_Name
           and then Kind (E) = K_Scoped_Name
         then
            null;

         elsif Kind (C) /= K_Scoped_Name
           and then Kind (E) = K_Scoped_Name
         then
            return;

         elsif Is_A_Forward_Of (C, E) then
            if Kind (C) = K_Forward_Interface_Declaration then
               Set_Forward    (C, E);
               Set_Is_Visible (C, False);
               Set_Scope      (H, No_Entity);
            end if;

         else
            Error_Loc  (1) := Loc      (N);
            Error_Loc  (2) := Loc      (C);
            Error_Name (1) := IDL_Name (N);
            DE ("#conflicts with declaration!");
            return;
         end if;
      end if;

      H := First_Homonym (N);
      Set_Homonym        (N, H);
      Set_First_Homonym  (N, N);
      Set_Scope          (N, S);
      Set_Scope_Depth    (N, D);

      if Verbose then
         W_Str      ("enter  """);
         Write_Name (Name (N));
         W_Str      (""" in scope ");
         W_Int      (Int (S));
         W_Eol;
      end if;

      if Kind (E) /= K_Scoped_Name then
         if Present (S)
           and then Kind (S) = K_Interface_Declaration
         then
            H := First_Homonym (N);
            while Present (H) loop
               C := Entity (H);
               if Is_Visible (C)
                 and then Scope_Depth (H) = D
               then
                  Set_Is_Visible (C, False);
               end if;
               H := Homonym (H);
            end loop;
         end if;
         Set_Is_Visible  (E, True);
      end if;
   end Enter_Name_In_Scope;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      Init;
      Increment_Last;
      Set_Str_To_Name_Buffer (" ");
      Root_Name := Name_Find;
   end Initialize;

   ------------------------------------
   -- Make_Enclosed_Entities_Visible --
   ------------------------------------

   procedure Make_Enclosed_Entities_Visible (E : Entity_Id; V : Boolean)
   is
      L : constant List_Id := Enclosed_Entities (E);
      C : Entity_Id;
   begin
      if Is_Empty (L) then
         return;
      end if;

      C := First_Entity (L);
      while Present (C) loop
         Make_Entity_Visible (C, V);
         C := Next_Entity (C);
      end loop;
   end Make_Enclosed_Entities_Visible;

   -------------------------
   -- Make_Entity_Visible --
   -------------------------

   procedure Make_Entity_Visible (E : Entity_Id; V : Boolean)
   is
      N : constant Node_Id := Identifier (E);
      D : constant Int     := Current_Scope_Depth;
   begin
      Set_Is_Visible (E, V);
      if V then
         Set_Scope_Depth   (N, D);
         Set_Homonym       (N, First_Homonym (N));
         Set_First_Homonym (N, N);
      else
         Remove_From_Homonyms (N);
      end if;
   end Make_Entity_Visible;

   ---------------
   -- Pop_Scope --
   ---------------

   procedure Pop_Scope
   is
      C : Entity_Id renames Table (Last).Scoped_Names;
   begin
      Make_Enclosed_Entities_Visible (Table (Last).Entity, False);
      while Present (C) loop
         Remove_From_Homonyms (Identifier (C));
         C := Next_Entity (C);
      end loop;
      if Verbose then
         W_Str      ("pop  """);
         Write_Name (Name (Identifier (Table (Last).Entity)));
         W_Str      (""" ");
         W_Int      (Last);
         W_Eol;
      end if;
      Decrement_Last;
   end Pop_Scope;

   ----------------
   -- Push_Scope --
   ----------------

   procedure Push_Scope (E : Entity_Id) is
   begin
      Increment_Last;
      Table (Last).Entity       := E;
      Table (Last).Scoped_Names := No_Entity;
      if Verbose then
         W_Str      ("push """);
         Write_Name (Name (Identifier (E)));
         W_Str      (""" ");
         W_Int      (Last);
         W_Eol;
      end if;
   end Push_Scope;

   --------------------------
   -- Remove_From_Homonyms --
   --------------------------

   procedure Remove_From_Homonyms (N : Node_Id) is
      H : Node_Id;
   begin
      Set_Scope_Depth (N, No_Scope_Depth);
      loop
         H := First_Homonym (N);
         exit when No (H) or else Scope_Depth (H) /= No_Scope_Depth;
         Set_First_Homonym (N, Homonym (H));
         Set_Homonym (H, No_Node);
      end loop;
   end Remove_From_Homonyms;

   ------------------
   -- Scoped_Names --
   ------------------

   function Scoped_Names return Entity_Id is
   begin
      return Table (Last).Scoped_Names;
   end Scoped_Names;

   ----------------------
   -- Set_Scoped_Names --
   ----------------------

   procedure Set_Scoped_Names (E : Entity_Id) is
   begin
      Table (Last).Scoped_Names := E;
   end Set_Scoped_Names;

end Scopes;

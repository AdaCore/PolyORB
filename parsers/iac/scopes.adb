with GNAT.Table;

with Errors;    use Errors;
with Flags;     use Flags;
with Locations; use Locations;
with Namet;     use Namet;
with Scopes;    use Scopes;
with Types;     use Types;

with Frontend.Debug;  use Frontend.Debug;
with Frontend.Nodes;  use Frontend.Nodes;
with Frontend.Nutils; use Frontend.Nutils;

package body Scopes is

   use Scope_Stack;

   procedure W_Homonym  (N : Node_Id);
   procedure W_Homonyms (N : Node_Id);
   procedure W_Scoped_Identifiers (S : Node_Id);

   procedure Insert_Into_Homonyms (N : Node_Id);
   --  Insert into homonyms chain if not already there

   procedure Remove_From_Homonyms (N : Node_Id);
   --  Remove from homonyms chain when no longer needed

   -------------------
   -- Current_Scope --
   -------------------

   function Current_Scope return Node_Id is
   begin
      if Last = No_Scope_Depth then
         return No_Node;
      else
         return Table (Last).Node;
      end if;
   end Current_Scope;

   -------------------------
   -- Enter_Name_In_Scope --
   -------------------------

   procedure Enter_Name_In_Scope (N : Node_Id)
   is
      E : constant Node_Id := Corresponding_Entity (N);
      S : constant Node_Id := Current_Scope;
      C : constant Node_Id := Node_In_Current_Scope (N);
      H : Node_Id;
   begin
      if Present (C) then
         H := Identifier (C);

         --  This same entity is already in the scope

         if C = E then
            return;

         --  This entity is an extension of a module

         elsif Kind (C) = K_Module
           and then Kind (E) = K_Module
         then
            null;

         --  This scoped name is already in the scope

         elsif Kind (E) = K_Scoped_Name then
            return;

         elsif Is_A_Forward_Of (C, E) then
            if Kind (C) = K_Forward_Interface_Declaration then
               Set_Forward             (C, E);
               Set_Scope               (H, No_Node);
               Set_Potential_Scope     (H, No_Node);
               Set_Explicitely_Visible (H, False);
               Set_Implicitely_Visible (H, False);
               Remove_From_Homonyms    (H);
            end if;

         else
            Error_Loc  (1) := Loc      (N);
            Error_Loc  (2) := Loc      (C);
            Error_Name (1) := IDL_Name (N);
            if Kind (C) = K_Scoped_Name then
               DE ("#conflicts with scoped name!");
            else
               DE ("#conflicts with declaration!");
            end if;
            return;
         end if;
      end if;

      Insert_Into_Homonyms    (N);
      if No (Scope (N)) then
         Set_Scope (N, S);
      end if;
      Set_Potential_Scope     (N, S);
      Set_Explicitely_Visible (N, True);
      Set_Next_Entity         (N, Scoped_Identifiers (S));
      Set_Scoped_Identifiers  (S, N);
   end Enter_Name_In_Scope;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      Init;
      Increment_Last;
      IDL_Spec_Name := No_Name;
   end Initialize;

   --------------------------
   -- Insert_Into_Homonyms --
   --------------------------

   procedure Insert_Into_Homonyms (N : Node_Id) is
      H : Node_Id;
      S : constant Node_Id := Current_Scope;
   begin
      --  N is already in the homonyms chain

      if Potential_Scope (N) = S
        or else Explicitely_Visible (N)
        or else Implicitely_Visible (N)
      then
         return;
      end if;

      H := First_Homonym (N);
      if D_Scopes then
         W_Str      ("insert ");
         W_Homonym  (N);
         W_Str      (" into homonyms (");
         Write_Name (Name (N));
         W_Str      (") = [");
         W_Homonyms (N);
         W_Str      ("]");
         W_Eol;
      end if;
      Set_Homonym       (N, H);
      Set_First_Homonym (N, N);
   end Insert_Into_Homonyms;

   ------------------------------
   -- Make_Implicitely_Visible --
   ------------------------------

   procedure Make_Implicitely_Visible (N : Node_Id; Visible : Boolean) is
   begin
      --  Scoped names are in the scope but cannot be made visible

      if Kind (Corresponding_Entity (N)) = K_Scoped_Name then
         return;
      end if;

      --  A visible entity has to be inserted in the homonyms chain

      if Visible then
         Insert_Into_Homonyms (N);
      end if;

      Set_Implicitely_Visible (N, Visible);

      --  When no longer visible, an entity is removed from the
      --  homonyms chain

      if not Visible then
         Remove_From_Homonyms (N);
      end if;
   end Make_Implicitely_Visible;

   ------------------------------
   -- Node_Explicitly_In_Scope --
   ------------------------------

   function Node_Explicitly_In_Scope
     (N : Node_Id;
      S : Node_Id)
      return Node_Id
   is
      C : Node_Id := Scoped_Identifiers (S);
      X : constant Name_Id := Name (N);
   begin
      --  Loop through scope S to find N. Entities potentially in the
      --  scope are present in S but they are not candidates here. As
      --  the scope can be different from the current scope, N is not
      --  always present in the homonyms chain.

      while Present (C) loop
         if Scope (C) = S
           and then Name (C) = X
         then
            return Corresponding_Entity (C);
         end if;
         C := Next_Entity (C);
      end loop;

      return No_Node;
   end Node_Explicitly_In_Scope;

   ---------------------------
   -- Node_In_Current_Scope --
   ---------------------------

   function Node_In_Current_Scope (N : Node_Id) return Node_Id
   is
      S : constant Node_Id := Current_Scope;
      H : Node_Id := First_Homonym (N);
      X : Node_Id;
   begin
      while Present (H) loop
         X := Corresponding_Entity (H);

         if Potential_Scope (H) = S then
            return X;

         elsif X = S then

            --  The name of an interface, value type, struct, union,
            --  exception or a module may not be redefined within the
            --  immediate scope of the interface, value type, struct,
            --  union, exception, or the module.

            case Kind (S) is
               when K_Interface_Declaration
                 | K_Value_Declaration
                 | K_Structure_Type
                 | K_Union_Type
                 | K_Exception_Declaration
                 | K_Module =>
                  return X;
               when others =>
                  null;
            end case;
         end if;
         H := Homonym (H);
      end loop;

      return No_Node;
   end Node_In_Current_Scope;

   ---------------
   -- Pop_Scope --
   ---------------

   procedure Pop_Scope
   is
      S : constant Node_Id := Current_Scope;
      C : Node_Id;
      E : Node_Id;
      N : Node_Id;

   begin
      if D_Scopes then
         W_Str      ("pop scope """);
         if Kind (S) /= K_Specification then
            Write_Name (Name (Identifier (S)));
         end if;
         W_Str      (""" ");
         if Present (Identifier (S)) then
            W_Str ("[");
            W_Homonyms (Identifier (S));
            W_Str ("] ");
         end if;
         W_Scoped_Identifiers (S);
         W_Eol;
      end if;

      --  Pop scope

      Decrement_Last;
      if Last = First then
         return;
      end if;

      --  When the previous scope was a type name that is nested in a
      --  non module scope definition, the potential scope extends to
      --  the enclosing non-module scope. We introduced the scoped
      --  names in the enclosing scope.

      declare
         Export : constant Boolean :=
           Is_A_Type (S) and then Is_A_Non_Module (Current_Scope);
      begin
         C := Scoped_Identifiers (S);
         while Present (C) loop
            Set_Explicitely_Visible (C, False);
            Remove_From_Homonyms (C);
            if Export then
               E := Corresponding_Entity (C);
               if Kind (E) = K_Scoped_Name then
                  N := Identifier (E);
                  N := Make_Identifier
                    (Loc (N), Name (N), Corresponding_Entity (N), Scope (N));
                  Set_Potential_Scope  (N, S);
                  Enter_Name_In_Scope  (N);
               end if;
            end if;
            C := Next_Entity (C);
         end loop;
      end;

      if D_Scopes then
         W_Str      ("show scope """);
         if Kind (Current_Scope) /= K_Specification then
            Write_Name (Name (Identifier (Current_Scope)));
         end if;
         W_Str      (""" ");
         W_Scoped_Identifiers (Current_Scope);
         W_Eol;
      end if;
   end Pop_Scope;

   ----------------
   -- Push_Scope --
   ----------------

   procedure Push_Scope (S : Node_Id)
   is
   begin
      Increment_Last;
      Table (Last).Node := S;
      if D_Scopes then
         W_Str      ("push scope """);
         if Kind (S) /= K_Specification then
            Write_Name (Name (Identifier (S)));
         end if;
         W_Str      ("""");
         if Present (Identifier (S)) then
            W_Str (" [");
            W_Homonyms (Identifier (S));
            W_Str ("]");
         end if;
         W_Eol;
      end if;
   end Push_Scope;

   --------------------------
   -- Remove_From_Homonyms --
   --------------------------

   procedure Remove_From_Homonyms (N : Node_Id) is
      S : constant Node_Id := Current_Scope;
      H : Node_Id;
   begin
      if Potential_Scope (N) = S
        or else Implicitely_Visible (N)
        or else Explicitely_Visible (N)
      then
         return;
      end if;

      H := First_Homonym (N);
      if H = N then
         Set_First_Homonym (N, Homonym (H));

      else
         while Present (H) loop
            if Homonym (H) = N then
               Set_Homonym (H, Homonym (N));
               exit;
            end if;
            H := Homonym (H);
         end loop;
      end if;

      if D_Scopes then
         W_Str      ("remove ");
         W_Homonym  (N);
         W_Str      (" from homonyms (");
         Write_Name (Name (N));
         W_Str      (") = [");
         W_Homonyms (N);
         W_Str      ("]");
         W_Eol;
      end if;
   end Remove_From_Homonyms;

   ------------------
   -- Visible_Node --
   ------------------

   function Visible_Node (N : Node_Id) return Node_Id
   is
      H : Node_Id := First_Homonym (N);
      E : Node_Id;
   begin
      if Present (H) then
         E := Corresponding_Entity (H);

         --  The current visible entity has already been entered in the scope

         if Kind (E) = K_Scoped_Name then
            return Reference (E);
         end if;

         if Explicitely_Visible (H) then
            return Corresponding_Entity (H);

         elsif Implicitely_Visible (H) then
            H := Homonym (H);

            if Present (H) and then Implicitely_Visible (H) then
               Error_Loc  (1)  := Loc      (N);
               Error_Name (1)  := IDL_Name (N);
               DE ("multiple#declarations");

               H := First_Homonym (N);
               while Present (H) and then Implicitely_Visible (H) loop
                  Error_Loc  (1)  := Loc (N);
                  Error_Loc  (2)  := Loc (H);
                  DE ("found declaration!", K_None);
                  H := Homonym (H);
               end loop;

               return No_Node;

            else
               return Corresponding_Entity (First_Homonym (N));
            end if;
         end if;
      end if;

      Error_Loc  (1) := Loc      (N);
      Error_Name (1) := IDL_Name (N);
      DE ("#is undefined");

      return No_Node;
   end Visible_Node;

   ----------------
   -- W_Homonyms --
   ----------------

   procedure W_Homonyms (N : Node_Id)
   is
      H : Node_Id := First_Homonym (N);
   begin
      if No (H) then
         return;
      end if;
      loop
         W_Homonym (H);
         H := Homonym (H);
         exit when No (H);
         W_Str (" ");
      end loop;
   end W_Homonyms;

   ---------------
   -- W_Homonym --
   ---------------

   procedure W_Homonym (N : Node_Id) is
   begin
      W_Str (Image (Loc (N)));
      W_Str ("(");
      if Kind (Corresponding_Entity (N)) = K_Scoped_Name then
         W_Str ("S");
      elsif Explicitely_Visible (N) then
         W_Str ("V");
      elsif Implicitely_Visible (N) then
         W_Str ("v");
      else
         W_Str ("?");
      end if;
      W_Str (")");
   end W_Homonym;

   --------------------------
   -- W_Scoped_Identifiers --
   --------------------------

   procedure W_Scoped_Identifiers (S : Node_Id) is
      C : Node_Id := Scoped_Identifiers (S);
   begin
      if No (C) then
         return;
      end if;
      loop
         Write_Name (Name (C));
         W_Str (" [");
         W_Homonyms (C);
         W_Str ("]");
         C := Next_Entity (C);
         exit when No (C);
         W_Str (" ");
      end loop;
   end W_Scoped_Identifiers;

end Scopes;

with GNAT.Table;

with Debug;     use Debug;
pragma Warnings (Off, Debug);
with Errors;    use Errors;
with Flags;     use Flags;
with Locations; use Locations;
with Names;     use Names;
with Namet;     use Namet;
with Nodes;     use Nodes;
with Nutils;    use Nutils;
with Scopes;    use Scopes;
with Types;     use Types;

package body Scopes is

   use Scope_Stack;

   procedure W_Homonym  (N : Node_Id);
   procedure W_Homonyms (N : Node_Id);
   procedure W_Scoped_Identifiers (S : Node_Id);

   procedure Insert_Into_Homonyms (N : Node_Id);
   procedure Remove_From_Homonyms (N : Node_Id);


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

   --------------------
   -- Current_Node --
   --------------------

   function Current_Node (N : Node_Id) return Node_Id
   is
      H : Node_Id := First_Homonym (N);
      E : Node_Id;
   begin
      if Present (H) then
         E := Node (H);

         --  The current visible entity has already been entered in the scope

         if Kind (E) = K_Scoped_Name then
            return Reference (E);
         end if;

         if Immediately_Visible (H) then
            return Node (H);

         elsif Potentially_Visible (H) then
            H := Homonym (H);

            if Present (H) and then Potentially_Visible (H) then
               Error_Loc  (1)  := Loc      (N);
               Error_Name (1)  := IDL_Name (N);
               DE ("multiple#declarations");

               H := First_Homonym (N);
               while Present (H) and then Potentially_Visible (H) loop
                  Error_Loc  (1)  := Loc (N);
                  Error_Loc  (2)  := Loc (H);
                  DE ("found declaration!", K_None);
                  H := Homonym (H);
               end loop;

               return No_Node;

            else
               return Node (First_Homonym (N));
            end if;
         end if;
      end if;

      Error_Loc  (1) := Loc      (N);
      Error_Name (1) := IDL_Name (N);
      DE ("#is undefined");

      return No_Node;
   end Current_Node;

   -----------------------------
   -- Node_In_Current_Scope --
   -----------------------------

   function Node_In_Current_Scope (N : Node_Id) return Node_Id is
   begin
      return Node_In_Scope (N, Current_Scope);
   end Node_In_Current_Scope;

   -------------------
   -- Node_In_Scope --
   -------------------

   function Node_In_Scope (N : Node_Id; S : Node_Id) return Node_Id
   is
      H : Node_Id := First_Homonym (N);
      X : Node_Id;
   begin
      while Present (H) loop
         X := Node (H);

         if Scope (H) = S then
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
   end Node_In_Scope;

   -------------------------
   -- Enter_Name_In_Scope --
   -------------------------

   procedure Enter_Name_In_Scope (N : Node_Id)
   is
      C : constant Node_Id := Node_In_Current_Scope (N);
      E : constant Node_Id := Node (N);
      S : constant Node_Id := Current_Scope;
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
               Set_Immediately_Visible (H, False);
               Set_Scope               (H, No_Node);
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

      if Kind (E) /= K_Scoped_Name then
         Set_Immediately_Visible (N, True);
      end if;

      Set_Next_Node        (N, Scoped_Identifiers (S));
      Set_Scoped_Identifiers     (S, N);
      Insert_Into_Homonyms (N);
      Set_Scope            (N, S);
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

   --------------------------
   -- Insert_Into_Homonyms --
   --------------------------

   procedure Insert_Into_Homonyms (N : Node_Id) is
      F : constant Node_Id := First_Homonym (N);
   begin
      if D_Scopes then
         W_Str      ("homonyms ");
         Write_Name (Name (N));
         W_Str      (" ");
         W_Homonym  (N);
         W_Str      (" --> ");
         W_Homonyms (N);
         W_Eol;
      end if;
      Set_Homonym       (N, F);
      Set_First_Homonym (N, N);
   end Insert_Into_Homonyms;

   ------------------------------------
   -- Make_Enclosed_Nodes_Visible --
   ------------------------------------

   procedure Make_Enclosed_Nodes_Visible
     (E : Node_Id; Visible : Boolean; Immediately : Boolean := True)
   is
      I : Node_Id := Scoped_Identifiers (E);
      C : Node_Id;
   begin
      while Present (I) loop
         C := Node (I);
         if Kind (C) /= K_Scoped_Name then
            Make_Node_Visible (C, Visible, Immediately);
         end if;
         I := Next_Node (I);
      end loop;
   end Make_Enclosed_Nodes_Visible;

   -------------------------
   -- Make_Node_Visible --
   -------------------------

   procedure Make_Node_Visible
     (E : Node_Id; Visible : Boolean; Immediately : Boolean := True)
   is
      N : constant Node_Id := Identifier (E);
   begin
      if Visible then
         Insert_Into_Homonyms (N);
      else
         Remove_From_Homonyms (N);
      end if;

      if Immediately then
         Set_Immediately_Visible (N, Visible);
         Set_Potentially_Visible (N, False);
      else
         Set_Immediately_Visible (N, False);
         Set_Potentially_Visible (N, Visible);
      end if;

      if D_Scopes then
         W_Str      ("make visible ");
         Write_Name (Name (N));
         W_Str      (" ");
         W_Homonym  (N);
         W_Str      (" --> ");
         W_Homonyms (N);
         W_Eol;
      end if;
   end Make_Node_Visible;

   ---------------
   -- Pop_Scope --
   ---------------

   procedure Pop_Scope
   is
      S : constant Node_Id := Table (Last).Node;
      T : constant Boolean := Is_A_Type (S);
      C : Node_Id;
      E : Node_Id;
      N : Node_Id;
   begin
      if D_Scopes then
         W_Str      ("pop scope """);
         if Kind (S) /= K_Specification then
            Write_Name (Name (Identifier (S)));
         end if;
         W_Str      ("""");
         W_Eol;
         W_Scoped_Identifiers (S);
      end if;
      Decrement_Last;

      --  When the current scope is a type name the potential scope
      --  extends to the enclosing non-module scope. We introduced the
      --  scoped names in the enclosing scope.

      C := Scoped_Identifiers (S);
      while Present (C) loop
         Remove_From_Homonyms (C);
         if T then
            E := Node (C);
            if Kind (E) = K_Scoped_Name then
               E := New_Copy (E);
               N := New_Copy (C);
               Bind_Identifier_To_Entity (N, E);
               Enter_Name_In_Scope (N);
            end if;
         end if;
         C := Next_Node (C);
      end loop;
      if D_Scopes then
         W_Eol;
      end if;
   end Pop_Scope;

   ----------------
   -- Push_Scope --
   ----------------

   procedure Push_Scope (S : Node_Id) is
   begin
      Increment_Last;
      Table (Last).Node := S;
      if D_Scopes then
         W_Str      ("push scope """);
         if Kind (S) /= K_Specification then
            Write_Name (Name (Identifier (S)));
         end if;
         W_Str      ("""");
         W_Eol;
      end if;
   end Push_Scope;

   --------------------------
   -- Remove_From_Homonyms --
   --------------------------

   procedure Remove_From_Homonyms (N : Node_Id) is
      H : Node_Id;
   begin
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
         W_Str      ("homonyms ");
         Write_Name (Name (N));
         W_Str      (" ");
         W_Homonym  (N);
         W_Str      (" <-- ");
         W_Homonyms (N);
         W_Eol;
      end if;
   end Remove_From_Homonyms;

   ----------------
   -- W_Homonyms --
   ----------------

   procedure W_Homonyms (N : Node_Id)
   is
      H : Node_Id := First_Homonym (N);
   begin
      while Present (H) loop
         W_Homonym (H);
         W_Str (" ");
         H := Homonym (H);
      end loop;
   end W_Homonyms;

   ---------------
   -- W_Homonym --
   ---------------

   procedure W_Homonym (N : Node_Id) is
   begin
      W_Str (Image (Loc (N)));
      W_Str ("(");
      if Kind (Node (N)) = K_Scoped_Name then
         W_Str ("S");
      elsif Immediately_Visible (N) then
         W_Str ("V");
      elsif Potentially_Visible (N) then
         W_Str ("v");
      else
         W_Str ("?");
      end if;
      W_Str (") ");
   end W_Homonym;

   --------------------------
   -- W_Scoped_Identifiers --
   --------------------------

   procedure W_Scoped_Identifiers (S : Node_Id) is
      C : Node_Id := Scoped_Identifiers (S);
   begin
      W_Str ("scoped identifiers =");
      while Present (C) loop
         W_Str (" ");
         Write_Name (Name (C));
         C := Next_Node (C);
      end loop;
      W_Eol;
   end W_Scoped_Identifiers;

end Scopes;

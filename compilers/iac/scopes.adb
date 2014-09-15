------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                               S C O P E S                                --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2005-2014, Free Software Foundation, Inc.          --
--                                                                          --
-- This is free software;  you can redistribute it  and/or modify it  under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for  more details.                                               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
--                  PolyORB is maintained by AdaCore                        --
--                     (email: sales@adacore.com)                           --
--                                                                          --
------------------------------------------------------------------------------

with GNAT.Table;

with Errors;    use Errors;
with Locations; use Locations;
with Namet;     use Namet;

with Frontend.Debug;  use Frontend.Debug;
with Frontend.Nodes;  use Frontend.Nodes;
with Frontend.Nutils; use Frontend.Nutils;

package body Scopes is

   type Scope_Stack_Entry is record
      Node : Node_Id;
   end record;

   No_Scope_Depth : constant Int := -1;
   package Scope_Stack is
      new GNAT.Table (Scope_Stack_Entry, Int, No_Scope_Depth + 1, 10, 10);

   use Scope_Stack;

   procedure W_Homonym  (N : Node_Id);
   procedure W_Homonyms (N : Node_Id);
   procedure W_Scoped_Identifiers (S : Node_Id);

   procedure Insert_Into_Homonyms (N : Node_Id);
   --  Insert into homonyms chain if not already there

   procedure Remove_From_Homonyms (N : Node_Id);
   --  Remove from homonyms chain when no longer needed

   procedure Remove_From_Scope (Homonym : Node_Id; Scope : Node_Id);
   --  Remove from Scope the first entity in Homonym chain

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

   procedure Enter_Name_In_Scope (N : Node_Id) is

      procedure Display_Conflict (N, C : Node_Id);
      --  Output that N conflicts with C

      function Is_Inherited (E : Node_Id) return Boolean;
      --  To introduce an inherited entity in the scope of an
      --  interface, we introduce an identifier corresponding to this
      --  entity. However, the identifier of this entity is different
      --  from this new identifier. In particular, the original
      --  identifier refers to the original scope in which the entity
      --  was defined. To decide whether an entity is inherited or
      --  not, we check that the scope of the original identifier is
      --  not null (otherwise, it is a newly-added entity) and that
      --  this scope is different from the current scope.

      ----------------------
      -- Display_Conflict --
      ----------------------

      procedure Display_Conflict (N, C : Node_Id) is
      begin
         Error_Loc  (1) := Loc      (N);
         Error_Loc  (2) := Loc      (C);
         Error_Name (1) := IDL_Name (N);
         if Kind (C) = K_Scoped_Name then
            DE ("#conflicts with scoped name!");
         else
            DE ("#conflicts with declaration!");
         end if;
      end Display_Conflict;

      ------------------
      -- Is_Inherited --
      ------------------

      function Is_Inherited (E : Node_Id) return Boolean is
         S : constant Node_Id := Scope_Entity (Identifier (E));

      begin
         return Present (S) and then S /= Current_Scope;
      end Is_Inherited;

      E  : constant Node_Id := Corresponding_Entity (N);
      S  : constant Node_Id := Current_Scope;
      C  : Node_Id := Node_In_Current_Scope (N);
      H  : Node_Id;
      KC : Node_Kind;
      KE : constant Node_Kind := Kind (E);
      KS : constant Node_Kind := Kind (S);

   --  Start of processing for Enter_Name_In_Scope

   begin
      if D_Scopes then
         W_Str ("enter name ");
         Write_Name (Name (N));
         W_Eol;
      end if;

      if Present (C) then
         KC := Kind (C);
         H  := Identifier (C);

         --  This same entity is already in the scope

         if C = E then
            return;

         --  This name is already in the scope and the scoped name has
         --  not to be introduced.

         elsif KE = K_Scoped_Name then
            return;

         --  The current entity conflicts with the current scope

         elsif C = S then
            Display_Conflict (N, S);
            return;

         --  This entity is an extension of a module. Reload the
         --  previous scope.

         elsif KC = K_Module
           and then KE = K_Module
         then
            Set_Scoped_Identifiers (E, Scoped_Identifiers (C));
            Set_Reopened (C, True);

         --  If the current entity is a scoped name, it has been
         --  introduced on purpose and cannot be removed.

         elsif KC = K_Scoped_Name then
            Display_Conflict (N, C);
            return;

         --  If the current entity is a forward entity then we can
         --  freely override it and enter the new entity.

         elsif Is_A_Forward_Of (C, E) then

            --  We do not handle forward structures or unions

            if KC = K_Forward_Interface_Declaration
              or else KC = K_Value_Forward_Declaration
            then
               Set_Forward       (C, E);
               Remove_From_Scope (H, Scope_Entity (H));
            end if;

         elsif KS = K_Interface_Declaration
           or else KS = K_Value_Declaration
           or else KS = K_Abstract_Value_Declaration
         then

            --  If the current entity is an attribute or an operation,
            --  then it cannot be overridden.

            if KC = K_Attribute_Declaration
              or else KC = K_Operation_Declaration
            then
               Display_Conflict (N, C);
               return;
            end if;

            --  If the current entity is already in this scope, we
            --  have a conflict and the entity cannot be overridden.

            if not Is_Inherited (C) then
               Display_Conflict (N, C);
               return;
            end if;

            --  If the new entity is not inherited, remove all the
            --  inherited occurrences since they are now overridden.

            if not Is_Inherited (E) then
               while Is_Inherited (C) loop
                  Remove_From_Scope (H, S);
                  C := Node_In_Current_Scope (N);
                  exit when No (C);
                  H := Identifier (C);
               end loop;
               pragma Assert (No (C));
            end if;

         else
            Display_Conflict (N, C);
            return;
         end if;
      end if;

      Insert_Into_Homonyms (N);
      if No (Scope_Entity (N)) then
         Set_Scope_Entity (N, S);
      end if;
      Set_Potential_Scope    (N, S);
      Set_Visible            (N, True);
      Set_Next_Entity        (N, Scoped_Identifiers (S));
      Set_Scoped_Identifiers (S, N);
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

   begin
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
         if Scope_Entity (C) = S
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

         --  In order to parse the <orb.idl> file, we must accept, in an
         --  operation, that the parameter and its type have the same names
         --  when the type is an interface or a valuetype.

         if Kind (X) = K_Scoped_Name and then
           (Kind (Reference (X)) = K_Interface_Declaration or else
            Kind (Reference (X)) = K_Forward_Interface_Declaration or else
            Kind (Reference (X)) = K_Value_Declaration or else
            Kind (Reference (X)) = K_Value_Forward_Declaration or else
            Kind (Reference (X)) = K_Type_Declaration or else
            Kind (Reference (X)) = K_Structure_Type or else
            Kind (Reference (X)) = K_Union_Type or else
            Kind (Reference (X)) = K_Enumeration_Type) and then
           (Kind (S) = K_Operation_Declaration or else
            Kind (S) = K_Structure_Type or else
            Kind (S) = K_Union_Type)
         then
            null;

         elsif Potential_Scope (H) = S then
            return X;

         elsif X = S then

            --  The name of an interface, value type, struct, union,
            --  exception or a module may not be redefined within the
            --  immediate scope of the interface, value type, struct,
            --  union, exception, or the module.

            case Kind (S) is
               when K_Interface_Declaration
                 | K_Forward_Interface_Declaration
                 | K_Value_Declaration
                 | K_Value_Forward_Declaration
                 | K_Structure_Type
                 | K_Forward_Structure_Type
                 | K_Union_Type
                 | K_Forward_Union_Type
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

   procedure Pop_Scope is
      S : constant Node_Id := Current_Scope;
      C : Node_Id;
      E : Node_Id;
      N : Node_Id;

   begin
      if D_Scopes then
         W_Str ("pop scope """);
         if Kind (S) /= K_Specification then
            Write_Name (Name (Identifier (S)));
         end if;
         W_Str ("""");
         W_Eol;
      end if;

      --  Pop scope

      Decrement_Last;
      if Last = First then
         return;
      end if;

      --  When the previous scope was a type name that is nested in a
      --  non-module scope definition, the potential scope extends to
      --  the enclosing non-module scope. We introduced the scoped
      --  names in the enclosing scope.

      declare
         Export : constant Boolean :=
           Is_Noninterface_Type (S) and then Is_A_Non_Module (Current_Scope);
      begin
         C := Scoped_Identifiers (S);
         while Present (C) loop
            Set_Visible (C, False);
            Remove_From_Homonyms (C);
            if Export then
               E := Corresponding_Entity (C);
               if Kind (E) = K_Scoped_Name then
                  N := Identifier (E);
                  N := Make_Identifier
                    (Loc (N), Name (N),
                     Corresponding_Entity (N),
                     Scope_Entity (N));
                  Set_Potential_Scope  (N, S);
                  Enter_Name_In_Scope  (N);
               end if;
            end if;
            C := Next_Entity (C);
         end loop;
      end;

      if D_Scopes then
         W_Str ("show scope """);
         if Kind (Current_Scope) /= K_Specification then
            Write_Name (Name (Identifier (Current_Scope)));
         end if;
         W_Str (""" ");
         W_Scoped_Identifiers (Current_Scope);
         W_Eol;
      end if;
   end Pop_Scope;

   ----------------
   -- Push_Scope --
   ----------------

   procedure Push_Scope (S : Node_Id) is
      I : Node_Id;

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

      I := Scoped_Identifiers (S);
      while Present (I) loop
         Insert_Into_Homonyms (I);
         Set_Visible (I, True);
         Set_Scope_Entity (I, S);
         Set_Potential_Scope (I, S);
         I := Next_Entity (I);
      end loop;
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

   -----------------------
   -- Remove_From_Scope --
   -----------------------

   procedure Remove_From_Scope (Homonym : Node_Id; Scope : Node_Id) is
      Identifier : Node_Id := Scoped_Identifiers (Scope);
      Parent     : Node_Id := Identifier;
      Entity     : constant Node_Id := Corresponding_Entity (Homonym);

   begin
      if Entity = Corresponding_Entity (Identifier) then
         Set_Scoped_Identifiers (Scope, Next_Entity (Identifier));

      else
         loop
            Identifier := Next_Entity (Parent);
            exit when Entity = Corresponding_Entity (Identifier);
            Parent := Identifier;
         end loop;
         Set_Next_Entity (Parent, Next_Entity (Identifier));
      end if;

      if D_Scopes then
         W_Str      ("remove ");
         W_Homonym  (Identifier);
         W_Str      (" from scope");
      end if;

      Set_Next_Entity (Identifier, No_Node);
      Set_Visible (Identifier, False);
      Remove_From_Homonyms    (Identifier);
   end Remove_From_Scope;

   ------------------
   -- Visible_Node --
   ------------------

   function Visible_Node (N : Node_Id) return Node_Id
   is
      H : constant Node_Id := First_Homonym (N);
      E : Node_Id;

   begin
      if Present (H) then
         E := Corresponding_Entity (H);

         --  The current visible entity has already been entered in the scope

         if Kind (E) = K_Scoped_Name then
            return Reference (E);
         end if;

         if Visible (H) then
            return Corresponding_Entity (H);
         end if;
      end if;

      Error_Loc  (1) := Loc      (N);
      Error_Name (1) := IDL_Name (N);
      DE ("#is undefined");

      return No_Node;
   end Visible_Node;

   ---------------
   -- W_Homonym --
   ---------------

   procedure W_Homonym (N : Node_Id) is
   begin
      W_Str (Image (Loc (N)));
      W_Str ("(");
      if Kind (Corresponding_Entity (N)) = K_Scoped_Name then
         W_Str ("S");
      elsif Visible (N) then
         W_Str ("V");
      else
         W_Str ("?");
      end if;
      W_Str (")");
   end W_Homonym;

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

with Types;  use Types;

with Backend.BE_Ada.Nodes;   use Backend.BE_Ada.Nodes;
with Backend.BE_Ada.Nutils;  use Backend.BE_Ada.Nutils;

with Frontend.Nodes;         use Frontend.Nodes;
with Frontend.Nutils;

package body Backend.BE_Ada.Expand is

   package BEN renames Backend.BE_Ada.Nodes;
   package FEN renames Frontend.Nodes;
   package FEU renames Frontend.Nutils;

   procedure Expand_Exception_Declaration (Entity : Node_Id);
   procedure Expand_Forward_Interface_Declaration (Entity : Node_Id);
   procedure Expand_Interface_Declaration (Entity : Node_Id);
   procedure Expand_Module (Entity : Node_Id);
   procedure Expand_Specification (Entity : Node_Id);
   procedure Expand_Structure_Type (Entity : Node_Id);
   procedure Expand_Type_Declaration (Entity : Node_Id);
   procedure Expand_Union_Type (Entity : Node_Id);

   --  This function tests if the type spec is an interface based type
   --  and then tests if the scopr entity of this interface is the same as the
   --  declarator.
   function  Is_Forward_Necessary
     (Entity      : Node_Id;
      Type_Spec   : Node_Id)
     return Boolean;

   --  This procedure does the following :
   --  * Insert a definition of the structure type corresponding to the
   --    "Member" type spec outside the enclosing entity "Entity".
   --  * Replaces the type spec of "Member" by a scoped name which represents
   --    the new defined type.
   procedure Define_Type_Outside
     (Member : Node_Id;
      Entity : Node_Id);

   --  This function :
   --  * Adds a forward declaration fro the interfac to the IDL tree if it is
   --    not already forwarded
   --  * Sets the interface as forwarded
   --  * Returns the new or the already existing node.
   function  Add_Forward_Declaration (Interface : Node_Id) return Node_Id;

   -----------------------
   -- Expand_Designator --
   -----------------------

   function Expand_Designator
     (N               : Node_Id;
      Add_With_Clause : Boolean := True)
     return Node_Id
   is
      P  : Node_Id;
      D  : Node_Id := No_Node;
      X  : Node_Id := N;
      FE : Node_Id;

   begin
      case BEN.Kind (N) is
         when K_Full_Type_Declaration |
           K_Subprogram_Specification =>
            P  := Parent (X);
            FE := FE_Node (X);

         when K_Object_Declaration
           | K_Exception_Declaration =>
            P  := Parent (X);
            FE := FE_Node (X);

         when K_Package_Specification =>
            X  := Package_Declaration (N);
            P  := Parent (X);
            FE := FE_Node (IDL_Unit (X));

         when K_Package_Declaration =>
            P  := Parent (N);
            FE := FE_Node (IDL_Unit (X));

         when others =>
            raise Program_Error;
      end case;

      D := Defining_Identifier_To_Designator
        (N           => Defining_Identifier (X),
         Keep_Parent => False);

      if Present (FE) then
         Set_FE_Node (D, FE);
      end if;

      if No (P) then
         return D;
      end if;

      --  This handles the particular case of the forward declaration of
      --  interfaces.

      if BEN.Kind (N) = K_Full_Type_Declaration
        and then Present (Parent_Unit_Name (Defining_Identifier (N)))
        and then BEN.Kind
        (Corresponding_Node
         (Parent_Unit_Name
          (Defining_Identifier
           (N)))) = K_Package_Instantiation
      then
         Set_Correct_Parent_Unit_Name
           (D,
            Parent_Unit_Name (Defining_Identifier (N)));
         P := Expand_Designator (P);
      else
         Set_Correct_Parent_Unit_Name
           (D, Expand_Designator (P, False));
         P := BEN.Parent_Unit_Name (D);
      end if;

      --  Adding the with clause

      if Add_With_Clause and then Present (P) then
         Add_With_Package (P);
      end if;

      return D;
   end Expand_Designator;

   --------------------------
   -- Is_Forward_Necessary --
   --------------------------

   function Is_Forward_Necessary
     (Entity      : Node_Id;
      Type_Spec   : Node_Id)
     return Boolean
   is
      Result       : Boolean := False;
      S_Entity : Node_Id;
      S_Type_Spec  : Node_Id;
   begin
      pragma Assert (FEN.Kind (Entity) = K_Simple_Declarator
                     or else
                     FEN.Kind (Entity) = K_Complex_Declarator
                     or else
                     FEN.Kind (Entity) = K_Structure_Type
                     or else
                     FEN.Kind (Entity) = K_Union_Type
                     or else
                     FEN.Kind (Entity) = K_Exception_Declaration);
      S_Entity := FEN.Scope_Entity (FEN.Identifier (Entity));
      if FEN.Kind (Type_Spec) = K_Scoped_Name
        and then FEN.Kind (FEN.Reference (Type_Spec)) = K_Interface_Declaration
      then
         S_Type_Spec := FEN.Scope_Entity
           (FEN.Identifier
            (FEN.Reference
             (Type_Spec)));
         if S_Type_Spec = S_Entity then
            Result := True;
         end if;
      end if;
      return Result;
   end Is_Forward_Necessary;

   -----------------------------
   -- Add_Forward_Declaration --
   -----------------------------

   function  Add_Forward_Declaration (Interface : Node_Id) return Node_Id is
      Forward_Node : Node_Id;
      F_Identifier : Node_Id;
      Definitions  : List_Id;
      Definition   : Node_Id;
   begin
      pragma Assert (FEN.Kind (Interface) = K_Interface_Declaration);
      Definitions := FEN.Definitions (Scope_Entity (Identifier (Interface)));
      if Is_Forwarded (Interface) then
         --  Looking for the forward declaration
         Definition := First_Entity (Definitions);
         while Present (Definition) loop
            if FEN.Kind (Definition) = K_Forward_Interface_Declaration
              and then Forward (Definition) = Interface then
               return Definition;
            end if;
            Definition := Next_Entity (Definition);
         end loop;
         --  We cannot reach this code
         raise Program_Error;
      else
         Set_Forwarded (Interface);
         Forward_Node := FEU.New_Node
           (K_Forward_Interface_Declaration,
            FEN.Loc (Interface));
         Set_Forward (Forward_Node, Interface);

         F_Identifier := FEU.Make_Identifier
           (Loc          => FEN.Loc (Identifier (Interface)),
            IDL_Name     => IDL_Name (Identifier (Interface)),
            Node         => No_Node,
            Scope_Entity => Scope_Entity (Identifier (Interface)));
         FEU.Bind_Identifier_To_Entity (F_Identifier, Forward_Node);

         Set_Is_Abstract_Interface
           (Forward_Node,
            Is_Abstract_Interface (Interface));
         Set_Is_Local_Interface
           (Forward_Node,
            Is_Local_Interface (Interface));

         --  Insert the forward declaration immediatly before the interface
         --  declaration
         Definition := First_Entity (Definitions);
         if Definition = Interface then
            Set_Next_Entity (Forward_Node, Definition);
            Set_First_Entity (Definitions, Forward_Node);
            return Forward_Node;
         end if;
         while Present (Definition) loop
            exit when Next_Entity (Definition) = Interface;
            Definition := Next_Entity (Definition);
         end loop;
         FEU.Insert_After_Node (Forward_Node, Definition);
         return Forward_Node;
      end if;
   end Add_Forward_Declaration;

   -------------------------
   -- Define_Type_Outside --
   -------------------------

   procedure Define_Type_Outside
     (Member : Node_Id;
      Entity : Node_Id)
   is
      Type_Spec       : Node_Id;
      New_Identifier  : Node_Id;
      New_Scoped_Name : Node_Id;
      Definitions     : List_Id;
      Definition      : Node_Id;
   begin
      Type_Spec := FEN.Type_Spec (Member);
      pragma Assert (FEN.Kind (Type_Spec) = K_Structure_Type);

      --  Create the scoped name which will be the new type spec
      New_Identifier := FEU.Make_Identifier
        (Loc          => FEN.Loc (Identifier (Type_Spec)),
         IDL_Name     => IDL_Name (Identifier (Type_Spec)),
         Node         => No_Node,
         Scope_Entity => Type_Spec);

      New_Scoped_Name := FEU.Make_Scoped_Name
        (Loc        => FEN.Loc (New_Identifier),
         Identifier => New_Identifier,
         Parent     => No_Node,
         Reference  => Type_Spec);
      --  Modifying the type spec of the memeber
      Set_Type_Spec (Member, New_Scoped_Name);

      --  Move the Type_Spec declaration immediatly before the declaration
      --  of entity
      Definitions := FEN.Definitions
        (FEN.Scope_Entity
         (FEN.Identifier
          (Entity)));
      Definition := First_Entity (Definitions);
      if Definition = Entity then
         Set_Next_Entity (Type_Spec, Definition);
         Set_First_Entity (Definitions, Type_Spec);
      end if;
      while Present (Definition) loop
         exit when Next_Entity (Definition) = Entity;
         Definition := Next_Entity (Definition);
      end loop;
      FEU.Insert_After_Node (Type_Spec, Definition);
      --  Modify the Scope_Entity and the Potential_Scope of the Type_Spec
      FEN.Set_Scope_Entity
        (FEN.Identifier (Type_Spec),
         Scope_Entity (Identifier (Entity)));
      FEN.Set_Potential_Scope
        (FEN.Identifier (Type_Spec),
         Potential_Scope (Identifier (Entity)));
      --  We expand the new created type
      Expand_Structure_Type (Type_Spec);
   end Define_Type_Outside;

   ------------
   -- Expand --
   ------------

   --  The goals of the expansion phase are :
   --  * Adding the necessary forwards which are implicit in the IDL tree
   --  * Modify the types in the operation declarations; attribute declarations
   --    exception declarations so that they take in account the forward added
   --  * remove the unnexessary forwards.
   procedure Expand (Entity : Node_Id) is
   begin
      case FEN.Kind (Entity) is
         when K_Specification =>
            Expand_Specification (Entity);

         when K_Exception_Declaration =>
            Expand_Exception_Declaration (Entity);

         when K_Forward_Interface_Declaration =>
            Expand_Forward_Interface_Declaration (Entity);

         when K_Interface_Declaration =>
            Expand_Interface_Declaration (Entity);

         when K_Structure_Type =>
            Expand_Structure_Type (Entity);

         when K_Union_Type =>
            Expand_Union_Type (Entity);

         when K_Type_Declaration =>
            Expand_Type_Declaration (Entity);

         when K_Module =>
            Expand_Module (Entity);

         when others =>
            null;
      end case;
   end Expand;

   ----------------------------------
   -- Expand_Exception_Declaration --
   ----------------------------------

   procedure Expand_Exception_Declaration (Entity : Node_Id) is
      Members     : List_Id;
      Member      : Node_Id;
      Declarator  : Node_Id;
      Member_Type : Node_Id;
   begin
      Members := FEN.Members (Entity);
      Member := First_Entity (Members);
      Main_Loop :
      while Present (Member) loop
         Declarator := First_Entity (Declarators (Member));
         Member_Type := Type_Spec (Member);
         while Present (Declarator) loop
            if Is_Forward_Necessary (Entity, Member_Type) then
               Set_Reference
                 (Member_Type,
                  Add_Forward_Declaration
                  (FEN.Reference
                   (Member_Type)));
               exit Main_Loop;
            end if;
            Declarator := Next_Entity (Declarator);
         end loop;
         --  If the member type is a structure type, extract the nested
         --  structure definition outside.
         if FEN.Kind (Member_Type) = FEN.K_Structure_Type then
            Define_Type_Outside
              (Member => Member,
               Entity => Entity);
         end if;
         Member := Next_Entity (Member);
      end loop Main_Loop;
   end Expand_Exception_Declaration;

   ------------------------------------------
   -- Expand_Forward_Interface_Declaration --
   ------------------------------------------

   procedure Expand_Forward_Interface_Declaration (Entity : Node_Id) is
   begin
      Set_Forwarded (Forward (Entity));
   end Expand_Forward_Interface_Declaration;

   ----------------------------------
   -- Expand_Interface_Declaration --
   ----------------------------------

   procedure Expand_Interface_Declaration (Entity : Node_Id) is
      N       : Node_Id;
   begin
      N := First_Entity (Interface_Body (Entity));
      while Present (N) loop
         Expand (N);
         N := Next_Entity (N);
      end loop;
   end Expand_Interface_Declaration;

   -------------------
   -- Expand_Module --
   -------------------

   procedure Expand_Module (Entity : Node_Id) is
      D : Node_Id;
   begin
      D := First_Entity (Definitions (Entity));
      while Present (D) loop
         Expand (D);
         D := Next_Entity (D);
      end loop;
   end Expand_Module;

   --------------------------
   -- Expand_Specification --
   --------------------------

   procedure Expand_Specification (Entity : Node_Id) is
      Definition : Node_Id;
   begin
      Definition := First_Entity (Definitions (Entity));
      while Present (Definition) loop
         Expand (Definition);
         Definition := Next_Entity (Definition);
      end loop;
   end Expand_Specification;

   ---------------------------
   -- Expand_Structure_Type --
   ---------------------------

   procedure Expand_Structure_Type (Entity : Node_Id) is
      Members     : List_Id;
      Member      : Node_Id;
      Declarator  : Node_Id;
      Member_Type : Node_Id;
   begin
      Members := FEN.Members (Entity);
      Member := First_Entity (Members);
      Main_Loop :
      while Present (Member) loop
         Declarator := First_Entity (Declarators (Member));
         Member_Type := Type_Spec (Member);
         while Present (Declarator) loop
            if Is_Forward_Necessary (Entity, Member_Type) then
               Set_Reference
                 (Member_Type,
                  Add_Forward_Declaration
                  (FEN.Reference
                   (Member_Type)));
            end if;

            Declarator := Next_Entity (Declarator);
         end loop;
         --  If the member type is a structure type, extract the nested
         --  structure definition outside.
         if FEN.Kind (Member_Type) = FEN.K_Structure_Type then
            Define_Type_Outside
              (Member => Member,
               Entity => Entity);
         end if;
         Member := Next_Entity (Member);
      end loop Main_Loop;
   end Expand_Structure_Type;

   -----------------------------
   -- Expand_Type_Declaration --
   -----------------------------

   procedure Expand_Type_Declaration (Entity : Node_Id) is
      D                : Node_Id;
      Type_Spec_Node   : Node_Id;
   begin
      Type_Spec_Node := Type_Spec (Entity);

      --  For the particular case of sequences, we change the type spec
      --  of the sequence.
      if FEN.Kind (Type_Spec_Node) = K_Sequence_Type then
         Type_Spec_Node := Type_Spec (Type_Spec_Node);
      end if;

      D := First_Entity (Declarators (Entity));
      while Present (D) loop
         if Is_Forward_Necessary (D, Type_Spec_Node) then
            Set_Reference
              (Type_Spec_Node,
               Add_Forward_Declaration
               (FEN.Reference
                (Type_Spec_Node)));
            exit;
         end if;
         D := Next_Entity (D);
      end loop;
   end Expand_Type_Declaration;

   -----------------------
   -- Expand_Union_Type --
   -----------------------

   procedure Expand_Union_Type (Entity : Node_Id) is
      Alternatives : List_Id;
      Alternative  : Node_Id;
      Element      : Node_Id;
      Type_Spec    : Node_Id;
   begin
      Alternatives := Switch_Type_Body (Entity);
      Alternative := First_Entity (Alternatives);
      while Present (Alternative) loop
         Element := FEN.Element (Alternative);
         Type_Spec := FEN.Type_Spec (Element);
         if Is_Forward_Necessary (Entity, Type_Spec) then
            Set_Reference
              (Type_Spec,
               Add_Forward_Declaration
               (FEN.Reference
                (Type_Spec)));
         end if;

         if FEN.Kind (Type_Spec) = FEN.K_Structure_Type then
            Define_Type_Outside
              (Member  => Element,
               Entity  => Entity);
         end if;
         Alternative := Next_Entity (Alternative);
      end loop;
   end Expand_Union_Type;

end Backend.BE_Ada.Expand;

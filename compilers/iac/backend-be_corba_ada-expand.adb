------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--          B A C K E N D . B E _ C O R B A _ A D A . E X P A N D           --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2005-2010, Free Software Foundation, Inc.          --
--                                                                          --
-- PolyORB is free software; you  can  redistribute  it and/or modify it    --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. PolyORB is distributed  in the hope that it will be  useful,    --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with PolyORB; see file COPYING. If    --
-- not, write to the Free Software Foundation, 51 Franklin Street, Fifth    --
-- Floor, Boston, MA 02111-1301, USA.                                       --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
--                  PolyORB is maintained by AdaCore                        --
--                     (email: sales@adacore.com)                           --
--                                                                          --
------------------------------------------------------------------------------

with Namet;     use Namet;
with Locations; use Locations;
with Values;    use Values;

with Backend.BE_CORBA_Ada.Nutils;     use Backend.BE_CORBA_Ada.Nutils;
with Backend.BE_CORBA_Ada.IDL_To_Ada; use Backend.BE_CORBA_Ada.IDL_To_Ada;

with Frontend.Nodes;         use Frontend.Nodes;
with Frontend.Nutils;
with Parser;
with Lexer;

package body Backend.BE_CORBA_Ada.Expand is

   package FEN renames Frontend.Nodes;
   package FEU renames Frontend.Nutils;

   procedure Expand_Attribute_Declaration (Entity : Node_Id);
   procedure Expand_Exception_Declaration (Entity : Node_Id);
   procedure Expand_Forward_Interface_Declaration (Entity : Node_Id);
   procedure Expand_Interface_Declaration (Entity : Node_Id);
   procedure Expand_Module (Entity : Node_Id);
   procedure Expand_Specification (Entity : Node_Id);
   procedure Expand_Structure_Type (Entity : Node_Id);
   procedure Expand_Type_Declaration (Entity : Node_Id);
   procedure Expand_Union_Type (Entity : Node_Id);
   procedure Expand_Constant_Declaration (Entity : Node_Id);
   procedure Expand_Operation_Declaration (Entity : Node_Id);
   procedure Expand_Element (Entity : Node_Id);
   procedure Expand_Member (Entity : Node_Id);
   procedure Expand_Parameter_Declaration (Entity : Node_Id)
     renames Expand_Element;

   procedure Forward_Current_Interface_Designing_Components
     (Interface_Node : Node_Id;
      Type_Spec_Node : Node_Id);
   --  For Sequence types, the item parameter cannot:
   --  * denote the current interface
   --  * have a component whose type is the current interface
   --  The instantiation of the sequences generic package would
   --  otherwise cause freezing.

   function  Is_Forward_Necessary
     (Entity    : Node_Id;
      Type_Spec : Node_Id)
     return Boolean;
   --  Return True if the type spec is an interface based type and
   --  then if the scope entity of this interface is the same as
   --  Entity's scope entity.

   procedure Insert_Definition (E, Before, In_Container : Node_Id);
   --  Inserts E in the container's definition list immediately before
   --  Before. The container must be a module, specification, or interface
   --  declaration, and Before must be in its list of definitions.

   procedure Define_Array_Type_Outside
     (Member : Node_Id;
      Entity : Node_Id;
      Before : Node_Id);
   --  This procedure does the following :
   --  * Insert a definition of the array type corresponding to
   --  the "Member" type spec outside the enclosing entity "Entity".
   --  * Replaces the type spec of "Member" by a scoped name which
   --  represents the new defined type.

   procedure Define_Structure_Type_Outside
     (Member : Node_Id;
      Entity : Node_Id;
      Before : Node_Id);
   --  This procedure does the following :
   --  * Insert a definition of the structure type corresponding to
   --  the "Member" type spec outside the enclosing entity "Entity".
   --  * Replaces the type spec of "Member" by a scoped name which
   --  represents the new defined type.

   function  Add_Forward_Declaration (Iface : Node_Id) return Node_Id;
   --  This function :
   --  * Adds a forward declaration fro the interface to the IDL tree
   --  if it is not already forwarded
   --  * Sets the interface as forwarded
   --  * Returns the new or the already existing node.

   function Is_CORBA_IR_Entity (Entity : Node_Id) return Boolean;
   --  This function returns True if the entity passed as parameter
   --  should be generated in the CORBA.Repository_Root package.

   function Is_CORBA_Sequence (Entity : Node_Id) return Boolean;
   --  This function returns True if the entity passed as parameter
   --  should be generated in the CORBA.IDL_Sequences package.

   procedure Handle_Anonymous_Type
     (Entity : Node_Id;
      Parent : Node_Id;
      Before : Node_Id);
   --  This procedure looks whether the type spec of the entity is an
   --  anonymous type and adds a type definition before the 'Before'
   --  entity declaration in the 'Parent' node.

   Anonymous_Type_Index_Value : Nat := 0;
   function New_Anonymous_Type_Index return Nat;
   --  The two entities below are used to avoid name collision when
   --  handling anonymous types.

   function Has_Complex_Declarators (Entity : Node_Id) return Boolean;
   --  This function returns True when the type declaration has one or
   --  more complex declarators.

   ----------------------------------------------------
   -- Forward_Current_Interface_Designing_Components --
   ----------------------------------------------------

   procedure Forward_Current_Interface_Designing_Components
     (Interface_Node : Node_Id;
      Type_Spec_Node : Node_Id)
   is
      Members      : List_Id;
      Member       : Node_Id;
      Alternatives : List_Id;
      Alternative  : Node_Id;
      Type_Spec    : Node_Id;
   begin
      --  The anonymous nested types are deprecated in CORBA 3.0.3, so
      --  the only case in which we can find a interface type
      --  component is the case of a Scoped_Name type spec.

      if FEN.Kind (Type_Spec_Node) = K_Scoped_Name then
         if FEN.Reference (Type_Spec_Node) = Interface_Node then
            Set_Reference
              (Type_Spec_Node,
               Add_Forward_Declaration
               (FEN.Reference
                (Type_Spec_Node)));
         elsif  FEN.Kind (FEN.Reference (Type_Spec_Node)) =
           K_Simple_Declarator
           or else
           FEN.Kind (FEN.Reference (Type_Spec_Node)) =
           K_Complex_Declarator
         then
            Type_Spec := FEN.Type_Spec
              (Declaration
               (Reference
                (Type_Spec_Node)));
            Forward_Current_Interface_Designing_Components
              (Interface_Node, Type_Spec);
         elsif FEN.Kind (FEN.Reference (Type_Spec_Node)) =
           K_Structure_Type
           or else FEN.Kind (FEN.Reference (Type_Spec_Node)) =
           K_Exception_Declaration
         then
            Members := FEN.Members (FEN.Reference (Type_Spec_Node));
            Member := First_Entity (Members);
            while Present (Member) loop
               Type_Spec := FEN.Type_Spec (Member);
               Forward_Current_Interface_Designing_Components
                 (Interface_Node, Type_Spec);
               Member := Next_Entity (Member);
            end loop;
         elsif  FEN.Kind (FEN.Reference (Type_Spec_Node)) =
           K_Union_Type
         then
            Alternatives := Switch_Type_Body (FEN.Reference (Type_Spec_Node));
            Alternative := First_Entity (Alternatives);
            while Present (Alternative) loop
               Type_Spec := FEN.Type_Spec (FEN.Element (Alternative));
               Forward_Current_Interface_Designing_Components
                 (Interface_Node, Type_Spec);
               Alternative := Next_Entity (Alternative);
            end loop;
         end if;
      end if;
   end Forward_Current_Interface_Designing_Components;

   --------------------------
   -- Is_Forward_Necessary --
   --------------------------

   function Is_Forward_Necessary
     (Entity    : Node_Id;
      Type_Spec : Node_Id)
     return Boolean
   is
      Result       : Boolean := False;
      S_Entity     : Node_Id;
      S_Type_Spec  : Node_Id;
   begin
      pragma Assert (FEN.Kind (Entity) = K_Simple_Declarator       or else
                     FEN.Kind (Entity) = K_Complex_Declarator      or else
                     FEN.Kind (Entity) = K_Structure_Type          or else
                     FEN.Kind (Entity) = K_Union_Type              or else
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

   function  Add_Forward_Declaration (Iface : Node_Id) return Node_Id is
      Forward_Node : Node_Id;
      F_Identifier : Node_Id;
      Definitions  : List_Id;
      Definition   : Node_Id;
   begin
      pragma Assert (FEN.Kind (Iface) = K_Interface_Declaration);

      Definitions := FEN.Definitions (Scope_Entity (Identifier (Iface)));

      if Is_Forwarded (Iface) then
         --  Looking for the forward declaration

         Definition := First_Entity (Definitions);

         while Present (Definition) loop
            if FEN.Kind (Definition) = K_Forward_Interface_Declaration
              and then Forward (Definition) = Iface then
               return Definition;
            end if;

            Definition := Next_Entity (Definition);
         end loop;

         --  We cannot reach this code unless a serious problem
         --  occured during the parsing.

         raise Program_Error;
      else
         Set_Forwarded (Iface);
         Forward_Node := FEU.New_Node
           (K_Forward_Interface_Declaration,
            FEN.Loc (Iface));
         Set_Forward (Forward_Node, Iface);

         F_Identifier := FEU.Make_Identifier
           (Loc          => FEN.Loc (Identifier (Iface)),
            IDL_Name     => IDL_Name (Identifier (Iface)),
            Node         => No_Node,
            Scope_Entity => Scope_Entity (Identifier (Iface)));
         FEU.Bind_Identifier_To_Entity (F_Identifier, Forward_Node);

         Set_Is_Abstract_Interface
           (Forward_Node,
            Is_Abstract_Interface (Iface));
         Set_Is_Local_Interface
           (Forward_Node,
            Is_Local_Interface (Iface));

         --  Insert the forward declaration immediately before the interface
         --  declaration

         Definition := First_Entity (Definitions);

         if Definition = Iface then
            Set_Next_Entity (Forward_Node, Definition);
            Set_First_Entity (Definitions, Forward_Node);
            return Forward_Node;
         end if;

         while Present (Definition) loop
            exit when Next_Entity (Definition) = Iface;
            Definition := Next_Entity (Definition);
         end loop;

         FEU.Insert_After_Node (Forward_Node, Definition);
         return Forward_Node;
      end if;
   end Add_Forward_Declaration;

   -------------------------------
   -- Define_Array_Type_Outside --
   -------------------------------

   procedure Define_Array_Type_Outside
     (Member : Node_Id;
      Entity : Node_Id;
      Before : Node_Id)
   is
      Declarator            : Node_Id;
      Type_Spec             : Node_Id;
      Array_Id              : Node_Id;
      Container             : Node_Id;
      Old_Declarator_Id     : Node_Id;
      New_Type_Def          : Node_Id;
      New_Scoped_Name       : Node_Id;
      New_Simple_Declarator : Node_Id;
   begin
      --  The given member has to be created by the expander

      case FEN.Kind (Entity) is
         when K_Structure_Type
           | K_Exception_Declaration =>

            pragma Assert (FEU.Length (Declarators (Member)) = 1);

            Declarator := First_Entity (Declarators (Member));

            pragma Assert (FEN.Kind (Declarator) = K_Complex_Declarator);

         when K_Union_Type =>
            Declarator := FEN.Declarator (Member);

            pragma Assert (FEN.Kind (Declarator) = K_Complex_Declarator);

         when others =>
            declare
               Msg : constant String := "Cannot expand complex member in a "
                 & FEN.Node_Kind'Image (FEN.Kind (Entity));
            begin
               raise Program_Error with Msg;
            end;
      end case;

      --  Get the type spec of the member

      Type_Spec := FEN.Type_Spec (Member);

      --  Get the old identifier of the declarator

      Old_Declarator_Id := Identifier (Declarator);

      --  Get the declaration list depending on Container kind

      Container := FEN.Scope_Entity (FEN.Identifier (Entity));

      --  Create the identifier of the new array type

      Array_Id := FEU.Make_Identifier
        (Loc          => FEN.Loc (Type_Spec),
         IDL_Name     => Add_Suffix_To_Name
         ("_Array",
          FEN.IDL_Name (FEN.Identifier (Declarator))),
         Node         => No_Node,
         Scope_Entity => Container);

      --  Adjust the scope entity of the complex declarator identifier

      FEN.Set_Scope_Entity (Array_Id, Scope_Entity (Identifier (Entity)));
      FEN.Set_Potential_Scope
        (Array_Id, Potential_Scope (Identifier (Entity)));

      FEU.Bind_Identifier_To_Entity (Array_Id, Declarator);

      --  Create the new type definition

      New_Type_Def := FEU.New_Node (K_Type_Declaration, FEN.Loc (Entity));

      Set_Type_Spec (New_Type_Def, Type_Spec);
      Set_Declarators (New_Type_Def,
                       FEU.New_List (FEN.Loc (Entity)));
      FEU.Append_To (Declarators (New_Type_Def), Declarator);
      Set_Declaration (Declarator, New_Type_Def);

      --  Insert the new type declaration

      Insert_Definition (New_Type_Def, Before, In_Container => Container);

      --  Re-Create the identifier of the new array type

      Array_Id := FEU.Make_Identifier
        (Loc          => FEN.Loc (Type_Spec),
         IDL_Name     => Add_Suffix_To_Name
         ("_Array",
          FEN.IDL_Name (FEN.Identifier (Declarator))),
         Node         => No_Node,
         Scope_Entity => Entity);

      --  Create a scoped name to designate the new defined type

      New_Scoped_Name := FEU.Make_Scoped_Name
        (Loc        => FEN.Loc (Array_Id),
         Identifier => Array_Id,
         Parent     => No_Node,
         Reference  => Declarator);

      FEU.Bind_Identifier_To_Entity (Array_Id, New_Scoped_Name);

      --  Create a new simple declarator

      New_Simple_Declarator := FEU.New_Node
        (K_Simple_Declarator, FEN.Loc (Member));
      Set_Declaration (New_Simple_Declarator, Member);
      Set_Identifier (New_Simple_Declarator, Old_Declarator_Id);

      --  Modify the member declaration

      Set_Type_Spec (Member, New_Scoped_Name);

      case FEN.Kind (Entity) is
         when K_Structure_Type
           | K_Exception_Declaration =>
            Set_Declarators
              (Member,
               FEU.New_List (FEN.Loc (Member)));
            FEU.Append_To (Declarators (Member), New_Simple_Declarator);

         when K_Union_Type =>
            Set_Declarator (Member, New_Simple_Declarator);

         when others =>
            null;
      end case;
   end Define_Array_Type_Outside;

   -----------------------------------
   -- Define_Structure_Type_Outside --
   -----------------------------------

   procedure Define_Structure_Type_Outside
     (Member : Node_Id;
      Entity : Node_Id;
      Before : Node_Id)
   is
      Type_Spec       : Node_Id;
      New_Identifier  : Node_Id;
      New_Scoped_Name : Node_Id;
      Container       : Node_Id;
   begin
      Type_Spec := FEN.Type_Spec (Member);

      pragma Assert (FEN.Kind (Type_Spec) = K_Structure_Type);

      --  The new type spec scoped name

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

      --  Modifying the type spec of the member

      Set_Type_Spec (Member, New_Scoped_Name);

      --  Move the Type_Spec declaration immediately before the
      --  declaration of entity.

      Container := FEN.Scope_Entity (FEN.Identifier (Entity));

      Insert_Definition (Type_Spec, Before, In_Container => Container);

      --  Modify the Scope_Entity and the Potential_Scope of the Type_Spec

      FEN.Set_Scope_Entity
        (FEN.Identifier (Type_Spec),
         Scope_Entity (Identifier (Entity)));
      FEN.Set_Potential_Scope
        (FEN.Identifier (Type_Spec),
         Potential_Scope (Identifier (Entity)));

      --  Expand the new created type to detect any nested anonymous
      --  types, implicit forwards or complex declarators.

      Expand_Structure_Type (Type_Spec);
   end Define_Structure_Type_Outside;

   ---------------------------
   -- Handle_Anonymous_Type --
   ---------------------------

   procedure Handle_Anonymous_Type
     (Entity : Node_Id;
      Parent : Node_Id;
      Before : Node_Id)
   is
      Anon_Type_Prefix : constant String := "IDL_AT_";
      Anon_Type_Name   : Name_Id;
      B                : Int;
      New_Identifier   : Node_Id;
      New_Scoped_Name  : Node_Id;
      Declarator       : Node_Id;
      Node             : Node_Id;
      List             : List_Id;
      Entity_Type_Spec : Node_Id;
   begin
      --  The name of the type spec fields is different for union type
      --  nodes.

      if FEN.Kind (Entity) = K_Union_Type then
         Entity_Type_Spec := Switch_Type_Spec (Entity);
      else
         Entity_Type_Spec := Type_Spec (Entity);
      end if;

      case FEN.Kind (Entity_Type_Spec) is
         when K_Sequence_Type =>
            declare
               Max_S          : Value_Type;
               Type_Spec_Name : Name_Id;
            begin
               --  First of all, handle the type spec of the sequence

               Handle_Anonymous_Type (Entity_Type_Spec, Parent, Before);

               --  Begin the handling of the sequence type

               Set_Str_To_Name_Buffer (Anon_Type_Prefix);

               --  For type declaration, the expansion of the type
               --  does not occur only when there are complex
               --  declarators.

               if FEN.Kind (Entity) /= K_Type_Declaration
                 or else Has_Complex_Declarators (Entity)
               then
                  Add_Str_To_Name_Buffer ("Sequence_");

                  if Present (Max_Size (Entity_Type_Spec)) then
                     Max_S := FEU.Expr_Value (Max_Size (Entity_Type_Spec));
                     Add_Dnat_To_Name_Buffer (Dnat (Max_S.IVal));
                     Add_Char_To_Name_Buffer ('_');
                  end if;

                  Anon_Type_Name := Name_Find;

                  if Is_Base_Type (Type_Spec (Entity_Type_Spec)) then
                     Type_Spec_Name :=
                       (FEN.Image
                        (Base_Type
                         (Type_Spec
                          (Entity_Type_Spec))));

                  elsif FEN.Kind (Type_Spec (Entity_Type_Spec))
                    = K_Scoped_Name
                  then
                     Type_Spec_Name := FEU.Fully_Qualified_Name
                       (FEN.Identifier
                        (FEN.Reference
                         (Type_Spec
                          (Entity_Type_Spec))),
                        Separator => "_");
                  else
                     raise Program_Error;
                  end if;

                  Anon_Type_Name := Add_Suffix_To_Name
                    (Get_Name_String (Type_Spec_Name),
                     Anon_Type_Name);

                  --  If the type name consists of two or more words,
                  --  replace spaces by underscores.

                  Get_Name_String (Anon_Type_Name);

                  for Index in 1 .. Name_Len loop
                     if Name_Buffer (Index) = ' ' then
                        Name_Buffer (Index) := '_';
                     end if;
                  end loop;

                  Anon_Type_Name := Name_Find;
               else
                  --  Do not expand anything

                  return;
               end if;
            end;

         when K_String_Type
           | K_Wide_String_Type =>
            declare
               Max_S : Value_Type;
            begin
               Set_Str_To_Name_Buffer (Anon_Type_Prefix);

               --  For type declarations, the expansion of the type
               --  occurs only when there are complex declarators.

               if FEN.Kind (Entity) = K_Type_Declaration and then
                 not Has_Complex_Declarators (Entity)
               then
                  return;
               else
                  if FEN.Kind (Entity_Type_Spec) = K_Wide_String_Type then
                     Add_Str_To_Name_Buffer ("Wide_");
                  end if;

                  Add_Str_To_Name_Buffer ("String_");
                  Max_S := FEU.Expr_Value (Max_Size (Entity_Type_Spec));
                  Add_Dnat_To_Name_Buffer (Dnat (Max_S.IVal));
                  Anon_Type_Name := Name_Find;
               end if;
            end;

         when K_Fixed_Point_Type =>
            begin
               Set_Str_To_Name_Buffer (Anon_Type_Prefix);

               --  For type declaration, the expansion of the type
               --  does not occur only when there are complex
               --  declarators.

               if FEN.Kind (Entity) /= K_Type_Declaration
                 or else Has_Complex_Declarators (Entity)
               then
                  Add_Str_To_Name_Buffer ("Fixed_");
                  Add_Nat_To_Name_Buffer (Nat (N_Total (Entity_Type_Spec)));
                  Add_Char_To_Name_Buffer ('_');
                  Add_Nat_To_Name_Buffer (Nat (N_Scale (Entity_Type_Spec)));
                  Anon_Type_Name := Name_Find;
               else
                  --  We do not expand anything

                  return;
               end if;
            end;

         when K_Enumeration_Type =>
            begin
               Get_Name_String (IDL_Name (Identifier (Entity_Type_Spec)));
               Anon_Type_Name := Name_Find;
            end;

         when others =>
            return;
      end case;

      --  Verify that there is no other handled anonymous type with
      --  the same name in the 'Parent' scope.

      B := Get_Name_Table_Info (Anon_Type_Name);

      if B = Int (Parent) then
         Get_Name_String (Anon_Type_Name);
         Add_Char_To_Name_Buffer ('_');
         Add_Nat_To_Name_Buffer (New_Anonymous_Type_Index);
         Anon_Type_Name := Name_Find;
      end if;

      Set_Name_Table_Info (Anon_Type_Name, Int (Parent));

      --  Create the type declaration

      if FEN.Kind (Entity_Type_Spec) = K_Enumeration_Type then
         declare
            Enumerator : Node_Id;
         begin
            --  Readjusting the scope entity of elements

            Set_Scope_Entity (Identifier (Entity_Type_Spec), Parent);
            Set_Potential_Scope (Identifier (Entity_Type_Spec), Parent);
            Enumerator := First_Entity (Enumerators (Entity_Type_Spec));

            while Present (Enumerator) loop
               Set_Scope_Entity (Identifier (Enumerator), Parent);
               Set_Potential_Scope (Identifier (Enumerator), Parent);
               Enumerator := Next_Entity (Enumerator);
            end loop;

            if FEN.Kind (Entity) = K_Union_Type then
               --  Readjusting the scope entity of labels

               declare
                  Alternatives : List_Id;
                  Alternative  : Node_Id;
                  Labels       : List_Id;
                  Label        : Node_Id;
                  X            : Node_Id;
               begin
                  Alternatives := Switch_Type_Body (Entity);
                  Alternative := First_Entity (Alternatives);

                  while Present (Alternative) loop
                     Labels := FEN.Labels (Alternative);
                     Label := First_Entity (Labels);

                     while Present (Label) loop
                        X := FEN.Expression (Label);

                        if Present (X)
                          and then FEN.Kind (X) = K_Scoped_Name
                        then
                           Set_Scope_Entity
                             (Identifier (Reference (X)),
                              Parent);
                           Set_Potential_Scope
                             (Identifier (Reference (X)),
                              Parent);
                        end if;

                        Label := Next_Entity (Label);
                     end loop;

                     Alternative := Next_Entity (Alternative);
                  end loop;
               end;
            end if;

            Node := Entity_Type_Spec;
            Declarator := Entity_Type_Spec;
         end;
      else
         New_Identifier := FEU.Make_Identifier
           (Loc          => FEN.Loc (Entity),
            IDL_Name     => Anon_Type_Name,
            Node         => No_Node,
            Scope_Entity => Parent);

         Declarator := FEU.New_Node (K_Simple_Declarator, FEN.Loc (Entity));
         FEU.Bind_Identifier_To_Entity (New_Identifier, Declarator);

         List := FEU.New_List (FEN.Loc (Entity));
         FEU.Append_To (List, Declarator);

         Node := FEU.New_Node (K_Type_Declaration, FEN.Loc (Entity));
         Set_Type_Spec (Node, Type_Spec (Entity));
         Set_Declarators (Node, List);
         FEU.Bind_Declarators_To_Entity (List, Node);
      end if;

      --  Insert the new declaration

      Insert_Definition (Node, Before, In_Container => Parent);

      --  The type spec has to be modified using the new defined type
      --  declaration.

      New_Identifier := FEU.Make_Identifier
        (Loc          => FEN.Loc (Entity),
         IDL_Name     => Anon_Type_Name,
         Node         => No_Node,
         Scope_Entity => Node);

      New_Scoped_Name := FEU.Make_Scoped_Name
        (Loc        => FEN.Loc (Entity),
         Identifier => New_Identifier,
         Parent     => No_Node,
         Reference  => Declarator);

      FEU.Bind_Identifier_To_Entity (New_Identifier, New_Scoped_Name);

      if FEN.Kind (Entity) = K_Union_Type then
         Set_Switch_Type_Spec (Entity, New_Scoped_Name);
      else
         Set_Type_Spec (Entity, New_Scoped_Name);
      end if;
   end Handle_Anonymous_Type;

   ------------------------------
   -- New_Anonymous_Type_Index --
   ------------------------------

   function New_Anonymous_Type_Index return Nat is
   begin
      Anonymous_Type_Index_Value := Anonymous_Type_Index_Value + 1;
      return Anonymous_Type_Index_Value;
   end New_Anonymous_Type_Index;

   -----------------------------
   -- Has_Complex_Declarators --
   -----------------------------

   function Has_Complex_Declarators (Entity : Node_Id) return Boolean is
      pragma Assert (FEN.Kind (Entity) = K_Type_Declaration);
      Declarator : Node_Id := First_Entity (Declarators (Entity));
   begin
      while Present (Declarator) loop
         if FEN.Kind (Declarator) = K_Complex_Declarator then
            return True;
         end if;

         Declarator := Next_Entity (Declarator);
      end loop;

      return False;
   end Has_Complex_Declarators;

   ------------
   -- Expand --
   ------------

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

         when K_Attribute_Declaration =>
            Expand_Attribute_Declaration (Entity);

         when K_Constant_Declaration =>
            Expand_Constant_Declaration (Entity);

         when K_Operation_Declaration =>
            Expand_Operation_Declaration (Entity);

         when K_Parameter_Declaration =>
            Expand_Parameter_Declaration (Entity);

         when K_Element =>
            Expand_Element (Entity);

         when K_Member =>
            Expand_Member (Entity);

         when others =>
            null;
      end case;
   end Expand;

   ----------------------------------
   -- Expand_Attribute_Declaration --
   ----------------------------------

   procedure Expand_Attribute_Declaration (Entity : Node_Id) is
      Getter_Prefix     : constant String := "get_";
      Setter_Prefix     : constant String := "set_";
      Parent_Interface  : Node_Id;
      D                 : Node_Id;
      Accessor          : Node_Id;
      Accessor_Name     : Name_Id;
      Accessor_Id       : Node_Id;
      Node              : Node_Id;
      Param_Declaration : Node_Id;
      Parameters        : List_Id;
   begin

      D := First_Entity (Declarators (Entity));
      while Present (D) loop
         Parent_Interface := Scope_Entity (Identifier (D));

         if not Is_Readonly (Entity) then

            --  Building the Set_<declarator> operation

            Accessor := FEU.New_Node (K_Operation_Declaration, FEN.Loc (D));

            --  Set_<declarator> identifier

            Accessor_Name := Add_Prefix_To_Name
              (Setter_Prefix,
               FEN.IDL_Name (Identifier (D)));
            Accessor_Id := FEU.Make_Identifier
              (No_Location,
               Accessor_Name,
               No_Node,
               Parent_Interface);
            FEU.Bind_Identifier_To_Entity (Accessor_Id, Accessor);

            --   Profile and type spec

            Set_Type_Spec
              (Accessor,
               Parser.Resolve_Base_Type ((1 => Lexer.T_Void), FEN.Loc (D)));

            Parameters := FEU.New_List (FEN.Loc (D));
            Set_Parameters (Accessor, Parameters);

            --   Adding the 'To' parameter

            Set_Str_To_Name_Buffer ("To");
            Accessor_Id := FEU.Make_Identifier
              (No_Location,
               Name_Find,
               No_Node,
               Accessor);

            Node := FEU.New_Node (K_Simple_Declarator, No_Location);
            FEU.Bind_Identifier_To_Entity (Accessor_Id, Node);

            Param_Declaration :=
              FEU.New_Node (K_Parameter_Declaration, FEN.Loc (D));
            FEN.Set_Parameter_Mode (Param_Declaration, Mode_In);
            Set_Type_Spec      (Param_Declaration, Type_Spec (Entity));
            Set_Declarator     (Param_Declaration, Node);
            FEU.Bind_Declarator_To_Entity (Node, Param_Declaration);

            FEU.Append_To (Parameters, Param_Declaration);

            --  Exceptions

            Set_Exceptions (Accessor, Setter_Exceptions (Entity));

            --  Inserting the new operation

            FEU.Insert_After_Node (Accessor, Entity);
         end if;

         --  Building the Get_<declarator> operation

         Accessor := FEU.New_Node (K_Operation_Declaration, FEN.Loc (D));

         --  Get_<declarator> identifier

         Accessor_Name := Add_Prefix_To_Name
           (Getter_Prefix,
            FEN.IDL_Name (Identifier (D)));
         Accessor_Id := FEU.Make_Identifier
           (No_Location,
            Accessor_Name,
            No_Node,
            Parent_Interface);
         FEU.Bind_Identifier_To_Entity (Accessor_Id, Accessor);

         --   Profile and type spec

         Set_Type_Spec (Accessor, Type_Spec (Entity));

         Parameters := FEU.New_List (FEN.Loc (D));
         Set_Parameters (Accessor, Parameters);

         --  Exceptions

         Set_Exceptions (Accessor, Getter_Exceptions (Entity));

         --  Inserting the new operation

         FEU.Insert_After_Node (Accessor, Entity);

         D := Next_Entity (D);
      end loop;
   end Expand_Attribute_Declaration;

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

      --  1st pass to handle anonymous types in members

      Main_Loop : while Present (Member) loop
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

         --  If the member type is a structure type, extract the
         --  nested structure definition outside.

         if FEN.Kind (Member_Type) = FEN.K_Structure_Type then
            Define_Structure_Type_Outside
              (Member => Member,
               Entity => Entity,
               Before => Entity);
         end if;

         Member := Next_Entity (Member);
      end loop Main_Loop;

      --  2nd pass to expand complex declarators into array type
      --  definitions.

      Member := First_Entity (Members);
      while Present (Member) loop

         Member_Type := Type_Spec (Member);
         Declarator := First_Entity (Declarators (Member));

         while Present (Declarator) loop
            if FEN.Kind (Declarator) = FEN.K_Complex_Declarator then
               declare
                  New_Member     : Node_Id;
                  New_Declarator : Node_Id;
               begin
                  --  Remove the declarator from the declarators list
                  --  of the member.

                  FEU.Remove_Node_From_List (Declarator, Declarators (Member));

                  --  Remove the member from the member list if it has
                  --  no more declarators.

                  if FEU.Is_Empty (Declarators (Member)) then
                     FEU.Remove_Node_From_List (Member, Members);
                  end if;

                  --  Add a new member after the current one

                  New_Member := FEU.New_Node (K_Member, FEN.Loc (Member));

                  --  Set the declarator of the member

                  Set_Declarators
                    (New_Member,
                     FEU.New_List (FEN.Loc (Declarator)));
                  New_Declarator := FEU.New_Node
                    (K_Complex_Declarator, FEN.Loc (Declarator));
                  Set_Identifier (New_Declarator, Identifier (Declarator));
                  Set_Declaration (New_Declarator, New_Member);
                  Set_Array_Sizes (New_Declarator, Array_Sizes (Declarator));
                  FEU.Append_To (Declarators (New_Member), New_Declarator);

                  --  Set the type spec of the new member as eqaul to
                  --  the type spec of the current member.

                  Set_Type_Spec (New_Member, Member_Type);

                  --  Declare the array type before the structure type
                  --  and modify the new member.

                  Define_Array_Type_Outside (New_Member, Entity, Entity);

                  --  Insert the new member in the structure type

                  FEU.Insert_Before_Node (New_Member,
                                          Next_Entity (Member),
                                          Members);
               end;
            end if;

            Declarator := Next_Entity (Declarator);
         end loop;

         Member := Next_Entity (Member);
      end loop;
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
      N : Node_Id;
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
      D                    : Node_Id;
      New_CORBA_Contents   : List_Id;
      Definition           : Node_Id;
      CORBA_IR_Root_Node   : Node_Id;
      CORBA_Sequences_Node : Node_Id;
      L                    : Location;

      procedure Relocate (Parent : Node_Id; Child : Node_Id);
      --  Reparent Node and its named subnodes to the new Parent. This
      --  procedure is useful when generating code related to the
      --  CORBA Module.

      --------------
      -- Relocate --
      --------------

      procedure Relocate (Parent : Node_Id; Child : Node_Id) is
         pragma Assert (FEN.Kind (Parent) = K_Module);
         Definitions        : constant List_Id := FEN.Definitions (Parent);
         Dcl_Or_Enum_List   : List_Id;
         Dcl_Or_Enum        : Node_Id;
         Has_Named_Subnodes : Boolean :=  False;

      begin
         --  We must be very careful, because Append_To
         --  does not add only the node but all the Next_Entities (for
         --  details, see the calls to this procedure).

         FEU.Append_To (Definitions, Child);

         if FEN.Kind (Child) = K_Type_Declaration then
            Has_Named_Subnodes := True;
            Dcl_Or_Enum_List   := Declarators (Child);
         else
            --  Changing the parent. We change only the scope entity
            --  which is used for Ada code generation. The potential
            --  scope is kept unchanged in order to generate correct
            --  repository ids.

            if Present (Identifier (Child)) then
               Set_Scope_Entity (Identifier (Child), Parent);
            end if;

            if FEN.Kind (Child) = K_Enumeration_Type then
               Has_Named_Subnodes := True;
               Dcl_Or_Enum_List   := Enumerators (Child);
            end if;
         end if;

         if Has_Named_Subnodes then
            Dcl_Or_Enum := First_Entity (Dcl_Or_Enum_List);
            while Present (Dcl_Or_Enum) loop
               --  Changing the parent

               if Present (Identifier (Dcl_Or_Enum)) then
                  Set_Scope_Entity (Identifier (Dcl_Or_Enum), Parent);
               end if;

               Dcl_Or_Enum := Next_Entity (Dcl_Or_Enum);
            end loop;
         end if;
      end Relocate;
   begin
      --  The parsing of the CORBA module is a very particular case

      if FEN.IDL_Name (Identifier (Entity)) = CORBA_Name then
         --  This workaround is done to be able to take in account the
         --  prefix "omg.org". This is due to the fact that the
         --  created modules do not exist in reality.

         L := FEN.Loc (Entity);
         L.Scan := Text_Ptr'Last;

         New_CORBA_Contents := FEU.New_List (No_Location);

         --  Creating the CORBA.Repository_Root module

         declare
            Identifier  : Node_Id;
            Module_Name : Name_Id;
         begin
            CORBA_IR_Root_Node := FEU.New_Node (K_Module, L);
            Set_Imported (CORBA_IR_Root_Node, Imported (Entity));
            Module_Name := Repository_Root_Name;
            Identifier := FEU.Make_Identifier
              (Loc          => No_Location,
               IDL_Name     => Module_Name,
               Node         => No_Node,
               Scope_Entity => Entity);
            FEU.Bind_Identifier_To_Entity (Identifier, CORBA_IR_Root_Node);

            Set_Definitions (CORBA_IR_Root_Node, FEU.New_List (No_Location));

            FEU.Append_To (Definitions (Entity), CORBA_IR_Root_Node);
         end;

         --  Creating the CORBA.IDL_Sequences module

         declare
            Identifier  : Node_Id;
            Module_Name : Name_Id;
         begin
            CORBA_Sequences_Node := FEU.New_Node (K_Module, L);
            Set_Imported (CORBA_Sequences_Node, Imported (Entity));
            Module_Name := IDL_Sequences_Name;
            Identifier := FEU.Make_Identifier
              (Loc          => No_Location,
               IDL_Name     => Module_Name,
               Node         => No_Node,
               Scope_Entity => Entity);
            FEU.Bind_Identifier_To_Entity (Identifier, CORBA_Sequences_Node);

            Set_Definitions (CORBA_Sequences_Node, FEU.New_List (No_Location));

            FEU.Append_To (Definitions (Entity), CORBA_Sequences_Node);
         end;

         --  Relocating the CORBA Module entities

         D := First_Entity (Definitions (Entity));
         while Present (D) loop
            Definition := D;
            D := Next_Entity (D);

            --  We must alterate the list because we don't want to
            --  append all the elements after "Definition".

            Set_Next_Entity (Definition, No_Node);

            if Is_CORBA_IR_Entity (Definition) then
               Relocate (CORBA_IR_Root_Node, Definition);
            elsif Is_CORBA_Sequence (Definition) then
               Relocate (CORBA_Sequences_Node, Definition);
            else
               FEU.Append_To (New_CORBA_Contents, Definition);
            end if;
         end loop;

         Set_Definitions (Entity, New_CORBA_Contents);

      end if; --  End of the CORBA Module special handling

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
      Backend.BE_CORBA_Ada.Nutils.Initialize;
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
      Parent      : constant Node_Id := Scope_Entity (Identifier (Entity));
   begin
      Members := FEN.Members (Entity);

      --  1st pass to handle anonymous types in members

      Member := First_Entity (Members);
      while Present (Member) loop
         --  Handling anonymous types

         Handle_Anonymous_Type (Member, Parent, Entity);

         --  Handling implicit forward declarations

         Member_Type := Type_Spec (Member);
         Declarator := First_Entity (Declarators (Member));

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

         --  If the member type is a structure type, extract the
         --  nested structure definition outside.

         if FEN.Kind (Member_Type) = FEN.K_Structure_Type then
            Define_Structure_Type_Outside
              (Member => Member,
               Entity => Entity,
               Before => Entity);
         end if;

         Member := Next_Entity (Member);
      end loop;

      --  2nd pass to expand complex declarators into array type
      --  definitions.

      Member := First_Entity (Members);
      while Present (Member) loop

         Member_Type := Type_Spec (Member);
         Declarator := First_Entity (Declarators (Member));

         while Present (Declarator) loop
            if FEN.Kind (Declarator) = FEN.K_Complex_Declarator then
               declare
                  New_Member     : Node_Id;
                  New_Declarator : Node_Id;
               begin
                  --  Remove the declarator from the declarators list
                  --  of the member.

                  FEU.Remove_Node_From_List (Declarator, Declarators (Member));

                  --  Remove the member from the member list if it has
                  --  no more declarators.

                  if FEU.Is_Empty (Declarators (Member)) then
                     FEU.Remove_Node_From_List (Member, Members);
                  end if;

                  --  Add a new member after the current one

                  New_Member := FEU.New_Node (K_Member, FEN.Loc (Member));

                  --  Set the declarator of the member

                  Set_Declarators
                    (New_Member, FEU.New_List (FEN.Loc (Declarator)));
                  New_Declarator := FEU.New_Node
                    (K_Complex_Declarator, FEN.Loc (Declarator));
                  Set_Identifier (New_Declarator, Identifier (Declarator));
                  Set_Declaration (New_Declarator, New_Member);
                  Set_Array_Sizes (New_Declarator, Array_Sizes (Declarator));
                  FEU.Append_To (Declarators (New_Member), New_Declarator);

                  --  Set the type spec of the new member as eqaul to
                  --  the type spec of the current member.

                  Set_Type_Spec (New_Member, Member_Type);

                  --  Declare the array type before the structure type
                  --  and modify the new member.

                  Define_Array_Type_Outside (New_Member, Entity, Entity);

                  --  Insert the new member in the structure type

                  FEU.Insert_Before_Node (New_Member,
                                          Next_Entity (Member),
                                          Members);
               end;
            end if;

            Declarator := Next_Entity (Declarator);
         end loop;

         Member := Next_Entity (Member);
      end loop;

   end Expand_Structure_Type;

   -----------------------------
   -- Expand_Type_Declaration --
   -----------------------------

   procedure Expand_Type_Declaration (Entity : Node_Id) is
      D                : Node_Id;
      Type_Spec_Node   : Node_Id;
      Is_Seq_Type      : Boolean := False;
      Parent           : constant Node_Id := Scope_Entity
        (Identifier (First_Entity (Declarators (Entity))));
   begin
      --  Handling anonymous types

      Handle_Anonymous_Type (Entity, Parent, Entity);

      --  Handling Implicit Forward declarations

      Type_Spec_Node := Type_Spec (Entity);

      --  For the particular case of sequences, we change the type
      --  spec of the sequence.

      if FEN.Kind (Type_Spec_Node) = K_Sequence_Type then
         Type_Spec_Node := Type_Spec (Type_Spec_Node);
         Is_Seq_Type    := True;

      elsif FEN.Kind (Type_Spec_Node) = FEN.K_Structure_Type then
         --  If the type spec is a structure type, extract the nested
         --  structure definition outside.

         Define_Structure_Type_Outside
           (Member => Entity,
            Entity => Type_Spec_Node,
            Before => Entity);
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
         elsif Is_Seq_Type then
            Forward_Current_Interface_Designing_Components
              (FEN.Scope_Entity (FEN.Identifier (D)),
               Type_Spec_Node);
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
      Label        : Node_Id;
      Case_Labels  : List_Id;
      Parent       : constant Node_Id := Scope_Entity (Identifier (Entity));
   begin
      --  Expanding the switch type spec

      Handle_Anonymous_Type (Entity, Parent, Entity);

      --  1st pass to handle anonymous types in elements

      --  Expanding switch alternatives

      Alternatives := Switch_Type_Body (Entity);
      Alternative := First_Entity (Alternatives);

      while Present (Alternative) loop
         Element := FEN.Element (Alternative);

         --  Handling anonymous types

         Handle_Anonymous_Type (Element, Parent, Entity);

         --  Handling implicit forward declarations

         Type_Spec := FEN.Type_Spec (Element);

         if Is_Forward_Necessary (Entity, Type_Spec) then
            Set_Reference
              (Type_Spec,
               Add_Forward_Declaration
               (FEN.Reference
                (Type_Spec)));
         end if;

         if FEN.Kind (Type_Spec) = FEN.K_Structure_Type then
            Define_Structure_Type_Outside
              (Member => Element,
               Entity => Entity,
               Before => Entity);
         end if;

         Alternative := Next_Entity (Alternative);
      end loop;

      --  2nd pass to expand complex declarators into array type
      --  definitions.

      Alternative := First_Entity (Alternatives);

      while Present (Alternative) loop
         Element := FEN.Element (Alternative);

         if FEN.Kind (Declarator (Element)) = FEN.K_Complex_Declarator then
            --  Declare the array type before the structure type
            --  and modify the new member.

            Define_Array_Type_Outside (Element, Entity, Entity);
         end if;

         Alternative := Next_Entity (Alternative);
      end loop;

      --  3rd pass to reduce any label choice list containing
      --  "default:" and other cases to the simple "default:" case

      Alternative := First_Entity (Alternatives);

      External_Loop :
      while Present (Alternative) loop
         if FEU.Length (Labels (Alternative)) > 1 then
            Label := First_Entity (Labels (Alternative));

            while Present (Label) loop
               if Value (Label) = No_Value then
                  FEU.Remove_Node_From_List (Label, Labels (Alternative));
                  Set_Next_Entity (Label, No_Node);
                  Case_Labels := FEU.New_List (Loc (Alternative));
                  FEU.Append_To (Case_Labels, Label);
                  Set_Labels (Alternative, Case_Labels);

                  exit External_Loop;
               end if;

               Label := Next_Entity (Label);
            end loop;
         end if;

         Alternative := Next_Entity (Alternative);
      end loop External_Loop;
   end Expand_Union_Type;

   ---------------------------------
   -- Expand_Constant_Declaration --
   ---------------------------------

   procedure Expand_Constant_Declaration (Entity : Node_Id) is
      Parent : constant Node_Id := Scope_Entity (Identifier (Entity));
   begin
      Handle_Anonymous_Type (Entity, Parent, Entity);
   end Expand_Constant_Declaration;

   ----------------------------------
   -- Expand_Operation_Declaration --
   ----------------------------------

   procedure Expand_Operation_Declaration (Entity : Node_Id) is
      Parent : constant Node_Id := Scope_Entity (Identifier (Entity));
      N      : Node_Id;
   begin
      Handle_Anonymous_Type (Entity, Parent, Entity);

      N := First_Entity (Parameters (Entity));

      while Present (N) loop
         Expand (N);
         N := Next_Entity (N);
      end loop;
   end Expand_Operation_Declaration;

   --------------------
   -- Expand_Element --
   --------------------

   procedure Expand_Element (Entity : Node_Id) is
      Before : constant Node_Id := Scope_Entity
        (Identifier
         (Declarator
          (Entity)));
      Parent : constant Node_Id := Scope_Entity
        (Identifier
         (Before));
   begin
      Handle_Anonymous_Type (Entity, Parent, Before);
   end Expand_Element;

   -------------------
   -- Expand_Member --
   -------------------

   procedure Expand_Member (Entity : Node_Id) is
      Before : constant Node_Id := Scope_Entity
        (Identifier
         (First_Entity
          (Declarators
           (Entity))));
      Parent : constant Node_Id := Scope_Entity
        (Identifier
         (Before));
   begin
      Handle_Anonymous_Type (Entity, Parent, Before);
   end Expand_Member;

   -----------------------
   -- Insert_Definition --
   -----------------------

   procedure Insert_Definition (E, Before, In_Container : Node_Id) is
      Definitions : List_Id;
   begin
      if FEN.Kind (In_Container) = K_Module or else
        FEN.Kind (In_Container) = K_Specification
      then
         Definitions := FEN.Definitions (In_Container);
      elsif FEN.Kind (In_Container) = K_Interface_Declaration then
         Definitions := FEN.Interface_Body (In_Container);
      else
         raise Program_Error with "Bad container";
      end if;

      FEU.Insert_Before_Node (E, Before, Definitions);
   end Insert_Definition;

   ------------------------
   -- Is_CORBA_IR_Entity --
   ------------------------

   --  CORBA 3.0 Interface Repository entities

   CORBA_IR_Names : constant array (Positive range <>) of String_Ptr
     := (new String'("CORBA::AbstractInterfaceDef"),       --  interface
         new String'("CORBA::AbstractInterfaceDefSeq"),    --  typedef/sequence
         new String'("CORBA::AliasDef"),                   --  interface
         new String'("CORBA::ArrayDef"),                   --  interface
         new String'("CORBA::AttrDescriptionSeq"),         --  typedef/sequence
         new String'("CORBA::AttributeDef"),               --  interface
         new String'("CORBA::AttributeDescription"),       --  struct
         new String'("CORBA::AttributeMode"),              --  enum
         new String'("CORBA::ComponentIR"),                --  module
         new String'("CORBA::ConstantDef"),                --  interface
         new String'("CORBA::ConstantDescription"),        --  struct
         new String'("CORBA::Contained"),                  --  interface
         new String'("CORBA::ContainedSeq"),               --  typedef/sequence
         new String'("CORBA::Container"),                  --  interface
         new String'("CORBA::ContextIdentifier"),          --  typedef
         new String'("CORBA::ContextIdSeq"),               --  typedef/sequence
         new String'("CORBA::DefinitionKind"),             --  enum
         new String'("CORBA::EnumDef"),                    --  interface
         new String'("CORBA::EnumMemberSeq"),              --  typedef/sequence
         new String'("CORBA::ExcDescriptionSeq"),          --  typedef/sequence
         new String'("CORBA::ExceptionDef"),               --  interface
         new String'("CORBA::ExceptionDefSeq"),            --  typedef/sequence
         new String'("CORBA::ExceptionDescription"),       --  struct
         new String'("CORBA::ExtAttrDescriptionSeq"),      --  typedef/sequence
         new String'("CORBA::ExtAttributeDef"),            --  interface
         new String'("CORBA::ExtAttributeDescription"),    --  struct
         new String'("CORBA::ExtAbstractInterfaceDef"),    --  interface
         new String'("CORBA::ExtAbstractInterfaceDefSeq"), --  typedef/sequence
         new String'("CORBA::ExtInterfaceDef"),            --  interface
         new String'("CORBA::ExtInterfaceDefSeq"),         --  typedef/sequence
         new String'("CORBA::ExtInitializer"),             --  struct
         new String'("CORBA::ExtInitializerSeq"),          --  typedef/sequence
         new String'("CORBA::ExtLocalInterfaceDef"),       --  interface
         new String'("CORBA::ExtLocalInterfaceDefSeq"),    --  typedef/sequence
         new String'("CORBA::ExtValueDef"),                --  interface
         new String'("CORBA::ExtValueDefSeq"),             --  typedef/sequence
         new String'("CORBA::FixedDef"),                   --  interface
         new String'("CORBA::IDLType"),                    --  interface
         new String'("CORBA::InterfaceAttrExtension"),     --  interface
         new String'("CORBA::InterfaceDef"),               --  interface
         new String'("CORBA::InterfaceDefSeq"),            --  typedef/sequence
         new String'("CORBA::InterfaceDescription"),       --  struct
         new String'("CORBA::Initializer"),                --  struct
         new String'("CORBA::InitializerSeq"),             --  typedef/sequence
         new String'("CORBA::IRObject"),                   --  interface
         new String'("CORBA::LocalInterfaceDef"),          --  interface
         new String'("CORBA::LocalInterfaceDefSeq"),       --  typedef/sequence
         new String'("CORBA::ModuleDef"),                  --  interface
         new String'("CORBA::ModuleDescription"),          --  struct
         new String'("CORBA::NativeDef"),                  --  interface
         new String'("CORBA::OpDescriptionSeq"),           --  typedef/sequence
         new String'("CORBA::OperationDef"),               --  interface
         new String'("CORBA::OperationDescription"),       --  struct
         new String'("CORBA::OperationMode"),              --  enum
         new String'("CORBA::ParameterDescription"),       --  struct
         new String'("CORBA::ParameterMode"),              --  enum
         new String'("CORBA::ParDescriptionSeq"),          --  typedef/sequence
         new String'("CORBA::PrimitiveDef"),               --  interface
         new String'("CORBA::PrimitiveKind"),              --  enum
         new String'("CORBA::Repository"),                 --  interface
         new String'("CORBA::RepositoryIdSeq"),            --  typedef/sequence
         new String'("CORBA::SequenceDef"),                --  interface
         new String'("CORBA::StringDef"),                  --  interface
         new String'("CORBA::StructDef"),                  --  interface
         new String'("CORBA::StructMember"),               --  struct
         new String'("CORBA::StructMemberSeq"),            --  typedef/sequence
         new String'("CORBA::TypedefDef"),                 --  interface
         new String'("CORBA::TypeDescription"),            --  struct
         new String'("CORBA::UnionDef"),                   --  interface
         new String'("CORBA::UnionMember"),                --  struct
         new String'("CORBA::UnionMemberSeq"),             --  typedef/sequence
         new String'("CORBA::ValueBoxDef"),                --  interface
         new String'("CORBA::ValueDef"),                   --  interface
         new String'("CORBA::ValueDefSeq"),                --  typedef/sequence
         new String'("CORBA::ValueDescription"),           --  struct
         new String'("CORBA::ValueMember"),                --  struct
         new String'("CORBA::ValueMemberSeq"),             --  typedef/sequence
         new String'("CORBA::ValueMemberDef"),             --  interface
         new String'("CORBA::VersionSpec"),                --  typedef
         new String'("CORBA::WstringDef"));

   function Is_CORBA_IR_Entity (Entity : Node_Id) return Boolean is
      NK : constant FEN.Node_Kind := FEN.Kind (Entity);
      N  : Node_Id := Entity;
   begin
      if NK /= K_Interface_Declaration
        and then NK /= K_Forward_Interface_Declaration
        and then NK /= K_Simple_Declarator
        and then NK /= K_Complex_Declarator
        and then NK /= K_Type_Declaration
        and then NK /= K_Structure_Type
        and then NK /= K_Enumeration_Type
      then
         return False;
      end if;

      if NK = K_Type_Declaration then
         N := First_Entity (Declarators (Entity));
      end if;

      declare
         Name : constant Name_Id := FEU.Fully_Qualified_Name
           (Identifier (N),
            Separator => "::");
      begin
         for J in CORBA_IR_Names'Range loop
            if CORBA_IR_Names (J).all = Get_Name_String (Name) then
               return True;
            end if;
         end loop;
      end;

      return False;

   end Is_CORBA_IR_Entity;

   -----------------------
   -- Is_CORBA_Sequence --
   -----------------------

   --  CORBA 3.0 sequences relocated to CORBA.IDL_Sequences package

   CORBA_Sequences_Names : constant array (Positive range <>) of String_Ptr
     := (new String'("CORBA::AnySeq"),
         new String'("CORBA::BooleanSeq"),
         new String'("CORBA::CharSeq"),
         new String'("CORBA::WCharSeq"),
         new String'("CORBA::OctetSeq"),
         new String'("CORBA::ShortSeq"),
         new String'("CORBA::UShortSeq"),
         new String'("CORBA::LongSeq"),
         new String'("CORBA::ULongSeq"),
         new String'("CORBA::LongLongSeq"),
         new String'("CORBA::ULongLongSeq"),
         new String'("CORBA::FloatSeq"),
         new String'("CORBA::DoubleSeq"),
         new String'("CORBA::LongDoubleSeq"),
         new String'("CORBA::StringSeq"),
         new String'("CORBA::WStringSeq"));

   function Is_CORBA_Sequence (Entity : Node_Id) return Boolean is
      NK : constant FEN.Node_Kind := FEN.Kind (Entity);
      N  : Node_Id := Entity;
   begin
      if NK /= K_Type_Declaration then
         return False;
      end if;

      N := First_Entity (Declarators (Entity));

      declare
         Name : constant Name_Id := FEU.Fully_Qualified_Name
           (Identifier (N),
            Separator => "::");
      begin
         for J in CORBA_Sequences_Names'Range loop
            if CORBA_Sequences_Names (J).all = Get_Name_String (Name) then
               return True;
            end if;
         end loop;
      end;

      return False;
   end Is_CORBA_Sequence;

end Backend.BE_CORBA_Ada.Expand;

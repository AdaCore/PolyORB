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

--  Various ASIS queries for CIAO.
--  $Id: //droopi/main/compilers/ciao/ciao-asis_queries.adb#2 $

with Ada.Characters.Handling;
with Ada.Unchecked_Deallocation;
with Ada.Wide_Text_Io; use Ada.Wide_Text_Io;

with Asis.Compilation_Units.Relations;
use  Asis.Compilation_Units.Relations;

with Asis.Compilation_Units; use Asis.Compilation_Units;
with Asis.Declarations;      use Asis.Declarations;
with Asis.Definitions;       use Asis.Definitions;
with Asis.Elements;          use Asis.Elements;
with Asis.Expressions;       use Asis.Expressions;
with Asis.Statements;        use Asis.Statements;
with Asis.Text;              use Asis.Text;

package body CIAO.ASIS_Queries is

   use Asis;

   function Is_Ancestor (Ancestor_Compilation_Unit : Asis.Compilation_Unit;
                         Compilation_Unit          : Asis.Compilation_Unit)
     return Boolean is
      Parent : constant Asis.Compilation_Unit :=
        Corresponding_Parent_Declaration (Compilation_Unit);
   begin
      if Is_Identical (Ancestor_Compilation_Unit, Compilation_Unit) then
         return True;
      elsif Is_Nil (Parent) then
         --  Compilation_Unit is predefined package Standard.
         return False;
      else
         return Is_Ancestor (Ancestor_Compilation_Unit, Parent);
      end if;
   end Is_Ancestor;

   function Corresponding_Entity_Name_Definition
     (Reference : Asis.Expression)
     return Asis.Defining_Name is
      Identifier : Asis.Expression;
   begin
      case Expression_Kind (Reference) is
         when An_Identifier =>
            Identifier := Reference;
         when A_Selected_Component =>
            Identifier := Selector (Reference);
         when An_Attribute_Reference =>
            return Corresponding_Entity_Name_Definition
              (Prefix (Reference));
         when others =>
            raise ASIS_Inappropriate_Element;
      end case;

      return Corresponding_Name_Definition (Identifier);
   end Corresponding_Entity_Name_Definition;

   function Corresponding_Entity_Name_Declaration
     (Reference : Asis.Expression)
     return Asis.Declaration is
      Result : Asis.Element;
   begin
      Result := Corresponding_Entity_Name_Definition (Reference);
      if not Is_Nil (Result) then
         Result := Enclosing_Element (Result);
      end if;
      return Result;
   end Corresponding_Entity_Name_Declaration;

   function Discrete_Subtype_Name
     (Definition : Asis.Definition)
     return Asis.Program_Text is
   begin
     case Discrete_Range_Kind (Definition) is
        when Not_A_Discrete_Range =>
           raise ASIS_Failed;

        when A_Discrete_Subtype_Indication =>
           return Ada_Full_Name
             (Corresponding_Entity_Name_Declaration
              (Asis.Definitions.Subtype_Mark (Definition)));

        when A_Discrete_Range_Attribute_Reference =>
           return Ada_Full_Name
             (Corresponding_Entity_Name_Declaration
              (Prefix (Definition)));

        when A_Discrete_Simple_Expression_Range =>
           return Ada_Full_Name
              (Corresponding_Expression_Type
               (Asis.Definitions.Lower_Bound (Definition)));

     end case;
   end Discrete_Subtype_Name;

   --  Internal subprogram
   function Corresponding_Base_Type
     (Subtype_Mark : Asis.Expression)
     return Asis.Declaration is
      Identifier : Asis.Expression;
   begin
      case Expression_Kind (Subtype_Mark) is
         when An_Identifier =>
            Identifier := Subtype_Mark;
         when A_Selected_Component =>
            Identifier := Selector (Subtype_Mark);
         -- XXX An_Attribute_Reference??
         when others =>
            raise ASIS_Failed;
      end case;
      return Corresponding_First_Subtype
        (Corresponding_Entity_Name_Declaration (Identifier));
   end Corresponding_Base_Type;

   function Is_Type_Conformant
     (Declaration_1, Declaration_2 : Asis.Declaration)
     return Boolean is

      DK_1 : constant Asis.Declaration_Kinds
        := Declaration_Kind (Declaration_1);
      DK_2 : constant Asis.Declaration_Kinds
        := Declaration_Kind (Declaration_2);
   begin
      if (DK_1 /= DK_2)
        or else not (DK_1 = A_Procedure_Declaration
                     or else DK_1 = A_Function_Declaration) then
         return False;
      end if;

      -- XXX Warning! Toto'Class == Toto'Class ??
      if DK_1 = A_Function_Declaration
        and then not Is_Identical
          (Corresponding_Base_Type (Result_Profile (Declaration_1)),
           Corresponding_Base_Type (Result_Profile (Declaration_2)))
      then
         return False;
      end if;

      declare
         Params_1 : constant Parameter_Specification_List
           := Parameter_Profile (Declaration_1);
         Params_2 : constant Parameter_Specification_List
           := Parameter_Profile (Declaration_2);
      begin
         for I in Params_1'Range loop
            if not Is_Identical
              (Corresponding_Base_Type
               (Declaration_Subtype_Mark (Params_1 (I))),
               Corresponding_Base_Type
               (Declaration_Subtype_Mark (Params_2 (I))))
            then
               return False;
            end if;
         end loop;
      end;
      return True;
   end Is_Type_Conformant;

   function Is_Inheritance_Homograph
     (Ancestor_Subprogram, Ancestor,
        Child_Subprogram, Child : Asis.Declaration)
     return Boolean is

      DK_1 : constant Asis.Declaration_Kinds
        := Declaration_Kind (Ancestor_Subprogram);
      DK_2 : constant Asis.Declaration_Kinds
        := Declaration_Kind (Child_Subprogram);

      function Is_Same_Type (Ancestor_Decl, Child_Decl : Asis.Declaration)
        return Boolean is
      begin
         return Is_Identical (Ancestor_Decl, Child_Decl)
           or else (Is_Identical (Ancestor_Decl, Ancestor)
                    and then Is_Identical (Child_Decl, Child));
      end Is_Same_Type;

   begin
      if (DK_1 /= DK_2)
        or else not (DK_1 = A_Procedure_Declaration
                     or else DK_1 = A_Function_Declaration) then
         return False;
      end if;

      -- XXX Warning! Toto'Class == Toto'Class ??
      if DK_1 = A_Function_Declaration
        and then not Is_Same_Type
          (Corresponding_Base_Type (Result_Profile (Ancestor_Subprogram)),
           Corresponding_Base_Type (Result_Profile (Child_Subprogram)))
      then
         return False;
      end if;

      declare
         Params_1 : constant Parameter_Specification_List
           := Parameter_Profile (Ancestor_Subprogram);
         Params_2 : constant Parameter_Specification_List
           := Parameter_Profile (Child_Subprogram);
      begin
         if Params_1'Length /= Params_2'Length then
            return False;
         end if;

         for I in Params_1'Range loop
            if not Is_Same_Type (Corresponding_Base_Type
                                   (Declaration_Subtype_Mark (Params_1 (I))),
                                 Corresponding_Base_Type
                                   (Declaration_Subtype_Mark
                                    (Params_2 (I + Params_2'First - Params_1'First))))
            then
               return False;
            end if;
         end loop;
      end;
      return True;
   end Is_Inheritance_Homograph;

   function Is_Tagged_Type (Declaration : Asis.Declaration)
     return Boolean is
   begin
      case Declaration_Kind (Declaration) is
         when A_Subtype_Declaration =>
            return Is_Tagged_Type (Corresponding_First_Subtype (Declaration));
         when
           An_Ordinary_Type_Declaration    |
           A_Private_Type_Declaration      |
           A_Private_Extension_Declaration =>
            declare
               Def : constant Asis.Definition
                 := Type_Declaration_View (Declaration);
            begin
               case Definition_Kind (Def) is
                  when
                    A_Tagged_Private_Type_Definition |
                    A_Private_Extension_Definition   =>
                     return True;
                  when A_Type_Definition             =>
                     case Type_Kind (Def) is
                        when
                          A_Tagged_Record_Type_Definition       |
                          A_Derived_Record_Extension_Definition =>
                           return True;
                        when A_Derived_Type_Definition =>
                           return False;
                        when others                             =>
                           return False;
                     end case;
                  when others                        =>
                     return False;
               end case;
            end;
         when others =>
            --  An unexpected Declaration_Kind
            return False;
      end case;
   end Is_Tagged_Type;

   --  Local subprogram.
   function Has_Limited_Component (Components : Asis.Record_Component_List)
     return Boolean is
   begin
      for I in Components'Range loop
         case Element_Kind (Components (I)) is
            when
              A_Pragma |
              A_Clause =>
               return False;
            when A_Declaration =>
               declare
                  View : constant Asis.Definition
                    := Object_Declaration_View (Components (I));
               begin
                  if Trait_Kind (View) = A_Limited_Trait
                    or else Is_Limited_Type
                    (Corresponding_Entity_Name_Declaration
                     (Asis.Definitions.Subtype_Mark
                      (Component_Subtype_Indication (View)))) then
                     return True;
                  end if;
               end;
            when A_Definition => --  A_Variant_Part
               -- XXX DEBUG assertion
               pragma Assert (Definition_Kind (Components (I)) = A_Variant_Part);
               declare
                  Variants : constant Asis.Variant_List
                    := Asis.Definitions.Variants (Components (I));
               begin
                  for I in Variants'Range loop
                     if Has_Limited_Component (Record_Components (Variants (I))) then
                        return True;
                     end if;
                  end loop;
               end;
            when others =>
               --  Cannot happen!
               raise ASIS_Failed;
         end case;
      end loop;
      return False;
   end Has_Limited_Component;

   function Is_Limited_Type (Declaration : Asis.Declaration)
     return Boolean is
   begin
      case Declaration_Kind (Declaration) is
         when
           A_Task_Type_Declaration      |
           A_Protected_Type_Declaration =>
            return True;

         when A_Subtype_Declaration =>
            return Is_Limited_Type (Corresponding_First_Subtype (Declaration));

         when
           An_Ordinary_Type_Declaration    |
           A_Private_Type_Declaration      |
           A_Private_Extension_Declaration =>

            declare
               Def : constant Asis.Definition
                 := Type_Declaration_View (Declaration);
               TK  : constant Trait_Kinds
                 := Trait_Kind (Def);
            begin
               if False
                 or else TK = A_Limited_Trait
                 or else TK = A_Limited_Private_Trait
                 or else TK = An_Abstract_Limited_Trait
                 or else TK = An_Abstract_Limited_Private_Trait
               then
                  return True;
               end if;

               case Definition_Kind (Def) is
                  when A_Type_Definition             =>
                     case Type_Kind (Def) is
                        when
                          A_Derived_Type_Definition             |
                          A_Derived_Record_Extension_Definition =>
                           return Is_Limited_Type (Corresponding_Root_Type (Def));
                        when
                          An_Unconstrained_Array_Definition |
                          A_Constrained_Array_Definition    =>

                           return (Trait_Kind (Component_Definition (Def))
                                   = A_Limited_Trait)
                             or else Is_Limited_Type
                                (Corresponding_Entity_Name_Declaration
                                 (Asis.Definitions.Subtype_Mark
                                  (Component_Subtype_Indication (Component))));

                        when
                          A_Record_Type_Definition        |
                          A_Tagged_Record_Type_Definition =>
                           declare
                              Record_Def : constant Asis.Definition
                                := Definitions.Record_Definition (Def);
                           begin
                              if Definition_Kind (Record_Def) = A_Null_Record_Definition then
                                 return False;
                              else
                                 return Has_Limited_Component
                                   (Record_Components (Asis.Definitions.Record_Definition (Def)));
                              end if;
                           end;
                        when others =>
                           return False;
                     end case;
                  when A_Private_Extension_Definition =>
                     return Is_Limited_Type
                       (Corresponding_Entity_Name_Declaration
                        (Asis.Definitions.Subtype_Mark
                         (Ancestor_Subtype_Indication (Def))));

                  when others =>
                     return False;
               end case;
            end;
         when others =>
            --  An unexpected Declaration_Kind
            return False;
      end case;
   end Is_Limited_Type;

   --  Internal subprogram: determines whether Subprogram_Declaration
   --  is overriding an implicit subprogram inherited from an ancestor
   --  of the type declared by the Derived_Type_Declaration as a result
   --  of the derivation Leaf_Type_Declaration.

   function Recursive_Is_Overriding_Inherited_Subprogram
     (Subprogram_Declaration   : Asis.Declaration;
      Derived_Type_Declaration : Asis.Declaration;
      Leaf_Type_Declaration    : Asis.Declaration)
     return Boolean is

      type Declaration_List_Access is access Declaration_List;

      procedure Free is new Ada.Unchecked_Deallocation
        (Declaration_List, Declaration_List_Access);

      --  True iff the defining names in Names_1
      --  and Names_2 are the same in the sense of 8.3(2).
      function Same_Defining_Names
        (Names_1, Names_2 : Asis.Defining_Name_List)
        return Boolean;

      function Same_Defining_Names
        (Names_1, Names_2 : Asis.Defining_Name_List)
        return Boolean is

         use Ada.Characters.Handling;

      begin
         if Names_1'Length /= Names_2'Length then
            return False;
         end if;

         for I in Names_1'Range loop
            declare
               Name_1 : constant Asis.Program_Text
                 := Isolated_Element_Image (Names_1 (I));
               Name_2 : constant Asis.Program_Text
                 := Isolated_Element_Image
                 (Names_2 (Names_2'First + I - Names_1'First));
            begin
               for J in Name_1'Range loop
                  declare
                     Char_1 : Wide_Character
                       := Name_1 (J);
                     Char_2 : Wide_Character
                       := Name_2 (J + Name_2'First - Name_1'First);
                  begin
                     if (Is_Character (Char_1) and then
                       To_Lower (To_Character (Char_1))
                       /= To_Lower (To_Character (Char_2)))
                       or else Char_1 /= Char_2 then
                        return False;
                     end if;
                  end;
               end loop;
            end;
         end loop;

         return True;
      end Same_Defining_Names;

      Subprogram_DK   : constant Asis.Declaration_Kinds
        := Declaration_Kind (Subprogram_Declaration);
      Type_DK         :  constant Asis.Declaration_Kinds
        := Declaration_Kind (Derived_Type_Declaration);

      Type_Definition : constant Asis.Definition
        := Type_Declaration_View (Derived_Type_Declaration);
      Incomplete_Parent_Type_Declaration : Asis.Declaration;

      Parent_Type_Declaration : Asis.Declaration;
      Parent_Type_Scope       : Asis.Element;

      Scope_Items : Declaration_List_Access
        := null;

      Result : Boolean := False;

   begin  --  Recursive_Is_Overriding_Inherited_Subprogram

      --  Find the immediate parent type.
      case Type_DK is
         when A_Private_Type_Declaration =>
            --  A private type declaration that is not a private
            --  extension declaration has no inherited subprograms.
            return False;
         when A_Full_Type_Declaration =>
            if Type_Kind (Type_Definition) /= A_Derived_Type_Definition then
               --  A full type declaration that is not a derivation
               --  has no inherited subprograms.
               return False;
            end if;
            Parent_Type_Declaration := Corresponding_Entity_Name_Declaration
              (Asis.Definitions.Subtype_Mark
               (Parent_Subtype_Indication (Type_Definition)));
         when A_Private_Extension_Declaration =>
            Parent_Type_Declaration := Corresponding_Entity_Name_Declaration
              (Asis.Definitions.Subtype_Mark
               (Ancestor_Subtype_Indication (Type_Definition)));
         when others =>
            --  An unexpected Declaration_Kind
            return False;
      end case;

      Incomplete_Parent_Type_Declaration :=
        Corresponding_Type_Declaration (Parent_Type_Declaration);

      --  Find the region containing the parent declaration.
      Parent_Type_Scope := Enclosing_Element (Parent_Type_Declaration);

      case Element_Kind (Parent_Type_Scope) is
         when A_Declaration =>
            case Declaration_Kind (Parent_Type_Scope) is
               when
                 A_Generic_Package_Declaration |
                 A_Package_Declaration         =>
                  Scope_Items := new Declaration_List'
                    (Visible_Part_Declarative_Items (Parent_Type_Scope)
                     & Private_Part_Declarative_Items (Parent_Type_Scope));
               when
                 A_Function_Body_Declaration  |
                 A_Procedure_Body_Declaration |
                 A_Package_Body_Declaration   |
                 A_Task_Body_Declaration      |
                 An_Entry_Body_Declaration    =>
                  Scope_Items := new Declaration_List'
                    (Body_Declarative_Items (Parent_Type_Scope));
               when others =>
                  raise ASIS_Failed;
            end case;

         when A_Statement =>
            if Statement_Kind (Parent_Type_Scope) /= A_Block_Statement then
               raise ASIS_Failed;
            end if;
            Scope_Items := new Declaration_List'
              (Block_Declarative_Items (Parent_Type_Scope));

         when others =>
            raise ASIS_Failed;
      end case;

  Parent_Scope:
      for I in Scope_Items.all'Range loop
         declare
            Item    : constant Asis.Declaration
              := Scope_Items.all (I);
            Item_DK : constant Asis.Declaration_Kinds
              := Declaration_Kind (Item);
            Type_Decl : Asis.Declaration;
            Is_Primitive : Boolean := False;
         begin
            exit Parent_Scope when Is_Identical (Item, Derived_Type_Declaration);
            --  Primitives declared after the derivation are not inherited.

            if False
              or else Item_DK = A_Procedure_Declaration
              or else Item_DK = A_Function_Declaration
            then
               if Item_DK = A_Function_Declaration then
                  Type_Decl := Corresponding_Base_Type (Result_Profile (Item));
                  if Is_Identical (Type_Decl, Parent_Type_Declaration)
                    or else Is_Identical
                    (Type_Decl,
                     Incomplete_Parent_Type_Declaration) then
                     Is_Primitive := True;
                  end if;
               end if;

               declare
                  Params : constant Parameter_Specification_List
                    := Parameter_Profile (Item);
               begin
              Profile:
                  for I in Params'Range loop
                     exit Profile when Is_Primitive;
                     Type_Decl := Corresponding_Base_Type
                       (Declaration_Subtype_Mark (Params (I)));
                     if Is_Identical (Type_Decl, Parent_Type_Declaration)
                       or else Is_Identical
                       (Type_Decl,
                        Incomplete_Parent_Type_Declaration) then
                        Is_Primitive := True;
                     end if;
                  end loop Profile;
               end;

               --  At this point Is_Primitive = True if Item is the declaration
               --  of a primitive operation of the parent type.

               if True
                 and then Is_Primitive
                 and then Subprogram_DK = Item_DK
                 and then Same_Defining_Names
                   (Names (Subprogram_Declaration),
                    Names (Item))
                 and then Is_Inheritance_Homograph
                   (Ancestor_Subprogram => Item,
                    Ancestor            => Parent_Type_Declaration,
                    Child_Subprogram    => Subprogram_Declaration,
                    Child               => Leaf_Type_Declaration)
               then
                  --  This primitive operation is homograph to Subprogram_Declaration.
                  Result := True;
                  exit Parent_Scope;
               end if;
            end if;
         end;
      end loop Parent_Scope;

      Free (Scope_Items);
      if Result then
         return True;
      else
         return Recursive_Is_Overriding_Inherited_Subprogram
           (Subprogram_Declaration,
            Parent_Type_Declaration,
            Leaf_Type_Declaration);
      end if;

   exception
      when others =>
         Free (Scope_Items);
         raise;
   end Recursive_Is_Overriding_Inherited_Subprogram;

   function Is_Overriding_Inherited_Subprogram
     (Subprogram_Declaration    : Asis.Declaration;
      Derived_Type_Declaration  : Asis.Declaration)
     return Boolean is
   begin
      return Recursive_Is_Overriding_Inherited_Subprogram
        (Subprogram_Declaration,
         Derived_Type_Declaration,
         Derived_Type_Declaration);
   end Is_Overriding_Inherited_Subprogram;

   --  Determine whether a formal parameter or result of a
   --  subprogram declaration is a controlling formal parameter,
   --  resp. controlling result.
   function Is_Controlling_Formal_Or_Result
     (Subprogram_Declaration : Asis.Declaration;
      Subtype_Mark           : Asis.Expression)
   return Boolean is
     Type_Declaration : constant Asis.Declaration
       := Corresponding_Entity_Name_Declaration (Subtype_Mark);
     Type_Enclosing_Element : Asis.Element;
   begin
      if not Is_Tagged_Type (Type_Declaration) then
         return False;
      end if;

      Type_Enclosing_Element := Enclosing_Element (Type_Declaration);
      if not Is_Identical (Type_Enclosing_Element,
                           Enclosing_Element (Subprogram_Declaration))
      then
         return False;
      end if;

      if Element_Kind (Type_Enclosing_Element) = A_Declaration then
         declare
            DK : constant Asis.Declaration_Kinds
              := Declaration_Kind (Type_Enclosing_Element);
         begin
            if DK = A_Package_Declaration
              or else DK = A_Generic_Package_Declaration then
               --  A package_specification
               return True;
            end if;
         end;
      end if;

      return Is_Overriding_Inherited_Subprogram
        (Subprogram_Declaration, Type_Declaration);
   end Is_Controlling_Formal_Or_Result;

   function Is_Controlling_Formal
     (Parameter_Specification : Asis.Parameter_Specification)
     return Boolean is
   begin
      return Is_Controlling_Formal_Or_Result
        (Enclosing_Element (Parameter_Specification),
         Declaration_Subtype_Mark (Parameter_Specification));
   end Is_Controlling_Formal;

   function Is_Controlling_Result
     (Result_Profile : Asis.Expression)
     return Boolean is
      T : Asis.Declaration;
      Type_Enclosing_Element : Asis.Element;
      Subprogram_Declaration : constant Asis.Declaration
        := Enclosing_Element (Result_Profile);
   begin
      return Is_Controlling_Formal_Or_Result
        (Enclosing_Element (Result_Profile),
         Result_Profile);
   end Is_Controlling_Result;

   function Controlling_Formal_Parameters (Declaration : Asis.Declaration)
     return Asis.Parameter_Specification_List is
      Params : constant Asis.Parameter_Specification_List
        := Parameter_Profile (Declaration);
      Controlling_Params : Asis.Parameter_Specification_List (Params'Range);
      Controlling_Count  : Natural range 0 .. Controlling_Params'Length
        := 0;
   begin
      for I in Params'Range loop
         if Is_Controlling_Formal (Params (I)) then
            Controlling_Params
              (Controlling_Params'First + Controlling_Count)
              := Params (I);
            Controlling_Count := Controlling_Count + 1;
         end if;
      end loop;

      if Controlling_Count = 0 then
              return Nil_Element_List;
      else
         return Controlling_Params
           (Controlling_Params'First ..
            Controlling_Params'First + Controlling_Count - 1);
      end if;
   end Controlling_Formal_Parameters;

   function Enclosing_Basic_Declaration (Element : Asis.Element)
     return Asis.Declaration is
   begin
      case Element_Kind (Element) is
         when
           Not_An_Element       |
           A_Statement          |
           A_Path               |
           An_Exception_Handler =>
            raise ASIS_Inappropriate_Element;

         when
           A_Pragma        |
           A_Clause        |
           A_Defining_Name |
           A_Definition    |
           An_Expression   |
           An_Association  =>
            return Enclosing_Basic_Declaration (Enclosing_Element (Element));

         when
           A_Declaration =>
            case Declaration_Kind (Element) is
               when
                  An_Ordinary_Type_Declaration    |
                  A_Task_Type_Declaration         |
                  A_Protected_Type_Declaration    |
                  An_Incomplete_Type_Declaration  |
                  A_Private_Type_Declaration      |
                  A_Private_Extension_Declaration |
                  --  type_declaration

                  A_Subtype_Declaration           |
                  --  subtype_declaration

                  A_Variable_Declaration          |
                  A_Constant_Declaration          |
                  A_Deferred_Constant_Declaration |
                  A_Single_Task_Declaration       |
                  A_Single_Protected_Declaration  |
                  --  object_declaration

                  An_Integer_Number_Declaration   |
                  A_Real_Number_Declaration       |
                  --  number_declaration

                  A_Procedure_Declaration         |
                  A_Function_Declaration          |
                  --  subprogram_declaration
                  --  abstract_subprogram_declaration

                  A_Package_Declaration           |
                  --  package_declaration

                  An_Object_Renaming_Declaration           |
                  An_Exception_Renaming_Declaration        |
                  A_Package_Renaming_Declaration           |
                  A_Procedure_Renaming_Declaration         |
                  A_Function_Renaming_Declaration          |
                  A_Generic_Package_Renaming_Declaration   |
                  A_Generic_Procedure_Renaming_Declaration |
                  A_Generic_Function_Renaming_Declaration  |
                  --  renaming_declaration

                  An_Exception_Declaration        |
                  --  exception_declaration

                  A_Generic_Procedure_Declaration |
                  A_Generic_Function_Declaration  |
                  A_Generic_Package_Declaration   |
                  --  generic_declaration

                  A_Package_Instantiation         |
                  A_Procedure_Instantiation       |
                  A_Function_Instantiation        =>
                  --  generic_instanciation

                  return Element;

               when
                 An_Enumeration_Literal_Specification  |
                 A_Discriminant_Specification          |
                 A_Component_Declaration               |
                 A_Parameter_Specification             |
                 An_Entry_Declaration                  |
                 An_Entry_Index_Specification          |
                 A_Formal_Object_Declaration           |
                 A_Formal_Type_Declaration             |
                 A_Formal_Procedure_Declaration        |
                 A_Formal_Function_Declaration         |
                 A_Formal_Package_Declaration          |
                 A_Formal_Package_Declaration_With_Box =>
                  return Enclosing_Basic_Declaration
                    (Enclosing_Element (Element));
               when others =>
                  raise ASIS_Inappropriate_Element;
            end case;
      end case;
   end Enclosing_Basic_Declaration;

   function Isolated_Element_Image (Element : Asis.Element)
     return Program_Text is
      Padded_Image : constant Program_Text
        := Asis.Text.Element_Image (Element);
   begin
      return Padded_Image (Element_Span (Element).First_Column .. Padded_Image'Last);
   end Isolated_Element_Image;

   function Ada_Full_Name (Declaration : Asis.Declaration)
     return Asis.Program_Text is
      Origin : constant Compilation_Unit
        := Enclosing_Compilation_Unit (Declaration);

      -- XXX This is a work-around for a bug in ASIS-for-GNAT 3.11p
      --  remove it for later versions.
      function Safe_Names (Declaration : Asis.Declaration)
        return Asis.Defining_Name_List is
         Empty_List : Asis.Defining_Name_List (1 .. 0);
      begin
         return Names (Declaration);
      exception
         when others =>
            return Empty_List;
      end Safe_Names;

   begin
      if Is_Nil (Corresponding_Parent_Declaration (Origin)) then
         declare
            Declaration_Names : constant Asis.Defining_Name_List
              := Safe_Names (Declaration);
         begin
            if Declaration_Names'Length > 0 then
               return "Standard." & Defining_Name_Image
                 (Declaration_Names (Declaration_Names'First));
            else
               --  The implicit declaration of a root or universal
               --  numeric type.

               pragma Assert (True
                 and then Declaration_Kind (Declaration) = An_Ordinary_Type_Declaration);

               declare
                  Definition : constant Asis.Definition
                    := Type_Declaration_View (Declaration);
               begin
                  -- XXX pragma Assert (Type_Kind (Definition) = A_Root_Type_Definition);
                  -- XXX This should be checked, but ASIS for GNAT up to 3.13w 19991007
                  --     makes this assertion fail (Not_A_Root_Type_Definition, see below).

                  case Root_Type_Kind (Definition) is
                     when Not_A_Root_Type_Definition =>
                        -- raise ASIS_Failed;
                        -- XXX
                        --  XXX The only scalar type that has an implicit
                        --  declaration is universal_integer.
                        return "Standard.Integer";

                     when A_Root_Integer_Definition =>
                        --  Integer'Base
                        return "Standard.Integer'Base";
                     when A_Root_Real_Definition =>
                        --  Float'Base
                        return "Standard.Float'Base";

                     when A_Universal_Integer_Definition =>
                        --  Integer'Class
                        return "Standard.Integer";
                     when A_Universal_Real_Definition =>
                        --  Float'Class
                        return "Standard.Float";
                     when A_Universal_Fixed_Definition =>
                        --  Fixed'Class
                        return "Standard.Universal_FixedXXX";
                  end case;
               end;
            end if;
         end;
      end if;

      if Is_Identical (Declaration, Unit_Declaration (Origin)) then
         return Unit_Full_Name (Origin);
      end if;

      declare
         Enclosing_Declaration : constant Asis.Declaration
           := Enclosing_Element (Declaration);
         Denoted_Definition    : constant Asis.Definition
           := Declaration_Name (Declaration);
      begin
         pragma Assert (Element_Kind (Enclosing_Declaration) = A_Declaration);

         return Ada_Full_Name (Enclosing_Declaration)
           & "." & Defining_Name_Image (Denoted_Definition);
      end;
   end Ada_Full_Name;

   function Declaration_Name (Declaration : Asis.Declaration)
     return Asis.Defining_Name is
      Defining_Names : constant Asis.Defining_Name_List
        := Names (Declaration);
   begin
      pragma Assert (Defining_Names'Length = 1);
      return Defining_Names (Defining_Names'First);
   end Declaration_Name;

end CIAO.ASIS_Queries;

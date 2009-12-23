------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--           B A C K E N D . B E _ C O R B A _ A D A . S T U B S            --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2005-2009, Free Software Foundation, Inc.          --
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
with Values;    use Values;
with Locations; use Locations;

with Frontend.Nutils;
with Frontend.Nodes;  use Frontend.Nodes;

with Backend.BE_CORBA_Ada.IDL_To_Ada; use Backend.BE_CORBA_Ada.IDL_To_Ada;
with Backend.BE_CORBA_Ada.Nodes;      use Backend.BE_CORBA_Ada.Nodes;
with Backend.BE_CORBA_Ada.Nutils;     use Backend.BE_CORBA_Ada.Nutils;
with Backend.BE_CORBA_Ada.Runtime;    use Backend.BE_CORBA_Ada.Runtime;
with Backend.BE_CORBA_Ada.Common;     use Backend.BE_CORBA_Ada.Common;

package body Backend.BE_CORBA_Ada.Stubs is

   package FEN renames Frontend.Nodes;
   package BEN renames Backend.BE_CORBA_Ada.Nodes;
   package FEU renames Frontend.Nutils;

   function Visible_Is_A_Spec (E : Node_Id) return Node_Id;
   --  Specification for the Is_A routine which must be present in the
   --  stub spec as specified by the mapping rules. This routine is
   --  visible and may be called by any other Ada entity.

   function Visible_Is_A_Body (E : Node_Id) return Node_Id;

   function Local_Is_A_Spec return Node_Id;
   --  This is a private routine and it is called only inside the stub
   --  and its package hierarchy (Helper, Skel, Impl...).

   package body Package_Spec is

      procedure Visit_Attribute_Declaration (E : Node_Id);
      procedure Visit_Constant_Declaration (E : Node_Id);
      procedure Visit_Enumeration_Type (E : Node_Id);
      procedure Visit_Exception_Declaration (E : Node_Id);
      procedure Visit_Forward_Interface_Declaration (E : Node_Id);
      procedure Visit_Interface_Declaration (E : Node_Id);
      procedure Visit_Module (E : Node_Id);
      procedure Visit_Native_Type (E : Node_Id);
      procedure Visit_Operation_Declaration
        (E       : Node_Id;
         Binding : Boolean := True);
      procedure Visit_Specification (E : Node_Id);
      procedure Visit_Structure_Type (E : Node_Id);
      procedure Visit_Type_Declaration (E : Node_Id);
      procedure Visit_Union_Type (E : Node_Id);

      -----------
      -- Visit --
      -----------

      procedure Visit (E : Node_Id) is
      begin
         case FEN.Kind (E) is
            when K_Specification =>
               Visit_Specification (E);

            when K_Constant_Declaration =>
               Visit_Constant_Declaration (E);

            when K_Enumeration_Type =>
               Visit_Enumeration_Type (E);

            when K_Exception_Declaration =>
               Visit_Exception_Declaration (E);

            when K_Forward_Interface_Declaration =>
               Visit_Forward_Interface_Declaration (E);

            when K_Interface_Declaration =>
               Visit_Interface_Declaration (E);

            when K_Operation_Declaration =>
               Visit_Operation_Declaration (E);

            when K_Structure_Type =>
               Visit_Structure_Type (E);

            when K_Union_Type =>
               Visit_Union_Type (E);

            when K_Attribute_Declaration =>
               Visit_Attribute_Declaration (E);

            when K_Type_Declaration =>
               Visit_Type_Declaration (E);

            when K_Native_Type =>
               Visit_Native_Type (E);

            when K_Module =>
               Visit_Module (E);

            when others =>
               null;
         end case;
      end Visit;

      ---------------------------------
      -- Visit_Attribute_Declaration --
      ---------------------------------

      procedure Visit_Attribute_Declaration (E : Node_Id) is
         A : Node_Id;
      begin
         --  IDL attributes are expanded into a couple of Get/Set IDL
         --  subprograms. We only generate the Repository ID when
         --  visiting IDL attribute nodes.

         A := First_Entity (Declarators (E));

         while Present (A) loop
            Set_Main_Spec;

            --  Insert repository declaration. We don't add the
            --  Repository_Id declaration in the case of an Attribute
            --  inherited from the second until the last parent. These
            --  attributes are known by the fact that their parent
            --  interface is different from the current interface.

            if Scope_Entity (Identifier (A)) =
              Corresponding_Entity
              (FE_Node (Current_Entity))
            then
               Append_To (Visible_Part (Current_Package),
                 Map_Repository_Id_Declaration (A));
            end if;

            A := Next_Entity (A);
         end loop;
      end Visit_Attribute_Declaration;

      --------------------------------
      -- Visit_Constant_Declaration --
      --------------------------------

      procedure Visit_Constant_Declaration (E : Node_Id) is
         N             : Node_Id;
         Expression    : Node_Id;
         Constant_Type : constant Node_Id := Map_Expanded_Name (Type_Spec (E));
         K             : FEN.Node_Kind;
         Otyp          : constant Node_Id :=
                           FEU.Get_Original_Type_Specifier (Type_Spec (E));
      begin
         Set_Main_Spec;

         case FEU.Expr_Value (E).K is
            when K_Short .. K_Unsigned_Long_Long
              | K_Octet
              | K_Fixed_Point_Type
              | K_Float .. K_Long_Double =>

               --  If the constant has integer or real type and has negative
               --  value, use the expanded name for "-" operator because it
               --  might not be directly visible.

               declare
                  Minus : Node_Id;
               begin
                  if Negative (Value_Id'(FEU.Expr_Value (E))) then
                     Minus := Make_Selected_Component
                       (Get_Parent_Unit_Name (Constant_Type),
                        Make_Defining_Identifier (SN (S_Minus)));

                     Expression := Make_Subprogram_Call
                       (Minus,
                        New_List
                          (Make_Literal (New_Value (-FEU.Expr_Value (E)))));

                  else
                     Expression := Make_Literal (FEU.Expr_Value (E));
                  end if;
               end;
            when others =>
               Expression := Make_Literal (FEU.Expr_Value (E));
         end case;

         --  If the constant type is of a string type, it needs to be
         --  converted using To_CORBA_[Wide_]String (for the unbounded case),
         --  To_Bounded_[Wide_]String (for the bounded case).
         --  Determine the expanded name of these subprograms according to
         --  whether the type is directly CORBA.[Wide_]String, or a derived
         --  type thereof.

         K := FEN.Kind (Otyp);

         case K is
            when K_String           |
                 K_String_Type      |
                 K_Wide_String      |
                 K_Wide_String_Type =>

               declare
                  S : Node_Id := No_Node;
                  --  Selected_Component denoting a conversion function, for
                  --  the case where the constant's type is not a root
                  --  unbounded string type.

                  Converter : Node_Id;
               begin
                  case K is
                     when K_String =>
                        Converter := RE (RE_To_CORBA_String);
                     when K_Wide_String =>
                        Converter := RE (RE_To_CORBA_Wide_String);

                     when K_String_Type      |
                          K_Wide_String_Type =>
                        declare
                           Str_Instance : constant Node_Id :=
                                            Defining_Identifier
                                              (Instantiation_Node
                                                (BE_Node (Otyp)));
                           Id : Node_Id;
                        begin
                           if K = K_String_Type then
                              Id := Make_Identifier
                                      (SN ((S_To_Bounded_String)));
                           else
                              Id := Make_Identifier
                                      (SN ((S_To_Bounded_Wide_String)));
                           end if;
                           Converter := Make_Selected_Component
                                          (Str_Instance, Id);
                        end;
                     when others =>
                        raise Program_Error;
                  end case;

                  if Otyp = Type_Spec (E) then
                     Expression := Make_Subprogram_Call
                                     (Converter, New_List (Expression));
                  else
                     S := Make_Selected_Component
                            (Get_Parent_Unit_Name
                               (Get_Type_Definition_Node (Type_Spec (E))),
                                  Selector_Name (Converter));

                     --  The call to Copy_Node ensures the addition of
                     --  necessary WITH clauses.

                     Expression := Make_Subprogram_Call
                       (Copy_Node (S), New_List (Expression));
                  end if;
               end;

            when others =>
               null;
         end case;

         N := Make_Object_Declaration
           (Defining_Identifier => Map_Defining_Identifier (E),
            Constant_Present    => True,
            Object_Definition   => Constant_Type,
            Expression          => Expression);
         Bind_FE_To_BE (Identifier (E), N, B_Stub);
         Append_To (Visible_Part (Current_Package), N);
      end Visit_Constant_Declaration;

      ----------------------------
      -- Visit_Enumeration_Type --
      ----------------------------

      procedure Visit_Enumeration_Type (E : Node_Id) is
         Enumerator     : Node_Id;
         Enum_Literals  : List_Id;
         Enum_Literal   : Node_Id;
         Enum_Type_Decl : Node_Id;

      begin
         Set_Main_Spec;

         Enum_Literals := New_List;
         Enumerator := First_Entity (Enumerators (E));

         while Present (Enumerator) loop
            Enum_Literal := Map_Defining_Identifier (Enumerator);
            Append_To (Enum_Literals, Enum_Literal);
            Enumerator := Next_Entity (Enumerator);
         end loop;

         Enum_Type_Decl :=
           Make_Full_Type_Declaration
           (Map_Defining_Identifier (E),
            Make_Enumeration_Type_Definition (Enum_Literals));

         Bind_FE_To_BE  (Identifier (E), Enum_Type_Decl, B_Stub);
         Bind_FE_To_BE (Identifier (E), Enum_Type_Decl, B_Type_Def);
         Append_To (Visible_Part (Current_Package), Enum_Type_Decl);
         Append_To (Visible_Part (Current_Package),
           Map_Repository_Id_Declaration (E));
      end Visit_Enumeration_Type;

      ---------------------------------
      -- Visit_Exception_Declaration --
      ---------------------------------

      procedure Visit_Exception_Declaration (E : Node_Id) is
         Identifier : Node_Id;
         N          : Node_Id;
      begin
         Set_Main_Spec;

         --  Declaration of the exception

         Get_Name_String (To_Ada_Name (IDL_Name (FEN.Identifier (E))));
         Identifier := Make_Defining_Identifier (Name_Find);
         N := Make_Exception_Declaration (Identifier);
         Append_To (Visible_Part (Current_Package), N);

         --  Link the frontend node to the backend exception

         Bind_FE_To_BE (FEN.Identifier (E), N, B_Stub);

         --  Definition of the "Exception_Name"_Members type

         Get_Name_String (To_Ada_Name (IDL_Name (FEN.Identifier (E))));
         Add_Str_To_Name_Buffer ("_Members");
         Identifier := Make_Defining_Identifier (Name_Find);
         N := Make_Full_Type_Declaration
           (Defining_Identifier => Identifier,
            Type_Definition     => Make_Derived_Type_Definition
            (RE (RE_IDL_Exception_Members),
             Make_Record_Definition
             (Map_Members_Definition (Members (E)))));
         Append_To (Visible_Part (Current_Package), N);

         --  Link the frontend node to the backend type definition

         Bind_FE_To_BE (FEN.Identifier (E), N, B_Type_Def);

         --  Insert repository declaration

         Append_To (Visible_Part (Current_Package),
           Map_Repository_Id_Declaration (E));

         --  Insert the Get_Members procedure specification

         N := Map_Get_Members_Spec
           (Make_Selected_Component
            (Defining_Identifier (Stubs_Package (Current_Entity)),
             Identifier));

         Append_To (Visible_Part (Current_Package), N);
      end Visit_Exception_Declaration;

      -----------------------------------------
      -- Visit_Forward_Interface_Declaration --
      -----------------------------------------

      procedure Visit_Forward_Interface_Declaration (E : Node_Id) is
         Identifier    : Node_Id;
         N             : Node_Id;
         Ref_Type_Node : Node_Id;
      begin
         --  The "Interface_Name"_Forward package is instantiated :

         --   * In the module main package if the interface is
         --  declared in a module.

         --   * In the XXXX_IDL_FILE main package if the interface is
         --  declared outside any module.

         Set_Main_Spec;

         --  Setting the interface as forwarded to be able to add the
         --  additional code related to forwarding.

         Set_Forwarded (Forward (E));

         Get_Name_String (To_Ada_Name (IDL_Name (FEN.Identifier (E))));
         Add_Str_To_Name_Buffer ("_Forward");
         Identifier := Make_Defining_Identifier (Name_Find);

         --  Generic Package Instantiation

         N := Make_Package_Instantiation
           (Defining_Identifier => Identifier,
            Generic_Package     => RU (RU_CORBA_Forward));
         Bind_FE_To_BE (FEN.Identifier (E), N, B_Instantiation);

         --  Adding the binding between the interface declaration and
         --  the instantiated package.

         Bind_FE_To_BE  (FEN.Identifier (Forward (E)), N, B_Forward);

         Append_To (Visible_Part (Current_Package), N);

         --  This workaround is used to permit the use of the Ref type
         --  declared in the instantiated package.

         Identifier := Map_Ref_Type (E);
         Ref_Type_Node := Make_Full_Type_Declaration
           (Identifier,
            Make_Derived_Type_Definition
            (Subtype_Indication    =>
               Map_Ref_Type_Ancestor (E, False),
             Record_Extension_Part =>
               Make_Record_Type_Definition
               (Record_Definition => Make_Record_Definition (No_List))),
           Parent => N);

         --  We don't add this node!

         Bind_FE_To_BE (FEN.Identifier (E), Ref_Type_Node, B_Type_Def);
      end Visit_Forward_Interface_Declaration;

      ---------------------------------
      -- Visit_Interface_Declaration --
      ---------------------------------

      procedure Visit_Interface_Declaration (E : Node_Id) is
         P        : Node_Id;
         N        : Node_Id;
         L        : List_Id;
         I        : Node_Id;
         Is_Local : constant Boolean := Is_Local_Interface (E);
      begin
         P := Map_IDL_Unit (E);
         Append_To (Packages (Current_Entity), P);
         Push_Entity (P);
         Set_Main_Spec;
         L := Interface_Spec (E);

         --  Checking whether the interface inherits from other
         --  interfaces or not.

         --  Extract from the Ada mapping specifications :
         --
         --  "Single inheritance of IDL interface is directly mapped
         --  to inheritance in the Ada mapping; that is, an interface
         --  with a parent is mapped to a tagged type that is derived
         --  from the tagged type mapped from the parent. The
         --  definitions of types, constants, and exceptions in the
         --  parent package are renamed or sub-typed so that they are
         --  also inherited in accordance with the IDL semantics"
         --
         --  "The client side of multiple inheritance in IDL maps to
         --  single Ref tagged type, as with single inheritance, where
         --  the parent type is the first interface listed in the IDL
         --  parent interface list. The IDL compiler must generate
         --  additional primitive subprograms that correspond to the
         --  operations inherited from the second and subsequent
         --  parent interfaces listed in the IDL."

         if FEU.Is_Empty (L) then
            --  The reference type ancestor depends on the nature of
            --  the interface (unconstrained, local or abstract)

            N := Map_Ref_Type_Ancestor (E);
         else
            N := Map_Expanded_Name (First_Entity (L));
         end if;

         --  The designator of the reference type is also dependant of
         --  the nature of the interface (unconstrained, local or
         --  abstract).

         I := Map_Ref_Type (E);

         N := Make_Full_Type_Declaration
           (I, Make_Derived_Type_Definition
            (Subtype_Indication    => N,
             Record_Extension_Part =>
               Make_Record_Type_Definition
             (Record_Definition => Make_Record_Definition (No_List))));
         Append_To (Visible_Part (Current_Package), N);

         --  An Interface Declaration is also a type definition. So we
         --  link the type declaration node to the IDL interface node
         --  to be able to fetch it when needed.

         Bind_FE_To_BE (Identifier (E), N, B_Type_Def);

         N := Map_Repository_Id_Declaration (E);
         Append_To (Visible_Part (Current_Package), N);
         Set_FE_Node (N, Identifier (E));

         N := First_Entity (Interface_Body (E));

         while Present (N) loop
            Visit (N);
            N := Next_Entity (N);
         end loop;

         --  In case of multiple inheritance, generate the mappings
         --  for the operations and attributes of the parents except
         --  the first one which is handled by the Ada inheritence
         --  mechanism.

         Map_Inherited_Entities_Specs
           (Current_Interface    => E,
            Visit_Operation_Subp => Visit_Operation_Declaration'Access,
            Stub                 => True);

         --  Local interfaces don't have Is_A function in their stub
         --  spec.

         if not Is_Local then
            N := Visible_Is_A_Spec (E);
            Append_To (Visible_Part (Current_Package), N);
         end if;

         --  If we handle a forwarded interface we must instantiate
         --  the "Interface_Name"_Forward.Convert package.

         if Is_Forwarded (E) then
            declare
               Pack_Inst : Node_Id;
               Gen_Pack  : Node_Id;
            begin
               Pack_Inst := RE (RE_Convert_Forward);

               Gen_Pack := Make_Selected_Component
                 (Expand_Designator
                  (Forward_Node
                   (BE_Node
                    (Identifier (E)))),
                  RE (RE_Convert));

               --  To guarantee that the "with" clause of the generic
               --  package would be added, we use the Copy_Expanded_Name
               --  function.

               N := Make_Package_Instantiation
                 (Defining_Identifier => Pack_Inst,
                  Generic_Package     => Copy_Expanded_Name (Gen_Pack),
                  Parameter_List      => New_List (Map_Ref_Type (E)));
               Append_To (Visible_Part (Current_Package), N);
            end;
         end if;

         if not Is_Local then
            N := Local_Is_A_Spec;
            Append_To (Private_Part (Current_Package), N);
         end if;

         Pop_Entity;
      end Visit_Interface_Declaration;

      ------------------
      -- Visit_Module --
      ------------------

      procedure Visit_Module (E : Node_Id) is
         D : Node_Id;
         S : Node_Id;

      begin
         if not Map_Particular_CORBA_Parts (E, PK_Stub_Spec) then
            S := Map_IDL_Unit (E);
            Append_To (Packages (Current_Entity), S);
            Push_Entity (S);
            Set_Main_Spec;
            Append_To (Visible_Part (Current_Package),
              Map_Repository_Id_Declaration (E));

            D := First_Entity (Definitions (E));

            while Present (D) loop
               Visit (D);
               D := Next_Entity (D);
            end loop;

            Pop_Entity;
         end if;
      end Visit_Module;

      ---------------------------------
      -- Visit_Operation_Declaration --
      ---------------------------------

      procedure Visit_Operation_Declaration
        (E       : Node_Id;
         Binding : Boolean := True)
      is
         Subp_Spec       : Node_Id;
         Profile         : List_Id;
         IDL_Param       : Node_Id;
         Ada_Param       : Node_Id;
         Mode            : Mode_Id := Mode_In;
         Returns         : Node_Id := No_Node;
         Type_Designator : Node_Id;
         Container       : constant Node_Id := Scope_Entity (Identifier (E));

         function Map_Parameter_Type_Designator
           (Entity : Node_Id)
           return Node_Id;
         --  Maps Ada type from the entity type specifier

         -----------------------------------
         -- Map_Parameter_Type_Designator --
         -----------------------------------

         function Map_Parameter_Type_Designator
           (Entity : Node_Id)
           return Node_Id
         is
            Result : Node_Id;
         begin
            Result := Map_Expanded_Name (Type_Spec (Entity));

            if Is_Class_Wide (Entity) then
               Result := Make_Attribute_Reference (Result, A_Class);
            end if;

            return Result;
         end Map_Parameter_Type_Designator;

      begin
         Profile := New_List;

         --  Create a dispatching parameter

         Ada_Param := Make_Parameter_Specification
           (Make_Defining_Identifier (PN (P_Self)),
            Map_Ref_Type (Container));
         Append_To (Profile, Ada_Param);

         --  Create an Ada subprogram parameter for each IDL subprogram
         --  parameter. Check whether there is one inout or out parameter.

         IDL_Param := First_Entity (Parameters (E));

         while Present (IDL_Param) loop
            Type_Designator := Map_Parameter_Type_Designator (IDL_Param);

            Set_FE_Node (Type_Designator, Type_Spec (IDL_Param));
            Ada_Param := Make_Parameter_Specification
              (Map_Defining_Identifier (Declarator (IDL_Param)),
               Type_Designator,
               FEN.Parameter_Mode (IDL_Param));

            if FEN.Parameter_Mode (IDL_Param) /= Mode_In then
               Mode := Mode_Out;
            end if;

            Append_To (Profile, Ada_Param);
            IDL_Param := Next_Entity (IDL_Param);
         end loop;

         --  If the operation has a non-empty context specification, add an
         --  extra parameter 'In_Context'.

         --  XXX : The contexts are not completely implemented in PolyORB. Once
         --  they are implemented a routine which tests the consistency of the
         --  context must be generated.

         declare
            L : constant List_Id := Contexts (E);
         begin
            if not FEU.Is_Empty (L) then
               Ada_Param := Make_Parameter_Specification
                 (Make_Defining_Identifier (PN (P_In_Context)),
                  RE (RE_Ref_8),
                  Mode_In,
                  RE (RE_Get_Default_Context));
               Append_To (Profile, Ada_Param);
            end if;
         end;

         --  For a non-void IDL operation, check for parameters of mode out or
         --  inout parameters, in which case it must be mapped to an Ada
         --  procedure, instead of an Ada function.

         if FEN.Kind (Type_Spec (E)) /= K_Void then
            if Mode = Mode_In then
               Returns := Map_Parameter_Type_Designator (E);
               Set_FE_Node (Returns, Type_Spec (E));

               --  If the IDL function is mapped to an Ada procedure, add a new
               --  out parameter Returns to hold the returned value.
            else
               Type_Designator := Map_Parameter_Type_Designator (E);
               Set_FE_Node (Type_Designator, Type_Spec (E));
               Ada_Param := Make_Parameter_Specification
                 (Make_Defining_Identifier (PN (P_Returns)),
                  Type_Designator,
                  Mode_Out);
               Append_To (Profile, Ada_Param);
            end if;
         end if;

         --  Add the subprogram to main specification

         Set_Main_Spec;
         Subp_Spec := Make_Subprogram_Specification
           (Map_Defining_Identifier (E), Profile, Returns);
         Append_To (Visible_Part (Current_Package), Subp_Spec);

         --  We don't add the Repository_Id declaration in the case of
         --  an Operation inherited from the second until the last
         --  parent.

         --  We don't add repository declaration in the case of an
         --  operation expanded from an attribute declaration.
         --  Operations that are expanded from an attribute
         --  declaration are known because the identifiers have no
         --  locations.

         if Scope_Entity (Identifier (E)) =
           Corresponding_Entity (FE_Node (Current_Entity))
           and then FEN.Loc (Identifier (E)) /= No_Location
         then
            Append_To (Visible_Part (Current_Package),
              Map_Repository_Id_Declaration (E));
         end if;

         if Binding then
            Bind_FE_To_BE (Identifier (E), Subp_Spec, B_Stub);
         end if;
      end Visit_Operation_Declaration;

      -------------------------
      -- Visit_Specification --
      -------------------------

      procedure Visit_Specification (E : Node_Id) is
         Definition : Node_Id;
      begin
         Push_Entity (Stub_Node (BE_Node (Identifier (E))));
         Definition := First_Entity (Definitions (E));

         while Present (Definition) loop
            Visit (Definition);
            Definition := Next_Entity (Definition);
         end loop;

         Pop_Entity;
      end Visit_Specification;

      --------------------------
      -- Visit_Structure_Type --
      --------------------------

      procedure Visit_Structure_Type (E : Node_Id) is
         N : Node_Id;
      begin
         Set_Main_Spec;

         N := Make_Full_Type_Declaration
           (Map_Defining_Identifier (E),
            Make_Record_Type_Definition
            (Make_Record_Definition
             (Map_Members_Definition (Members (E)))));

         Bind_FE_To_BE (Identifier (E), N, B_Stub);
         Bind_FE_To_BE (Identifier (E), N, B_Type_Def);

         Append_To (Visible_Part (Current_Package), N);
         Append_To (Visible_Part (Current_Package),
           Map_Repository_Id_Declaration (E));
      end Visit_Structure_Type;

      ----------------------------
      -- Visit_Type_Declaration --
      ----------------------------

      procedure Visit_Type_Declaration (E : Node_Id) is
         D                : Node_Id;
         T                : Node_Id;
         N                : Node_Id;
         Is_Subtype       : Boolean := False;
         Type_Spec_Node   : Node_Id;
      begin
         Set_Main_Spec;
         Type_Spec_Node := Type_Spec (E);

         if FEN.Kind (Type_Spec_Node) = K_Fixed_Point_Type then
            --  The fixed type shall be mapped to an equivalent Ada
            --  decimal type which will be the original type
            --  definition of all the declarators of the IDL type
            --  declaration.

            declare
               Fixed_Type_Node : Node_Id;
               Fixed_Name      : constant Name_Id
                 := Map_Fixed_Type_Name (Type_Spec_Node);
            begin
               T := Make_Defining_Identifier (Fixed_Name);

               Fixed_Type_Node := Make_Full_Type_Declaration
                 (Defining_Identifier => T,
                  Type_Definition     => Make_Decimal_Type_Definition
                    (Type_Spec_Node));
               Append_To (Visible_Part (Current_Package), Fixed_Type_Node);

               T := Make_Selected_Component
                 (Defining_Identifier (Stubs_Package (Current_Entity)), T);

               --  Link the front end node to the Ada type definition

               Bind_FE_To_BE (Type_Spec_Node, Fixed_Type_Node, B_Type_Def);
            end;
         elsif FEN.Kind (Type_Spec_Node) = K_Sequence_Type then
            --  The sequence type is mapped into a generic package
            --  instantiation. The Sequence type of the instantiated
            --  package will be the original type of each one of the
            --  declarators of the type declaration.

            declare
               Seq_Package_Inst : Node_Id;
               Bounded          : constant Boolean :=
                 Present (Max_Size (Type_Spec_Node));
               CORBA_Seq        : Node_Id;
               Seq_Package_Name : Name_Id;
               Seq_Package_Node : Node_Id;
               Type_Node        : Node_Id;
            begin
               --  We create an Instantiation of the generic package
               --  CORBA.Sequences.Bounded or
               --  CORBA.Sequences.Unbounded. Then, the sequence type
               --  is derived from the "Sequence" Type of the
               --  instantiated package.

               --  Creating the package name conforming to the Ada
               --  mapping specification.

               Seq_Package_Name := Map_Sequence_Pkg_Name (Type_Spec_Node);

               if Bounded then
                  CORBA_Seq := RU (RU_CORBA_Sequences_Bounded);
               else
                  CORBA_Seq := RU (RU_CORBA_Sequences_Unbounded);
               end if;

               --  Building the sequence package node

               Type_Node := Map_Expanded_Name (Type_Spec (Type_Spec_Node));

               Seq_Package_Node := Make_Defining_Identifier
                 (Seq_Package_Name);

               if Bounded then
                  Seq_Package_Inst := Make_Package_Instantiation
                    (Defining_Identifier => Seq_Package_Node,
                     Generic_Package     => CORBA_Seq,
                     Parameter_List      => New_List
                     (Type_Node,
                      Make_Literal
                      (FEU.Expr_Value
                       (Max_Size
                        (Type_Spec_Node)))));
               else
                  Seq_Package_Inst := Make_Package_Instantiation
                    (Defining_Identifier => Seq_Package_Node,
                     Generic_Package     => CORBA_Seq,
                     Parameter_List      => New_List (Type_Node));
               end if;
               Append_To (Visible_Part (Current_Package), Seq_Package_Inst);

               --  Link the frontend node to the package instantiation

               Bind_FE_To_BE (Type_Spec_Node,
                              Seq_Package_Inst,
                              B_Instantiation);

               T := Make_Selected_Component
                 (Make_Selected_Component
                  (Defining_Identifier (Stubs_Package (Current_Entity)),
                   Seq_Package_Node),
                  Make_Identifier (TN (T_Sequence)));

               --  Link the frontend node to the Sequence type designator

               Bind_FE_To_BE (Type_Spec_Node, T, B_Type_Def);
            end;

         elsif FEN.Kind (Type_Spec_Node) = K_String_Type or else
           FEN.Kind (Type_Spec_Node) = K_Wide_String_Type
         then
            --  The IDL bounded string or wide string types are mapped
            --  into a generic package instantiation. The
            --  Bounded_String type or Bounded_Wide_String type of the
            --  instantiated package will be the original type of each
            --  one of the declarators of the type declaration.

            declare
               Str_Package_Inst : Node_Id;
               Pkg_Name         : Name_Id;
               Pkg_Node         : Node_Id;
               CORBA_String_Pkg : Node_Id;
            begin
               --  We create an Instantiation of the generic package
               --  CORBA.Bounded_Strings (or
               --  CORBA.Bounded_Wide_Strings).  Then, the string type
               --  is derived from the 'Bounded_String' type (or the
               --  'Bounded_Wide_String' type of the instantiated
               --  package.

               --  Creating the package name conforming to the Ada
               --  mapping specification.

               Pkg_Name := Map_String_Pkg_Name (Type_Spec_Node);

               if FEN.Kind (Type_Spec_Node) = K_Wide_String_Type then
                  CORBA_String_Pkg := RU (RU_CORBA_Bounded_Wide_Strings);
                  T := Make_Identifier (TN (T_Bounded_Wide_String));
               else
                  CORBA_String_Pkg := RU (RU_CORBA_Bounded_Strings);
                  T := Make_Identifier (TN (T_Bounded_String));
               end if;

               --  Building the string package node

               Pkg_Node := Make_Defining_Identifier
                 (Pkg_Name);

               Str_Package_Inst := Make_Package_Instantiation
                 (Defining_Identifier => Pkg_Node,
                  Generic_Package     => CORBA_String_Pkg,
                  Parameter_List      => New_List
                  (Make_Literal (FEU.Expr_Value (Max_Size (Type_Spec_Node)))));
               Append_To (Visible_Part (Current_Package), Str_Package_Inst);

               --  Link the frontend node to the package instantiation

               Bind_FE_To_BE (Type_Spec_Node,
                              Str_Package_Inst,
                              B_Instantiation);

               --  Setting the correct parent unit name of the
               --  instantiated type.

               T := Make_Selected_Component
                 (Make_Selected_Component
                  (Defining_Identifier (Stubs_Package (Current_Entity)),
                   Pkg_Node),
                  T);

               --  Link the frontend node to the Sequence type designator

               Bind_FE_To_BE (Type_Spec_Node, T, B_Type_Def);
            end;
         else
            --  General case

            T := Map_Expanded_Name (Type_Spec_Node);
         end if;

         --  According to the Ada mapping specification. Most of the
         --  type definitions in an IDL file should be mapped to :
         --  "type ... is new ...;". However, there are exception to
         --  this rule : "interface Base {...}; typedef Base Root;"
         --  should be mapped : "subtype Root is Base.Ref;"

         --  Determining whether we map the type definition to a "type
         --  ... is new ...;" or a "subtype ... is ...;" statement.

         Is_Subtype := Is_Object_Type (Type_Spec (E));

         D := First_Entity (Declarators (E));
         while Present (D) loop
            if Kind (D) = K_Complex_Declarator then
               N := Make_Full_Type_Declaration
                 (Defining_Identifier => Map_Defining_Identifier (D),
                  Type_Definition     => Make_Array_Type_Definition
                  (Map_Range_Constraints
                   (FEN.Array_Sizes (D)), T));
            else
               N := Make_Full_Type_Declaration
                 (Defining_Identifier => Map_Defining_Identifier (D),
                  Type_Definition     => Make_Derived_Type_Definition
                  (Subtype_Indication    => T,
                   Record_Extension_Part => No_Node,
                   Is_Subtype => Is_Subtype),
                  Is_Subtype => Is_Subtype);
            end if;

            --  Create the bindings between the IDL tree and the Ada
            --  tree.

            Bind_FE_To_BE (Identifier (D), N, B_Stub);
            Bind_FE_To_BE (Identifier (D), N, B_Type_Def);

            Append_To (Visible_Part (Current_Package), N);
            Append_To (Visible_Part (Current_Package),
              Map_Repository_Id_Declaration (D));
            D := Next_Entity (D);
         end loop;
      end Visit_Type_Declaration;

      -----------------------
      -- Visit_Native_Type --
      -----------------------

      procedure Visit_Native_Type (E : Node_Id) is
         --  Extract from the CORBA 3.0.3 spec (§3.11.5) concerning
         --  native types :

         --  "This declaration defines a new type with the specified
         --  name. A native type is similar to an IDL basic type. The
         --  possible values of a native type are language-mapping
         --  dependent, as are the means for constructing them and
         --  manipulating them.  Any interface that defines a native
         --  type requires each language mapping to define how the
         --  native type is mapped into that programming language."

         --  So, we put a comment to indicate that.

         N         : Node_Id;
         Type_Name : constant Name_Id :=
           IDL_Name (Identifier (Declarator (E)));
      begin
         Set_Str_To_Name_Buffer ("Type ");
         Get_Name_String_And_Append (Type_Name);
         Add_Str_To_Name_Buffer (" is implementation defined");
         N := Make_Ada_Comment (Name_Find);
         Append_To (Visible_Part (Current_Package), N);
      end Visit_Native_Type;

      ----------------------
      -- Visit_Union_Type --
      ----------------------

      procedure Visit_Union_Type (E : Node_Id) is
         N              : Node_Id;
         S              : constant Node_Id := Switch_Type_Spec (E);
         Orig_Type      : constant Node_Id :=
           FEU.Get_Original_Type_Specifier (S);
         T              : Node_Id;
         L              : List_Id;
         Literal_Parent : Node_Id := No_Node;
      begin
         Set_Main_Spec;
         T := Map_Expanded_Name (S);

         --  If the discriminator is an enumeration type, we must put
         --  the full names of literals

         if FEN.Kind (Orig_Type) = K_Enumeration_Type then
            Literal_Parent := Map_Expanded_Name
              (Scope_Entity
               (Identifier
                (Orig_Type)));
         end if;

         L := New_List;
         Append_To (L,
           Make_Variant_Part
             (Make_Defining_Identifier (CN (C_Switch)),
                Map_Variant_List (Switch_Type_Body (E), Literal_Parent)));

         N := Make_Full_Type_Declaration
           (Map_Defining_Identifier (E),
            Make_Record_Type_Definition
            (Make_Record_Definition (L)),
            New_List
            (Make_Component_Declaration
             (Make_Defining_Identifier (CN (C_Switch)), T,
              Make_Attribute_Reference (T, A_First))));
         Bind_FE_To_BE (Identifier (E), N, B_Stub);
         Bind_FE_To_BE (Identifier (E), N, B_Type_Def);

         Append_To (Visible_Part (Current_Package), N);
         Append_To (Visible_Part (Current_Package),
           Map_Repository_Id_Declaration (E));
      end Visit_Union_Type;
   end Package_Spec;

   package body Package_Body is

      function Stub_Statements (E : Node_Id) return List_Id;
      --  Creates the statement list of the stub body

      function Stub_Declarations (E : Node_Id) return List_Id;
      --  Creates the declaration list of the stub body

      procedure Visit_Interface_Declaration (E : Node_Id);
      procedure Visit_Operation_Declaration (E : Node_Id);
      procedure Visit_Specification (E : Node_Id);
      procedure Visit_Exception_Declaration (E : Node_Id);
      procedure Visit_Module (E : Node_Id);

      -----------
      -- Visit --
      -----------

      procedure Visit (E : Node_Id) is
      begin
         case FEN.Kind (E) is
            when K_Specification =>
               Visit_Specification (E);

            when K_Exception_Declaration =>
               Visit_Exception_Declaration (E);

            when K_Interface_Declaration =>
               Visit_Interface_Declaration (E);

            when K_Operation_Declaration =>
               Visit_Operation_Declaration (E);

            when K_Module =>
               Visit_Module (E);

            when others =>
               null;
         end case;
      end Visit;

      ---------------------------------
      -- Visit_Exception_Declaration --
      ---------------------------------

      procedure Visit_Exception_Declaration (E : Node_Id) is
         Spec : Node_Id := No_Node;
         D    : constant List_Id := No_List;
         S    : constant List_Id := New_List;
         N    : Node_Id;
         Parameters : List_Id;
      begin
         Set_Main_Body;

         Spec := Map_Get_Members_Spec
           (Expand_Designator
            (Type_Def_Node
             (BE_Node
              (FEN.Identifier
               (E)))));

         Parameters := New_List;
         Append_To (Parameters, Make_Defining_Identifier (PN (P_From)));
         Append_To (Parameters, Make_Defining_Identifier (PN (P_To)));

         N := Make_Subprogram_Call
           (RE (RE_User_Get_Members),
            Parameters);
         Append_To (S, N);

         N := Make_Subprogram_Body
           (Specification => Spec,
            Declarations => D,
            Statements => S);
         Append_To (Statements (Current_Package), N);
      end Visit_Exception_Declaration;

      ------------------
      -- Visit_Module --
      ------------------

      procedure Visit_Module (E : Node_Id) is
         S : Node_Id;
         D : Node_Id;
      begin
         if not Map_Particular_CORBA_Parts (E, PK_Stub_Body) then
            S := Stub_Node (BE_Node (Identifier (E)));
            Push_Entity (S);
            D := First_Entity (Definitions (E));
            while Present (D) loop
               Visit (D);
               D := Next_Entity (D);
            end loop;
            Pop_Entity;
         end if;
      end Visit_Module;

      ---------------------
      -- Visit_Interface --
      ---------------------

      procedure Visit_Interface_Declaration (E : Node_Id) is
         N        : Node_Id;
         Is_Local : constant Boolean := Is_Local_Interface (E);
      begin
         N := BEN.Parent (Type_Def_Node (BE_Node (Identifier (E))));
         Push_Entity (BEN.IDL_Unit (Package_Declaration (N)));
         Set_Main_Body;
         N := First_Entity (Interface_Body (E));

         while Present (N) loop
            Visit (N);
            N := Next_Entity (N);
         end loop;

         --  In case of multiple inheritance, generate the mappings
         --  for the operations and attributes of the parents except
         --  the first one.

         Map_Inherited_Entities_Bodies
           (Current_Interface    => E,
            Visit_Operation_Subp => Visit_Operation_Declaration'Access,
            Stub                 => True);

         if not Is_Local then
            N := Visible_Is_A_Body (E);
            Append_To (Statements (Current_Package), N);
            N := Local_Is_A_Body (E);
            Append_To (Statements (Current_Package), N);
         end if;

         Pop_Entity;
      end Visit_Interface_Declaration;

      ---------------------------------
      -- Visit_Operation_Declaration --
      ---------------------------------

      procedure Visit_Operation_Declaration (E : Node_Id) is
         Spec         : constant Node_Id :=
           Stub_Node (BE_Node (Identifier (E)));
         Declarations : List_Id;
         Statements   : List_Id;
         N : Node_Id;
      begin
         Set_Main_Body;

         --  The declarative part of the subprogram

         Declarations := Stub_Declarations (E);

         --  The statements of the subprogram

         Statements   := Stub_Statements (E);

         N := Make_Subprogram_Body (Spec, Declarations, Statements);
         Append_To (BEN.Statements (Current_Package), N);
      end Visit_Operation_Declaration;

      -------------------------
      -- Visit_Specification --
      -------------------------

      procedure Visit_Specification (E : Node_Id) is
         Definition : Node_Id;

      begin
         Push_Entity (Stub_Node (BE_Node (Identifier (E))));

         Definition := First_Entity (Definitions (E));

         while Present (Definition) loop
            Visit (Definition);
            Definition := Next_Entity (Definition);
         end loop;

         Pop_Entity;
      end Visit_Specification;

      ---------------------
      -- Stub_Statements --
      ---------------------

      function Stub_Statements (E : Node_Id) return List_Id is
         pragma Assert (FEN.Kind (E) = K_Operation_Declaration);

         Statements      : constant List_Id := New_List;
         N               : Node_Id;
         M               : Node_Id;
         C               : Node_Id;
         P               : Node_Id;
         Profile         : List_Id;
         I               : Node_Id;
         Param           : Node_Id;
         R               : Name_Id;
         Operation_Name  : constant Name_Id := FEN.IDL_Name (Identifier (E));
         Argument_Name   : Name_Id;
         Container       : constant Node_Id := Scope_Entity (Identifier (E));
         Local_Interface : constant Boolean :=
           (FEN.Kind (Container) = K_Interface_Declaration
            and then Is_Local_Interface (Container));
         NVList_Name     : Name_Id;

         --  The flags below indicate whether the operation is mapped
         --  to an Ada function or an Ada procedure.

         Has_Out_Params  : constant Boolean := Contains_Out_Parameters (E);
         Non_Void        : constant Boolean :=
           FEN.Kind (Type_Spec (E)) /= K_Void;
         Is_Function     : constant Boolean :=
           Non_Void and then not Has_Out_Params;
      begin
         --  Generate nil reference check for Self

         --  FIXME: In the case of an abstract interface, we should test
         --  whether the Object passed is a concrete interface type, in which
         --  which case we pass it as a reference, or whether it is a value
         --  type, in which case we pass it as a value. However, since
         --  ValueTypes are not supported yet, we do only the first test.

         C := Make_Subprogram_Call
           (RE (RE_Is_Nil),
            New_List
            (Make_Subprogram_Call
             (RE (RE_Ref_2),
              New_List (Make_Defining_Identifier (PN (P_Self))))));

         N := Make_Subprogram_Call
           (RE (RE_Raise_Inv_Objref),
            New_List
            (RE (RE_Default_Sys_Member)));

         N := Make_If_Statement
           (Condition       => C,
            Then_Statements => New_List (N));
         Append_To (Statements, N);

         --  If the interface is local, we just call the
         --  implementation.

         if Local_Interface then
            declare
               Implem_Node  : Node_Id;
               Impl_Profile : constant List_Id :=
                 New_List;
               Param        : Node_Id;
            begin
               N := Make_Subprogram_Call
                 (Make_Defining_Identifier (SN (S_Entity_Of)),
                  New_List
                  (Make_Defining_Identifier (PN (P_Self))));

               --  Get the Object_Ptr type full name

               Implem_Node := Expand_Designator
                 (Next_Node
                  (Impl_Node
                   (BE_Node
                    (Identifier
                     (Container)))));

               N := Make_Subprogram_Call
                 (Implem_Node,
                  New_List (N));

               Append_To (Impl_Profile, N);

               --  Adding the rest of the parameters

               P := First_Entity (Parameters (E));

               while Present (P) loop
                  Param := Map_Defining_Identifier (Declarator (P));

                  --  If the parameter type is a class-wide type,
                  --  we cast it.

                  if Is_Class_Wide (P) then
                     Param := Make_Type_Conversion
                       (Get_Type_Definition_Node (Type_Spec (P)),
                        Param);
                  end if;

                  Append_To (Impl_Profile, Param);

                  P := Next_Entity (P);
               end loop;

               --  If a non void operation has OUT parameters, append
               --  the returns additional parameter.

               if Non_Void and then not Is_Function then
                  Append_To (Impl_Profile, Make_Identifier (PN (P_Returns)));
               end if;

               Implem_Node := Expand_Designator
                 (Impl_Node
                  (BE_Node
                   (Identifier
                    (E))));

               N := Make_Subprogram_Call
                 (Implem_Node,
                  Impl_Profile);

               if Is_Function then
                  N := Make_Return_Statement (N);
               end if;

               Append_To (Statements, N);
               return Statements;
            end;
         end if;

         --  The argument list nature is different depending on the
         --  way requests are handled (SII or DII)

         --  Create argument list

         if not Use_SII then
            Set_Str_To_Name_Buffer
              ("Create the Argument list");
            Append_To (Statements, Make_Ada_Comment (Name_Find));

            C := Make_Subprogram_Call
              (RE (RE_Create),
               New_List
               (Make_Defining_Identifier (VN (V_Argument_List))));
            Append_To (Statements, C);
         end if;

         --  Add arguments to argument  list

         P := First_Entity (Parameters (E));

         if Present (P) then
            Set_Str_To_Name_Buffer ("Fill the Argument list");
            Append_To (Statements, Make_Ada_Comment (Name_Find));
         end if;

         while Present (P) loop
            Argument_Name := To_Ada_Name
              (IDL_Name (Identifier (Declarator (P))));

            if Use_Compiler_Alignment then
               C := Make_Identifier (Argument_Name);
               Marshall_Args (Statements, Type_Spec (P), C);
            elsif Use_SII then
               --  Updating the record field corresponding to the
               --  parameter When the parameter mode is IN or INOUT

               if FEN.Parameter_Mode (P) = Mode_In
                 or else FEN.Parameter_Mode (P) = Mode_Inout
               then
                  --  Record field:

                  N := Make_Selected_Component
                    (PN (P_Arg_List), Argument_Name);

                  --  Parameter:

                  --  If the parameter type is a class-wide type,
                  --  we cast it.

                  M := Map_Defining_Identifier (Declarator (P));

                  if Is_Class_Wide (P) then
                     M := Make_Type_Conversion
                       (Get_Type_Definition_Node (Type_Spec (P)),
                        M);
                  end if;

                  N := Make_Assignment_Statement (N, M);

                  --  Assignment :

                  Append_To (Statements, N);
               end if;

            else
               --  Preparing the parameter list of the Add_Item call

               Profile := New_List;

               --  1st param

               N := Make_Identifier (VN (V_Argument_List));
               Append_To (Profile, N);

               --  2nd param

               N := Make_Identifier
                 (Map_Argument_Identifier_Name
                  (Argument_Name, Operation_Name));
               Append_To (Profile, N);

               --  3rd param

               N := Make_Identifier (Map_Argument_Any_Name (Argument_Name));
               N := Make_Type_Conversion (RE (RE_Any_1), N);

               --  If the operation is oneway, transmit a copy of the "Any"

               if Is_Oneway (E) then
                  N := Make_Subprogram_Call
                    (RE (RE_Copy_Any), New_List (N));
               end if;

               Append_To (Profile, N);

               --  4th param

               if FEN.Parameter_Mode (P) = Mode_Out then
                  N := RE (RE_ARG_OUT_1);
               elsif FEN.Parameter_Mode (P) = Mode_In then
                  N := RE (RE_ARG_IN_1);
               else
                  N := RE (RE_ARG_INOUT_1);
               end if;

               Append_To (Profile, N);

               --  Call the Add_Item procedure

               N := Make_Subprogram_Call (RE (RE_Add_Item_1), Profile);
               Append_To (Statements, N);

            end if;

            P := Next_Entity (P);
         end loop;

         --  If the operation may raise IDL exeptions, we generate the
         --  the code that initializes the operation exception list.

         if not FEU.Is_Empty (Exceptions (E)) then
            declare
               Excep_FE : Node_Id;
               Excep_TC : Node_Id;
            begin
               Set_Str_To_Name_Buffer ("Create the Exception list");
               Append_To (Statements, Make_Ada_Comment (Name_Find));

               N := Make_Subprogram_Call
                 (RE (RE_Create_List_1),
                  New_List
                  (Make_Identifier (VN (V_Exception_List))));
               Append_To (Statements, N);

               Excep_FE := First_Entity (Exceptions (E));

               while Present (Excep_FE) loop
                  --  Getting the TC_"Exception_Name" identifier. It
                  --  is declared at the first place in the Helper
                  --  spec.

                  Excep_TC := Get_TC_Node (Excep_FE);

                  N := Make_Subprogram_Call
                    (RE (RE_Add_1),
                     New_List
                     (Make_Identifier
                      (VN (V_Exception_List)),
                      Excep_TC));
                  Append_To (Statements, N);

                  Excep_FE := Next_Entity (Excep_FE);
               end loop;
            end;
         end if;

         --  The subprogram that sets the operation result is not
         --  needed when we use SII

         if not Use_SII then
            --  Create the inlined subprogram that set the Result name
            --  value.

            Profile := New_List;

            --  Build the record aggregate

            --  1st component association

            N := Make_Component_Association
              (Selector_Name => Make_Identifier (PN (P_Name)),
               Expression    => Make_Identifier
               (Map_Result_Identifier_Name (Operation_Name)));
            Append_To (Profile, N);

            --  2nd component association

            if Non_Void then
               Param := Get_TC_Node (Type_Spec (E));
            else
               Param := RE (RE_TC_Void);
            end if;

            C := Make_Subprogram_Call
              (Defining_Identifier  => RE (RE_Get_Empty_Any),
               Actual_Parameter_Part => New_List (Param));
            N := Make_Component_Association
              (Selector_Name => Make_Defining_Identifier (PN (P_Argument)),
               Expression    => C);
            Append_To (Profile, N);

            --  3rd component association

            N := Make_Component_Association
              (Selector_Name => Make_Defining_Identifier (PN (P_Arg_Modes)),
               Expression    => Make_Literal (Int0_Val));
            Append_To (Profile, N);

            --  Build the record aggregate

            N := Make_Record_Aggregate (Profile);
            N := Make_Return_Statement (N);

            --  Build the subprogram

            R := Map_Result_Subprogram_Name (Operation_Name);

            I := Make_Pragma
              (Pragma_Inline, New_List (Make_Identifier (R)));
            C := Make_Subprogram_Specification
              (Make_Defining_Identifier (R),
               No_List,
               RE (RE_NamedValue));
            N := Make_Subprogram_Body
              (C, New_List (I), New_List (N));
            Append_To (BEN.Statements (Current_Package), N);

            --  Setting the result Value (if any)

            if Non_Void then
               Set_Str_To_Name_Buffer ("Setting the result value");
               Append_To (Statements, Make_Ada_Comment (Name_Find));

               N := Make_Selected_Component
                 (VN (V_Result_NV), CN (C_Argument));
               N := Make_Subprogram_Call
                 (RE (RE_Get_Container_2),
                  New_List (N));
               N := Make_Explicit_Dereference (N);

               C := Make_Attribute_Reference
                 (Make_Identifier
                  (Map_Argument_Content_Name
                   (VN (V_Result))),
                  A_Unrestricted_Access);

               N := Make_Subprogram_Call (RE (RE_Set_Value),
                                          New_List (N, C));
               Append_To (Statements, N);
            end if;
         end if;

         --  Creating the request

         Set_Str_To_Name_Buffer ("Creating the request");
         Append_To (Statements, Make_Ada_Comment (Name_Find));

         --  Build the parameter associations

         NVList_Name := VN (V_Argument_List);
         Profile := New_List;

         --  1st parameter association

         N := Make_Type_Conversion
           (RE (RE_Ref_2),
            Make_Defining_Identifier (PN (P_Self)));
         N := Make_Subprogram_Call
           (RE (RE_To_PolyORB_Ref), New_List (N));
         N := Make_Parameter_Association
           (Selector_Name    => Make_Defining_Identifier (PN (P_Target)),
            Actual_Parameter => N);
         Append_To (Profile, N);

         --  2nd parameter association

         R := Map_Operation_Name_Literal (E);
         N := Make_Parameter_Association
           (Selector_Name    => Make_Defining_Identifier (PN (P_Operation)),
            Actual_Parameter => Make_Literal (New_String_Value (R, False)));
         Append_To (Profile, N);

         --  3rd parameter association

         N := Make_Parameter_Association
           (Selector_Name    => Make_Defining_Identifier (PN (P_Arg_List)),
            Actual_Parameter => Make_Defining_Identifier (NVList_Name));
         Append_To (Profile, N);

         --  4th parameter association

         N := Make_Parameter_Association
           (Selector_Name    => Make_Defining_Identifier (PN (P_Result)),
            Actual_Parameter => Make_Defining_Identifier (VN (V_Result_NV)));
         Append_To (Profile, N);

         --  If the operation throws an exception, we add an
         --  additional parameter to the Create_Request call.

         if  not FEU.Is_Empty (Exceptions (E)) then
            N := Make_Subprogram_Call
              (RE (RE_To_PolyORB_Ref_1),
               New_List
               (Make_Identifier
                (VN (V_Exception_List))));

            --  5th parameter association

            N := Make_Parameter_Association
              (Selector_Name    => Make_Defining_Identifier (PN (P_Exc_List)),
               Actual_Parameter => N);
            Append_To (Profile, N);
         end if;

         --  6th parameter association

         N := Make_Parameter_Association
           (Selector_Name    => Make_Defining_Identifier (PN (P_Req)),
            Actual_Parameter => Make_Defining_Identifier (VN (V_Request)));
         Append_To (Profile, N);

         --  Handling the case of Oneway Operations.  Extract from The
         --  CORBA mapping specification : "IDL oneway operations are
         --  mapped the same as other operation; that is, there is no
         --  indication whether an operation is oneway or not in the
         --  mapped Ada specification".

         --  The extract above means that the call to a oneway
         --  operation is performed in the same way as a call to a
         --  classic synchronous operation. However, the ORB need to
         --  know oneway operations. The stub precise that by adding
         --  an additional parameter to the procedure
         --  "PolyORB.Requests.Create_Request". This additional
         --  parameter indicate the calling way of the operation (see
         --  the file polyorb-requests.ads for more information about
         --  different ways of calls)

         if FEN.Is_Oneway (E) then
            --  7th parameter association

            N := Make_Parameter_Association
              (Selector_Name    => Make_Defining_Identifier (PN (P_Req_Flags)),
               Actual_Parameter => RE (RE_Sync_With_Transport));
            Append_To (Profile, N);
         end if;

         --  Call Create_Request

         N := Make_Subprogram_Call (RE (RE_Create_Request), Profile);
         Append_To (Statements, N);

         if Use_SII then
            --  Get the GIOP session

            Profile := New_List;

            M := Make_Subprogram_Call
              (RE (RE_Ref_2),
               New_List (Make_Identifier (PN (P_Self))));

            N := Make_Subprogram_Call
              (RE (RE_To_PolyORB_Ref),
               New_List (M));

            Append_To (Profile, N);
            Append_To (Profile, RE (RE_The_ORB));

            N := Make_Subprogram_Call
              (RE (RE_Get_Request_QoS),
               New_List (Make_Identifier (VN (V_Request))));
            Append_To (Profile, N);
            Append_To (Profile, Make_Identifier (VN (V_Component)));
            Append_To (Profile, Make_Identifier (VN (V_Binding_Profile)));
            Append_To (Profile, RE (RE_False));
            Append_To (Profile, Make_Identifier (VN (V_Error)));

            --  Call to the bind method to get the client Session and
            --  the binding_profile.

            N := Make_Subprogram_Call (RE (RE_Bind), Profile);
            Append_To (Statements, N);

            --  The session resulting of the bind operation and the
            --  session representation.

            N := Make_Type_Conversion
              (RE (RE_GIOP_Session),
               (Make_Explicit_Dereference
                (Make_Identifier
                 (VN (V_Component)))));

            N := Make_Attribute_Reference (N, A_Unrestricted_Access);

            N := Make_Subprogram_Call
              (RE (RE_Get_Representation),
               New_List (N));
            N := Make_Assignment_Statement
              (Make_Identifier (VN (V_Representation)), N);
            Append_To (Statements, N);

            if Use_Compiler_Alignment then
               declare
                  Par  : Node_Id;
                  Disc : constant List_Id := New_List;
                  J    : Unsigned_Long_Long;
               begin
                  Par := First_Entity (Parameters (E));

                  while Present (Par) loop
                     if  FEN.Parameter_Mode (Par) = Mode_In then
                        Get_Discriminants_Value
                          (Par,
                           Type_Spec (Par),
                           Disc);
                     end if;

                     Par := Next_Entity (Par);
                  end loop;

                  J := Unsigned_Long_Long (Length (Disc));
                  C := Make_Attribute_Reference
                    (Make_Identifier (VN (V_Args_In)), A_Address);

                  N := Make_Subprogram_Call
                    (RE (RE_Insert_Raw_Data),
                     New_List
                     (Make_Identifier (VN (V_Request)),
                      C,
                      Make_Attribute_Reference
                      (Make_Identifier (VN (V_Args_In)), A_Size),
                      Make_Literal (New_Integer_Value (J, 1, 10)),
                      Make_Identifier (VN (V_Buffer))));
                  Append_To (Statements, N);
               end;
            else
               --  In this context, we use a QoS attribute to store a
               --  CDR buffer that holds the CDR representation of the
               --  requests parameters.

               Profile := New_List;
               Append_To (Profile, Make_Identifier (VN (V_Binding_Profile)));
               Append_To (Profile, Make_Identifier (VN (V_Component)));
               Append_To (Profile, Make_Identifier (VN (V_Error)));

               --  Get the marshaller

               C := Get_Marshaller_Node (E);

               Profile := New_List;
               Append_To (Profile, RE (RE_True));

               --  The arguments list, we use the method_name_Arg_Type
               --  instead of the Request_Args type

               N := Make_Identifier (PN (P_Arg_List));
               N := Make_Attribute_Reference (N, A_Access);
               Append_To (Profile, N);

               Append_To (Profile, Make_Defining_Identifier (VN (V_Buffer)));

               N := Make_Explicit_Dereference
                 (Make_Identifier (VN (V_Representation)));
               Append_To (Profile, N);

               --  There is no alignment it will be done in
               --  Marshall_Argument_List

               Append_To (Profile, Make_Literal (Int1_Val));
               Append_To (Profile, Make_Defining_Identifier (VN (V_Error)));

               --  Call of the Marshaller method

               N := Make_Subprogram_Call (C, Profile);
               Append_To (Statements, N);

               --  If any error we raise a program_error

               N := Make_Subprogram_Call
                 (RE (RE_Found),
                  New_List (Make_Identifier (VN (V_Error))));

               N := Make_If_Statement
                 (Condition       => N,
                  Then_Statements => New_List
                  (Make_Raise_Statement
                   (Make_Identifier (EN (E_Program_Error)))));
               Append_To (Statements, N);
            end if;

            --  Add the buffer as a QoS parameter for the request

            Set_Str_To_Name_Buffer
              ("Add the buffer to the request QoS parameters");
            Append_To (Statements, Make_Ada_Comment (Name_Find));

            N := Make_Record_Aggregate
              (New_List
               (RE (RE_GIOP_Static_Buffer),
                Make_Defining_Identifier (VN (V_Buffer))));

            N := Make_Object_Instantiation
              (Make_Qualified_Expression
               (RE (RE_QoS_GIOP_Static_Buffer_Parameter),
                N));

            N := Make_Subprogram_Call
              (RE (RE_Add_Request_QoS),
               New_List
               (Make_Defining_Identifier (VN (V_Request)),
                RE (RE_GIOP_Static_Buffer),
                N));

            Append_To (Statements, N);

         end if;

         --  Invoking the request (synchronously or asynchronously),
         --  it depends on the type of the operation (oneway or not).

         Set_Str_To_Name_Buffer
           ("Invoking the request (synchronously or asynchronously)");
         Append_To (Statements, Make_Ada_Comment (Name_Find));

         N := Make_Type_Conversion
           (RE (RE_Flags),
            Make_Literal (Int0_Val));
         N := Make_Subprogram_Call
           (RE (RE_Client_Invoke),
            New_List
            (Make_Defining_Identifier (VN (V_Request)),
             N));
         Append_To (Statements, N);

         if Use_SII and then (Has_Out_Params or else Non_Void) then
            --  Unmarshall the request using the generated SII
            --  marshaller. In DII mode the unmarshalling is performed
            --  transparently.

            C := Get_Unmarshaller_Node (E);

            Profile := New_List;
            Append_To (Profile, RE (RE_True));

            N := Make_Identifier (PN (P_Arg_List));
            N := Make_Attribute_Reference (N, A_Access);
            Append_To (Profile, N);

            N := Make_Subprogram_Call
              (RE (RE_Extract_Request_Parameter),
               New_List
                 (RE (RE_GIOP_Static_Buffer),
                  Make_Defining_Identifier (VN (V_Request))));

            N := Make_Type_Conversion
              (RE (RE_QoS_GIOP_Static_Buffer_Parameter_Access), N);

            N := Make_Selected_Component (N, Make_Identifier (PN (P_Buffer)));

            Append_To (Profile, N);

            N := Make_Explicit_Dereference
              (Make_Identifier (VN (V_Representation)));
            Append_To (Profile, N);

            --  There is no alignment it will be done in
            --  Marshall_Argument_List.

            Append_To (Profile, Make_Literal (New_Integer_Value (8, 1, 10)));
            Append_To (Profile, Make_Defining_Identifier (VN (V_Error)));

            --  Call of the Unmarshaller method

            N := Make_Subprogram_Call
              (C, Profile);
            Append_To (Statements, N);

         end if;

         --  Raise exception, if needed

         Set_Str_To_Name_Buffer ("Raise exception, if needed");
         Append_To (Statements, Make_Ada_Comment (Name_Find));
         Append_To (Statements,
           Make_Subprogram_Call
             (RE (RE_Request_Raise_Occurrence),
              New_List (Make_Identifier (VN (V_Request)))));

         --  Destroy the request

         N := Make_Subprogram_Call
           (RE (RE_Destroy_Request),
            New_List (Make_Identifier (VN (V_Request))));
         Append_To (Statements, N);

         --  Retrieve return value

         if Is_Function then
            Set_Str_To_Name_Buffer ("Return value");
            Append_To (Statements, Make_Ada_Comment (Name_Find));

            if Use_SII then
               N := Make_Selected_Component (PN (P_Arg_List), PN (P_Returns));
               N := Make_Return_Statement (N);
               Append_To (Statements, N);
            else
               N := Make_Return_Statement (Make_Identifier (VN (V_Result)));
               Append_To (Statements, N);
            end if;
         else
            --  Non-void IDL operations with OUT/INOUT parameters are mapped to
            --  Ada procedures with an extra OUT formal for the return value.

            if Non_Void and then Use_SII then
               Set_Str_To_Name_Buffer ("Return value");
               Append_To (Statements, Make_Ada_Comment (Name_Find));

               N := Make_Selected_Component (PN (P_Arg_List), PN (P_Returns));

               --  If the return value is a class-wide type, cast
               --  the record field.

               if Is_Class_Wide (E) then
                  N := Make_Type_Conversion
                    (Make_Attribute_Reference
                     (Get_Type_Definition_Node (Type_Spec (E)), A_Class),
                     N);
               end if;

               N := Make_Assignment_Statement
                 (Make_Identifier (PN (P_Returns)), N);
               Append_To (Statements, N);
            end if;
         end if;

         --  In case of SII, retreive the OUT parameter values. In the case
         --  of DII, this is performed transparently.

         if Use_SII then
            P := First_Entity (Parameters (E));

            if Present (P) then
               Set_Str_To_Name_Buffer ("Retrieve out argument values");
               Append_To (Statements, Make_Ada_Comment (Name_Find));
            end if;

            while Present (P) loop
               if FEN.Parameter_Mode (P) = Mode_Out
                 or else FEN.Parameter_Mode (P) = Mode_Inout
               then
                  Argument_Name := To_Ada_Name
                    (IDL_Name (Identifier (Declarator (P))));

                  --  Record field:

                  if Use_Compiler_Alignment then
                     N := Make_Selected_Component
                       (VN (V_Args_Out), Argument_Name);
                  else
                     N := Make_Selected_Component
                       (PN (P_Arg_List), Argument_Name);
                  end if;

                  --  Parameter:

                  M := Map_Defining_Identifier (Declarator (P));

                  --  If the parameter type is a class-wide type, cast
                  --  the record field.

                  if Is_Class_Wide (P) then
                     N := Make_Type_Conversion
                       (Make_Attribute_Reference
                        (Get_Type_Definition_Node (Type_Spec (P)), A_Class),
                        N);
                  end if;

                  N := Make_Assignment_Statement (M, N);

                  --  Assignment:

                  Append_To (Statements, N);
               end if;

               P := Next_Entity (P);
            end loop;
         end if;

         return Statements;
      end Stub_Statements;

      -----------------------
      -- Stub_Declarations --
      -----------------------

      function Stub_Declarations (E : Node_Id) return List_Id is
         pragma Assert (FEN.Kind (E) = K_Operation_Declaration);

         L                : constant List_Id := New_List;
         P                : Node_Id;
         N                : Node_Id;
         V                : Value_Id;
         C                : Node_Id;
         R                : Name_Id;
         Operation_Name   : constant Name_Id := FEN.IDL_Name (Identifier (E));
         Argument_Name    : Name_Id;
         Container        : constant Node_Id := Scope_Entity (Identifier (E));
         Local_Interface  : constant Boolean :=
           (FEN.Kind (Container) = K_Interface_Declaration
            and then Is_Local_Interface (Container));
         Res_Exp          : Node_Id;

         --  The flags below indicates whether the operation is mapped
         --  to an Ada function or an Ada procedure.

         Has_Out_Params  : constant Boolean := Contains_Out_Parameters (E);
         Non_Void        : constant Boolean :=
           FEN.Kind (Type_Spec (E)) /= K_Void;
         Is_Function     : constant Boolean :=
           Non_Void and then not Has_Out_Params;
      begin
         if not Local_Interface then

            --  Argument_List_Ü declaration

            --  In the case of the use of SII, the NVList is not used
            --  when handling a request. However, it remains necessary
            --  for the request creation. So we declare it as a global
            --  variable and we avoid the creation of an NVList each
            --  time the operation is invoked.

            if not Use_SII then
               N := Make_Object_Declaration
                 (Defining_Identifier => Make_Defining_Identifier
                  (VN (V_Argument_List)),
                  Constant_Present    => False,
                  Object_Definition   => RE (RE_Ref_3),
                  Expression          => No_Node);
               Append_To (L, N);
            end if;

            --  In the case of SII, the NVList is not filled by the
            --  NameValues corresponding to the operation parameters

            if not Use_SII then
               --  Non-void return type case

               if Non_Void then
                  --  Declare the Result_Ü variable

                  if Is_Function then
                     --  No Returns formal in the Ada mapped subprogram

                     N := No_Node;
                  else
                     --  Extra Returns formal present

                     N := Make_Identifier (PN (P_Returns));
                  end if;

                  N := Make_Object_Declaration
                    (Defining_Identifier => Make_Defining_Identifier
                     (VN (V_Result)),
                     Object_Definition   => Get_Type_Definition_Node
                     (Type_Spec (E)),
                     Renamed_Object      => N);
                  Append_To (L, N);

                  --  Disable warning on the returned value

                  if Is_Function then
                     --  No Returns formal in the Ada mapped subprogram

                     N := Make_Identifier (VN (V_Result));
                  else
                     --  Extra Returns formal present

                     N := Make_Identifier (PN (P_Returns));
                  end if;

                  N := Make_Pragma
                         (Pragma_Warnings, New_List (RE (RE_Off), N));
                  Append_To (L, N);

                  --  Declaration of the `Content' argument variable

                  C := Get_Wrap_Node
                    (FEU.Get_Original_Type_Declarator
                     (Type_Spec (E)));

                  N := Make_Identifier (VN (V_Result));

                  --  Cast the parameter when necessary

                  Cast_When_Necessary
                    (N,
                     Type_Spec (E),
                     FEU.Get_Original_Type_Declarator (Type_Spec (E)),
                     True);

                  C := Make_Subprogram_Call
                    (C,
                     New_List (Make_Attribute_Reference
                                   (N,
                                    A_Unrestricted_Access)));
                  N := Make_Object_Declaration
                    (Defining_Identifier => Make_Defining_Identifier
                     (Map_Argument_Content_Name (VN (V_Result))),
                     Constant_Present    => False,
                     Object_Definition   => Make_Attribute_Reference
                     (RE (RE_Content), A_Class),
                     Expression          => C,
                     Aliased_Present     => True);
                  Append_To (L, N);
               end if;

               --  Handling the parameters

               P := First_Entity (Parameters (E));

               while Present (P) loop
                  Argument_Name := To_Ada_Name
                    (IDL_Name
                     (Identifier
                      (Declarator
                       (P))));

                  --  Map the `Identifier' global variable name
                  --  corresponding to the parameter `I'.

                  R := Map_Argument_Identifier_Name
                    (Argument_Name, Operation_Name);

                  --  Expression of the variable

                  C := Make_Subprogram_Call
                    (RE (RE_To_PolyORB_String),
                     New_List
                     (Make_Literal
                      (New_String_Value
                       (Argument_Name,
                        False))));

                  --  Declare the global variable

                  N := Make_Object_Declaration
                    (Defining_Identifier => Make_Defining_Identifier (R),
                     Constant_Present    => True,
                     Object_Definition   => RE (RE_Identifier),
                     Expression          => C);
                  Append_To (Statements (Current_Package), N);

                  --  Declaration of the `Content' argument variable

                  C := Get_Wrap_Node
                    (FEU.Get_Original_Type_Declarator
                     (Type_Spec (P)));

                  N := Make_Identifier (Argument_Name);

                  --  Cast the parameter when necessary

                  Cast_When_Necessary
                    (N,
                     Type_Spec (P),
                     FEU.Get_Original_Type_Specifier (Type_Spec (P)),
                     True);

                  C := Make_Subprogram_Call
                    (C,
                     New_List (Make_Attribute_Reference
                                   (N,
                                    A_Unrestricted_Access)));
                  N := Make_Object_Declaration
                    (Defining_Identifier => Make_Defining_Identifier
                       (Map_Argument_Content_Name (Argument_Name)),
                     Constant_Present    => False,
                     Object_Definition   => Make_Attribute_Reference
                     (RE (RE_Content), A_Class),
                     Expression          => C,
                     Aliased_Present     => True);
                  Append_To (L, N);

                  --  Declaration of the `Any' argument variable

                  R := Map_Argument_Any_Name (Argument_Name);
                  C := Make_Attribute_Reference
                    (Make_Identifier
                     (Map_Argument_Content_Name (Argument_Name)),
                     A_Unchecked_Access);
                  C := Make_Subprogram_Call
                    (RE (RE_Get_Wrapper_Any),
                     New_List
                     (Get_TC_Node (Type_Spec (P)),
                      C));
                  N := Make_Object_Declaration
                    (Defining_Identifier => Make_Defining_Identifier (R),
                     Constant_Present    => True,
                     Object_Definition   => RE (RE_Any),
                     Expression          => C);
                  Append_To (L, N);

                  --  If the parameter is OUT, we disable warnings on
                  --  it.

                  if FEN.Parameter_Mode (P) = Mode_Out then
                     N := Make_Pragma
                       (Pragma_Warnings,
                        New_List
                        (RE (RE_Off),
                         Make_Identifier (Argument_Name)));
                     Append_To (L, N);
                  end if;

                  P := Next_Entity (P);
               end loop;
            else
               C := Make_Object_Declaration
                 (Defining_Identifier =>
                    Make_Defining_Identifier (VN (V_Representation)),
                  Object_Definition   => RE (RE_CDR_Representation_Access),
                  Expression          => No_Node);
               Append_To (L, C);

               --  SII/SSI invocation

               C := Make_Object_Instantiation (RE (RE_Buffer_Type));

               --  Buffer declaration and instantiation

               N := Make_Object_Declaration
                 (Defining_Identifier =>
                    Make_Defining_Identifier (VN (V_Buffer)),
                  Object_Definition => RE (RE_Buffer_Access),
                  Constant_Present  => True,
                  Expression => C);
               Append_To (L, N);

               N := Make_Object_Declaration
                 (Defining_Identifier =>
                    Make_Defining_Identifier (VN (V_Argument_List)),
                  Object_Definition => RE (RE_Ref_3));
               Append_To (L, N);

               --  Error container

               N := Make_Object_Declaration
                 (Defining_Identifier =>
                    Make_Defining_Identifier (VN (V_Error)),
                  Object_Definition   => RE (RE_Error_Container));
               Append_To (L, N);

               --  Binding_Profile and GIOP Session

               N := Make_Object_Declaration
                 (Defining_Identifier =>
                    Make_Defining_Identifier (VN (V_Binding_Profile)),
                  Object_Definition   => RE (RE_Profile_Access));
               Append_To (L, N);

               N := Make_Object_Declaration
                 (Defining_Identifier => Make_Defining_Identifier
                  (VN (V_Component)),
                  Object_Definition   => RE (RE_Component_Access));
               Append_To (L, N);
            end if;

            --  Request_Ü declaration

            N := Make_Object_Declaration
              (Defining_Identifier =>
                 Make_Defining_Identifier (VN (V_Request)),
               Constant_Present    => False,
               Object_Definition   => RE (RE_Request_Access),
               Expression          => No_Node);
            Append_To (L, N);

            --  Exception_List_Ü declaration

            if not FEU.Is_Empty (Exceptions (E)) then
               N := Make_Object_Declaration
                 (Defining_Identifier =>
                    Make_Defining_Identifier (VN (V_Exception_List)),
                  Constant_Present    => False,
                  Object_Definition   => RE (RE_Ref_5),
                  Expression          => No_Node);
               Append_To (L, N);
            end if;

            --  Result_NV_Ü declaration

            --  In the case of the SII, the Result_NV_Ü is not
            --  used. However it remains necessary for the request
            --  creation.

            R := Map_Result_Subprogram_Name (Operation_Name);

            if Use_SII then
               Res_Exp := No_Node;
            else
               Res_Exp := Make_Subprogram_Call
                 (Make_Identifier (R), No_List);
            end if;

            N := Make_Object_Declaration
              (Defining_Identifier =>
                 Make_Defining_Identifier (VN (V_Result_NV)),
               Constant_Present    => False,
               Object_Definition   => RE (RE_NamedValue),
               Expression          => Res_Exp);
            Append_To (L, N);

            if not Use_SII then

               --  Result_Name_Ü declaration

               R := Map_Result_Identifier_Name (Operation_Name);

               Set_Str_To_Name_Buffer ("Result");
               V := New_String_Value (Name_Find, False);
               C := Make_Subprogram_Call
                 (RE (RE_To_PolyORB_String),
                  New_List (Make_Literal (V)));

               N := Make_Object_Declaration
                 (Defining_Identifier =>
                    Make_Defining_Identifier (R),
                  Constant_Present    => True,
                  Object_Definition   => RE (RE_Identifier),
                  Expression          => C);
               Append_To (Statements (Current_Package), N);
            end if;

            --  In the case of the SII use, the argument list is an
            --  aliased record variable.

            if Use_Compiler_Alignment then
               declare
                  Disc : constant List_Id := New_List;
               begin
                  C := Expand_Designator
                    (Args_In_Node
                     (BE_Node
                      (Identifier
                       (E))));

                  P := First_Entity (Parameters (E));

                  while Present (P) loop
                     if  FEN.Parameter_Mode (P) = Mode_In then
                        --  FIXME to be factorized !!!!

                        Get_Discriminants_Value
                          (P,
                           Type_Spec (P),
                           Disc);
                     end if;

                     P := Next_Entity (P);
                  end loop;

                  N := Make_Subprogram_Call (C, Disc);

                  N := Make_Object_Declaration
                    (Defining_Identifier =>
                       Make_Defining_Identifier (VN (V_Args_In)),
                     Aliased_Present     => True,
                     Object_Definition   => N);
                  Append_To (L, N);
               end;

               N := Get_Type_Definition_Node (E);

               N := Make_Object_Declaration
                 (Defining_Identifier =>
                    Make_Defining_Identifier (PN (P_Arg_List)),
                  Aliased_Present     => True,
                  Object_Definition   => N);
               Append_To (L, N);

            elsif Use_SII then
               N := Get_Type_Definition_Node (E);
               N := Make_Object_Declaration
                 (Defining_Identifier =>
                    Make_Defining_Identifier (PN (P_Arg_List)),
                  Aliased_Present     => True,
                  Object_Definition   => N);
               Append_To (L, N);
            end if;
         end if;

         return L;
      end Stub_Declarations;

   end Package_Body;

   ---------------------
   -- Local_Is_A_Body --
   ---------------------

   function Local_Is_A_Body
     (E    : Node_Id;
      Spec : Node_Id := No_Node)
     return Node_Id is
      N                : Node_Id;
      S                : constant List_Id := New_List;
      M                : Node_Id;
      Repository_Id    : Node_Id;
      Rep_Value        : Value_Id;
      Parent_Statement : Node_Id;

      function Is_Equivalent_Statement (E : Node_Id) return Node_Id;
      --  This function returns a logical "or else" expression. The
      --  operands of the expression are calls to CORBA.Is_Equivalent
      --  function on all the parents (direct parents as well as in
      --  direct parents) of the interface. It returns a null node in
      --  the case where the interface does not inherit from another
      --  interface.

      -----------------------------
      -- Is_Equivalent_Statement --
      -----------------------------

      function Is_Equivalent_Statement (E : Node_Id) return Node_Id is
         Result           : Node_Id := No_Node;
         Parent_Statement : Node_Id;
         Par_Int          : Node_Id;
         L                : List_Id;
         Rep_Id           : Node_Id;
         T                : Node_Id;
      begin
         pragma Assert (FEN.Kind (E) = K_Interface_Declaration);

         L := Interface_Spec (E);

         if not FEU.Is_Empty (L) then
            Par_Int := First_Entity (L);

            while Present (Par_Int) loop
               --  Get the type definition corresponding to the parent
               --  interface.

               T := Get_Type_Definition_Node (Par_Int);

               --  Build the Repository_Id constant corresponding to
               --  the parent interface.

               Rep_Id := Make_Selected_Component
                 (Get_Parent_Unit_Name (T),
                  Make_Defining_Identifier (PN (P_Repository_Id)));

               if Present (Result) then
                  Result := Make_Expression
                    (Result,
                     Op_Or_Else,
                     Make_Subprogram_Call
                     (RE (RE_Is_Equivalent),
                      New_List
                      (Make_Defining_Identifier (PN (P_Logical_Type_Id)),
                       Rep_Id)));
               else
                  Result := Make_Subprogram_Call
                    (RE (RE_Is_Equivalent),
                     New_List
                     (Make_Defining_Identifier (PN (P_Logical_Type_Id)),
                      Rep_Id));
               end if;

               --  Adding recursively the parents of parents.

               Parent_Statement := Is_Equivalent_Statement
                 (Reference
                  (Par_Int));

               if Present (Parent_Statement) then
                  Result := Make_Expression
                    (Result, Op_Or_Else, Parent_Statement);
               end if;

               Par_Int := Next_Entity (Par_Int);
            end loop;
         end if;

         return Result;
      end Is_Equivalent_Statement;

   begin
      --  The Repository_Id constant is declared just after the Ada
      --  Ref type mapped from the interface.

      N := Next_Node (Type_Def_Node (BE_Node (Identifier (E))));
      Repository_Id := Expand_Designator (N);

      N := Make_Subprogram_Call
        (RE (RE_Is_Equivalent),
         New_List
         (Make_Defining_Identifier (PN (P_Logical_Type_Id)),
          Repository_Id));

      if FEN.Kind (E) = K_Interface_Declaration then
         Set_Str_To_Name_Buffer ("IDL:omg.org/CORBA/Object:1.0");
      else
         Set_Str_To_Name_Buffer ("IDL:omg.org/CORBA/ValueBase:1.0");
      end if;

      Rep_Value := New_String_Value (Name_Find, False);
      M := Make_Subprogram_Call
        (RE (RE_Is_Equivalent),
         New_List
         (Make_Defining_Identifier (PN (P_Logical_Type_Id)),
          Make_Literal (Rep_Value)));
      N := Make_Expression
        (N, Op_Or_Else, M);

      --  Add the parents (recusively).

      Parent_Statement := Is_Equivalent_Statement (E);

      if Present (Parent_Statement) then
         N := Make_Expression
           (N, Op_Or_Else, Parent_Statement);
      end if;

      N := Make_Expression
        (N, Op_Or_Else, RE (RE_False));
      N := Make_Return_Statement (N);
      Append_To (S, N);

      --  Get the spec of the Is_A function

      if Spec = No_Node then
         N := Local_Is_A_Spec;
      else
         N := Spec;
      end if;
      N := Make_Subprogram_Body
        (N, No_List, S);
      return N;
   end Local_Is_A_Body;

   ---------------------
   -- Local_Is_A_Spec --
   ---------------------

   function Local_Is_A_Spec return Node_Id is
      N       : Node_Id;
      Profile : List_Id;
      Param   : Node_Id;
   begin
      Param := Make_Parameter_Specification
        (Make_Defining_Identifier (PN (P_Logical_Type_Id)),
         RE (RE_String_2));
      Profile := New_List;
      Append_To (Profile, Param);
      N := Make_Subprogram_Specification
        (Make_Defining_Identifier (SN (S_Is_A)),
         Profile,
         RE (RE_Boolean));
      return N;
   end Local_Is_A_Spec;

   -----------------------
   -- Visible_Is_A_Body --
   -----------------------

   function Visible_Is_A_Body (E : Node_Id) return Node_Id is
      N : Node_Id;
      M : Node_Id;
      S : constant List_Id := New_List;
   begin
      M := Make_Subprogram_Call
        (RE (RE_Ref_2),
         New_List (Make_Defining_Identifier (PN (P_Self))));
      M := Make_Subprogram_Call
        (RE (RE_Is_A),
         New_List (M, Make_Defining_Identifier (PN (P_Logical_Type_Id))));
      N := Make_Subprogram_Call
        (Make_Defining_Identifier (SN (S_Is_A)),
         New_List (Make_Identifier (PN (P_Logical_Type_Id))));
      N := Make_Expression
        (RE (RE_False),
         Op_Or_Else,
         Make_Expression
         (N,
          Op_Or_Else,
          M));
      N := Make_Return_Statement (N);
      Append_To (S, N);
      N := Make_Subprogram_Body
        (Visible_Is_A_Spec (E), No_List, S);
      return N;
   end Visible_Is_A_Body;

   -----------------------
   -- Visible_Is_A_Spec --
   -----------------------

   function Visible_Is_A_Spec (E : Node_Id) return Node_Id is
      N       : Node_Id;
      Profile : List_Id;
      Param   : Node_Id;
   begin
      Profile := New_List;
      Param := Make_Parameter_Specification
        (Make_Defining_Identifier (PN (P_Self)),
         Map_Ref_Type (E));
      Append_To (Profile, Param);
      Param := Make_Parameter_Specification
        (Make_Defining_Identifier (PN (P_Logical_Type_Id)),
         RE (RE_String_2));
      Append_To (Profile, Param);
      N := Make_Subprogram_Specification
        (Make_Defining_Identifier (SN (S_Is_A)),
         Profile,
         RE (RE_Boolean));
      return N;
   end Visible_Is_A_Spec;

end Backend.BE_CORBA_Ada.Stubs;

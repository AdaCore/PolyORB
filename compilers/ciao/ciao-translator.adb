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

--  This unit generates a decorated IDL tree
--  by traversing the ASIS tree of a DSA package
--  specification.
--  $Id: //droopi/main/compilers/ciao/ciao-translator.adb#11 $

with Ada.Exceptions;
with Ada.Wide_Text_IO;  use Ada.Wide_Text_IO;
with Ada.Characters.Handling; use  Ada.Characters.Handling;

with Asis.Clauses;
with Asis.Compilation_Units;
with Asis.Declarations;
with Asis.Definitions;
with Asis.Elements;
with Asis.Expressions;
with Asis.Extensions;
with Asis.Iterator;
pragma Elaborate_All (Asis.Iterator);
with Asis.Text;

with CIAO.ASIS_Queries; use CIAO.ASIS_Queries;

with Idl_Fe.Types; use Idl_Fe.Types;
with Idl_Fe.Tree;  use Idl_Fe.Tree;
with Idl_Fe.Tree.Synthetic;
with Idl_Fe.Utils; use Idl_Fe.Utils;
with Errors;       use Errors;

with CIAO.Translator.Maps;  use CIAO.Translator.Maps;
with CIAO.Translator.State; use CIAO.Translator.State;

package body CIAO.Translator is

   use Asis;
   use Asis.Definitions;
   use Asis.Elements;
   use Asis.Expressions;
   use Asis.Declarations;

   use CIAO;

   ---------------------------------------------------
   -- Raise_Translation_Error                       --
   -- Print an error message and abort translation. --
   ---------------------------------------------------

   procedure Raise_Translation_Error
     (Element : Asis.Element;
      Message : String);
   pragma No_Return (Raise_Translation_Error);

   procedure Raise_Translation_Error
     (Element : Asis.Element;
      Message : String)
   is
      use Asis.Text;

      E_Span       : constant Span
        := Element_Span (Element);

      Line_Number_Wide_Image : Wide_String
        := Line_Number'Wide_Image (E_Span.First_Line);

      E_Lines : Line_List :=
        Lines (Element    => Element,
               First_Line => E_Span.First_Line,
               Last_Line  => E_Span.First_Line);
   begin
      New_Line;
      New_Line;
      Put (Line_Number_Wide_Image);
      --  (2 .. Line_Number_Wide_Image'Last));
      Put (". ");
      Put (Line_Image (E_Lines (E_Lines'First)));
      New_Line;

      for I in 1 .. E_Span.First_Column
        + Line_Number_Wide_Image'Length + 1
      loop
         Put (' ');
      end loop;

      Put ('|');
      New_Line;
      Put (">>> ");

      Ada.Exceptions.Raise_Exception
        (Translation_Error'Identity, Message);
   end Raise_Translation_Error;

   function Unit_Category (LU : in Compilation_Unit)
     return Unit_Categories;
   --  Returns the category (Pure, RT, RCI or Other)
   --  of a library unit.

   function Unit_Category (LU : in Compilation_Unit)
     return Unit_Categories is
      D : constant Declaration := Unit_Declaration (LU);
      K : constant Declaration_Kinds := Declaration_Kind (D);
   begin
      --  Check that LU is a package specification
      if K /= A_Package_Declaration then
         Raise_Translation_Error
           (Nil_Element, "Unexpected unit declaration kind.");
      end if;

      --  Find the category of LU.
      declare
         Unit_Pragmas : constant Pragma_Element_List := Pragmas (D);
      begin
         for I in Unit_Pragmas'Range loop
            case Pragma_Kind (Unit_Pragmas (I)) is
               when A_Pure_Pragma =>
                  return Pure;
               when A_Remote_Types_Pragma =>
                  return Remote_Types;
               when A_Remote_Call_Interface_Pragma =>
                  return Remote_Call_Interface;
               when others =>
                  null;
            end case;
         end loop;

         return Other;
      end;
   end Unit_Category;

   --------------------------------------
   -- {Pre,Post}_Translate_Element     --
   -- The pre- and post-operations for --
   -- Asis.Iterator.Traverse_Element.  --
   --------------------------------------

   procedure Pre_Translate_Element
     (Element : in Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Translator_State);

   procedure Post_Translate_Element
     (Element : in Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Translator_State);

   -------------------------------------------
   -- Translate_Tree                        --
   -- Translate an Ada syntax tree into the --
   -- corresponding IDL tree.               --
   -------------------------------------------

   procedure Translate_Tree is new Iterator.Traverse_Element
     (State_Information => Translator_State,
      Pre_Operation     => Pre_Translate_Element,
      Post_Operation    => Post_Translate_Element);

   ------------------------------------------------------------------
   -- Process_*                                                    --
   -- Helper subprograms for Pre_Translate_Element that            --
   -- handle specific Element_Kinds.                               --
   -- These subprograms act strictly like Pre_Translate_Elements:  --
   -- the caller should return immediately to the Traverse_Element --
   -- instance after calling any of them.                          --
   ------------------------------------------------------------------

   procedure Process_Declaration
     (Element : in Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Translator_State);
   procedure Process_Definition
     (Element : in Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Translator_State);
   procedure Process_Expression
     (Element : in Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Translator_State);
   procedure Process_Type_Definition
     (Element : in Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Translator_State);

   ----------------------------------------------------------
   -- Translate_*                                          --
   -- These procedures are called by Pre_Translate_Element --
   -- to take care of particular Element_Kinds.            --
   ----------------------------------------------------------

   function Map_Defining_Name
     (Name : in Asis.Defining_Name)
     return String;
   --  Return the IDL representation of Name.

--    procedure Translate_Defining_Name
--      (Name    : in Asis.Defining_Name;
--       State   : in out Translator_State);

--    procedure Translate_Subtype_Mark
--      (Exp     : in Asis.Expression;
--       State   : in out Translator_State);

   function Translate_Subtype_Mark
     (Exp : in Asis.Expression)
     return Node_Id;
   --  Return the node id corresponding to the definition of
   --  the type denoted by subtype_mark Exp.

--    procedure Translate_Discriminant_Part
--      (Def     : in Asis.Definition;
--       State   : in out Translator_State);

--    procedure Translate_Type_Definition
--      (Def     : in Asis.Definition;
--       State   : in out Translator_State);
   procedure Translate_List
     (List  : in     Asis.Element_List;
      State : in out Translator_State);

   procedure Translate_Formal_Parameter
     (Specification    : in Asis.Definition;
      Is_Implicit_Self : in Boolean;
      State            : in out Translator_State);

   function New_Opaque_Type return Node_Id;
   --  Return a new node corresponding to the type to be
   --  assigned to opaque entities.

   function New_Integer_Literal (Value : Integer) return Node_Id;
   --  Return a new node corresponding to an integer literal
   --  with the given Value.

   function New_Opaque_Type return Node_Id is
   begin
      return Make_Native (No_Location);
   end New_Opaque_Type;

   function New_Integer_Literal (Value : Integer) return Node_Id
   is
      Result : constant Node_Id := Make_Lit_Integer (No_Location);
   begin
      Set_Expr_Value
        (Result, new Constant_Value (Kind => C_General_Integer));
      Expr_Value (Result).Integer_Value := Long_Long_Integer (Value);
      return Result;
   end New_Integer_Literal;

   ---------------------------------------------------------------
   -- Pre_Translate_Element                                     --
   -- Translate an element into IDL.                            --
   -- Used as pre-operation for Iterator.Traverse_Element.      --
   ---------------------------------------------------------------

   procedure Pre_Translate_Element
     (Element : in Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Translator_State) is
   begin
      case Element_Kind (Element) is
         when
           Not_An_Element       |
           A_Statement          |
           A_Path               |
           An_Exception_Handler =>
            Raise_Translation_Error
              (Element, "Unexpected element.");

         when A_Pragma =>
            --  XXX Ignore all pragmas for now. This is
            --  probably wrong. At *least* a pragma
            --  Asynchronous should be translated to a
            --  semantic marker.
            Control := Abandon_Children;

         when A_Defining_Name =>
            null;
            --  Defining names are translated explicitly when
            --  processing the enclosing declaration. The children
            --  of that declaration may then be traversed recursively,
            --  so we need to just ignore the defining_name here.

         when A_Declaration =>
            Process_Declaration (Element, Control, State);

         when A_Definition =>
            Process_Definition (Element, Control, State);

         when An_Expression =>
            Process_Expression (Element, Control, State);

         when An_Association =>
            --  In a DSA unit declaration, An_Association can occur
            --  in a discriminant_constraint in a member or subtype
            --  definition. Such a constraint is not translated,
            --  therefore this point should never be reached (but
            --  the generated helper code might later consult
            --  explicitly the constraints of a declaration to ensure
            --  validity of an object when converting it from an external
            --  representation (Any or marshalled data stream) to its
            --  native representation
            Raise_Translation_Error
              (Element, "Unexpected element (An_Association).");

         when A_Clause =>
            --  XXX
            Control := Abandon_Children;
      end case;
   exception
      when Ex : others =>
         Put_Line ("Unexpected exception in Pre_Translate_Element:");
         Put_Line (To_Wide_String (Ada.Exceptions.Exception_Information (Ex)));

         raise;
   end Pre_Translate_Element;

   procedure Process_Declaration
     (Element : in Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Translator_State) is

      use Asis.Definitions;

      Node             : Node_Id := No_Node;
      DK               : constant Declaration_Kinds
        := Declaration_Kind (Element);
      Defining_Names   : constant Defining_Name_List
        := Declarations.Names (Element);
      Defining_Name    : Asis.Defining_Name
        renames Defining_Names (Defining_Names'First);
      Old_Current_Node : constant Node_Id
        := State.Current_Node;

      -----------------------
      -- Local subprograms --
      -----------------------

      procedure Process_Distributed_Object_Declaration
        (Element : in Asis.Element;
         Control : in out Traverse_Control;
         State   : in out Translator_State);
      --  Process the declaration of a potentially
      --  distributed object (the declaration of any
      --  tagged limited private type).

      procedure Process_Operation_Declaration
        (State                   : in out Translator_State;
         Parameter_Profile       : Asis.Parameter_Specification_List;
         Result_Profile          : Asis.Element := Nil_Element;
         Implicit_Self_Parameter : Asis.Element := Nil_Element);
      --  Core processing for the declaration of a method of a
      --  remote entity, i. e. either a subprogram_declaration,
      --  or a full_type_declaration that declares a RAS.
      --
      --  State              - The translator state.
      --  Parameter_Profile  - The calling profile of the operation.
      --  Result_Profile     - The return profile (for a funtion),
      --                       Nil_Element (for a procedure).
      --  Implicit_Self_Parameter - The Parameter_Specification that
      --                       corresponds to the implicit "Self"
      --                       parameter, if this is a primitive operation
      --                       for a distributed object type, Nil_Element
      --                       otherwise.
      --
      --  Pre-condition:  State.Current_Node is the <interface_dcl>.
      --  Post-condition: State.Current_Node is the <op_dcl> node
      --  and its name has not been set.

      procedure Process_Distributed_Object_Declaration
        (Element : in Asis.Element;
         Control : in out Traverse_Control;
         State   : in out Translator_State)
      is
         --  Forward_Node : Node_Id;
         Success : Boolean;
      begin
--          Forward_Node := New_Forward_Interface;
--          Set_Parent (Forward_Node, State.Current_Node);
--          Add_Definition (State.Current_Node, Forward_Node);

--          Node := New_Interface;
--          Set_Parent (Node, State.Current_Node);
--          Add_Interface (State.Current_Node, Node);
--          Forward_Node := Make_Forward_Interface (No_Location);
         --  XXX for now do not define the forward interface.

         Node := Make_Interface (No_Location);
--          Set_Forward (Node, Forward_Node);
--          Set_Forward (Forward_Node, Node);
--          Set_Definition (Node, Idl_Fe.Tree.Definition (Forward_Node));
         Success := Add_Identifier
           (Node, Map_Defining_Name (Defining_Name));
         pragma Assert (Success);
         Append_Node_To_Contents
           (State.Current_Node, Node);

         if DK = A_Private_Extension_Declaration then
            --  This is an extension of a distributed object declaration:
            --  either a private_extension_declaration or an
            --  ordinary_type_declaration which is a derived_type_declaration
            --  for a distributed object type, without an extension part.
            declare
               Ancestor_Definition : constant Asis.Defining_Name
                 := Corresponding_Entity_Name_Definition
                 (Asis.Definitions.Subtype_Mark
                  (Ancestor_Subtype_Indication
                   (Type_Declaration_View (Element))));
            begin
               Append_Node_To_Parents
                 (Node, Get_Translation (Ancestor_Definition));
            end;
         else
            --  This is a root distributed object declaration.
            null;
         end if;

         Set_Translation (Element, Node);
         --  The translation information for a tagged type
         --  definition is the corresponding Interface node.

         Control := Abandon_Children;
         --  Children were processed explicitly.
      end Process_Distributed_Object_Declaration;

      procedure Process_Operation_Declaration
        (State                   : in out Translator_State;
         Parameter_Profile       : Asis.Parameter_Specification_List;
         Result_Profile          : Asis.Element := Nil_Element;
         Implicit_Self_Parameter : Asis.Element := Nil_Element) is

         Op_Node         : Node_Id;
         --  The <op_dcl>
         Value_Type_Node : Node_Id;
         --  <param_type_spec> or "void", for use in <op_type_spec>.

      begin
         Op_Node := Make_Operation (No_Location);
         Set_Is_Oneway (Op_Node, False);
         Append_Node_To_Contents (State.Current_Node, Op_Node);
         Push_Scope (Op_Node);

         if  Is_Nil (Result_Profile) then
            Value_Type_Node := Make_Void (No_Location);
         else
            Value_Type_Node := Translate_Subtype_Mark
              (Result_Profile);
         end if;

         Set_Operation_Type (Op_Node, Value_Type_Node);

         State.Current_Node := Op_Node;

         for I in Parameter_Profile'Range loop
            if Is_Identical (Parameter_Profile (I),
                             Implicit_Self_Parameter) then
               Translate_Formal_Parameter
                 (Specification    => Parameter_Profile (I),
                  Is_Implicit_Self => True,
                  State            => State);
            else
               Translate_Formal_Parameter
                 (Specification    => Parameter_Profile (I),
                  Is_Implicit_Self => False,
                  State            => State);
            end if;
         end loop;
         Pop_Scope;
      end Process_Operation_Declaration;

   begin

      --  Process_Declaration

      case DK is
         when
           An_Ordinary_Type_Declaration |              --  3.2.1(3)
           A_Subtype_Declaration        |              --  3.2.2(2)
           A_Task_Type_Declaration      |              --  9.1(2)
           A_Protected_Type_Declaration =>             --  9.4(2)
            declare
               Type_Definition      : constant Asis.Definition
                 := Declarations.Type_Declaration_View (Element);
               Success : Boolean;
            begin
               pragma Assert (Defining_Names'Length = 1);
               --  Only one defining_name in a full_type_declaration,
               --  subtype_declaration, task_type_declaration or
               --  protected_type_declaration.

               if True
                 and then Type_Kind (Type_Definition)
                   = An_Access_Type_Definition
                 and then Access_Type_Kind (Type_Definition)
                   in Access_To_Subprogram_Definition
               then
                  --  This is the definition of a Remote Access to
                  --  Subprogram type.
                  Node := Make_Interface (No_Location);
                  Append_Node_To_Contents
                    (State.Current_Node, Node);
                  Success := Add_Identifier
                    (Node, Map_Defining_Name
                     (Defining_Names (Defining_Names'First)));
                  pragma Assert (Success);
                  State.Current_Node := Node;

                  case Access_Type_Kind (Type_Definition) is
                     when
                       An_Access_To_Procedure           |
                       An_Access_To_Protected_Procedure =>
                        Process_Operation_Declaration
                          (State,
                           Access_To_Subprogram_Parameter_Profile
                           (Type_Definition));
                     when
                       An_Access_To_Function           |
                       An_Access_To_Protected_Function =>
                        Process_Operation_Declaration
                          (State,
                           Access_To_Subprogram_Parameter_Profile
                           (Type_Definition),
                           Result_Profile =>
                             Access_To_Function_Result_Profile
                           (Type_Definition));
                     when others =>
                        --  This cannot happen because we checked that
                        --  Access_Kind in Access_To_Subprogram_Definition
                        raise ASIS_Failed;
                  end case;
                  Success := Add_Identifier (State.Current_Node, "Call");
                  pragma Assert (Success);

                  Set_Translation (Element, State.Current_Node);
                  --  The translation of a RAS declaration is
                  --  an <op_dcl>.
               else

                  --  This is the definition of a normal type or
                  --  of a subtype.

                  Node := No_Node;

                  case Type_Kind (Type_Definition) is
                     when An_Enumeration_Type_Definition =>
                        Node := Make_Enum (No_Location);
                        Success := Add_Identifier
                          (Node, Map_Defining_Name (Defining_Name));

                     when A_Record_Type_Definition =>
                        if not Is_Limited_Type (Element)
                          or else Is_Tagged_Type (Element)
                        then
                           Node := Make_Struct (No_Location);
                           Success := Add_Identifier
                             (Node, Map_Defining_Name (Defining_Name));
                        end if;

                     when
                       A_Constrained_Array_Definition    |
                       An_Unconstrained_Array_Definition =>

                        if not Is_Limited_Type (Element) then
                           Node := Make_Struct (No_Location);
                           Success := Add_Identifier
                             (Node, Map_Defining_Name (Defining_Name));
                        end if;

                     when others =>
                        null;
                  end case;

                  if Node /= No_Node then

                     --  An enumeration, record or array definition.

                     pragma Assert (Success);
                     Append_Node_To_Contents
                       (State.Current_Node, Node);
                     Set_Translation (Element, Node);

                     Set_Previous_Current_Node
                       (Element, State.Current_Node);
                     State.Current_Node := Node;

                     --  Process children recursively.
                     return;

                  else

                     --  Any other type definition.

                     Node := Make_Type_Declarator (No_Location);
                     Append_Node_To_Contents (State.Current_Node, Node);

                     declare
                        Declarator_Node : constant Node_Id
                          := Make_Declarator (No_Location);
                     begin
                        Set_Parent (Declarator_Node, Node);
                        Append_Node_To_Declarators (Node, Declarator_Node);
                        Set_Translation (Element, Declarator_Node);
                        --  The translation of a type declaration is
                        --  a <declarator> in a <type_dcl>.

                        if False
                        --  For now, we cannot determine the bounds of a
                        --  static constrained array.
                          and then Definition_Kind (Type_Definition)
                            = A_Type_Definition
                          and then Type_Kind (Type_Definition)
                            = A_Constrained_Array_Definition
                        then
--                         Node := New_Node (N_Array_Declarator);
--                         Set_Parent (Node, Declarator_Node);
--                         Set_Specific_Declarator (Declarator_Node, Node);

--                         State.Current_Node := Node;
--                         Translate_Defining_Name (Defining_Name, State);

                           --  Here we should process the array dimensions

                           raise Program_Error;
                        else
                           Success := Add_Identifier
                             (Declarator_Node, Map_Defining_Name
                              (Defining_Name));
                           pragma Assert (Success);
                        end if;
                     end;

                     if DK = A_Subtype_Declaration then
                        Set_T_Type
                          (Node, Translate_Subtype_Mark
                           (Asis.Definitions.Subtype_Mark
                            (Type_Definition)));
                        --  In A_Subtype_Declaration, the Type_Definition
                        --  is A_Subtype_Indication.
                     elsif Is_Limited_Type (Element)
                       or else Is_Tagged_Type (Element)
                     then
                        --  Limited types and (non-private, hence
                        --  non-distributed-objects) tagged types
                        --  are mapped to an opaque type.
                        Set_T_Type (Node, New_Opaque_Type);
                     else
                        Set_Previous_Current_Node
                          (Element, State.Current_Node);
                        State.Current_Node := Node;

                        --  Process children recursively
                        return;

                     end if;
                  end if;
               end if;

               State.Current_Node := Old_Current_Node;

               Control := Abandon_Children;
               --  Children were processed explicitly.
            end;

         when An_Incomplete_Type_Declaration =>        --  3.2.1(2), 3.10(2)
            --  An incomplete_type_declaration is translated
            --  when completed. The only place where the name
            --  could be used before completion in the context
            --  of CIAO is as part of an access_definition in
            --  the profile of a subprogram_declaration.
            --  Since the mapping of the subprogram_declaration
            --  is produced after all other definitions in the <module>,
            --  this is not an issue => we do nothing.

            Control := Abandon_Children;
            --  No child processing required.

         when A_Private_Type_Declaration =>            --  3.2.1(2), 7.3(2)
            declare
               TK : Trait_Kinds := Trait_Kind (Element);
               Type_Definition  : constant Asis.Definition
                 := Declarations.Type_Declaration_View (Element);
            begin
               pragma Assert (Defining_Names'Length = 1);
               --  Only one defining_name in a private_type_declaration.

               if (TK = An_Abstract_Limited_Private_Trait
                   or else TK = A_Limited_Private_Trait)
                 and then Definition_Kind (Type_Definition)
                   = A_Tagged_Private_Type_Definition then
                  --  This is the declaration of a potentially
                  --  distributed object.

                  Process_Distributed_Object_Declaration
                    (Element, Control, State);

               else
                  --  For A_Private_Type_Declaration that is not
                  --  a tagged limited private (possibly abstract)
                  --  type declaration, the type is mapped to an
                  --  opaque sequence of octets.
                  declare
                     Type_Dcl_Node        : Node_Id;
                     Declarator_Node      : Node_Id;
                     Success : Boolean;
                  begin
                     Type_Dcl_Node := Make_Type_Declarator (No_Location);
                     Append_Node_To_Contents
                       (Type_Dcl_Node, State.Current_Node);

                     Declarator_Node := Make_Declarator (No_Location);
                     Set_Parent (Declarator_Node, Type_Dcl_Node);

                     Append_Node_To_Declarators
                       (Type_Dcl_Node, Declarator_Node);
                     Set_Translation (Element, Declarator_Node);

                     Success := Add_Identifier
                       (Declarator_Node, Map_Defining_Name (Defining_Name));
                     pragma Assert (Success);
                     Set_T_Type (Type_Dcl_Node, New_Opaque_Type);

                     State.Current_Node := Old_Current_Node;
                  end;

                  Control := Abandon_Children;
                  --  Children were processed explicitly.
               end if;
            end;

         when A_Private_Extension_Declaration =>       --  3.2.1(2), 7.3(3)
            pragma Assert (Defining_Names'Length = 1);
            --  Only one defining_name in a private_extension_declaration.

            if Is_Limited_Type (Element) then
               --  A private_extension_declaration declares
               --  a tagged private type. If it is limited as well,
               --  then it is an extension of a potentially
               --  distributed object.

               Process_Distributed_Object_Declaration
                 (Element, Control, State);

            end if;
            --  For A_Private_Extension_Declaration that is not
            --  a tagged limited private (possibly abstract)
            --  type declaration, the implicit processing is done,
            --  resulting in an opaque type mapping.
            --  XXX NOT CHECKED!

         when
           A_Variable_Declaration         |            --  3.3.1(2)
           A_Single_Task_Declaration      |            --  3.3.1(2), 9.1(3)
           A_Single_Protected_Declaration =>           --  3.3.1(2), 9.4(2)
            Raise_Translation_Error
            (Element, "Unexpected variable declaration"
             & " (according to unit categorization).");

         when
           A_Constant_Declaration          |           --  3.3.1(4)
           A_Deferred_Constant_Declaration |           --  3.3.1(6), 7.4(2)
           An_Integer_Number_Declaration   |           --  3.3.2(2)
           A_Real_Number_Declaration       =>          --  3.5.6(2)
            raise Not_Implemented;

         when An_Enumeration_Literal_Specification =>  --  3.5.1(3)
            pragma Assert (Kind (State.Current_Node) = K_Enum);

            declare
               Enumerator_Node : constant Node_Id
                 := Make_Enumerator (No_Location);
               Success : Boolean;
            begin
               pragma Assert (Defining_Names'Length = 1);
               --  Only one defining_name in an
               --  enumeration_literal_specification.

               Append_Node_To_Enumerators
                 (State.Current_Node, Enumerator_Node);

               Success := Add_Identifier
                 (Enumerator_Node, Map_Defining_Name
                  (Defining_Name));
               pragma Assert (Success);
               Set_Translation (Element, Enumerator_Node);

               Control := Abandon_Children;
               --  Children were processed explicitly.
            end;

         when
           A_Discriminant_Specification |              --  3.7(5)
           A_Component_Declaration      =>             --  3.8(6)
            pragma Assert (Kind (State.Current_Node) = K_Struct);

            declare
               Component_Subtype_Mark : Asis.Expression;
               Declarator_Node        : Node_Id;
               Success : Boolean;
            begin
               if DK = A_Discriminant_Specification then
                  Component_Subtype_Mark := Declaration_Subtype_Mark (Element);
               else
                  Component_Subtype_Mark := Asis.Definitions.Subtype_Mark
                    (Component_Subtype_Indication
                     (Object_Declaration_View (Element)));
               end if;

               Node := Make_Member (No_Location);
               Append_Node_To_Members (State.Current_Node, Node);

               for I in Defining_Names'Range loop
                  Declarator_Node := Make_Declarator (No_Location);
                  Set_Parent (Declarator_Node, Node);
                  Append_Node_To_Decl (Node, Declarator_Node);

                  Success := Add_Identifier
                    (Declarator_Node, Map_Defining_Name (Defining_Name));
                  pragma Assert (Success);
               end loop;

               Set_M_Type
                 (Node, Translate_Subtype_Mark
                   (Component_Subtype_Mark));

               State.Current_Node := Old_Current_Node;

               Control := Abandon_Children;
               --  Child elements were processed explicitly.
            end;

         when
           A_Procedure_Declaration |                   --  6.1(4)
           A_Function_Declaration  =>                  --  6.1(4)
            declare
               Is_Function             : constant Boolean
                 := (DK = A_Function_Declaration);
               Profile                 : constant Parameter_Specification_List
                 := Parameter_Profile (Element);
               Implicit_Self_Parameter : Asis.Element := Nil_Element;
               Interface_Dcl_Node      : Node_Id := No_Node;
               Old_Current_Node        : constant Node_Id
                 := State.Current_Node;
               Success : Boolean;
            begin
               pragma Assert (Defining_Names'Length = 1);
               --  Only one defining_name in a subprogram declaration.

               if State.Unit_Category = Remote_Call_Interface then
                  --  This is a remote subprogram of an RCI unit:
                  --  the current scope is the <interface> that maps
                  --  that unit.
                  Interface_Dcl_Node := State.Current_Node;
               else
                  declare
                     Controlling_Formals :
                       constant Parameter_Specification_List
                       := Controlling_Formal_Parameters (Element);
                     Tagged_Type_Declaration : Declaration
                       := Nil_Element;
                  begin
                     --  First determine if this is a primitive operation
                     --  of a tagged type.

                     if Is_Function then
                        declare
                           Subtype_Mark : constant Asis.Expression
                             := Result_Profile (Element);
                           Subtype_Declaration : constant Asis.Declaration
                             := Corresponding_Entity_Name_Declaration
                               (Subtype_Mark);
                        begin
                           if Is_Controlling_Result (Subtype_Mark) then
                              Tagged_Type_Declaration :=
                                Corresponding_First_Subtype
                                (Subtype_Declaration);
                           end if;
                        end;
                     end if;

                     if Is_Nil (Tagged_Type_Declaration)
                       and then Controlling_Formals'Length > 0 then
                        Implicit_Self_Parameter
                          := Controlling_Formals
                          (Controlling_Formals'First);

                        Tagged_Type_Declaration
                          := Corresponding_First_Subtype
                          (Corresponding_Entity_Name_Declaration
                           (Declaration_Subtype_Mark
                            (Implicit_Self_Parameter)));
                     end if;

                     if True
                       and then not Is_Nil (Tagged_Type_Declaration)
                       and then not Is_Overriding_Inherited_Subprogram
                         (Element, Tagged_Type_Declaration)
                     then
                        --  This is a new dispatching operation of a tagged
                        --  type (it does not override an inherited operation).
                        --  Obtain the corresponding <interface_dcl> node.

                        --  XXX For now, we do not check whether this operation
                        --  overloads another with a different signature. In
                        --  that case, a non-conformant IDL tree is produced
                        --  (it contains overloaded operation declarations).
                        Interface_Dcl_Node
                          := Get_Translation (Tagged_Type_Declaration);
                     end if;
                  end;
               end if;

               if Kind (Interface_Dcl_Node) = K_Interface then
                  State.Current_Node := Interface_Dcl_Node;
                  if Is_Function then
                     Process_Operation_Declaration
                       (State, Profile,
                        Result_Profile => Result_Profile (Element),
                        Implicit_Self_Parameter => Implicit_Self_Parameter);
                  else
                     Process_Operation_Declaration
                       (State, Profile,
                        Result_Profile => Nil_Element,
                        Implicit_Self_Parameter => Implicit_Self_Parameter);
                  end if;
                  Success := Add_Identifier
                    (State.Current_Node, Map_Defining_Name (Defining_Name));
                  pragma Assert (Success);

                  Set_Translation (Element, State.Current_Node);
                  --  The translation of a subprogram declaration is
                  --  an <op_dcl>.
               end if;

               State.Current_Node := Old_Current_Node;
               Control := Abandon_Children;
               --  Children were processed explicitly.
            end;

         when A_Parameter_Specification =>             --  6.1(15)
            declare
               use CIAO.Translator.State;

               Defining_Names   : constant Defining_Name_List
                 := Declarations.Names (Element);
               Subtype_Mark     : constant Asis.Expression
                 := Declarations.Declaration_Subtype_Mark (Element);
               Declarator_Node : Node_Id;
               Mode : Param_Mode;
               Old_Current_Node : constant Node_Id
                 := State.Current_Node;
               Success : Boolean;
            begin
               pragma Assert (False
                 or else State.Pass = Self_Formal_Parameter
                 or else State.Pass = Normal_Formal_Parameter);

               for I in Defining_Names'Range loop
                  if State.Pass /= Self_Formal_Parameter
                    or else I /= Defining_Names'First then
                     Node := Make_Param (No_Location);
                     Append_Node_To_Parameters (State.Current_Node, Node);
                     Declarator_Node := Make_Declarator (No_Location);
                     Set_Declarator (Node, Declarator_Node);
                     Set_Parent (Declarator_Node, Node);
                     Success := Add_Identifier
                       (Declarator_Node,
                        Map_Defining_Name (Defining_Names (I)));
                     pragma Assert (Success);

                     Set_Param_Type
                       (Node, Translate_Subtype_Mark (Subtype_Mark));

                     State.Current_Node := Old_Current_Node;

                     if Trait_Kind (Element) = An_Access_Definition_Trait then
                        Mode := Mode_Inout;
                     else
                        case Mode_Kind (Element) is
                           when Not_A_Mode     =>   --  An unexpected element
                              Raise_Translation_Error
                              (Element, "Unexpected element (Not_A_Mode).");
                           when
                             A_Default_In_Mode |    --  P :        T
                             An_In_Mode        =>   --  P : IN     T
                              Mode := Mode_In;
                           when An_Out_Mode    =>   --  P :    OUT T
                              Mode := Mode_Out;
                           when An_In_Out_Mode =>   --  P : IN OUT T
                              Mode := Mode_Inout;
                        end case;
                     end if;
                     Set_Mode (Node, Mode);
                  end if;
               end loop;
            end;

            Control := Abandon_Children;
            --  Children were processed explicitly.

         when A_Package_Declaration =>                 --  7.1(2)
            declare
               Visible_Part : constant Declarative_Item_List
                 := Declarations.Visible_Part_Declarative_Items
                 (Declaration     => Element,
                  Include_Pragmas => True);
               Success : Boolean;
            begin

               if State.Unit_Category = Remote_Call_Interface then

                  --  The translation of a Remote Call Interface
                  --  is an <interface>

                  Node := Make_Interface (No_Location);
                  Append_Node_To_Contents (State.Current_Node, Node);
                  Push_Scope (Node);

                  --  Set_Is_Remote_Subprograms (Node, True);
                  --  XXX CANNOT BE REPRESENTED in idlac tree!
                  --  but will we need this in code gen phase?

                  Success := Add_Identifier
                    (Node, Map_Defining_Name (Defining_Name));
                  pragma Assert (Success);

                  State.Current_Node := Node;
               else

                  --  The translation of a non-RCI package
                  --  declaration is a <module>

                  Node := Make_Module (No_Location);
                  Append_Node_To_Contents (State.Current_Node, Node);
                  Push_Scope (Node);

                  Set_Translation (Element, Node);
                  Success := Add_Identifier
                    (Node, Map_Defining_Name (Defining_Name));
                  pragma Assert (Success);
               end if;

               Translate_List (Visible_Part, State);

               Pop_Scope;
               State.Current_Node := Old_Current_Node;

               Control := Abandon_Children;
               --  Children were processed explicitly.
            end;

         when
           A_Procedure_Body_Declaration    |           --  6.3(2)
           A_Function_Body_Declaration     |           --  6.3(2)
           A_Task_Body_Declaration         |           --  9.1(6)
           A_Protected_Body_Declaration    |           --  9.4(7)
           A_Package_Body_Declaration      |           --  7.2(2)
           A_Procedure_Body_Stub           |           --  10.1.3(3)
           A_Function_Body_Stub            |           --  10.1.3(3)
           A_Package_Body_Stub             |           --  10.1.3(4)
           A_Task_Body_Stub                |           --  10.1.3(5)
           A_Protected_Body_Stub           |           --  10.1.3(6)
           An_Entry_Body_Declaration       =>          --  9.5.2(5)
            Raise_Translation_Error
            (Element, "Unexpected body declaration.");

         when
           An_Exception_Declaration        |           --  11.1(2)
            --  User-defined exceptions need not be
            --  mapped, as all Ada exceptions are propagated
            --  as ::CIAO::Ada_Exception.
           A_Generic_Procedure_Declaration |           --  12.1(2)
           A_Generic_Function_Declaration  |           --  12.1(2)
           A_Generic_Package_Declaration   =>          --  12.1(2)
            --  Generic declarations define no exported services,
            --  and are therefore not mapped.
            Control := Abandon_Children;

         when
           A_Package_Instantiation                  |  --  12.3(2)
           A_Procedure_Instantiation                |  --  12.3(2)
           A_Function_Instantiation                 |  --  12.3(2)

           An_Object_Renaming_Declaration           |  --  8.5.1(2)
           An_Exception_Renaming_Declaration        |  --  8.5.2(2)
           A_Package_Renaming_Declaration           |  --  8.5.3(2)
           A_Procedure_Renaming_Declaration         |  --  8.5.4(2)
           A_Function_Renaming_Declaration          |  --  8.5.4(2)
           A_Generic_Package_Renaming_Declaration   |  --  8.5.5(2)
           A_Generic_Procedure_Renaming_Declaration |  --  8.5.5(2)
           A_Generic_Function_Renaming_Declaration  => --  8.5.5(2)
            --  These constructs are not supported due to
            --  restrictions placed by the translation specification.
            Raise_Translation_Error
              (Element, "Construct not supported by translation schema.");

         when
           Not_A_Declaration                |          --  Unexpected element
           A_Loop_Parameter_Specification   |          --  5.5(4)
           An_Entry_Declaration             |          --  9.5.2(2)
           An_Entry_Index_Specification     |          --  9.5.2(2)
           A_Choice_Parameter_Specification |          --  11.2(4)
           A_Formal_Object_Declaration      |          --  12.4(2)
           A_Formal_Type_Declaration        |          --  12.5(2)
           A_Formal_Procedure_Declaration   |          --  12.6(2)
           A_Formal_Function_Declaration    |          --  12.6(2)
           A_Formal_Package_Declaration     |          --  12.7(2)
           A_Formal_Package_Declaration_With_Box =>    --  12.7(3)
            Raise_Translation_Error
              (Element, "Unexpected element (A_Declaration).");
      end case;
   end Process_Declaration;

   procedure Process_Definition
     (Element : in Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Translator_State) is
   begin
      case Definition_Kind (Element) is
         when Not_A_Definition =>              --  An unexpected element
            Raise_Translation_Error
              (Element, "Unexpected element (A_Definition).");

         when A_Type_Definition =>             --  3.2.1(4)
            Process_Type_Definition (Element, Control, State);

         when A_Subtype_Indication =>          --  3.2.2(3)
            --  Process child nodes:
            --  translate subtype_mark, ignore constraint.
            null;

         when A_Constraint =>                  --  3.2.2(5)
            --  Constraints cannot be represented in OMG IDL
            --  and are therefore ignored.
            Control := Abandon_Children;

         when A_Discrete_Subtype_Definition => --  3.6(6)
            --  XXX Does this ever happen?
            raise Program_Error;

         when A_Discrete_Range =>              --  3.6.1(3)
            case Kind (State.Current_Node) is
               when K_Union =>
                  --  A discrete_range as a discrete_choice for a variant
                  null;
                  --  XXX TODO: generate one Label for each value
                  --  of the anonymous subtype declared by the discrete_range,
                  --  and append each of these labels to
                  --  Labels (State.Current_Node).

               when others =>
                  null;
            end case;

         when An_Unknown_Discriminant_Part =>  --  3.7(3)
            --  XXX Does this ever happen?
            raise Program_Error;

         when A_Known_Discriminant_Part =>     --  3.7(2)
            --  Process child nodes recursively.
            null;

         when
           A_Component_Definition   |          --  3.6(7)
           A_Record_Definition      |          --  3.8(3)
            --  Process child nodes.
           A_Null_Record_Definition |          --  3.8(3)
           A_Null_Component         =>         --  3.8(4)
            --  Nothing to do, no child elements.
            null;

         when A_Variant_Part =>                --  3.8.1(2)
            declare
               Member_Node : constant Node_Id := Make_Member (No_Location);
               Decl_Node   : constant Node_Id := Make_Declarator (No_Location);
               Union_Node  : constant Node_Id := Make_Union (No_Location);
               Success : Boolean;
            begin
               Append_Node_To_Members (State.Current_Node, Member_Node);
               Append_Node_To_Decl (Member_Node, Decl_Node);
               Add_Identifier_With_Renaming (Decl_Node, "variant");
               Success := Add_Identifier
                 (Union_Node, Idl_Fe.Tree.Synthetic.Name
                  (Decl_Node) & "_union");
               pragma Assert (Success);
               Set_M_Type (Member_Node, Union_Node);

               Set_Switch_Type
                 (Union_Node,
                  Translate_Subtype_Mark
                  (Declaration_Subtype_Mark
                   (Corresponding_Entity_Name_Declaration
                    (Discriminant_Direct_Name (Element)))));
               Set_Translation (Element, Union_Node);
               Set_Previous_Current_Node (Element, State.Current_Node);
               State.Current_Node := Union_Node;

               --  Process children recursively.
            end;

         when A_Variant =>                     --  3.8.1(3)
            --  Enclosing node is the union corresponding to the
            --  Variant_Part.

            declare
               Case_Node : constant Node_Id := Make_Case (No_Location);
               Decl_Node : constant Node_Id := Make_Declarator (No_Location);
               Union_Node : constant Node_Id := State.Current_Node;

               Variant_Components : constant Asis.Record_Component_List
                 := Record_Components (Element);
               Variant_Component : Asis.Declaration
                 renames Variant_Components (Variant_Components'First);

               Struct_Node : Node_Id;
               Success : Boolean;
            begin
               Append_Node_To_Cases (State.Current_Node, Case_Node);
               State.Current_Node := Case_Node;
               Translate_List (Variant_Choices (Element), State);
               State.Current_Node := Union_Node;

               if Is_In_List (Labels (Case_Node), No_Node) then
                  --  The default label is denoted by an empty node.
                  Set_Default_Index
                    (State.Current_Node,
                     Long_Integer
                     (Length (Cases (Union_Node))) - 1);
               end if;

               Set_Case_Decl (Case_Node, Decl_Node);

               if Variant_Components'Length = 1
                 and then Element_Kind (Variant_Component) = A_Declaration
               then
                  declare
                     Component_Defining_Names :
                       constant Asis.Defining_Name_List
                       := Declarations.Names (Variant_Component);
                     Component_Defining_Name : Asis.Defining_Name
                       := Component_Defining_Names
                       (Component_Defining_Names'First);
                  begin
                     Success := Add_Identifier
                       (Decl_Node,
                        Map_Defining_Name (Component_Defining_Name));
                     pragma Assert (Success);

                     Set_Case_Type
                       (Case_Node, Translate_Subtype_Mark
                        (Asis.Definitions.Subtype_Mark
                         (Component_Subtype_Indication
                          (Object_Declaration_View
                           (Variant_Component)))));
                  end;
               else
                  Struct_Node := Make_Struct (No_Location);
                  Add_Identifier_With_Renaming
                    (Decl_Node, "variant_components");
                  Success := Add_Identifier
                    (Struct_Node, Idl_Fe.Tree.Synthetic.Name
                     (Decl_Node) & "_struct");
                  pragma Assert (Success);
                  Set_Case_Type (Case_Node, Struct_Node);
                  State.Current_Node := Struct_Node;
                  Translate_List (Variant_Components, State);
               end if;

               State.Current_Node := Union_Node;
               --  Restore value.

               Control := Abandon_Children;
               --  They have been processed explicitly.
            end;

         when An_Others_Choice =>              --  3.8.1(5)
            --  => 4.3.1(5) => 4.3.3(5) => 11.2(5)

            if Kind (State.Current_Node) = K_Case then
               Append_Node_To_Labels
                 (State.Current_Node, No_Node);
               --  The default label is denoted by an empty node.
            end if;

         when
           A_Private_Type_Definition        |  --  7.3(2)
           A_Tagged_Private_Type_Definition |  --  7.3(2)
           A_Private_Extension_Definition   => --  7.3(3)
            --  Should probably never happen.
            Raise_Translation_Error
              (Element, "Unexpected element (a private type definition).");

         when
           A_Task_Definition      |            --  9.1(4)
           A_Protected_Definition =>           --  9.4(4)
            --  A task type or protected type.
--             declare
--                Type_Spec_Node : Node_Id;
--             begin
--          Type_Spec_Node := Insert_New_Opaque_Type
--          (State.Current_Node);

               Control := Abandon_Children;
--                --  Children not processed (the mapping is opaque).
--             end;
            raise Not_Implemented;

         when A_Formal_Type_Definition =>      --  12.5(3)
            --  XXX Does this ever happen?
            --  We are not supposed to support generics?!?!
            raise Program_Error;

      end case;
   end Process_Definition;

   function Base_Type_For_Standard_Definition
     (Element : Asis.Type_Definition)
     return Node_Id;
   --  Return a <base_type_spec> node that denotes the standard IDL
   --  type corresponding to predefined type Element (which is
   --  expected to be a type definition within Standard).

   function Base_Type_For_Standard_Definition
     (Element : Asis.Type_Definition)
     return Node_Id
   is
   begin
      if Definition_Kind (Element) = A_Subtype_Indication then
         --  Unwind all levels of subtyping.
         return Base_Type_For_Standard_Definition
           (Type_Declaration_View
            (Corresponding_Entity_Name_Declaration
             (Asis.Definitions.Subtype_Mark (Element))));
      else
         case Type_Kind (Element) is
            when A_Signed_Integer_Type_Definition =>
               return Base_Type (Root_Integer);
            when A_Modular_Type_Definition =>
               return Base_Type (Root_Modular);
            when
              A_Floating_Point_Definition        |
              An_Ordinary_Fixed_Point_Definition |
              A_Decimal_Fixed_Point_Definition   =>
               return Base_Type (Root_Real);
            when An_Enumeration_Type_Definition =>
            --  This is "Boolean".
               return Base_Type (Root_Boolean);
            when An_Unconstrained_Array_Definition =>
               --  This is "String".
               return Base_Type (Root_String);
            when others =>
               null;
         end case;
      end if;
      Raise_Translation_Error
        (Element, "Unexpected standard type definition.");
   end Base_Type_For_Standard_Definition;

   procedure Process_Expression
     (Element : in     Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Translator_State)
   is
      use Asis.Extensions;
      --  In this subprogram we use ASIS-for-GNAT extensions
      --  to determine the value of static expressions.

      EK : constant Asis.Expression_Kinds
        := Expression_Kind (Element);
      EI : constant String
        := To_String (Static_Expression_Value_Image (Element));

      EI_Valid : constant Boolean := True
        and then Is_True_Expression (Element)
        and then Is_Static (Element)
        and then EI'Length /= 0;

   begin
      case Kind (State.Current_Node) is
         when K_Union =>
            pragma Assert (EK = An_Identifier);
            --  A Discriminant_Direct_Name.
            --  XXX record that the discriminant for this union
            --  is actually that member of the enclosing struct.
            return;
         when K_Case =>
            null;
            --  see below.
         when others =>
            Raise_Translation_Error
              (Element, "Unexpected expression.");
      end case;

      case EK is
         when Not_An_Expression =>                --  An unexpected element
            Raise_Translation_Error
              (Element, "Unexpected element (Not_An_Expression).");

--          when
--            An_Identifier        |                 --  4.1
--            A_Selected_Component =>                --  4.1.3
--             --  The expression shall be translated as
--             --  a <scoped_name>. State.Current_Node shall
--             --  accept a <scoped_name> subnode.
--             declare

--                use Asis.Compilation_Units;

--                Name_Definition : constant Asis.Element
--                  := Corresponding_Entity_Name_Definition (Element);
--                Origin          : constant Compilation_Unit :=
--                  Enclosing_Compilation_Unit (Name_Definition);
--                --  The library unit where the name is declared.
--                Node : Node_Id;
--             begin
--                if Is_Nil (Corresponding_Parent_Declaration (Origin)) then
--                   --  Element is a subtype_mark that denotes a type
--                   --  declared in predefined package Standard.
--                   Node := Base_Type_For_Standard_Definition
--    (Type_Declaration_View
--     (Enclosing_Element
--      (Name_Definition)));
--                   Set_Base_Type_Spec (State.Current_Node, Node);
--                   Set_Parent (Node, State.Current_Node);
--                else
--                   declare
--                      Include_Node : constant Node_Id
--                        := Get_Translation (Unit_Declaration (Origin));
--                   begin
--                      if Include_Node /= Empty
--                        and then Node_Kind (Include_Node)
--                          = N_Preprocessor_Include
--                      then
--                         Set_Unit_Used (Include_Node, True);
--                      end if;
--                      Node := Relative_Scoped_Name
--                        (Denoted_Definition => Name_Definition,
--                         Referer            => Element);
--                      Set_Scoped_Name (State.Current_Node, Node);
--                      Set_Parent (Node, State.Current_Node);
--                   end;
--                end if;
--                Control := Abandon_Children;
--                --  Children were processed explicitly.
--             end;

--          when An_Attribute_Reference =>           --  4.1.4
--             case Attribute_Kind (Element) is
--                when
--                  A_Base_Attribute  |
--                  A_Class_Attribute =>
--                   Translate_Subtype_Mark (Prefix (Element), State);

--                   Control := Abandon_Children;
--                   --  Children were processed explicitly.
--                when others =>
--                   Raise_Translation_Error
--                     (Element, "Unexpected element"
--                      & " (An_Attribute_Reference).");
--             end case;

         when An_Integer_Literal =>
            Append_Node_To_Labels
              (State.Current_Node,
               New_Integer_Literal (Integer'Value (EI)));

--          when
--            An_Integer_Literal     |               --  2.4
--            A_Character_Literal    |               --  4.1
--            An_Enumeration_Literal |               --  4.1
--            A_Null_Literal         =>              --  4.4
--             Raise_Translation_Error
--               (Element, "Unexpected element (a literal).");
         when
           A_Real_Literal         |               --  2.4.1
           A_String_Literal       =>              --  2.6
            Raise_Translation_Error
            (Element, "Unexpected element (a non-scalar literal).");

--         when
--            An_Operator_Symbol      |              --  4.1
--            A_Function_Call         =>             --  4.1
--             Raise_Translation_Error
--               (Element, "Unexpected element (a function or operator).");

--          when
--            An_Explicit_Dereference |              --  4.1
--            An_Indexed_Component |                 --  4.1.1
--            A_Slice              =>                --  4.1.2
--             Raise_Translation_Error
--               (Element, "Unexpected element"
--                & " (an indexed reference or explicit dereference).");

--          when
--            A_Record_Aggregate           |         --  4.3
--            An_Extension_Aggregate       |         --  4.3
--            A_Positional_Array_Aggregate |         --  4.3
--            A_Named_Array_Aggregate      =>        --  4.3
--             Raise_Translation_Error
--               (Element, "Unexpected element (an aggregate).");

--          when
--            An_And_Then_Short_Circuit      |       --  4.4
--            An_Or_Else_Short_Circuit       |       --  4.4
--            An_In_Range_Membership_Test    |       --  4.4
--            A_Not_In_Range_Membership_Test |       --  4.4
--            An_In_Type_Membership_Test     |       --  4.4
--            A_Not_In_Type_Membership_Test  |       --  4.4
--            A_Parenthesized_Expression     |       --  4.4
--            A_Type_Conversion              |       --  4.6
--            A_Qualified_Expression         =>      --  4.7
--             Raise_Translation_Error
--               (Element, "Unexpected element (An_Expression).");

         when
           An_Allocation_From_Subtype |           --  4.8
           An_Allocation_From_Qualified_Expression => --  4.8
            Raise_Translation_Error
               (Element, "Unexpected element (an allocator).");

         when others =>
            if EI_Valid then
               if Has_Enumeration_Type (Element) then
                  declare
                     Scoped_Name_Node : Node_Id := No_Node;
                     Enum_Type : constant Asis.Definition
                       := Type_Declaration_View
                       (Corresponding_First_Subtype
                        (Corresponding_Expression_Type (Element)));
                     Literals : constant Asis.Declaration_List
                       := Enumeration_Literal_Declarations (Enum_Type);
                  begin
                     Scan_Enumerators :
                     for I in Literals'Range loop
                        declare
                           Literal_Names : constant Asis.Defining_Name_List
                             := Names (Literals (I));
                           Literal_Name : Asis.Defining_Name
                             renames Literal_Names (Literal_Names'First);
                           Pos_Image : constant String
                             := To_String
                             (Position_Number_Image (Literal_Name));
                        begin
                           pragma Assert (Literal_Names'Length = 1);
                           if Pos_Image = EI then
                              Scoped_Name_Node := Make_Scoped_Name
                                (No_Location);
                              Set_Value
                                (Scoped_Name_Node,
                                 Get_Translation (Literals (I)));
                              Append_Node_To_Labels
                                (State.Current_Node, Scoped_Name_Node);
                              exit Scan_Enumerators;
                           end if;
                        end;
                     end loop Scan_Enumerators;
                     if Scoped_Name_Node = No_Node then
                        Raise_Translation_Error
                          (Element,
                           "Could not resolve enumerator name: " & EI);
                     end if;
                  end;
               elsif Has_Integer_Type (Element) then
                  Append_Node_To_Labels
                    (State.Current_Node,
                     New_Integer_Literal (Integer'Value (EI)));
               end if;
            else
               Raise_Translation_Error
                 (Element, "Cannot resolve expression value.");
            end if;
      end case;
   end Process_Expression;

   procedure Process_Type_Definition
     (Element : in Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Translator_State)
   is
      Old_Current_Node : constant Node_Id
        := State.Current_Node;
      TK : constant Asis.Type_Kinds
        := Type_Kind (Element);
   begin
      --  Translate the Element into a <type_spec>, and set the
      --  <type_spec> of State.Current_Node to that.

      case TK is
         when Not_A_Type_Definition =>              --  An unexpected element
            Raise_Translation_Error
              (Element, "Unexpected element (Not_A_Type_Definition).");

         when A_Root_Type_Definition =>             --  3.5.4(14) => 3.5.6(3)
            Raise_Translation_Error
            (Element, "Unexpected implicit element"
             & " (A_Root_Type_Definition).");

         when A_Derived_Type_Definition =>          --  3.4(2)

            Set_T_Type
            (State.Current_Node, Translate_Subtype_Mark
             (Asis.Definitions.Subtype_Mark
              (Asis.Definitions.Parent_Subtype_Indication
               (Element))));

            Control := Abandon_Children;
            --  Children were processed explicitly.

         when An_Enumeration_Type_Definition =>     --  3.5.1(2)
            null;
            --  Process all children recursively.

         when
           A_Signed_Integer_Type_Definition   |     --  3.5.4(3)
           A_Modular_Type_Definition          |     --  3.5.4(4)
           A_Floating_Point_Definition        |     --  3.5.7(2)
           An_Ordinary_Fixed_Point_Definition |     --  3.5.9(3)
           A_Decimal_Fixed_Point_Definition   =>    --  3.5.9(6)

            Set_T_Type
            (State.Current_Node,
             Base_Type_For_Standard_Definition (Element));

            Control := Abandon_Children;
            --  Children were processed explicitly.

         when
           An_Unconstrained_Array_Definition |      --  3.6(2)
           A_Constrained_Array_Definition    =>     --  3.6(2)
            declare
               Component_Subtype_Mark : constant Asis.Expression
                 := Asis.Definitions.Subtype_Mark
                 (Component_Subtype_Indication
                  (Array_Component_Definition (Element)));
            begin
               if Is_Limited_Type
                 (Corresponding_Entity_Name_Declaration
                  (Component_Subtype_Mark)) then

                  --  Current_Node is a typedef

                  Set_T_Type (State.Current_Node, New_Opaque_Type);

               else

                  --  Current_Node is a struct that will hold
                  --  a member array containing array bounds,
                  --  and a member sequence containing array values.

                  declare
                     Dimensions        : Natural;
                     Struct_Type_Node  : constant Node_Id
                       := State.Current_Node;
                     Member_Node       : Node_Id;
                     Declarator_Node   : Node_Id;
                     Parent_Node       : Node_Id;
                     Success           : Boolean;
                  begin
                     if TK = An_Unconstrained_Array_Definition then
                        Dimensions
                          := Index_Subtype_Definitions (Element)'Length;
                     else
                        Dimensions
                          := Discrete_Subtype_Definitions (Element)'Length;
                     end if;

                     --  State.Current_Node is the <struct>
                     --  associated with this type.

                     ----------------------------------------------------------
                     -- <member>: unsigned long long Low_Bound;              --
                     --        OR unsigned long long Low_Bounds[DIMENSIONS]; --
                     ----------------------------------------------------------

                     Member_Node := Make_Member (No_Location);
                     Append_Node_To_Members (Struct_Type_Node, Member_Node);
                     Set_M_Type (Member_Node, Base_Type (Root_Integer));

                     Declarator_Node := Make_Declarator (No_Location);
                     Set_Parent (Declarator_Node, Member_Node);
                     Append_Node_To_Decl (Member_Node, Declarator_Node);

                     if Dimensions = 1 then
                        Success := Add_Identifier
                          (Declarator_Node, "Low_Bound");
                        pragma Assert (Success);
                     else
                        declare
                           Size_Node : Node_Id;
                        begin
                           Success := Add_Identifier
                             (Declarator_Node, "Low_Bounds");
                           pragma Assert (Success);

                           Size_Node := New_Integer_Literal (Dimensions);
                           Append_Node_To_Array_Bounds
                             (Declarator_Node, Size_Node);

                        end;
                     end if;

                     --------------------------------------------------
                     -- <member>: sequence<...<TYPE>> Array_Values;  --
                     --                                              --
                     -- For now, we cannot determine the bounds of a --
                     -- static constrained array, so we always map   --
                     -- all arrays to sequences.                     --
                     --------------------------------------------------

                     Member_Node := Make_Member (No_Location);
                     Append_Node_To_Members (Struct_Type_Node, Member_Node);
                     Set_M_Type (Member_Node, Base_Type (Root_Integer));

                     Declarator_Node := Make_Declarator (No_Location);
                     Set_Parent (Declarator_Node, Member_Node);
                     Append_Node_To_Decl (Member_Node, Declarator_Node);

                     Success := Add_Identifier
                       (Declarator_Node, "Array_Values");
                     pragma Assert (Success);

                     Parent_Node := Member_Node;
                     for I in 1 .. Dimensions loop
                        declare
                           Sequence_Node : Node_Id;
                        begin
                           Sequence_Node := Make_Sequence (No_Location);

                           if Parent_Node = Member_Node then
                              Set_M_Type (Parent_Node, Sequence_Node);
                           else
                              Set_Sequence_Type (Parent_Node, Sequence_Node);
                           end if;
                           Parent_Node := Sequence_Node;
                        end;
                     end loop;

                     --  Parent_Node is innermost sequence.

                     Set_Sequence_Type
                       (Parent_Node, Translate_Subtype_Mark
                         (Component_Subtype_Mark));

                     State.Current_Node := Old_Current_Node;
                  end;
               end if;

               Control := Abandon_Children;
               --  Children were processed explicitly.
            end;

         when A_Record_Type_Definition =>           --  3.8(2)
            null;
            --  Process all children recursively.

         when
           A_Tagged_Record_Type_Definition       |  --  3.8(2)
           A_Derived_Record_Extension_Definition => --  3.4(2)

            --  Processed directly in Process_Declaration.
            raise Program_Error;

--             Set_T_Type (State.Current_Node, New_Opaque_Type);

--             Control := Abandon_Children;
--             --  Children were processed explicitly

         when An_Access_Type_Definition =>          --  3.10(2)

            --  This is the definition of a Remote Access to Class-Wide
            --  type (RAS were processed in Process_Declaration directly;
            --  other access-to-object types would not be allowed in the
            --  visible part of a declared pure, RT or RCI package).

            declare
               Designated_Subtype : constant Asis.Expression
                 := Asis.Definitions.Subtype_Mark
                 (Asis.Definitions.Access_To_Object_Definition (Element));
            begin
               pragma Assert (True
                 and then Expression_Kind (Designated_Subtype)
                   = An_Attribute_Reference
                 and then Attribute_Kind (Designated_Subtype)
                   = A_Class_Attribute);

               Set_T_Type
                 (State.Current_Node, Translate_Subtype_Mark
                  (Prefix (Designated_Subtype)));

               Control := Abandon_Children;
               --  Child elements were processed explicitly.
            end;
      end case;
   end Process_Type_Definition;

   function Map_Defining_Name
     (Name : in Asis.Defining_Name)
     return String
   is
      Name_Image : constant Program_Text
        := Declarations.Defining_Name_Image (Name);
   begin
      if Is_Identical
        (Enclosing_Element (Name),
         Unit_Declaration (Enclosing_Compilation_Unit (Name)))
      then
         return IDL_Module_Name (Enclosing_Compilation_Unit (Name));
      else
         case Defining_Name_Kind (Name) is
            when Not_A_Defining_Name =>
               Raise_Translation_Error
                 (Name, "Unexpected element (Not_A_Defining_Name).");
            when
              A_Defining_Identifier |
              A_Defining_Enumeration_Literal =>
               return To_String (Name_Image);

            when A_Defining_Character_Literal =>
               return Maps.Character_Literal_Identifier (Name_Image);

            when A_Defining_Operator_Symbol =>
               return Maps.Operator_Symbol_Identifier (Name);

            when A_Defining_Expanded_Name =>
               --  Cannot happen (this is a defining_program_unit_name,
               --  taken care of by "if" above.)
               raise ASIS_Failed;
         end case;
      end if;
   end Map_Defining_Name;

   function Translate_Subtype_Mark
     (Exp : in Asis.Expression)
     return Node_Id
   is
      EK : constant Expression_Kinds := Expression_Kind (Exp);
   begin
      case EK is
         when
           An_Identifier        |                 --  4.1
           A_Selected_Component =>                --  4.1.3

            declare
               use Asis.Compilation_Units;

               Name_Definition : constant Asis.Element
                 := Corresponding_Entity_Name_Definition (Exp);
               Origin          : constant Compilation_Unit :=
                 Enclosing_Compilation_Unit (Name_Definition);
               --  The library unit where the name is declared.

            begin
               if Is_Nil (Corresponding_Parent_Declaration (Origin)) then

                  --  Exp is a subtype_mark that denotes a type
                  --  declared in predefined package Standard.

                  return Base_Type_For_Standard_Definition
                    (Type_Declaration_View
                     (Enclosing_Element
                      (Name_Definition)));
               else

                  --  Exp is a name that resolves to denote a
                  --  user-defined type.

                  declare
                     N : constant Node_Id := Get_Translation
                       (Corresponding_Entity_Name_Declaration (Exp));
                     Name : constant Node_Id := Make_Scoped_Name (No_Location);
                  begin
                     if N = No_Node then
                        Raise_Translation_Error
                          (Exp, "Translation of element is unknown.");
                     end if;

                     if not Is_Named (N) then
                        Raise_Translation_Error
                          (Exp, "Translation of element is not named (it is a "
                           & Node_Kind'Image (Kind (N)) & ").");
                     end if;

                     Set_Value (Name, N);
                     return Name;
                  end;
               end if;
            end;

         when An_Attribute_Reference =>           --  4.1.4
            case Attribute_Kind (Exp) is
               when
                 A_Base_Attribute  |
                 A_Class_Attribute =>
                  return Translate_Subtype_Mark (Prefix (Exp));
               when others =>
                  Raise_Translation_Error
                    (Exp, "Unexpected element (An_Attribute_Reference).");
            end case;

         when others =>
            null;
      end case;

      Raise_Translation_Error
        (Exp, "Unexpected element (not a subtype mark).");

   end Translate_Subtype_Mark;

--    procedure Translate_Discriminant_Part
--      (Element : in Asis.Declaration;
--       State   : in out Translator_State) is
--       Control : Traverse_Control := Continue;
--       Current_Pass : constant Translation_Pass
--         := State.Pass;
--    begin
--       Translate_Tree (Discriminant_Part (Element), Control, State);
--    end Translate_Discriminant_Part;

--    procedure Translate_Type_Definition
--      (Def     : in Asis.Definition;
--       State   : in out Translator_State) is
--       Control : Traverse_Control := Continue;
--       Current_Pass : constant Translation_Pass
--         := State.Pass;
--    begin
--       State.Pass := CIAO.Translator.State.Type_Definition;
--       Translate_Tree (Def, Control, State);
--       State.Pass := Current_Pass;
--    end Translate_Type_Definition;

   procedure Translate_List
     (List  : in     Asis.Element_List;
      State : in out Translator_State)
   is
      Control : Traverse_Control := Continue;
   begin
      for I in List'Range loop
         Translate_Tree (List (I), Control, State);
         exit when Control = Abandon_Siblings;
      end loop;
   end Translate_List;

   procedure Translate_Formal_Parameter
     (Specification    : in Asis.Definition;
      Is_Implicit_Self : in Boolean;
      State            : in out Translator_State) is
      Control : Traverse_Control := Continue;
      Current_Pass : constant Translation_Pass
        := State.Pass;
   begin
      if Is_Implicit_Self then
         State.Pass := CIAO.Translator.State.Self_Formal_Parameter;
      else
         State.Pass := CIAO.Translator.State.Normal_Formal_Parameter;
      end if;
      Translate_Tree (Specification, Control, State);
      State.Pass := Current_Pass;
   end Translate_Formal_Parameter;

   -----------------------------------------------------------
   -- Post_Translate_Element                                --
   -- Restore Current_Node after a node has been            --
   -- entirely constructed.                                 --
   -- Used as post-operation for Iterator.Traverse_Element. --
   -----------------------------------------------------------

   procedure Post_Translate_Element
     (Element : in Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Translator_State) is
      Previous_Current_Node : constant Node_Id
        := Get_Previous_Current_Node (Element);
   begin
      if Previous_Current_Node /= No_Node then
         State.Current_Node := Previous_Current_Node;
      end if;
   end Post_Translate_Element;

   procedure Translate_Context_Clause
     (Library_Unit : Asis.Compilation_Unit;
      State        : in out Translator_State);
   --  Translate the context clause of a library unit
   --  into a set of subtree inclusions.

   procedure Translate_Context_Clause
     (Library_Unit : Asis.Compilation_Unit;
      State        : in out Translator_State) is
      Context_Clause_Items : constant Context_Clause_List
        := Context_Clause_Elements (Library_Unit);

      Defining_Names : constant Asis.Name_List
        := Names (Unit_Declaration (Library_Unit));
      Name  : constant Asis.Name := Defining_Names (Defining_Names'First);
   begin
      --  Include_Node := New_Include_Directive;
      --  Set_Parent (Include_Node, State.Current_Node);
      --  Add_Directive (State.Current_Node, Include_Node);
      --  Set_Name (Include_Node, New_Name ("ciao.idl"));
      --  Set_Unit_Used (Include_Node, True);

      for I in Context_Clause_Items'Range loop
         declare
            Clause : constant Context_Clause
              := Context_Clause_Items (I);
         begin
            case Clause_Kind (Clause) is
               when A_With_Clause =>
                  declare
                     Units : constant Name_List
                       := Asis.Clauses.Clause_Names (Clause);
                  begin
                     for J in Units'Range loop
                        declare
                           Unit_Declaration : constant Asis.Declaration
                             := Corresponding_Entity_Name_Declaration
                             (Units (J));
                           Unit_Translation : constant Node_Id
                             := Translate
                             (Enclosing_Compilation_Unit (Unit_Declaration));
                           Include_Node : constant Node_Id
                             := Make_Ben_Idl_File (No_Location);
                           Success : Boolean;
                        begin
                           Append_Node_To_Contents
                             (State.Current_Node, Include_Node);
                           Success := Add_Identifier
                             (Include_Node, IDL_Module_Name
                              (Enclosing_Compilation_Unit
                               (Unit_Declaration)));
                           pragma Assert (Success);
                           Set_Contents
                             (Include_Node, Contents (Unit_Translation));
                           Set_Translation (Unit_Declaration, Include_Node);
                           --  The translation of the declaration of a
                           --  withed unit is a Ben_Idl_File subtree.
                        end;
                     end loop;
                  end;

               when others =>
                  null;
            end case;
         end;
      end loop;

      --  If this is a child unit of a library unit, then its
      --  visible part has visibility on the visible part of
      --  its parent.
      if Defining_Name_Kind (Name) = A_Defining_Expanded_Name then
         declare
            Unit_Declaration : constant Asis.Declaration
              := Corresponding_Entity_Name_Declaration
              (Defining_Prefix (Name));
            Unit_Translation : constant Node_Id
              := Translate
              (Enclosing_Compilation_Unit (Unit_Declaration));

            Include_Node : constant Node_Id
              := Make_Ben_Idl_File (No_Location);
            Success : Boolean;
         begin
            Append_Node_To_Contents (State.Current_Node, Include_Node);
            Success := Add_Identifier
              (Include_Node, IDL_Module_Name
               (Enclosing_Compilation_Unit
                (Unit_Declaration)));
            pragma Assert (Success);

            Set_Contents
              (Include_Node, Contents (Unit_Translation));
            Set_Translation (Unit_Declaration, Include_Node);
            --  The translation of the declaration of a withed
            --  unit is a #include preprocessor directive node.
         end;
      end if;

   end Translate_Context_Clause;

   function Translate (LU : in Compilation_Unit) return Node_Id is
      Category : constant Unit_Categories
        := Unit_Category (LU);
   begin
      if Category = Other then
         Raise_Translation_Error
           (Nil_Element,
            "The unit is not a Pure, Remote Types or "
            & "Remote Call Interface package specification.");
      end if;

      declare
         D : constant Declaration
           := Unit_Declaration (LU);
         N : constant Node_Id := Get_Translation (D);
         C : Traverse_Control := Continue;
         S : Translator_State;
      begin
         if N /= No_Node then
            --  Unit already translated.
            return N;
         end if;
         Initialize_Translator_State
           (Category => Category,
            State    => S);
         Push_Scope (S.Current_Node);
         Translate_Context_Clause (LU, S);
         Translate_Tree (D, C, S);
         Pop_Scope;
         return S.IDL_Tree;
      end;
   end Translate;

end CIAO.Translator;

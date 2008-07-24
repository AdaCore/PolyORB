------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                             A N A L Y Z E R                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2005-2008, Free Software Foundation, Inc.          --
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

with GNAT.Table;
with GNAT.Bubble_Sort;

with Errors;    use Errors;
with Lexer;     use Lexer;
with Locations; use Locations;
with Scopes;    use Scopes;
with Utils;     use Utils;
with Values;    use Values;
with Namet;     use Namet;

with Frontend.Debug;  use Frontend.Debug;
with Frontend.Nodes;  use Frontend.Nodes;
with Frontend.Nutils; use Frontend.Nutils;

package body Analyzer is

   procedure Analyze_Attribute_Declaration (E : Node_Id);
   procedure Analyze_Complex_Declarator (E : Node_Id);
   procedure Analyze_Constant_Declaration (E : Node_Id);
   procedure Analyze_Element (E : Node_Id);
   procedure Analyze_Enumeration_Type (E : Node_Id);
   procedure Analyze_Exception_Declaration (E : Node_Id);
   procedure Analyze_Expression (E : Node_Id);
   procedure Analyze_Fixed_Point_Type (E : Node_Id);
   procedure Analyze_Forward_Interface_Declaration (E : Node_Id);
   procedure Analyze_Forward_Structure_Type (E : Node_Id);
   procedure Analyze_Forward_Union_Type (E : Node_Id);
   procedure Analyze_Initializer_Declaration (E : Node_Id);
   procedure Analyze_Interface_Declaration (E : Node_Id);
   procedure Analyze_Literal (E : Node_Id);
   procedure Analyze_Member (E : Node_Id);
   procedure Analyze_Module (E : Node_Id);
   procedure Analyze_Operation_Declaration (E : Node_Id);
   procedure Analyze_Native_Type (E : Node_Id);
   procedure Analyze_Parameter_Declaration (E : Node_Id);
   procedure Analyze_Pragma (E : Node_Id);
   procedure Analyze_Scoped_Name (E : Node_Id);
   procedure Analyze_Simple_Declarator (E : Node_Id);
   procedure Analyze_Sequence_Type (E : Node_Id);
   procedure Analyze_State_Member (E : Node_Id);
   procedure Analyze_String (E : Node_Id);
   procedure Analyze_Structure_Type (E : Node_Id);
   procedure Analyze_Type_Declaration (E : Node_Id);
   procedure Analyze_Type_Id_Declaration (E : Node_Id);
   procedure Analyze_Type_Prefix_Declaration (E : Node_Id);
   procedure Analyze_Union_Type (E : Node_Id);
   procedure Analyze_Value_Declaration (E : Node_Id);
   procedure Analyze_Value_Box_Declaration (E : Node_Id);
   procedure Analyze_Value_Forward_Declaration (E : Node_Id);

   --  These procedures factorize the analyzing type prefix and type ID code

   procedure Assign_Type_Id
     (Scope : Node_Id;
      Prefix : Node_Id;
      Unique : Boolean := False); --  To enable redefinition
   procedure Assign_Type_Prefix (Scope : Node_Id; Prefix : Node_Id);
   procedure Assign_Type_Version (Scope : Node_Id; Prefix : Node_Id);

   procedure Inherit_From (Parent : Node_Id);
   --  Add into the scope of the child interface all the entities from
   --  the scope of the parent interfaces. For each entity of a parent
   --  interface create a new identifier referencing the entity while
   --  the entity is still bound to its initial identifier.

   procedure Resolve_Expr (E : Node_Id; T : Node_Id);
   function  Resolve_Type (N : Node_Id) return Node_Id;

   procedure Display_Incorrect_Value
     (L  : Location;
      K1 : Node_Kind;
      K2 : Node_Kind := K_Void);

   package LT is new GNAT.Table (Node_Id, Natural, 1, 10, 10);
   --  Label table

   procedure Exchange (Op1, Op2 : Natural);
   function  Less_Than (Op1, Op2 : Natural) return Boolean;
   --  Sort the nodes by applying the following rules. A node with a
   --  wrong value is always the least value. A node representing
   --  "default" is always the greatest value. Otherwise, compare as
   --  usual.

   -------------
   -- Analyze --
   -------------

   procedure Analyze (E : Node_Id) is
   begin
      if No (E) then
         return;
      end if;

      if Kind (E) in K_Float .. K_Value_Base then
         return;
      end if;

      case Kind (E) is
         when K_Abstract_Value_Declaration
           | K_Value_Declaration =>
            Analyze_Value_Declaration (E);

         when K_Attribute_Declaration =>
            Analyze_Attribute_Declaration (E);

         when K_Complex_Declarator =>
            Analyze_Complex_Declarator (E);

         when K_Constant_Declaration =>
            Analyze_Constant_Declaration (E);

         when K_Element =>
            Analyze_Element (E);

         when K_Enumeration_Type =>
            Analyze_Enumeration_Type (E);

         when K_Exception_Declaration =>
            Analyze_Exception_Declaration (E);

         when K_Expression =>
            Analyze_Expression (E);

         when K_Fixed_Point_Type =>
            Analyze_Fixed_Point_Type (E);

         when K_Forward_Interface_Declaration =>
            Analyze_Forward_Interface_Declaration (E);

         when K_Forward_Structure_Type =>
            Analyze_Forward_Structure_Type (E);

         when K_Forward_Union_Type =>
            Analyze_Forward_Union_Type (E);

         when K_Initializer_Declaration =>
            Analyze_Initializer_Declaration (E);

         when K_Interface_Declaration =>
            Analyze_Interface_Declaration (E);

         when K_Literal =>
            Analyze_Literal (E);

         when K_Member =>
            Analyze_Member (E);

         when K_Module =>
            Analyze_Module (E);

         when K_Operation_Declaration =>
            Analyze_Operation_Declaration (E);

         when K_Native_Type =>
            Analyze_Native_Type (E);

         when K_Parameter_Declaration =>
            Analyze_Parameter_Declaration (E);

         when K_Pragma =>
            Analyze_Pragma (E);

         when K_Scoped_Name =>
            Analyze_Scoped_Name (E);

         when K_Sequence_Type =>
            Analyze_Sequence_Type (E);

         when K_Simple_Declarator =>
            Analyze_Simple_Declarator (E);

         when K_Specification =>
            Analyze_Module (E);

         when K_State_Member =>
            Analyze_State_Member (E);

         when K_String_Type | K_Wide_String_Type =>
            Analyze_String (E);

         when K_Structure_Type =>
            Analyze_Structure_Type (E);

         when K_Type_Declaration =>
            Analyze_Type_Declaration (E);

         when K_Type_Id_Declaration =>
            Analyze_Type_Id_Declaration (E);

         when K_Type_Prefix_Declaration =>
            Analyze_Type_Prefix_Declaration (E);

         when K_Union_Type =>
            Analyze_Union_Type (E);

         when K_Value_Box_Declaration =>
            Analyze_Value_Box_Declaration (E);

         when K_Value_Forward_Declaration =>
            Analyze_Value_Forward_Declaration (E);

         when K_Float .. K_Value_Base =>
            null;

         when others =>
            Dummy (E);
      end case;
   end Analyze;

   -----------------------------------
   -- Analyze_Attribute_Declaration --
   -----------------------------------

   procedure Analyze_Attribute_Declaration (E : Node_Id) is

      procedure No_Interface_Attribute_Of_Local_Type
        (T : Node_Id; I : Node_Id);

      ------------------------------------------
      -- No_Interface_Attribute_Of_Local_Type --
      ------------------------------------------

      procedure No_Interface_Attribute_Of_Local_Type
        (T : Node_Id; I : Node_Id)
      is
         PT : Node_Id := T;
         TK : Node_Kind;

      begin
         if Present (PT) and then Kind (PT) = K_Scoped_Name then
            PT := Reference (PT);
         end if;
         if No (PT) then
            return;
         end if;
         TK := Kind (PT);
         if (TK = K_Forward_Interface_Declaration
             or else TK = K_Forward_Interface_Declaration)
           and then Is_A_Local_Type (PT)
         then
            Error_Loc (1)  := Loc (T);
            Error_Name (1) := IDL_Name (Identifier (T));
            Error_Name (1) := IDL_Name (Identifier (I));
            DE ("local interface#cannot appear as attribute " &
                "in unconstrained interface#");
         end if;
      end No_Interface_Attribute_Of_Local_Type;

      Declarator     : Node_Id := First_Entity (Declarators (E));
      Decl_Type      : constant Node_Id := Type_Spec (E);
      Iface          : constant Node_Id := Current_Scope;
      Attr_Exception : Node_Id;

   begin
      Analyze (Decl_Type);
      if not Is_A_Local_Type (Iface) then
         No_Interface_Attribute_Of_Local_Type (Decl_Type, Iface);
      end if;

      while Present (Declarator) loop
         Analyze (Declarator);
         Declarator := Next_Entity (Declarator);
      end loop;

      --  Analyze exceptions

      if not Is_Empty (Getter_Exceptions (E)) then
         Attr_Exception := First_Entity (Getter_Exceptions (E));
         while Present (Attr_Exception) loop
            Analyze (Attr_Exception);

            Attr_Exception := Next_Entity (Attr_Exception);
         end loop;
      end if;

      if not Is_Empty (Setter_Exceptions (E)) then
         Attr_Exception := First_Entity (Setter_Exceptions (E));
         while Present (Attr_Exception) loop
            Analyze (Attr_Exception);

            Attr_Exception := Next_Entity (Attr_Exception);
         end loop;
      end if;
   end Analyze_Attribute_Declaration;

   --------------------------------
   -- Analyze_Complex_Declarator --
   --------------------------------

   procedure Analyze_Complex_Declarator (E : Node_Id)
   is
      C : Node_Id;
   begin
      Enter_Name_In_Scope (Identifier (E));

      --  The array sizes attribute is never empty

      C := First_Entity (Array_Sizes (E));
      while Present (C) loop
         Analyze (C);
         C := Next_Entity (C);
      end loop;
   end Analyze_Complex_Declarator;

   ----------------------------------
   -- Analyze_Constant_Declaration --
   ----------------------------------

   procedure Analyze_Constant_Declaration (E : Node_Id)
   is
      T : Node_Id;
      K : Node_Kind;

   begin
      T := Type_Spec (E);
      if No (T) then
         return;
      end if;

      Analyze (T);

      --  Resolve base type of T. Types of constant declarations are
      --  limited to integer types, character types, string types,
      --  floating point types, fixed point types.

      T := Resolve_Type (T);
      if No (T) then
         return;
      end if;

      K := Kind (T);
      case K is
         when
            K_Fixed_Point_Type |
            K_String_Type      |
            K_Wide_String_Type |
            K_Enumeration_Type |
            K_Float .. K_Octet =>

            null;

         when others =>
            Error_Loc (1) := Loc (Type_Spec (E));
            DE ("invalid type for constant");
            return;
      end case;

      --  Analyze expression, evaluate it and then convert result

      Enter_Name_In_Scope (Identifier (E));
      Analyze (Expression (E));
      Resolve_Expr (E, T);
   end Analyze_Constant_Declaration;

   ---------------------
   -- Analyze_Element --
   ---------------------

   procedure Analyze_Element (E : Node_Id) is
   begin
      Analyze (Type_Spec (E));
      Analyze (Declarator (E));
   end Analyze_Element;

   ------------------------------
   -- Analyze_Enumeration_Type --
   ------------------------------

   procedure Analyze_Enumeration_Type (E : Node_Id)
   is
      C : Node_Id;
      N : Node_Id;
      I : Node_Id;
   begin
      Enter_Name_In_Scope (Identifier (E));

      C := First_Entity (Enumerators (E));
      while Present (C) loop

         --  Define scoped name referencing enumeration type

         I := Make_Identifier
           (Loc (E), IDL_Name (Identifier (E)), No_Node, No_Node);
         N := Make_Scoped_Name
           (Loc (E), I, No_Node, E);
         Bind_Identifier_To_Entity (I, N);

         --  Define constant aliasing enumerator

         I := Make_Identifier
           (Loc (C), IDL_Name (Identifier (C)), No_Node, No_Node);
         N := Make_Constant_Declaration
           (Loc (E), N, I, C);
         Bind_Identifier_To_Entity (I, N);

         Analyze (N);
         C := Next_Entity (C);
      end loop;
   end Analyze_Enumeration_Type;

   -----------------------------------
   -- Analyze_Exception_Declaration --
   -----------------------------------

   procedure Analyze_Exception_Declaration (E : Node_Id)
   is
      C : Node_Id;
      L : List_Id;
   begin
      Enter_Name_In_Scope (Identifier (E));
      L := Members (E);
      if not Is_Empty (L) then
         Push_Scope (E);
         C := First_Entity (L);
         while Present (C) loop
            Analyze (C);
            C := Next_Entity (C);
         end loop;
         Pop_Scope;
      end if;
   end Analyze_Exception_Declaration;

   ------------------------
   -- Analyze_Expression --
   ------------------------

   procedure Analyze_Expression (E : Node_Id)
   is
      C : Node_Id;
   begin
      C := Left_Expr (E);
      if Present (C) then
         Analyze (C);
      end if;
      C := Right_Expr (E);
      if Present (C) then
         Analyze (C);
      end if;
   end Analyze_Expression;

   ------------------------------
   -- Analyze_Fixed_Point_Type --
   ------------------------------

   procedure Analyze_Fixed_Point_Type (E : Node_Id) is
   begin
      Dummy (E);
   end Analyze_Fixed_Point_Type;

   -------------------------------------------
   -- Analyze_Forward_Interface_Declaration --
   -------------------------------------------

   procedure Analyze_Forward_Interface_Declaration (E : Node_Id) is
   begin
      Enter_Name_In_Scope (Identifier (E));
   end Analyze_Forward_Interface_Declaration;

   ------------------------------------
   -- Analyze_Forward_Structure_Type --
   ------------------------------------

   procedure Analyze_Forward_Structure_Type (E : Node_Id) is
   begin
      Enter_Name_In_Scope (Identifier (E));
   end Analyze_Forward_Structure_Type;

   --------------------------------
   -- Analyze_Forward_Union_Type --
   --------------------------------

   procedure Analyze_Forward_Union_Type (E : Node_Id) is
   begin
      Enter_Name_In_Scope (Identifier (E));
   end Analyze_Forward_Union_Type;

   -------------------------------------
   -- Analyze_Initializer_Declaration --
   -------------------------------------

   procedure Analyze_Initializer_Declaration (E : Node_Id) is
   begin
      Analyze_Operation_Declaration (E);
   end Analyze_Initializer_Declaration;

   -----------------------------------
   -- Analyze_Interface_Declaration --
   -----------------------------------

   procedure Analyze_Interface_Declaration (E : Node_Id) is

      Parent      : Node_Id;
      Definition  : Node_Id;
      Scoped_Name : Node_Id;
      Is_Local    : constant Boolean := Is_A_Local_Type (E);
      Is_Abstract : constant Boolean := Is_Abstract_Interface (E);

   begin
      Enter_Name_In_Scope (Identifier (E));

      --  Analyze interface names in the current scope (before pushing
      --  a new scope and inheriting from other interfaces).

      Scoped_Name := First_Entity (Interface_Spec (E));
      while Present (Scoped_Name) loop
         Analyze (Scoped_Name);
         Parent := Reference (Scoped_Name);
         if Present (Parent) then
            if Kind (Parent) /= K_Interface_Declaration then
               if Kind (Parent) = K_Forward_Interface_Declaration then
                  Error_Loc (1) := Loc (E);
                  DE ("interface cannot inherit " &
                      "from a forward-declared interface");

               else
                  Error_Loc (1) := Loc (E);
                  DE ("interface cannot inherit " &
                      "from a non-interface");
               end if;

               --  Do not consider this interface later on.

               Set_Reference (Scoped_Name, No_Node);

            elsif not Is_Local then
               if Is_A_Local_Type (Parent) then
                  Error_Loc (1) := Loc (E);
                  DE ("interface cannot inherit " &
                      "from a local interface");
                  Set_Reference (Scoped_Name, No_Node);
               end if;

            elsif Is_Abstract then
               if not Is_Abstract_Interface (Parent) then
                  Error_Loc (1) := Loc (E);
                  DE ("abstract interface cannot inherit " &
                      "from a non-abstract interface");
                  Set_Reference (Scoped_Name, No_Node);
               end if;
            end if;
         end if;
         Scoped_Name := Next_Entity (Scoped_Name);
      end loop;

      --  Push a new scope and then inherit from the parent interfaces.

      Push_Scope (E);
      Scoped_Name := First_Entity (Interface_Spec (E));
      while Present (Scoped_Name) loop
         Parent := Reference (Scoped_Name);
         if Present (Parent) then
            Inherit_From (Parent);
         end if;
         Scoped_Name := Next_Entity (Scoped_Name);
      end loop;

      --  Append and analyze the interface entities

      Definition := First_Entity (Interface_Body (E));
      while Present (Definition) loop
         Analyze (Definition);
         Definition := Next_Entity (Definition);
      end loop;
      Pop_Scope;
   end Analyze_Interface_Declaration;

   ---------------------
   -- Analyze_Literal --
   ---------------------

   procedure Analyze_Literal (E : Node_Id) is
   begin
      Dummy (E);
   end Analyze_Literal;

   --------------------
   -- Analyze_Member --
   --------------------

   procedure Analyze_Member (E : Node_Id) is
      D : Node_Id := First_Entity (Declarators (E));

   begin
      Analyze (Type_Spec (E));
      while Present (D) loop
         Analyze (D);
         D := Next_Entity (D);
      end loop;
   end Analyze_Member;

   --------------------
   -- Analyze_Module --
   --------------------

   procedure Analyze_Module (E : Node_Id)
   is
      C : Node_Id;
      L : List_Id;
   begin
      if Kind (E) = K_Module then
         Enter_Name_In_Scope (Identifier (E));
      end if;

      L := Definitions (E);
      if not Is_Empty (L) then
         Push_Scope (E);
         C := First_Entity (L);
         while Present (C) loop
            Analyze (C);
            C := Next_Entity (C);
         end loop;
         Pop_Scope;
      end if;
   end Analyze_Module;

   -------------------------
   -- Analyze_Native_Type --
   -------------------------

   procedure Analyze_Native_Type (E : Node_Id) is
   begin
      Analyze (Declarator (E));
   end Analyze_Native_Type;

   -----------------------------------
   -- Analyze_Operation_Declaration --
   -----------------------------------

   procedure Analyze_Operation_Declaration (E : Node_Id) is

      procedure No_Operation_Parameter_Of_Local_Type
        (T : Node_Id; I : Node_Id);
      procedure No_Exception_Member_Of_Local_Type
        (X : Node_Id; I : Node_Id);

      ---------------------------------------
      -- No_Exception_Member_Of_Local_Type --
      ---------------------------------------

      procedure No_Exception_Member_Of_Local_Type
        (X : Node_Id; I : Node_Id)
      is
         EX : Node_Id := X;
         EM : Node_Id;
         MT : Node_Id;
         TK : Node_Kind;

      begin
         if Present (EX) and then Kind (EX) = K_Scoped_Name then
            EX := Reference (EX);
         end if;
         if No (EX) then
            return;
         end if;

         EM := First_Entity (Members (EX));
         while Present (EM) loop
            MT := Type_Spec (EM);
            if Present (MT) and then Kind (MT) = K_Scoped_Name then
               MT := Reference (MT);
            end if;
            if Present (MT) then
               TK := Kind (MT);
               if (TK = K_Forward_Interface_Declaration
                   or else TK = K_Forward_Interface_Declaration)
                 and then Is_A_Local_Type (MT)
               then
                  Error_Loc (1)  := Loc (EM);
                  Error_Name (1) := IDL_Name (Identifier (MT));
                  Error_Name (1) := IDL_Name (Identifier (I));
                  DE ("local interface#cannot appear " &
                      "as an exception declaration " &
                      "in unconstrained interface#");
               end if;
            end if;
            EM := Next_Entity (EM);
         end loop;
      end No_Exception_Member_Of_Local_Type;

      ------------------------------------------
      -- No_Operation_Parameter_Of_Local_Type --
      ------------------------------------------

      procedure No_Operation_Parameter_Of_Local_Type
        (T : Node_Id; I : Node_Id)
      is
         PT : Node_Id := T;
         TK : Node_Kind;

      begin
         if Present (PT) and then Kind (PT) = K_Scoped_Name then
            PT := Reference (PT);
         end if;
         if No (PT) then
            return;
         end if;
         TK := Kind (PT);
         if (TK = K_Forward_Interface_Declaration
             or else TK = K_Forward_Interface_Declaration)
           and then Is_A_Local_Type (PT)
         then
            Error_Loc (1)  := Loc (T);
            Error_Name (1) := IDL_Name (Identifier (T));
            Error_Name (1) := IDL_Name (Identifier (I));
            DE ("local interface#cannot appear as parameter " &
                "in unconstrained interface#");
         end if;
      end No_Operation_Parameter_Of_Local_Type;

      Iface         : constant Node_Id := Current_Scope;
      Is_Local      : constant Boolean := Is_A_Local_Type (Iface);
      Oneway        : Boolean := Is_Oneway (E);

      Return_Type_Id : Node_Id;
      Return_Type    : Node_Id;

      Param_Type     : Node_Id;
      Op_Parameter   : Node_Id;
      Op_Exception   : Node_Id;
      Op_Context     : Node_Id;

   begin
      Enter_Name_In_Scope (Identifier (E));

      if Kind (E) /= K_Initializer_Declaration then
         Return_Type_Id := Type_Spec (E);
         Analyze (Return_Type_Id);

         Return_Type := Return_Type_Id;
         if Kind (Return_Type) = K_Scoped_Name then
            Return_Type := Reference (Return_Type);
         end if;

         --  When operation is oneway, check return type is void

         if Oneway and then Kind (Return_Type) /= K_Void then
            Oneway := False;
            Error_Loc (1) := Loc (Return_Type);
            DE ("oneway operation cannot return a non-void result");
         end if;

         --  When the current interface is not local, check that its
         --  operations do not use local types.

         if not Is_Local then
            No_Operation_Parameter_Of_Local_Type (Return_Type, Iface);
         end if;
      end if;

      --  Analyze parameters

      if not Is_Empty (Parameters (E)) then
         Push_Scope (E);
         Op_Parameter := First_Entity (Parameters (E));
         while Present (Op_Parameter) loop
            Analyze (Op_Parameter);

            --  When operation is oneway, check parameter mode is "in"

            if Oneway and then Parameter_Mode (Op_Parameter) /= Mode_In then
               Oneway := False;
               Error_Loc (1) := Loc (Op_Parameter);
               DE ("oneway operation can only have ""in"" parameters");
            end if;

            --  When the current interface is not local, check
            --  operation parameter are not local types.

            Param_Type := Type_Spec (Op_Parameter);
            if not Is_Local then
               No_Operation_Parameter_Of_Local_Type (Param_Type, Iface);
            end if;

            Op_Parameter := Next_Entity (Op_Parameter);
         end loop;
         Pop_Scope;
      end if;

      --  Analyze exceptions

      if not Is_Empty (Exceptions (E)) then
         Op_Exception := First_Entity (Exceptions (E));
         while Present (Op_Exception) loop
            Analyze (Op_Exception);

            --  When operation is oneway, no exception is allowed

            if Oneway then
               Oneway := False;
               Error_Loc (1) := Loc (Op_Exception);
               DE ("oneway operation may not have raises expression");
            end if;

            --  When the current interface is not local, check
            --  an exception member is not of local type.

            if not Is_Local then
               No_Exception_Member_Of_Local_Type (Op_Exception, Iface);
            end if;

            Op_Exception := Next_Entity (Op_Exception);
         end loop;
      end if;

      --  Analyze contexts

      if not Is_Empty (Contexts (E)) then
         Op_Context := First_Entity (Contexts (E));
         while Present (Op_Context) loop
            Analyze (Op_Context);
            Op_Context := Next_Entity (Op_Context);
         end loop;
      end if;
   end Analyze_Operation_Declaration;

   -----------------------------------
   -- Analyze_Parameter_Declaration --
   -----------------------------------

   procedure Analyze_Parameter_Declaration (E : Node_Id) is
   begin
      Analyze (Type_Spec (E));
      Analyze (Declarator (E));
   end Analyze_Parameter_Declaration;

   --------------------
   -- Analyze_Pragma --
   --------------------

   procedure Analyze_Pragma (E : Node_Id) is
      R : Node_Id;
      N : Node_Id;
   begin
      if Pragma_Kind (E) /= Pragma_Unrecognized then
         N := Make_Identifier
           (Loc (E),
            Data (E),
            No_Node,
            No_Node);
      end if;

      case Pragma_Kind (E) is
         when Pragma_Id =>
            Analyze (Target (E));
            R := Reference (Target (E));
            Assign_Type_Id (R, N);

         when Pragma_Prefix =>
            Assign_Type_Prefix (Current_Scope, N);

         when Pragma_Version =>
            Analyze (Target (E));
            R := Reference (Target (E));
            Assign_Type_Version (R, N);

         when Pragma_Unrecognized =>
            Error_Loc (1) := Loc (E);
            DE ("?unknown pragma");
            --  ??? error message should include pragma name
      end case;
   end Analyze_Pragma;

   -------------------------
   -- Analyze_Scoped_Name --
   -------------------------

   procedure Analyze_Scoped_Name (E : Node_Id) is
      P : Node_Id := Parent_Entity (E);
      N : Node_Id := Identifier (E);
      C : Node_Id;

   begin
      --  This scoped name has already been analyzed.

      if Present (Reference (E)) then
         return;
      end if;

      --  Analyze single scoped name. First we have to find a possible
      --  visible entity. If there is one, associate the reference to
      --  the designated entity and check whether the casing is
      --  correct. Enter the name in the scope.

      if No (P) then
         if Name (N) = No_Name then
            Set_Reference (E, IDL_Spec);

         else
            C := Visible_Node (N);
            if Present (C) then
               Set_Reference (E, C);
               Enter_Name_In_Scope (N);
               Check_Identifier (N, Identifier (C));
            end if;
         end if;

      --  Analyze multiple scoped names. Analyze parent of P first and then the
      --  entity itself. Find the entity in the newly-analyzed parent
      --  scope. Check whether the scope is a correct scope for a scoped name
      --  (not an operation for instance).

      else
         Analyze_Scoped_Name (P);
         P := Reference (P);
         if Present (P) then
            if Is_A_Scope (P) then
               C := Node_Explicitly_In_Scope (N, P);
               if No (C) then
                  Error_Loc (1)  := Loc (N);
                  Error_Name (1) := IDL_Name (N);
                  Error_Name (2) := IDL_Name (Identifier (P));
                  DE ("#not declared in#");
                  return;
               end if;
               Set_Reference (E, C);
               Check_Identifier (N, Identifier (C));

               --  If this scoped name is the full scoped name (and
               --  not a part of the scoped name), if this designates
               --  a type name and if the scope is a non-module
               --  entity, then enter the name in the scope.

               if Depth (E) = 0
                 and then Is_A_Type (C)
                 and then Is_A_Non_Module (Current_Scope)
               then
                  Enter_Name_In_Scope (N);
               end if;

            else
               N := Identifier (P);
               Error_Loc  (1) := Loc (N);
               Error_Name (1) := IDL_Name (N);
               DE ("#does not form a scope");
            end if;
         end if;
      end if;
   end Analyze_Scoped_Name;

   ---------------------------
   -- Analyze_Sequence_Type --
   ---------------------------

   procedure Analyze_Sequence_Type (E : Node_Id) is
   begin
      Analyze (Type_Spec (E));
   end Analyze_Sequence_Type;

   -------------------------------
   -- Analyze_Simple_Declarator --
   -------------------------------

   procedure Analyze_Simple_Declarator (E : Node_Id) is
   begin
      Enter_Name_In_Scope (Identifier (E));
   end Analyze_Simple_Declarator;

   --------------------------
   -- Analyze_State_Member --
   --------------------------

   procedure Analyze_State_Member (E : Node_Id) is
   begin
      Analyze_Member (E);
   end Analyze_State_Member;

   --------------------
   -- Analyze_String --
   --------------------

   procedure Analyze_String (E : Node_Id) is
   begin
      Dummy (E);
   end Analyze_String;

   ----------------------------
   -- Analyze_Structure_Type --
   ----------------------------

   procedure Analyze_Structure_Type (E : Node_Id)
   is
      L : List_Id;
      C : Node_Id;
   begin
      Enter_Name_In_Scope (Identifier (E));
      L := Members (E);
      if not Is_Empty (L) then
         Push_Scope (E);
         C := First_Entity (L);
         while Present (C) loop
            Analyze (C);
            C := Next_Entity (C);
         end loop;
         Pop_Scope;
      end if;
   end Analyze_Structure_Type;

   ------------------------------
   -- Analyze_Type_Declaration --
   ------------------------------

   procedure Analyze_Type_Declaration (E : Node_Id)
   is
      D : Node_Id := First_Entity (Declarators (E));
   begin
      Analyze (Type_Spec (E));
      while Present (D) loop
         Analyze (D);
         D := Next_Entity (D);
      end loop;
   end Analyze_Type_Declaration;

   ---------------------------------
   -- Analyze_Type_Id_Declaration --
   ---------------------------------

   procedure Analyze_Type_Id_Declaration (E : Node_Id) is
      R : Node_Id;
      N : Node_Id;
   begin
      Analyze (Target (E));
      R := Reference (Target (E));

      N := Make_Identifier
        (Loc (Target (E)),
         Data (E),
         No_Node,
         No_Node);

      Assign_Type_Id (R, N, True);
   end Analyze_Type_Id_Declaration;

   -------------------------------------
   -- Analyze_Type_Prefix_Declaration --
   -------------------------------------

   procedure Analyze_Type_Prefix_Declaration (E : Node_Id)is
      R : Node_Id;
      N : Node_Id;
   begin
      Analyze (Target (E));
      R := Reference (Target (E));

      N := Make_Identifier
        (Loc (Target (E)),
         Data (E),
         No_Node,
         No_Node);

      Assign_Type_Prefix (R, N);
   end Analyze_Type_Prefix_Declaration;

   ------------------------
   -- Analyze_Union_Type --
   ------------------------

   procedure Analyze_Union_Type (E : Node_Id) is
      Alternative : Node_Id;
      Label       : Node_Id;
      Switch_Type : Node_Id := Switch_Type_Spec (E);
      L           : Natural;
   begin
      Enter_Name_In_Scope (Identifier (E));

      Push_Scope (E);
      Analyze (Switch_Type);

      --  Check that switch type is a discrete type

      Switch_Type := Resolve_Type (Switch_Type);
      case Kind (Switch_Type) is
         when K_Short .. K_Wide_Char
           | K_Boolean
           | K_Octet
           | K_Enumeration_Type =>
            null;

         when others =>
            Error_Loc (1) := Loc (Switch_Type);
            DE ("switch must have a discrete type");
            return;
      end case;

      --  Resolve labels and elements

      Alternative := First_Entity (Switch_Type_Body (E));
      while Present (Alternative) loop
         Label := First_Entity (Labels (Alternative));
         while Present (Label) loop
            Analyze (Expression (Label));
            Resolve_Expr (Label, Switch_Type);
            Label := Next_Entity (Label);
         end loop;
         Analyze (Element (Alternative));
         Alternative := Next_Entity (Alternative);
      end loop;

      --  Check there is no duplicated choice

      LT.Init;
      Alternative := First_Entity (Switch_Type_Body (E));
      while Present (Alternative) loop
         Label := First_Entity (Labels (Alternative));
         while Present (Label) loop
            LT.Append (Label);
            Label := Next_Entity (Label);
         end loop;
         Alternative := Next_Entity (Alternative);
      end loop;

      GNAT.Bubble_Sort.Sort (LT.Last, Exchange'Access, Less_Than'Access);
      for I in 1 .. LT.Last - 1 loop

         --  If this comparison is false once sorted, it means that
         --  the two nodes are equal. Take care of duplicated default
         --  case. Having two incorrect nodes equal is not a problem.

         if (No (Expression (LT.Table (I)))
             and then No (Expression (LT.Table (I + 1))))
           or else
            (Value (LT.Table (I)) /= No_Value
             and then not Less_Than (I, I + 1))
         then

            --  Reorder nodes in order to output the error message on
            --  the second node in the file.

            if Loc (LT.Table (I + 1)) < Loc (LT.Table (I)) then
               Error_Loc (1) := Loc (LT.Table (I));
               Error_Loc (2) := Loc (LT.Table (I + 1));

            else
               Error_Loc (1) := Loc (LT.Table (I + 1));
               Error_Loc (2) := Loc (LT.Table (I));
            end if;
            DE ("duplication of choice value at line!");
            exit;
         end if;
      end loop;
      Pop_Scope;

      --  Check for useless choices (explicit choices in alternative that
      --  includes the default label).

      Alternative := First_Entity (Switch_Type_Body (E));
      while Present (Alternative) loop
         Label := First_Entity (Labels (Alternative));
         L := Length (Labels (Alternative));
         if L > 1 then
            while Present (Label) loop
               if Value (Label) = No_Value then
                  --  Display a warning

                  Error_Loc (1) := Loc (Alternative);
                  DE ("Some labels are useless since one"
                      & " of them is the default clause?");
               end if;
               Label := Next_Entity (Label);
            end loop;
         end if;
         Alternative := Next_Entity (Alternative);
      end loop;
   end Analyze_Union_Type;

   -----------------------------------
   -- Analyze_Value_Box_Declaration --
   -----------------------------------

   procedure Analyze_Value_Box_Declaration (E : Node_Id) is
   begin
      Enter_Name_In_Scope (Identifier (E));
      Analyze (Type_Spec (E));
   end Analyze_Value_Box_Declaration;

   -------------------------------
   -- Analyze_Value_Declaration --
   -------------------------------

   procedure Analyze_Value_Declaration (E : Node_Id) is
      Scoped_Name  : Node_Id;
      Parent       : Node_Id;
      Definition   : Node_Id;
      Scoped_Names : List_Id;
      Parent_Kind  : Node_Kind;
      Is_Abstract  : constant Boolean :=
                       Kind (E) = K_Abstract_Value_Declaration;

   begin
      Enter_Name_In_Scope (Identifier (E));

      --  Analyze value type names in the current scope (before pushing a new
      --  scope and inheriting from other value types).

      Scoped_Names := Value_Names (Value_Spec (E));
      if not Is_Empty (Scoped_Names) then
         Scoped_Name := First_Entity (Scoped_Names);
         while Present (Scoped_Name) loop
            Analyze (Scoped_Name);
            Parent := Reference (Scoped_Name);
            if Present (Parent) then
               Parent_Kind := Kind (Parent);
               if Parent_Kind /= K_Value_Declaration
                 and then Parent_Kind /= K_Abstract_Value_Declaration
               then
                  if Parent_Kind = K_Value_Forward_Declaration then
                     Error_Loc (1) := Loc (E);
                     DE ("value type cannot inherit " &
                         "from a forward-declared value type");

                  else
                     Error_Loc (1) := Loc (E);
                     DE ("value type cannot inherit " &
                         "from a non-value type");
                  end if;

                  --  Do not consider this value type later on

                  Set_Reference (Scoped_Name, No_Node);

               elsif Is_Abstract
                 and then Parent_Kind /= K_Abstract_Value_Declaration
               then
                  Error_Loc (1) := Loc (E);
                  DE ("abstract value type cannot inherit " &
                      "from a non-abstract value type");
                  Set_Reference (Scoped_Name, No_Node);
               end if;
            end if;
            Scoped_Name := Next_Entity (Scoped_Name);
         end loop;
      end if;

      --  Analyze interface names in the current scope (before pushing a
      --  new scope).

      Scoped_Names := Interface_Names (Value_Spec (E));
      if not Is_Empty (Scoped_Names) then
         Scoped_Name := First_Entity (Scoped_Names);
         while Present (Scoped_Name) loop
            Analyze (Scoped_Name);
            Parent := Reference (Scoped_Name);
            if Present (Parent) then
               if Kind (Parent) /= K_Interface_Declaration then
                  if Kind (Parent) = K_Forward_Interface_Declaration then
                     Error_Loc (1) := Loc (E);
                     DE ("interface cannot inherit " &
                         "from a forward-declared interface");

                  else
                     Error_Loc (1) := Loc (E);
                     DE ("interface cannot inherit " &
                         "from a non-interface");
                  end if;

                  --  Do not consider this interface later on

                  Set_Reference (Scoped_Name, No_Node);
               end if;
            end if;
            Scoped_Name := Next_Entity (Scoped_Name);
         end loop;
      end if;

      --  Push a new scope, then inherit from the parent value types

      Push_Scope (E);
      Scoped_Names := Value_Names (Value_Spec (E));
      if not Is_Empty (Scoped_Names) then
         Scoped_Name := First_Entity (Scoped_Names);
         while Present (Scoped_Name) loop
            Parent := Reference (Scoped_Name);
            if Present (Parent) then
               Inherit_From (Parent);
            end if;
            Scoped_Name := Next_Entity (Scoped_Name);
         end loop;
      end if;

      --  Inherit from the parent interfaces

      Scoped_Names := Interface_Names (Value_Spec (E));
      if not Is_Empty (Scoped_Names) then
         Scoped_Name := First_Entity (Scoped_Names);
         while Present (Scoped_Name) loop
            Parent := Reference (Scoped_Name);
            if Present (Parent) then
               Inherit_From (Parent);
            end if;
            Scoped_Name := Next_Entity (Scoped_Name);
         end loop;
      end if;

      --  Append and analyze the value entities

      Definition := First_Entity (Value_Body (E));
      while Present (Definition) loop
         Analyze (Definition);
         Definition := Next_Entity (Definition);
      end loop;
      Pop_Scope;
   end Analyze_Value_Declaration;

   ---------------------------------------
   -- Analyze_Value_Forward_Declaration --
   ---------------------------------------

   procedure Analyze_Value_Forward_Declaration (E : Node_Id) is
   begin
      Enter_Name_In_Scope (Identifier (E));
   end Analyze_Value_Forward_Declaration;

   --------------------
   -- Assign_Type_Id --
   --------------------

   procedure Assign_Type_Id
     (Scope  : Node_Id;
      Prefix : Node_Id;
      Unique : Boolean := False) is
   begin

      case Kind (Scope) is
         when
           K_Module                        |
           K_Interface_Declaration         |
           K_Forward_Interface_Declaration |
           K_Value_Declaration             |
           K_Value_Forward_Declaration     |
           K_Value_Box_Declaration         |
           K_Constant_Declaration          |
           K_Type_Declaration              |
           K_Exception_Declaration         |
           K_Attribute_Declaration         |
           K_Operation_Declaration         |
           K_Enumeration_Type              =>
            null;

         when others =>
            Error_Loc  (1) := Loc (Prefix);
            DE ("A type Id should not be defined for this entity");
            return;
      end case;

      if Present (Type_Id (Scope))
           and then
         (IDL_Name (Type_Id (Scope)) /= IDL_Name (Prefix) or else Unique)
      then
         Error_Loc  (1) := Loc (Prefix);
         DE ("type id should not be redefined");
      else
         Set_Type_Id (Scope, Prefix);
      end if;
   end Assign_Type_Id;

   ------------------------
   -- Assign_Type_Prefix --
   ------------------------

   procedure Assign_Type_Prefix (Scope : Node_Id; Prefix : Node_Id) is
      Prefixes : List_Id;
   begin

      --  The Corba Spec 3.0 states that:

      --  "The specified prefix applies to Repository Ids generated after the
      --   pragma until the end of the current scope is reached or another
      --   prefix pragma is encountered. An IDL file forms a scope for this
      --   purpose, so a prefix resets to the previous prefix at the end of
      --   the scope of an included file..."

      --  Each time we encounter a type prefix, we put it in the Type_Prefixes
      --  list with its location. The locations will help to assign the right
      --  prefix to a Repository Id constant.

      if Kind (Scope) = K_Specification
        or else Kind (Scope) = K_Module
        or else Kind (Scope) = K_Interface_Declaration
        or else Kind (Scope) = K_Value_Declaration
      then
         Prefixes := Type_Prefixes (Scope);
         if Is_Empty (Prefixes) then
            Prefixes := New_List (K_List_Id, Loc (Scope));
            Set_Type_Prefixes (Scope, Prefixes);
         end if;
         Append_Node_To_List (Prefix, Prefixes);
      end if;
   end Assign_Type_Prefix;

   -------------------------
   -- Assign_Type_Version --
   -------------------------

   procedure Assign_Type_Version (Scope : Node_Id; Prefix : Node_Id) is
   begin
      case Kind (Scope) is
         when
           K_Module                        |
           K_Interface_Declaration         |
           K_Forward_Interface_Declaration |
           K_Value_Declaration             |
           K_Value_Forward_Declaration     |
           K_Value_Box_Declaration         |
           K_Constant_Declaration          |
           K_Type_Declaration              |
           K_Exception_Declaration         |
           K_Attribute_Declaration         |
           K_Operation_Declaration         |
           K_Enumeration_Type              =>
            null;

         when others =>
            Error_Loc  (1) := Loc (Prefix);
            DE ("A Version Id should not be defined for this entity");
            return;
      end case;

      if Present (Type_Version (Scope))
        and then IDL_Name (Type_Version (Scope)) /= IDL_Name (Prefix)
      then
         Error_Loc  (1) := Loc (Prefix);
         DE ("pragma version should not be redefined");

      elsif Present (Type_Id (Scope)) then
         declare
            Rep_Id : constant String
              := Get_Name_String (IDL_Name (Type_Id (Scope)));
            V_Id : constant String
              := Get_Name_String (IDL_Name (Prefix));
         begin
            --  We assume that the version appears at the end of the
            --  Repository_ID constant.

            if V_Id'Length <= Rep_Id'Length and then
              Rep_Id (Rep_Id'Last - V_Id'Length + 1 .. Rep_Id'Last) /= V_Id
            then
               Error_Loc  (1) := Loc (Prefix);
               DE ("Type ID should not be overridden");
            else
               Set_Type_Version (Scope, Prefix);
            end if;
         end;
      else
         Set_Type_Version (Scope, Prefix);
      end if;
   end Assign_Type_Version;

   -----------------------------
   -- Display_Incorrect_Value --
   -----------------------------

   procedure Display_Incorrect_Value
     (L  : Location;
      K1 : Node_Kind;
      K2 : Node_Kind := K_Void)
   is
   begin
      Error_Loc  (1) := L;
      Error_Name (1) := Quoted (Image (K1));
      if K2 = K_Void then
         DE ("value not in range of type of%");
      else
         Error_Name (2) := Quoted (Image (K2));
         DE ("value not in range of type of%or%");
      end if;
   end Display_Incorrect_Value;

   --------------
   -- Exchange --
   --------------

   procedure Exchange (Op1, Op2 : Natural) is
      N : constant Node_Id := LT.Table (Op1);
   begin
      LT.Table (Op1) := LT.Table (Op2);
      LT.Table (Op2) := N;
   end Exchange;

   ------------------
   -- Inherit_From --
   ------------------

   procedure Inherit_From (Parent : Node_Id) is
      Entity     : Node_Id;
      Identifier : Node_Id;

   begin
      Identifier := Scoped_Identifiers (Parent);
      while Present (Identifier) loop
         Entity := Corresponding_Entity (Identifier);

         --  Do not add to the scope a scoped name that was introduced in a
         --  parent scope. If the interface inherits from parent entities, this
         --  is a new scope in which the names introduced for the parents are
         --  no longer considered.

         if Present (Entity) and then Kind (Entity) /= K_Scoped_Name then
            Enter_Name_In_Scope
              (Make_Identifier
               (Loc (Entity),
                IDL_Name (Identifier),
                Entity,
                Current_Scope));
         end if;
         Identifier := Next_Entity (Identifier);
      end loop;
   end Inherit_From;

   ---------------
   -- Less_Than --
   ---------------

   function  Less_Than (Op1, Op2 : Natural) return Boolean is
      N1, N2 : Node_Id;
      V1, V2 : Value_Id;
   begin
      --  N1 is default

      N1 := LT.Table (Op1);
      if No (Expression (N1)) then
         return False;
      end if;
      V1 := Value (N1);

      --  N2 is default

      N2 := LT.Table (Op2);
      if No (Expression (N2)) then
         return True;
      end if;
      V2 := Value (N2);

      --  N1 is an incorrect node

      if V1 = No_Value then
         return V2 /= No_Value;

      elsif V2 = No_Value then
         return False;
      end if;

      return Value (V1) < Value (V2);
   end Less_Than;

   ------------------
   -- Resolve_Expr --
   ------------------

   procedure Resolve_Expr (E : Node_Id; T : Node_Id) is

      procedure Cannot_Interpret
        (E : Node_Id;
         S : String;
         T : Node_Kind);
      --  Output an error message to indicate that a value cannot be cast to
      --  a given type. E denotes the entity in which the cast occurs, V the
      --  source type and K the target type.  ???There's no V or K.

      function Convert
        (E : Node_Id;
         T : Node_Id;
         K : Node_Kind) return Value_Type;
      --  Convert the value from E into type T in the context K. The conversion
      --  depends on the context since for instance, an integer value is not
      --  converted the same way whether it is performed in a constant
      --  declaration or in an expression.

      function In_Range
        (I : Unsigned_Long_Long;
         S : Short_Short;
         F : Long_Long;
         L : Unsigned_Long_Long) return Boolean;
      --  Check whether S * I (Sign * Val) is in range F .. L

      ----------------------
      -- Cannot_Interpret --
      ----------------------

      procedure Cannot_Interpret (E : Node_Id; S : String; T : Node_Kind) is
      begin
         Error_Loc (1)  := Loc (E);
         Error_Name (1) := Quoted (Image (T));
         DE ("cannot interpret " & S & " as%");
      end Cannot_Interpret;

      -------------
      -- Convert --
      -------------

      function Convert
        (E    : Node_Id;
         T    : Node_Id;
         K    : Node_Kind)
         return Value_Type
      is
         KT : Node_Kind := Kind (T);
         RE : Node_Id := E;
         RV : Value_Type;
         R  : Value_Id;
         KE : Node_Kind := Kind (E);
         I  : Unsigned_Long_Long;
         S  : Short_Short;

      begin

         --  First resolve a scoped name

         if KE = K_Scoped_Name then
            RE := Reference (E);
            if No (RE) then
               return Bad_Value;
            end if;
         end if;

         --  Resolve the Result Value RV and the Kind of Type KT

         R := Value (RE);
         if R = No_Value then
            return Bad_Value;
         end if;
         RV := Value (R);

         --  For an enumeration type, check the reference designates either an
         --  enumerator or a valid constant value.

         if KT = K_Enumeration_Type then
            KE := Kind (RE);
            if KE = K_Enumerator then
               return RV;
            end if;

            if KE /= K_Constant_Declaration then
               Error_Loc (1)  := Loc (E);
               Error_Name (1) := IDL_Name (Identifier (T));
               DE ("expected type#");
               return Bad_Value;
            end if;

            declare
               CT : Node_Id := Type_Spec (RE);
            begin
               if Kind (CT) = K_Scoped_Name then
                  CT := Reference (CT);
               end if;

               if Kind (CT) /= K_Enumeration_Type
                 or else T /= CT
               then
                  Error_Loc (1)  := Loc (E);
                  Error_Name (1) := IDL_Name (Identifier (T));
                  DE ("expected type#");
                  return Bad_Value;
               end if;

               R := Value (RE);
               if R = No_Value then
                  return Bad_Value;
               end if;

               RV := Value (R);
               return RV;
            end;
         end if;

         case RV.K is
            when K_Short .. K_Unsigned_Long_Long | K_Octet =>

               --  When integer value, cast into integer type

               if KT not in K_Short .. K_Unsigned_Long_Long
                 and then KT /= K_Octet
               then
                  Cannot_Interpret (E, "integer", KT);
                  return Bad_Value;
               end if;
               I := RV.IVal;
               S := RV.Sign;

               --  In a constant declaration, subtyping is
               --  restrictive. In an expression, a literal or a
               --  scoped name, signed or unsigned integers of 8, 16
               --  and 32 bits are handled as signed or unsigned
               --  integers of 32 bits. Therefore, the cast is
               --  performed first to signed integers. Then to
               --  unsigned integers.

               if K /= K_Constant_Declaration then
                  if KT = K_Unsigned_Long_Long or else KT = K_Long_Long then
                     KT := K_Long_Long;
                  else
                     KT := K_Long;
                  end if;
               end if;

               --  When E is not a declaration, cast to signed
               --  integers or else to unsigned integers. When E is a
               --  declaration, cast to the exact type.

               declare
                  Conversion_Succcessful : Boolean := False;
               begin
                  for B in False .. True loop
                     case KT is
                        when K_Octet =>
                           if In_Range (I, S, FO, LO) then
                              RV := Convert (RV, KT);
                              Conversion_Succcessful := True;
                           end if;

                        when K_Short =>
                           if In_Range (I, S, FS, LS) then
                              RV := Convert (RV, KT);
                              Conversion_Succcessful := True;
                           end if;

                        when K_Long =>
                           if In_Range (I, S, FL, LL) then
                              RV := Convert (RV, KT);
                              Conversion_Succcessful := True;
                           end if;

                        when K_Long_Long =>
                           if In_Range (I, S, FLL, LLL) then
                              RV := Convert (RV, KT);
                              Conversion_Succcessful := True;
                           end if;

                        when K_Unsigned_Short =>
                           if In_Range (I, S, FUS, LUS) then
                              RV := Convert (RV, KT);
                              Conversion_Succcessful := True;
                           end if;

                        when K_Unsigned_Long =>
                           if In_Range (I, S, FUL, LUL) then
                              RV := Convert (RV, KT);
                              Conversion_Succcessful := True;
                           end if;

                        when K_Unsigned_Long_Long =>
                           if In_Range (I, S, FULL, LULL) then
                              RV := Convert (RV, KT);
                              Conversion_Succcessful := True;
                           end if;

                        when others =>
                           null;
                     end case;

                     exit when K = K_Constant_Declaration
                       or else Conversion_Succcessful;

                     --  Switch to unsigned integers

                     if KT = K_Long_Long then
                        KT := K_Unsigned_Long_Long;
                     elsif KT /= K_Unsigned_Long_Long then
                        KT := K_Unsigned_Long;
                     end if;
                  end loop;
               end;

               --  Cast cannot be performed. Output an error message
               --  according to the performed operation: exact cast,
               --  32-bits integer cast, 64-bits integer cast.

               if RV.K /= KT then
                  if K = K_Constant_Declaration then
                     Display_Incorrect_Value
                       (Loc (E), KT);

                  elsif KT = K_Unsigned_Long then
                     Display_Incorrect_Value
                       (Loc (E), K_Long, K_Unsigned_Long);

                  else
                     Display_Incorrect_Value
                       (Loc (E), K_Long_Long, K_Unsigned_Long_Long);
                  end if;
                  return Bad_Value;
               end if;

            when K_String | K_String_Type =>
               if RV.K /= K_String
                 and then RV.K /= K_String_Type
               then
                  Cannot_Interpret (E, "string", KT);
                  return Bad_Value;
               end if;
               RV := Convert (RV, KT);

            when K_Wide_String | K_Wide_String_Type =>
               if RV.K /= K_Wide_String
                 and then RV.K /= K_Wide_String_Type
               then
                  Cannot_Interpret (E, "wide string", KT);
                  return Bad_Value;
               end if;
               RV := Convert (RV, KT);

            when K_Char =>
               if RV.K /= KT then
                  Cannot_Interpret (E, "character", KT);
                  return Bad_Value;
               end if;
               RV := Convert (RV, KT);

            when K_Wide_Char =>
               if RV.K /= KT then
                  Cannot_Interpret (E, "wide character", KT);
                  return Bad_Value;
               end if;
               RV := Convert (RV, KT);

            when K_Fixed_Point_Type =>
               if RV.K /= KT then
                  Cannot_Interpret (E, "fixed point", KT);
                  return Bad_Value;
               end if;

               --  For constant declaration, subtyping is restrictive.
               --  The fixed point value must be truncated to the
               --  appropriate scale. It cannot exceed the appropriate
               --  total number of digits.

               declare
                  Total : Unsigned_Short_Short;
                  Scale : Unsigned_Short_Short;
               begin
                  if K = K_Constant_Declaration then
                     Total := Unsigned_Short_Short (N_Total (T));
                     Scale := Unsigned_Short_Short (N_Scale (T));
                  else
                     Total := Max_Digits;
                     Scale := Max_Digits;
                  end if;
                  Normalize_Fixed_Point_Value (RV, Total, Scale);
                  if RV = Bad_Value then
                     Error_Loc (1) := Loc (E);
                     Error_Int (1) := Int (Total);
                     Error_Int (2) := Int (Scale);
                     DE ("too many digits to fit fixed<$,$>");
                     return RV;
                  end if;
                  RV := Convert (RV, KT);
               end;

            when K_Float .. K_Long_Double =>
               if RV.K not in K_Float .. K_Long_Double then
                  Cannot_Interpret (E, "float", KT);
                  return Bad_Value;
               end if;
               RV := Convert (RV, KT);

            when K_Boolean =>
               if RV.K /= KT then
                  Cannot_Interpret (E, "boolean", KT);
                  return Bad_Value;
               end if;
               RV := Convert (RV, KT);

            when others =>
               return Bad_Value;
         end case;

         return RV;
      end Convert;

      --------------
      -- In_Range --
      --------------

      function In_Range
        (I : Unsigned_Long_Long;
         S : Short_Short;
         F : Long_Long;
         L : Unsigned_Long_Long)
        return Boolean
      is
         Minus_F : Unsigned_Long_Long;
      begin
         if S < 0 then
            if F < 0 then
               --  If F is equal to FLL (the lowest Long_Long), doing
               --  directly Unsigned_Long_Long (-F) will cause an
               --  overflow error because converting FLL to LLL + 1 is
               --  occured before the type conversion to
               --  Unsigned_Long_Long. The instructions below
               --  work-around this problem.

               Minus_F := Unsigned_Long_Long (-(F + 1));
               Minus_F := Minus_F + 1;

               if I <= Minus_F then
                  return True;
               end if;
            end if;

            return False;
         end if;

         return I <= L;
      end In_Range;

      KE     : Node_Kind;
      RE, LE : Node_Id;
      RV, LV : Value_Type;
      O      : Token_Type;

      --  Start of processing for Resolve_Expr

   begin
      if No (T) or else No (E) then
         return;
      end if;
      KE := Kind (E);

      --  For constant declaration, first resolve expression attached
      --  to declaration. The expression is evaluated as described in
      --  the next block. Second convert the value into the exact type
      --  and if the evaluation has been successful, set the constant
      --  value to it.

      if KE = K_Constant_Declaration
        or else KE = K_Case_Label
      then
         RE := Expression (E);
         if Present (RE) then
            Resolve_Expr (RE, T);
            RV := Convert (RE, T, KE);
            if RV = Bad_Value then
               Set_Value (E, No_Value);
               return;
            end if;
            Set_Value (E, New_Value (RV));
         end if;

      --  For expression, evaluate left part when possible and then
      --  right part of the expression. Each result is converted into
      --  type T following the specific rules for sub-expression (see
      --  function Convert). Then execute operation and check that the
      --  operation was successful. Do not convert to T at this point.

      elsif KE = K_Expression then
         LE := Left_Expr (E);
         if Present (LE) then

            --  Resolve and convert a possible left sub-expression

            Resolve_Expr (LE, T);
            LV := Convert (LE, T, KE);
            if LV = Bad_Value then
               Set_Value (E, No_Value);
               return;
            end if;
         end if;

         RE := Right_Expr (E);
         if No (RE) then
            Set_Value (E, No_Value);
            return;
         end if;

         --  Resolve and convert a right sub-expression

         Resolve_Expr (RE, T);
         RV := Convert (RE, T, KE);
         if RV = Bad_Value then
            Set_Value (E, No_Value);
            return;
         end if;

         --  For binary operator, check that the two operands have the
         --  same type.

         O := Token_Type'Val (Operator (E));
         if Present (LE)
           and then LV.K /= RV.K
         then
            Error_Loc (1)  := Loc (E);
            Error_Name (1) := Quoted (Image (O));
            DE ("invalid operand types for operator%");
            Set_Value (E, No_Value);
            return;
         end if;

         case O is
            when T_Tilde           =>
               RV := not RV;

            when T_Minus           =>
               if No (LE) then
                  RV := -RV;
               else
                  RV := LV - RV;
               end if;

            when T_Plus            =>
               if Present (LE) then
                  RV := LV + RV;
               end if;

            when T_Percent         =>
               RV := LV mod RV;

            when T_Slash           =>
               RV := LV / RV;

            when T_Star            =>
               RV := LV * RV;

            when T_Ampersand       =>
               RV := LV and RV;

            when T_Bar             =>
               RV := LV or RV;

            when T_Circumflex      =>
               RV := LV xor RV;

            when T_Greater_Greater =>
               RV := Shift_Right  (LV, RV);

            when T_Less_Less       =>
               RV := Shift_Left (LV, RV);

            when others            =>
               return;
         end case;

         if RV = Bad_Value then
            Set_Value (E, No_Value);
            return;
         end if;

         --  For integer types, we try to fit the new value in the smallest
         --  type.
         if (Kind (T) in K_Short .. K_Unsigned_Long_Long)
           or else Kind (T) = K_Octet then
            declare
               I  : constant Unsigned_Long_Long := RV.IVal;
               S  : constant Short_Short := RV.Sign;
            begin
               if In_Range (I, S, FO, LO) then
                  RV := Convert (RV, K_Octet);
               elsif In_Range (I, S, FS, LS) then
                  RV := Convert (RV, K_Short);
               elsif In_Range (I, S, FUS, LUS) then
                  RV := Convert (RV, K_Unsigned_Short);
               elsif In_Range (I, S, FL, LL) then
                  RV := Convert (RV, K_Long);
               elsif In_Range (I, S, FUL, LUL) then
                  RV := Convert (RV, K_Unsigned_Long);
               elsif In_Range (I, S, FLL, LLL) then
                  RV := Convert (RV, K_Long_Long);
               elsif In_Range (I, S, FULL, LULL) then
                  RV := Convert (RV, K_Unsigned_Long_Long);
               end if;
            end;
         end if;

         Set_Value (E, New_Value (RV));
      end if;
   end Resolve_Expr;

   ------------------
   -- Resolve_Type --
   ------------------

   function Resolve_Type (N : Node_Id) return Node_Id is
      T : Node_Id := N;

   begin
      while Present (T) loop
         case Kind (T) is
            when K_Simple_Declarator =>
               T := Type_Spec (Declaration (T));

            when K_Scoped_Name =>
               T := Reference (T);

            when
              K_Forward_Interface_Declaration |
              K_Value_Forward_Declaration     |
              K_Forward_Structure_Type        |
              K_Forward_Union_Type            =>
               T := Forward (T);

            when others =>
               exit;
         end case;
      end loop;

      return T;
   end Resolve_Type;

end Analyzer;

------------------------------------------------------------------------------
--                                                                          --
--                            POLYORB COMPONENTS                            --
--                                   IAC                                    --
--                                                                          --
--    B A C K E N D . B E _ C O R B A _ A D A . I N I T I A L I Z E R S     --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                           Copyright (c) 2006                             --
--            Ecole Nationale Superieure des Telecommunications             --
--                                                                          --
-- IAC is free software; you  can  redistribute  it and/or modify it under  --
-- terms of the GNU General Public License  as published by the  Free Soft- --
-- ware  Foundation;  either version 2 of the liscence or (at your option)  --
-- any  later version.                                                      --
-- IAC is distributed  in the hope that it will be  useful, but WITHOUT ANY --
-- WARRANTY; without even the implied warranty of MERCHANTABILITY or        --
-- FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for --
-- more details.                                                            --
-- You should have received a copy of the GNU General Public License along  --
-- with this program; if not, write to the Free Software Foundation, Inc.,  --
-- 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.            --
--                                                                          --
------------------------------------------------------------------------------

with Namet;  use Namet;
with Values; use Values;

with Frontend.Nodes;  use Frontend.Nodes;
with Frontend.Nutils;

with Backend.BE_CORBA_Ada.Nodes;       use Backend.BE_CORBA_Ada.Nodes;
with Backend.BE_CORBA_Ada.Nutils;      use Backend.BE_CORBA_Ada.Nutils;
with Backend.BE_CORBA_Ada.IDL_To_Ada;  use Backend.BE_CORBA_Ada.IDL_To_Ada;
with Backend.BE_CORBA_Ada.Runtime;     use Backend.BE_CORBA_Ada.Runtime;
with Backend.BE_CORBA_Ada.Expand;      use Backend.BE_CORBA_Ada.Expand;

package body Backend.BE_CORBA_Ada.Initializers is

   package FEN renames Frontend.Nodes;
   package FEU renames Frontend.Nutils;
   package BEN renames Backend.BE_CORBA_Ada.Nodes;

   ------------------
   -- Package_Spec --
   ------------------

   package body Package_Spec is

      procedure Visit_Enumeration_Type (E : Node_Id);
      procedure Visit_Forward_Interface_Declaration (E : Node_Id);
      procedure Visit_Interface_Declaration (E : Node_Id);
      procedure Visit_Module (E : Node_Id);
      procedure Visit_Specification (E : Node_Id);
      procedure Visit_Structure_Type (E : Node_Id);
      procedure Visit_Type_Declaration (E : Node_Id);
      procedure Visit_Union_Type (E : Node_Id);
      procedure Visit_Exception_Declaration (E : Node_Id);

      function Initialize_Spec (E : Node_Id) return Node_Id;
      --  Returns the spec of the Initialize procedure that builds the
      --  TypeCode corresponding to the IDL type E

      ---------------------
      -- Initialize_Spec --
      ---------------------

      function Initialize_Spec (E : Node_Id) return Node_Id is
         N        : Node_Id;
         Spg_Name : constant Name_Id := Add_Prefix_To_Name
           ("Initialize_", To_Ada_Name (FEN.IDL_Name (Identifier (E))));
      begin
         N := Make_Subprogram_Specification
           (Make_Defining_Identifier (Spg_Name), No_List);
         return N;
      end Initialize_Spec;

      -----------
      -- Visit --
      -----------

      procedure Visit (E : Node_Id) is
      begin
         case FEN.Kind (E) is
            when K_Enumeration_Type =>
               Visit_Enumeration_Type (E);

            when K_Forward_Interface_Declaration =>
               Visit_Forward_Interface_Declaration (E);

            when K_Interface_Declaration =>
               Visit_Interface_Declaration (E);

            when K_Module =>
               Visit_Module (E);

            when K_Specification =>
               Visit_Specification (E);

            when K_Structure_Type =>
               Visit_Structure_Type (E);

            when K_Type_Declaration =>
               Visit_Type_Declaration (E);

            when K_Union_Type =>
               Visit_Union_Type (E);

            when K_Exception_Declaration =>
               Visit_Exception_Declaration (E);

            when others =>
               null;
         end case;
      end Visit;

      ----------------------------
      -- Visit_Enumeration_Type --
      ----------------------------

      procedure Visit_Enumeration_Type (E : Node_Id) is
         N : Node_Id;
      begin
         Set_Init_Spec;

         N := Initialize_Spec (E);
         Bind_FE_To_Initialize (Identifier (E), N);
         Append_Node_To_List (N, Visible_Part (Current_Package));
      end Visit_Enumeration_Type;

      -----------------------------------------
      -- Visit_Forward_Interface_Declaration --
      -----------------------------------------

      procedure Visit_Forward_Interface_Declaration (E : Node_Id) is
         N : Node_Id;
      begin
         Set_Init_Spec;

         N := Initialize_Spec (E);
         Bind_FE_To_Initialize (Identifier (E), N);
         Append_Node_To_List (N, Visible_Part (Current_Package));
      end Visit_Forward_Interface_Declaration;

      ---------------------------------
      -- Visit_Interface_Declaration --
      ---------------------------------

      procedure Visit_Interface_Declaration (E : Node_Id) is
         N : Node_Id;
      begin
         N := BEN.Parent (Type_Def_Node (BE_Node (Identifier (E))));
         Push_Entity (BEN.IDL_Unit (Package_Declaration (N)));
         Set_Init_Spec;

         N := Initialize_Spec (E);
         Bind_FE_To_Initialize (Identifier (E), N);
         Append_Node_To_List (N, Visible_Part (Current_Package));

         N := First_Entity (Interface_Body (E));
         while Present (N) loop
            Visit (N);
            N := Next_Entity (N);
         end loop;

         Pop_Entity;
      end Visit_Interface_Declaration;

      ------------------
      -- Visit_Module --
      ------------------

      procedure Visit_Module (E : Node_Id) is
         D : Node_Id;
      begin
         if not Map_Particular_CORBA_Parts (E, PK_Init_Spec) then
            Push_Entity (Stub_Node (BE_Node (Identifier (E))));
            D := First_Entity (Definitions (E));
            while Present (D) loop
               Visit (D);
               D := Next_Entity (D);
            end loop;
            Pop_Entity;
         end if;
      end Visit_Module;

      -------------------------
      -- Visit_Specification --
      -------------------------

      procedure Visit_Specification (E : Node_Id) is
         D : Node_Id;
      begin
         Push_Entity (Stub_Node (BE_Node (Identifier (E))));
         D := First_Entity (Definitions (E));
         while Present (D) loop
            Visit (D);
            D := Next_Entity (D);
         end loop;
         Pop_Entity;
      end Visit_Specification;

      --------------------------
      -- Visit_Structure_Type --
      --------------------------

      procedure Visit_Structure_Type (E : Node_Id) is
         N          : Node_Id;
         Member     : Node_Id;
         Declarator : Node_Id;
      begin
         Set_Init_Spec;

         --  See the comment message in
         --  Helpers.Package_Spec.Visit_Structure_Type for more
         --  details on the instructions below

         Member := First_Entity (Members (E));
         while Present (Member) loop
            Declarator := First_Entity (Declarators (Member));
            while Present (Declarator) loop
               if FEN.Kind (Declarator) = K_Complex_Declarator then
                  N := Initialize_Spec (Declarator);
                  Bind_FE_To_Initialize (Identifier (Declarator), N);
                  Append_Node_To_List (N, Visible_Part (Current_Package));
               end if;

               Declarator := Next_Entity (Declarator);
            end loop;
            Member := Next_Entity (Member);
         end loop;

         N := Initialize_Spec (E);
         Bind_FE_To_Initialize (Identifier (E), N);
         Append_Node_To_List (N, Visible_Part (Current_Package));
      end Visit_Structure_Type;

      ----------------------------
      -- Visit_Type_Declaration --
      ----------------------------

      procedure Visit_Type_Declaration (E : Node_Id) is
         N : Node_Id;
         D : Node_Id;
      begin
         Set_Init_Spec;

         D := First_Entity (Declarators (E));
         while Present (D) loop
            N := Initialize_Spec (D);
            Bind_FE_To_Initialize (Identifier (D), N);
            Append_Node_To_List (N, Visible_Part (Current_Package));

            D := Next_Entity (D);
         end loop;
      end Visit_Type_Declaration;

      ----------------------
      -- Visit_Union_Type --
      ----------------------

      procedure Visit_Union_Type (E : Node_Id) is
         N            : Node_Id;
         Alternatives : List_Id;
         Alternative  : Node_Id;
         Declarator   : Node_Id;
      begin
         Set_Init_Spec;

         --  See the comment message in
         --  Helpers.Package_Spec.Visit_Union_Type for more
         --  details on the instructions below

         Alternatives := Switch_Type_Body (E);
         Alternative := First_Entity (Alternatives);
         while Present (Alternative) loop
            Declarator := FEN.Declarator (FEN.Element (Alternative));
            if FEN.Kind (Declarator) = K_Complex_Declarator then
               N := Initialize_Spec (Declarator);
               Bind_FE_To_Initialize (Identifier (Declarator), N);
               Append_Node_To_List (N, Visible_Part (Current_Package));
            end if;

            Alternative := Next_Entity (Alternative);
         end loop;

         N := Initialize_Spec (E);
         Bind_FE_To_Initialize (Identifier (E), N);
         Append_Node_To_List (N, Visible_Part (Current_Package));
      end Visit_Union_Type;

      ---------------------------------
      -- Visit_Exception_Declaration --
      ---------------------------------

      procedure Visit_Exception_Declaration (E : Node_Id) is
         N : Node_Id;
      begin
         Set_Init_Spec;

         N := Initialize_Spec (E);
         Bind_FE_To_Initialize (Identifier (E), N);
         Append_Node_To_List (N, Visible_Part (Current_Package));
      end Visit_Exception_Declaration;

   end Package_Spec;

   ------------------
   -- Package_Body --
   ------------------

   package body Package_Body is

      procedure Visit_Enumeration_Type (E : Node_Id);
      procedure Visit_Forward_Interface_Declaration (E : Node_Id);
      procedure Visit_Interface_Declaration (E : Node_Id);
      procedure Visit_Module (E : Node_Id);
      procedure Visit_Specification (E : Node_Id);
      procedure Visit_Structure_Type (E : Node_Id);
      procedure Visit_Type_Declaration (E : Node_Id);
      procedure Visit_Union_Type (E : Node_Id);
      procedure Visit_Exception_Declaration (E : Node_Id);

      function Initialized_Identifier (E : Node_Id) return Node_Id;
      --  Return a defining identifier designing the boolean flag that
      --  controls the IDL type E

      function Initialized_Flag_Declaration (E : Node_Id) return Node_Id;
      --  Declares a Boolean flag useful for the initialization of a
      --  TypeCode corresponding to the IDL type E

      function Initialize_Body (E : Node_Id) return Node_Id;
      --  Returns the body of the Initialize procedure that builds the
      --  TypeCode corresponding to the IDL type E

      procedure Initialize_Routine
        (E                : Node_Id;
         Declaration_List : List_Id;
         Statements       : List_Id);
      --  Fills the lists Declaration_List and Statements with the
      --  routines initializing the IDL type E

      ----------------------------
      -- Initialized_Identifier --
      ----------------------------

      function Initialized_Identifier (E : Node_Id) return Node_Id is
         Flag_Name : constant Name_Id := Add_Suffix_To_Name
           ("_Initialized", To_Ada_Name (FEN.IDL_Name (Identifier (E))));
      begin
         return Make_Defining_Identifier (Flag_Name);
      end Initialized_Identifier;

      ----------------------------------
      -- Initialized_Flag_Declaration --
      ----------------------------------

      function Initialized_Flag_Declaration (E : Node_Id) return Node_Id is
         N : Node_Id;
      begin
         N := Make_Object_Declaration
           (Defining_Identifier => Initialized_Identifier (E),
            Object_Definition   => RE (RE_Boolean_2),
            Expression          => RE (RE_False));
         return N;
      end Initialized_Flag_Declaration;

      ---------------------
      -- Initialize_Body --
      ---------------------

      function Initialize_Body (E : Node_Id) return Node_Id is
         N                : Node_Id;
         Spec             : constant Node_Id := Initialize_Node
           (BE_Node (Identifier (E)));
         Declarative_Part : constant List_Id := New_List (K_Declaration_List);
         Statements       : constant List_Id := New_List (K_Statement_List);
         Then_Statements  : constant List_Id := New_List (K_Statement_List);
         Condition        : Node_Id;
      begin
         --  Declare the boolean flag global variable that indicates
         --  whether the TypeCode has been initialized or not. There
         --  is no harm this variable is global, because the
         --  initialization is done only once at the beginning of the
         --  application and its not supposed to be done by more than
         --  one task

         N := Initialized_Flag_Declaration (E);
         Append_Node_To_List (N, BEN.Statements (Current_Package));

         --  Build the IF statement that controls the initialization
         --  of the TypeCode

         Condition := Make_Expression (Initialized_Identifier (E), Op_Not);
         N := Make_Assignment_Statement
           (Initialized_Identifier (E), RE (RE_True));
         Append_Node_To_List (N, Then_Statements);

         --  Append the initialization routines

         Initialize_Routine (E, Declarative_Part, Then_Statements);

         N := Make_If_Statement
           (Condition       => Condition,
            Then_Statements => Then_Statements);
         Append_Node_To_List (N, Statements);

         N := Make_Subprogram_Implementation
           (Spec, Declarative_Part, Statements);
         return N;
      end Initialize_Body;

      ------------------------
      -- Initialize_Routine --
      ------------------------

      procedure Initialize_Routine
        (E                : Node_Id;
         Declaration_List : List_Id;
         Statements       : List_Id)
      is
         function Add_Parameter
           (TC_Name : Name_Id; Var_Node : Node_Id)
           return Node_Id;
         --  Makes a call to the Add_Parametr Routine with the given
         --  parameters

         function Declare_Name
           (Var_Name : Name_Id; Value : Value_Id)
           return Node_Id;
         --  Makes a variable declaration using the given parameters

         function Get_TC_Fixed_Point (E : Node_Id) return Node_Id;
         --  Generate a TC constant for a fixed point type.

         procedure Debug_Message;

         -------------------
         -- Debug_Message --
         -------------------

         procedure Debug_Message is
            Comment : Node_Id;
         begin
            Set_Str_To_Name_Buffer ("FIXME : Handle dependency or "
                                    & "Initialize Parameter Type!");
            Comment := Make_Ada_Comment (Name_Find);
            Append_Node_To_List (Comment, Statements);
         end Debug_Message;

         -------------------
         -- Add_Parameter --
         -------------------

         function Add_Parameter
           (TC_Name  : Name_Id;
            Var_Node : Node_Id)
            return Node_Id
         is
            N : Node_Id;
         begin
            N := Make_Subprogram_Call
              (RE (RE_To_Any_0),
               Make_List_Id (Var_Node));
            N := Make_Subprogram_Call
              (RE (RE_Add_Parameter),
               Make_List_Id (Make_Designator (TC_Name), N));

            return N;
         end Add_Parameter;

         ------------------
         -- Declare_Name --
         ------------------

         function Declare_Name
           (Var_Name  : Name_Id;
            Value : Value_Id)
            return Node_Id
         is
            N : Node_Id;
         begin
            N := Make_Object_Declaration
              (Defining_Identifier => Make_Defining_Identifier (Var_Name),
               Object_Definition   => RE (RE_String_0),
               Expression          => Make_Subprogram_Call
                 (RE (RE_To_CORBA_String),
                  Make_List_Id (Make_Literal (Value))));

            return N;
         end Declare_Name;

         ------------------------
         -- Get_TC_Fixed_Point --
         ------------------------

         function Get_TC_Fixed_Point
           (E : Node_Id)
           return Node_Id
         is
            pragma Assert (FEN.Kind (E) = K_Fixed_Point_Type);

            Fixed_Name : Name_Id := Map_Fixed_Type_Name (E);
            Result     : Node_Id;
         begin
            Fixed_Name := Add_Prefix_To_Name ("TC_", Fixed_Name);

            Result := Make_Defining_Identifier (Fixed_Name);
            Set_Correct_Parent_Unit_Name
              (Result,
               Defining_Identifier (Helper_Package (Current_Entity)));

            return Result;
         end Get_TC_Fixed_Point;

         Stub             : Node_Id;
         N                : Node_Id;
         Entity_TC_Name   : Name_Id;
         Entity_Name_V    : Value_Id;
         Entity_Rep_Id_V  : Value_Id;
         Param1           : Node_Id;
         Param2           : Node_Id;
      begin
         --  Extract from polyorb-any.ads concerning the Encoding of
         --  TypeCodes:

         --  10. For sequence and array, the first parameter will
         --      be the length of the sequence or the array and the second
         --      the content type. As for strings, an unbounded sequence will
         --      have a length of 0.

         --  11. For fixed, the first parameter will be the digits
         --      number and the second the scale.

         --  So, we dont need the definitions below :

         if FEN.Kind (E) /= K_Complex_Declarator
           and then FEN.Kind (E) /= K_Fixed_Point_Type then

            --  For the forward interfaces, we use the name and the
            --  Rep_Id of the forwarded interface. The Repository_Id
            --  is declared just after the type définition

            if FEN.Kind (E) = K_Forward_Interface_Declaration then
               Stub := Type_Def_Node (BE_Node (Identifier (Forward (E))));
            else
               Stub := Type_Def_Node (BE_Node (Identifier (E)));
            end if;

            Entity_Rep_Id_V := BEN.Value (BEN.Expression (Next_Node (Stub)));
         end if;

         --  The fixed point types constitute a particular case since
         --  they don't have a corresponding node in the frontend tree

         if FEN.Kind (E) /= K_Fixed_Point_Type then
            Entity_TC_Name := BEN.Name (Defining_Identifier
                                        (Get_TC_Node (E)));
         else
            Entity_TC_Name := BEN.Name (Get_TC_Fixed_Point (E));
         end if;

         case FEN.Kind (E) is
            when K_Interface_Declaration
              | K_Forward_Interface_Declaration =>
               Stub := Package_Declaration
                 (BEN.Parent (Stub));

            when K_Complex_Declarator =>
               declare
                  V                : Value_Type;
                  TC               : Node_Id;
                  TC_Dim           : Node_Id          := No_Node;
                  TC_Previous_Name : Name_Id          := No_Name;
                  TC_Name          : Name_Id          := No_Name;
                  Sizes            : constant List_Id := Range_Constraints
                    (Type_Definition
                     (Type_Def_Node
                      (BE_Node
                       (Identifier
                        (E)))));
                  Sizes_Reverse    : constant List_Id := New_List (K_List_Id);
                  Constraint       : Node_Id;
                  Dimension        : constant Natural := Length (Sizes);
                  From_N           : Node_Id          := No_Node;
                  To_N             : Node_Id          := No_Node;
                  T                : Node_Id;
               begin
                  if Dimension > 1 then

                     --  First of all, we create a new list which
                     --  contains the elements of the list Sizes. All
                     --  manipulations on this list will not affect
                     --  the Sizes list because we create new nodes.

                     From_N := First_Node (Sizes);
                     while Present (From_N) loop
                        To_N := New_Node (K_Range_Constraint);
                        Set_Last (To_N, Last (From_N));
                        Append_Node_To_List
                          (To_N,
                           Sizes_Reverse);
                        From_N := Next_Node (From_N);
                     end loop;

                     --  The TC_XXXX constants used here are the ones
                     --  declared in the Helper spec

                     TC := TC_Node (BE_Node (Identifier (E)));
                     Constraint := Last_Node (Sizes_Reverse);
                     for Index in 1 .. Dimension - 1 loop
                        TC_Dim := Next_N_Node (TC, Dimension - Index);

                        TC_Previous_Name := TC_Name;
                        TC_Name := BEN.Name (BEN.Defining_Identifier (TC_Dim));
                        V := Values.Value (BEN.Value (Last (Constraint)));
                        V.IVal := V.IVal + 1;
                        Param1 := Make_Subprogram_Call
                          (RE (RE_Unsigned_Long),
                           Make_List_Id
                           (Make_Literal (New_Value (V))));

                        if TC_Previous_Name = No_Name then

                           --  The deepest dimension

                           T := Type_Spec (Declaration (E));
                           Param2 := Get_TC_Node (T);

                           --  FIXME : Handle dependancy
                           --  Helpers.Package_Body.Add_Dependency
                           --    (Parent_Unit_Name (Param2));

                           Debug_Message;
                        else --  Not the deepest dimension
                           Param2 := Make_Designator (TC_Previous_Name);
                        end if;

                        N := Add_Parameter (TC_Name, Param1);
                        Append_Node_To_List (N, Statements);
                        N := Add_Parameter (TC_Name, Param2);
                        Append_Node_To_List (N, Statements);

                        Remove_Node_From_List (Constraint, Sizes_Reverse);
                        Constraint := Last_Node (Sizes_Reverse);
                     end loop;

                     --  The case of the last TC_ variable which
                     --  represents the whole array is handled apart.

                     V := Values.Value (BEN.Value (Last (Constraint)));
                     V.IVal := V.IVal + 1;
                     Param1 := Make_Subprogram_Call
                       (RE (RE_Unsigned_Long),
                        Make_List_Id
                        (Make_Literal (New_Value (V))));
                     Param2 := Make_Designator (TC_Name);

                  else --  1 dimension array

                     V := Values.Value (BEN.Value (Last (First_Node (Sizes))));
                     V.IVal := V.IVal + 1;
                     Param1 := Make_Subprogram_Call
                       (RE (RE_Unsigned_Long),
                        Make_List_Id
                        (Make_Literal (New_Value (V))));

                     T := Type_Spec (Declaration (E));
                     Param2 := Get_TC_Node (T);

                     --  FIXME : Handle dependancy
                     --  Helpers.Package_Body.Add_Dependency
                     --    (Parent_Unit_Name (Param2));

                     Debug_Message;
                  end if;
               end;

            when K_Fixed_Point_Type =>
               Param1 := Make_Literal
                 (New_Integer_Value
                  (Unsigned_Long_Long (N_Total (E)), 1, 10));
               Param1 := Make_Subprogram_Call
                 (RE (RE_Unsigned_Short),
                  Make_List_Id (Param1));

               Param2 := Make_Literal
                 (New_Integer_Value
                  (Unsigned_Long_Long (N_Scale (E)), 1, 10));
               Param2 := Make_Subprogram_Call
                 (RE (RE_Short),
                  Make_List_Id (Param2));

            when K_Simple_Declarator
              | K_Enumeration_Type
              | K_Structure_Type
              | K_Union_Type
              | K_Exception_Declaration =>
               null;

            when others =>
               raise Program_Error;
         end case;

         if FEN.Kind (E) /= K_Complex_Declarator
           and then FEN.Kind (E) /= K_Fixed_Point_Type then
            Param1 := Make_Designator (VN (V_Name));
            Param2 := Make_Designator (VN (V_Id));

            --  Name_U declaration

            Entity_Name_V := New_String_Value
              (BEN.Name (Defining_Identifier (Stub)), False);
            N := Declare_Name (VN (V_Name), Entity_Name_V);
            Append_Node_To_List (N, Declaration_List);

            --  Id_U declaration

            N := Declare_Name (VN (V_Id), Entity_Rep_Id_V);
            Append_Node_To_List (N, Declaration_List);
         end if;

         --  Add the two parameters

         N := Add_Parameter (Entity_TC_Name, Param1);
         Append_Node_To_List (N, Statements);
         N := Add_Parameter (Entity_TC_Name, Param2);
         Append_Node_To_List (N, Statements);

         case FEN.Kind (E) is
            when K_Enumeration_Type =>
               declare
                  Enumerators : List_Id;
                  Enum_Item   : Node_Id;
                  Var_Name    : Name_Id;
               begin
                  Enumerators := Enumeration_Literals
                    (Type_Definition (Stub));
                  Enum_Item := First_Node (Enumerators);
                  loop
                     Var_Name := Add_Prefix_To_Name
                       (Get_Name_String (BEN.Name (Enum_Item)) & '_',
                        VN (V_Name));
                     Param1 := Make_Designator (Var_Name);
                     N := Declare_Name
                       (Var_Name,
                        New_String_Value (BEN.Name (Enum_Item), False));
                     Append_Node_To_List (N, Declaration_List);
                     N := Add_Parameter (Entity_TC_Name, Param1);
                     Append_Node_To_List (N, Statements);
                     Enum_Item := Next_Node (Enum_Item);
                     exit when No (Enum_Item);
                  end loop;
               end;

            when K_Union_Type =>
               declare
                  Switch_Alternative  : Node_Id;
                  Choice              : Node_Id;
                  Choices             : List_Id;
                  Label               : Node_Id;
                  To_Any_Helper       : Node_Id;
                  TC_Helper           : Node_Id;
                  Declarator          : Node_Id;
                  Designator          : Node_Id;
                  Arg_Name            : Name_Id;
                  Switch_Type         : Node_Id;
                  Literal_Parent      : Node_Id := No_Node;
                  Orig_Type           : constant Node_Id :=
                    FEU.Get_Original_Type
                    (Switch_Type_Spec (E));
                  Statements_List     : constant List_Id :=
                    New_List (K_List_Id);
                  Default_Index       : Value_Id :=
                    New_Integer_Value (0, 1, 10); --  (0)
                  There_Is_Default    : Boolean           :=
                    False;
               begin

                  --  Getting the dicriminator type and the To_Any
                  --  node corresponding to it

                  TC_Helper := Get_TC_Node (Switch_Type_Spec (E));

                  --  FIXME : Handle dependancy
                  --  Helpers.Package_Body.Add_Dependency
                  --    (Parent_Unit_Name (TC_Helper));

                  Debug_Message;

                  To_Any_Helper := Get_To_Any_Node (Switch_Type_Spec (E));
                  if Is_Base_Type (Switch_Type_Spec (E)) then
                     Switch_Type := RE
                       (Convert
                        (FEN.Kind
                         (Switch_Type_Spec (E))));

                  elsif FEN.Kind (Orig_Type) = K_Enumeration_Type then
                     Switch_Type := Map_Designator (Switch_Type_Spec (E));
                     Literal_Parent := Map_Designator
                       (Scope_Entity
                        (Identifier
                         (Orig_Type)));
                  else
                     Switch_Type := Map_Designator (Switch_Type_Spec (E));
                  end if;

                  --  The third parameter is the discriminator type

                  N := Add_Parameter (Entity_TC_Name, TC_Helper);
                  Append_Node_To_List (N, Statements);

                  --  The forth parameter is the index of default case
                  --  as a long. we put the ramining parameter in an
                  --  intermediary list. When we get the defaul case
                  --  index, we add the intermediari list to the
                  --  statements list.

                  Switch_Alternative := First_Entity (Switch_Type_Body (E));
                  while Present (Switch_Alternative) loop
                     Choices := New_List (K_List_Id);
                     Label   := First_Entity (Labels (Switch_Alternative));
                     while Present (Label) loop

                        Choice := Make_Literal
                          (Value             => FEN.Value (Label),
                           Parent_Designator => Literal_Parent);

                        --  If this is not a case statement, then we
                        --  increment the default case index. The
                        --  value of Default_Index will be correctly
                        --  set up after the end of the two loops

                        if BEN.Value (Choice) /= No_Value then
                           Set_Value
                             (Default_Index,
                              Value (Default_Index) + Value (Int1_Val));
                        else
                           There_Is_Default := True;
                        end if;

                        Append_Node_To_List (Choice, Choices);
                        Label := Next_Entity (Label);
                     end loop;

                     --  Declaring the argument name "Element" string

                     Declarator := FEN.Declarator
                       (Element (Switch_Alternative));

                     --  Getting the TC_XXX constant corresponding to
                     --  the element type.

                     if FEN.Kind (Declarator) = K_Simple_Declarator then
                        TC_Helper := Get_TC_Node
                          (Type_Spec
                           (Element
                            (Switch_Alternative)));
                     else --  Complex Declatator
                        TC_Helper := Expand_Designator
                          (TC_Node
                           (BE_Node
                            (Identifier
                             (Declarator))));
                     end if;

                     --  FIXME : Handle dependancy
                     --  Helpers.Package_Body.Add_Dependency
                     --    (Parent_Unit_Name (TC_Helper));

                     Debug_Message;

                     Designator := Map_Designator (Declarator);
                     Get_Name_String (VN (V_Argument_Name));
                     Add_Char_To_Name_Buffer ('_');
                     Get_Name_String_And_Append
                       (BEN.Name (Defining_Identifier (Designator)));
                     Arg_Name := Name_Find;
                     N := Make_Literal
                       (New_String_Value
                        (BEN.Name (Defining_Identifier (Designator)),
                         False));
                     N := Make_Subprogram_Call
                       (RE (RE_To_CORBA_String),
                        Make_List_Id (N));
                     N := Make_Object_Declaration
                       (Defining_Identifier =>
                          Make_Defining_Identifier (Arg_Name),
                        Object_Definition   => RE (RE_String_0),
                        Expression          => N);
                     Append_Node_To_List (N, Declaration_List);

                     --  For each case statement, 3 parameters are added :
                     --  * member label
                     --  * member type
                     --  * member name
                     --  This implies that the same element may be declared
                     --  more than once but with a different label.

                     Choice := First_Node (Choices);
                     while Present (Choice) loop
                        if BEN.Value (Choice) /= No_Value then
                           --  We make a copy of the Choice value to
                           --  avoid adding the next nodes of Choice
                           --  to the argument list

                           N := Make_Literal
                             (Value             =>
                                BEN.Value (Choice),
                              Parent_Designator =>
                                BEN.Parent_Designator (Choice));

                           N := Make_Qualified_Expression
                             (Subtype_Mark => Switch_Type,
                              Aggregate    => Make_Record_Aggregate
                                (Make_List_Id (N)));

                           N := Make_Subprogram_Call
                             (To_Any_Helper,
                              Make_List_Id (N));
                           N := Add_Parameter (Entity_TC_Name, N);
                           Append_Node_To_List (N, Statements_List);

                           N := Add_Parameter (Entity_TC_Name, TC_Helper);
                           Append_Node_To_List (N, Statements_List);

                           N := Add_Parameter
                             (Entity_TC_Name,
                              Make_Defining_Identifier (Arg_Name));
                           Append_Node_To_List (N, Statements_List);
                        else --  The default case
                           N := Make_Type_Attribute (Switch_Type, A_First);

                           N := Make_Subprogram_Call
                             (To_Any_Helper,
                              Make_List_Id (N));
                           N := Add_Parameter (Entity_TC_Name, N);
                           Append_Node_To_List (N, Statements_List);

                           N := Add_Parameter (Entity_TC_Name, TC_Helper);
                           Append_Node_To_List (N, Statements_List);

                           N := Add_Parameter
                             (Entity_TC_Name,
                              Make_Defining_Identifier (Arg_Name));
                           Append_Node_To_List (N, Statements_List);

                        end if;
                        Choice := Next_Node (Choice);
                     end loop;

                     Switch_Alternative := Next_Entity (Switch_Alternative);
                  end loop;
                  if not There_Is_Default then
                     Default_Index := New_Integer_Value (1, -1, 10); --  (-1)
                  end if;

                  --  Forth parameter

                  N := Make_Literal
                    (Value           => Default_Index);
                  N := Make_Subprogram_Call
                    (RE (RE_Long),
                     Make_List_Id (N));
                  N := Add_Parameter (Entity_TC_Name, N);
                  Append_Node_To_List (N, Statements);

                  --  Append the Statements_List list to the end of
                  --  the Statements list (we only append the first
                  --  node, the others are appended automatically)

                  Append_Node_To_List
                    (First_Node (Statements_List),
                     Statements);

               end;

            when K_Structure_Type =>
               declare
                  Member     : Node_Id;
                  Declarator : Node_Id;
                  Designator : Node_Id;
                  Arg_Name   : Name_Id;
               begin
                  Member := First_Entity (Members (E));
                  while Present (Member) loop
                     Declarator := First_Entity (Declarators (Member));

                     while Present (Declarator) loop
                        Designator := Map_Designator (Declarator);
                        Get_Name_String (VN (V_Argument_Name));
                        Add_Char_To_Name_Buffer ('_');
                        Get_Name_String_And_Append
                          (BEN.Name (Defining_Identifier (Designator)));
                        Arg_Name := Name_Find;
                        N := Make_Literal
                          (New_String_Value
                           (BEN.Name (Defining_Identifier (Designator)),
                           False));
                        N := Make_Subprogram_Call
                          (RE (RE_To_CORBA_String),
                           Make_List_Id (N));
                        N := Make_Object_Declaration
                          (Defining_Identifier =>
                             Make_Defining_Identifier (Arg_Name),
                           Object_Definition   => RE (RE_String_0),
                           Expression          => N);
                        Append_Node_To_List (N, Declaration_List);

                        --  For simple declarators :

                        if FEN.Kind (Declarator) = K_Simple_Declarator then
                           Param1 := Get_TC_Node
                             (Type_Spec
                              (Declaration
                               (Declarator)));
                        else --  Complex Declatator
                           Param1 := Expand_Designator
                             (TC_Node
                              (BE_Node
                               (Identifier
                                (Declarator))));
                        end if;

                        --  FIXME : Handle dependancy
                        --  Helpers.Package_Body.Add_Dependency
                        --    (Parent_Unit_Name (Param1));

                        Debug_Message;

                        Param2 := Make_Designator (Arg_Name);
                        N := Add_Parameter (Entity_TC_Name, Param1);
                        Append_Node_To_List (N, Statements);
                        N := Add_Parameter (Entity_TC_Name, Param2);
                        Append_Node_To_List (N, Statements);

                        Declarator := Next_Entity (Declarator);
                     end loop;
                     Member := Next_Entity (Member);
                  end loop;
               end;

            when K_Exception_Declaration =>

               declare
                  Raise_From_Any_Access_Node : Node_Id;
                  Raise_From_Any_Name        : Name_Id;
                  Member                     : Node_Id;
                  Members                    : List_Id;
                  Declarator                 : Node_Id;
                  Dcl_Name                   : Name_Id;
                  Arg_Name_Node              : Node_Id;
                  Register_Excp_Node         : constant Node_Id :=
                    RE (RE_Register_Exception);
               begin

                  --  Add a dependency to initialize correctly the
                  --  modules

                  --  FIXME : Handle dependancy
                  --  Helpers.Package_Body.Add_Dependency
                  --    (Parent_Unit_Name (Register_Excp_Node));

                  Debug_Message;

                  --  In case where the exception has members, we add
                  --  two two parameter for each member.

                  Members := FEN.Members (E);
                  if not FEU.Is_Empty (Members) then
                     Member := First_Entity (Members);
                     while Present (Member) loop
                        Declarator := First_Entity (Declarators (Member));
                        while Present (Declarator) loop

                           --  Declaring the Arg_Name_"member"
                           --  variable

                           Dcl_Name := To_Ada_Name
                             (IDL_Name (FEN.Identifier (Declarator)));
                           Set_Str_To_Name_Buffer ("Arg_Name_");
                           Get_Name_String_And_Append (Dcl_Name);
                           Arg_Name_Node := Make_Defining_Identifier
                             (Name_Find);

                           --  Obtaining a string literal of the
                           --  member name

                           N := Make_Subprogram_Call
                             (RE (RE_To_CORBA_String),
                              Make_List_Id
                              (Make_Literal
                               (New_Value
                                (Value_Type'
                                 (K    => K_String,
                                  SVal => Dcl_Name)))));

                           N := Make_Object_Declaration
                             (Defining_Identifier => Arg_Name_Node,
                              Object_Definition   => RE (RE_String_0),
                              Expression          => N);
                           Append_Node_To_List (N, Declaration_List);

                           --  Adding the two additional parameters

                           N := Get_TC_Node (Type_Spec (Member));

                           --  FIXME : Handle dependancy
                           --  Helpers.Package_Body.Add_Dependency
                           --    (Parent_Unit_Name (N));

                           Debug_Message;

                           N := Add_Parameter (Entity_TC_Name, N);
                           Append_Node_To_List (N, Statements);
                           N := Add_Parameter (Entity_TC_Name, Arg_Name_Node);
                           Append_Node_To_List (N, Statements);

                           Declarator := Next_Entity (Declarator);
                        end loop;
                        Member := Next_Entity (Member);
                     end loop;
                  end if;

                  --  Adding the call to the "Register_Exception"
                  --  procedure

                  Raise_From_Any_Access_Node := Helper_Node
                    (BE_Node (Identifier (E)));
                  Raise_From_Any_Access_Node := Next_Node
                    (Next_Node
                     (Next_Node
                      (Raise_From_Any_Access_Node)));
                  Raise_From_Any_Access_Node := Defining_Identifier
                    (Raise_From_Any_Access_Node);

                  --  The following workaround is due to the fact that
                  --  we have no direct access to the
                  --  "Exception_Name"_Raise_From_Any procedure node
                  --  because its spec is declared in the helper body
                  --  and not in the helper spec and is not used
                  --  outside the helper package.

                  Raise_From_Any_Name := BEN.Name (Raise_From_Any_Access_Node);
                  Raise_From_Any_Name := Add_Suffix_To_Name
                    ("_From_Any",  Raise_From_Any_Name);
                  Raise_From_Any_Access_Node := Make_Designator
                    (Raise_From_Any_Name);
                  Raise_From_Any_Access_Node := Make_Attribute_Designator
                    (Raise_From_Any_Access_Node, A_Access);
                  N := Make_Subprogram_Call
                    (RE (RE_To_PolyORB_Object),
                     Make_List_Id
                     (Make_Designator
                      (Entity_TC_Name)));
                  N := Make_Subprogram_Call
                    (Register_Excp_Node,
                     Make_List_Id
                     (N, Raise_From_Any_Access_Node));
                  Append_Node_To_List (N, Statements);
               end;

            when K_Simple_Declarator =>
               declare
                  T : Node_Id;
               begin
                  T := Type_Spec (Declaration (E));
                  if Is_Base_Type (T)
                    or else FEN.Kind (T) = K_Scoped_Name then
                     N := Get_TC_Node (T);
                  elsif FEN.Kind (T) = K_Fixed_Point_Type then

                     --  For types defined basing on a fixed point
                     --  type, we use the TypeCode constant of the
                     --  fixed point type.

                     N := Get_TC_Fixed_Point (T);
                  elsif FEN.Kind (T) = K_Sequence_Type then
                     N := Expand_Designator
                       (TC_Node
                        (BE_Ada_Instanciations
                         (BE_Node
                          (Identifier (E)))));
                  elsif Kind (T) = K_String_Type or else
                    Kind (T) = K_Wide_String_Type then
                     declare
                        Pkg_Inst : constant Node_Id :=
                          (Defining_Identifier
                           (Stub_Package_Node
                            (BE_Ada_Instanciations
                             (BE_Node
                              (Identifier (E))))));
                     begin

                        --  Getting the identifir of the TypeCode
                        --  function located in the instanciated
                        --  package Bounded_...  in the stub spec

                        if Kind (T) = K_String_Type then
                           N := RE (RE_TC_Bounded_String);
                        else
                           N := RE (RE_TC_Bounded_Wide_String);
                        end if;
                        Set_Correct_Parent_Unit_Name (N, Copy_Node (Pkg_Inst));
                     end;
                  else
                     raise Program_Error;
                  end if;
                  --  FIXME : Handle dependancy
                  --  Helpers.Package_Body.Add_Dependency
                  --    (Parent_Unit_Name (N));

                  Debug_Message;

                  N := Add_Parameter (Entity_TC_Name, N);
                  Append_Node_To_List (N, Statements);
               end;

            when others =>
               null;
         end case;
      end Initialize_Routine;

      -----------
      -- Visit --
      -----------

      procedure Visit (E : Node_Id) is
      begin
         case FEN.Kind (E) is
            when K_Enumeration_Type =>
               Visit_Enumeration_Type (E);

            when K_Forward_Interface_Declaration =>
               Visit_Forward_Interface_Declaration (E);

            when K_Interface_Declaration =>
               Visit_Interface_Declaration (E);

            when K_Module =>
               Visit_Module (E);

            when K_Specification =>
               Visit_Specification (E);

            when K_Structure_Type =>
               Visit_Structure_Type (E);

            when K_Type_Declaration =>
               Visit_Type_Declaration (E);

            when K_Union_Type =>
               Visit_Union_Type (E);

            when K_Exception_Declaration =>
               Visit_Exception_Declaration (E);

            when others =>
               null;
         end case;
      end Visit;

      ----------------------------
      -- Visit_Enumeration_Type --
      ----------------------------

      procedure Visit_Enumeration_Type (E : Node_Id) is
         N : Node_Id;
      begin
         Set_Init_Body;

         N := Initialize_Body (E);
         Append_Node_To_List (N, Statements (Current_Package));
      end Visit_Enumeration_Type;

      -----------------------------------------
      -- Visit_Forward_Interface_Declaration --
      -----------------------------------------

      procedure Visit_Forward_Interface_Declaration (E : Node_Id) is
         N : Node_Id;
      begin
         Set_Init_Body;

         N := Initialize_Body (E);
         Append_Node_To_List (N, Statements (Current_Package));
      end Visit_Forward_Interface_Declaration;

      ---------------------------------
      -- Visit_Interface_Declaration --
      ---------------------------------

      procedure Visit_Interface_Declaration (E : Node_Id) is
         N : Node_Id;
      begin
         N := BEN.Parent (Type_Def_Node (BE_Node (Identifier (E))));
         Push_Entity (BEN.IDL_Unit (Package_Declaration (N)));
         Set_Init_Body;

         N := Initialize_Body (E);
         Append_Node_To_List (N, Statements (Current_Package));

         N := First_Entity (Interface_Body (E));
         while Present (N) loop
            Visit (N);
            N := Next_Entity (N);
         end loop;

         Pop_Entity;
      end Visit_Interface_Declaration;

      ------------------
      -- Visit_Module --
      ------------------

      procedure Visit_Module (E : Node_Id) is
         D : Node_Id;
      begin
         if not Map_Particular_CORBA_Parts (E, PK_Init_Body) then
            Push_Entity (Stub_Node (BE_Node (Identifier (E))));
            D := First_Entity (Definitions (E));
            while Present (D) loop
               Visit (D);
               D := Next_Entity (D);
            end loop;
            Pop_Entity;
         end if;
      end Visit_Module;

      -------------------------
      -- Visit_Specification --
      -------------------------

      procedure Visit_Specification (E : Node_Id) is
         D : Node_Id;
      begin
         Push_Entity (Stub_Node (BE_Node (Identifier (E))));
         D := First_Entity (Definitions (E));
         while Present (D) loop
            Visit (D);
            D := Next_Entity (D);
         end loop;
         Pop_Entity;
      end Visit_Specification;

      --------------------------
      -- Visit_Structure_Type --
      --------------------------

      procedure Visit_Structure_Type (E : Node_Id) is
         N          : Node_Id;
         Member     : Node_Id;
         Declarator : Node_Id;
      begin
         Set_Init_Body;

         --  See the comment message in
         --  Helpers.Package_Spec.Visit_Structure_Type for more
         --  details on the instructions below

         Member := First_Entity (Members (E));
         while Present (Member) loop
            Declarator := First_Entity (Declarators (Member));
            while Present (Declarator) loop
               if FEN.Kind (Declarator) = K_Complex_Declarator then

                  N := Initialize_Body (Declarator);
                  Append_Node_To_List (N, Statements (Current_Package));
               end if;

               Declarator := Next_Entity (Declarator);
            end loop;
            Member := Next_Entity (Member);
         end loop;

         N := Initialize_Body (E);
         Append_Node_To_List (N, Statements (Current_Package));
      end Visit_Structure_Type;

      ----------------------------
      -- Visit_Type_Declaration --
      ----------------------------

      procedure Visit_Type_Declaration (E : Node_Id) is
         N : Node_Id;
         D : Node_Id;
      begin
         Set_Init_Body;

         D := First_Entity (Declarators (E));
         while Present (D) loop
            N := Initialize_Body (D);
            Append_Node_To_List (N, Statements (Current_Package));

            D := Next_Entity (D);
         end loop;
      end Visit_Type_Declaration;

      ----------------------
      -- Visit_Union_Type --
      ----------------------

      procedure Visit_Union_Type (E : Node_Id) is
         N            : Node_Id;
         Alternatives : List_Id;
         Alternative  : Node_Id;
         Declarator   : Node_Id;
      begin
         Set_Init_Body;

         --  See the comment message in
         --  Helpers.Package_Spec.Visit_Union_Type for more
         --  details on the instructions below

         Alternatives := Switch_Type_Body (E);
         Alternative := First_Entity (Alternatives);
         while Present (Alternative) loop
            Declarator := FEN.Declarator (FEN.Element (Alternative));
            if FEN.Kind (Declarator) = K_Complex_Declarator then
               N := Initialize_Body (Declarator);
               Append_Node_To_List (N, Statements (Current_Package));
            end if;

            Alternative := Next_Entity (Alternative);
         end loop;

         N := Initialize_Body (E);
         Append_Node_To_List (N, Statements (Current_Package));
      end Visit_Union_Type;

      ---------------------------------
      -- Visit_Exception_Declaration --
      ---------------------------------

      procedure Visit_Exception_Declaration (E : Node_Id) is
         N : Node_Id;
      begin
         Set_Init_Body;

         N := Initialize_Body (E);
         Append_Node_To_List (N, Statements (Current_Package));
      end Visit_Exception_Declaration;

   end Package_Body;

end Backend.BE_CORBA_Ada.Initializers;

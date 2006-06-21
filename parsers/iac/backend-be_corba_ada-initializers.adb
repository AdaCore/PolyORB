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

with Frontend.Nodes;  use Frontend.Nodes;

with Backend.BE_CORBA_Ada.Nodes;       use Backend.BE_CORBA_Ada.Nodes;
with Backend.BE_CORBA_Ada.Nutils;      use Backend.BE_CORBA_Ada.Nutils;
with Backend.BE_CORBA_Ada.IDL_To_Ada;  use Backend.BE_CORBA_Ada.IDL_To_Ada;
with Backend.BE_CORBA_Ada.Runtime;     use Backend.BE_CORBA_Ada.Runtime;

package body Backend.BE_CORBA_Ada.Initializers is

   package FEN renames Frontend.Nodes;
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
         --  application and its not supposed to be done bt many tasks

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
         pragma Unreferenced (E);
         pragma Unreferenced (Declaration_List);
         pragma Unreferenced (Statements);
      begin
         --  FIXME : To be completed
         null;
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

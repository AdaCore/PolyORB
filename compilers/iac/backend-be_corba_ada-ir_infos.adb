------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--        B A C K E N D . B E _ C O R B A _ A D A . I R _ I N F O S         --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2007-2008, Free Software Foundation, Inc.          --
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

with Values;    use Values;
with Locations; use Locations;

with Frontend.Nutils;
with Frontend.Nodes;    use Frontend.Nodes;

with Backend.BE_CORBA_Ada.IDL_To_Ada; use Backend.BE_CORBA_Ada.IDL_To_Ada;
with Backend.BE_CORBA_Ada.Nodes;      use Backend.BE_CORBA_Ada.Nodes;
with Backend.BE_CORBA_Ada.Nutils;     use Backend.BE_CORBA_Ada.Nutils;
with Backend.BE_CORBA_Ada.Runtime;    use Backend.BE_CORBA_Ada.Runtime;

package body Backend.BE_CORBA_Ada.IR_Infos is

   package FEN renames Frontend.Nodes;
   package BEN renames Backend.BE_CORBA_Ada.Nodes;
   package FEU renames Frontend.Nutils;

   ------------------
   -- Package_Spec --
   ------------------

   package body Package_Spec is

      procedure Visit_Attribute_Declaration (E : Node_Id);
      procedure Visit_Enumeration_Type (E : Node_Id);
      procedure Visit_Exception_Declaration (E : Node_Id);
      procedure Visit_Interface_Declaration (E : Node_Id);
      procedure Visit_Module (E : Node_Id);
      procedure Visit_Operation_Declaration (E : Node_Id);
      procedure Visit_Specification (E : Node_Id);
      procedure Visit_Structure_Type (E : Node_Id);
      procedure Visit_Type_Declaration (E : Node_Id);
      procedure Visit_Union_Type (E : Node_Id);

      function IR_Function_Spec (E : Node_Id) return Node_Id;
      --  Create the subprogram specification of the IR_<Name>
      --  function relative to the IDL entity E.

      function Register_IR_Info_Spec (E : Node_Id) return Node_Id;
      --  Create the 'Register_IR_Info' subprogram specification that
      --  must be generated in the IR_Info package.

      ----------------------
      -- IR_Function_Spec --
      ----------------------

      function IR_Function_Spec (E : Node_Id) return Node_Id is
         N : Node_Id;
      begin
         N := Make_Subprogram_Specification
           (Defining_Identifier => Make_Defining_Identifier (Map_IR_Name (E)),
            Parameter_Profile   => No_List,
            Return_Type         => RE (RE_Ref_12));
         return N;
      end IR_Function_Spec;

      ---------------------------
      -- Register_IR_Info_Spec --
      ---------------------------

      function Register_IR_Info_Spec (E : Node_Id) return Node_Id is
         pragma Unreferenced (E);

         N : Node_Id;
      begin
         N := Make_Subprogram_Specification
           (Defining_Identifier => RE (RE_Register_IR_Info),
            Parameter_Profile   => No_List,
            Return_Type         => No_Node);
         return N;
      end Register_IR_Info_Spec;

      -----------
      -- Visit --
      -----------

      procedure Visit (E : Node_Id) is
      begin
         case FEN.Kind (E) is
            when K_Specification =>
               Visit_Specification (E);

            when K_Enumeration_Type =>
               Visit_Enumeration_Type (E);

            when K_Exception_Declaration =>
               Visit_Exception_Declaration (E);

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
         N : Node_Id;
      begin
         A := First_Entity (Declarators (E));

         while Present (A) loop
            N := IR_Function_Spec (A);
            Bind_FE_To_BE (FEN.Identifier (A), N, B_IR_Function);
            Append_To (Visible_Part (Current_Package), N);

            A := Next_Entity (A);
         end loop;
      end Visit_Attribute_Declaration;

      ----------------------------
      -- Visit_Enumeration_Type --
      ----------------------------

      procedure Visit_Enumeration_Type (E : Node_Id) is
         N : Node_Id;
      begin
         N := IR_Function_Spec (E);
         Bind_FE_To_BE (FEN.Identifier (E), N, B_IR_Function);
         Append_To (Visible_Part (Current_Package), N);
      end Visit_Enumeration_Type;

      ---------------------------------
      -- Visit_Exception_Declaration --
      ---------------------------------

      procedure Visit_Exception_Declaration (E : Node_Id) is
         N : Node_Id;
      begin
         N := IR_Function_Spec (E);
         Bind_FE_To_BE (FEN.Identifier (E), N, B_IR_Function);
         Append_To (Visible_Part (Current_Package), N);
      end Visit_Exception_Declaration;

      ---------------------------------
      -- Visit_Interface_Declaration --
      ---------------------------------

      procedure Visit_Interface_Declaration (E : Node_Id) is
         N : Node_Id;
      begin
         N := BEN.Parent (Type_Def_Node (BE_Node (Identifier (E))));
         Push_Entity (BEN.IDL_Unit (Package_Declaration (N)));
         Set_IR_Info_Spec;

         N := Register_IR_Info_Spec (E);
         Bind_FE_To_BE (FEN.Identifier (E), N, B_Register_IR_Info);
         Append_To (Visible_Part (Current_Package), N);

         N := First_Entity (Interface_Body (E));
         while Present (N) loop
            Visit (N);
            N := Next_Entity (N);
         end loop;

         N := IR_Function_Spec (E);
         Bind_FE_To_BE (FEN.Identifier (E), N, B_IR_Function);
         Append_To (Visible_Part (Current_Package), N);

         Pop_Entity;
      end Visit_Interface_Declaration;

      ------------------
      -- Visit_Module --
      ------------------

      procedure Visit_Module (E : Node_Id) is
         D : Node_Id;
         N : Node_Id;
      begin
         if not Map_Particular_CORBA_Parts (E, PK_IR_Info_Spec) then
            D := Stub_Node (BE_Node (Identifier (E)));
            Push_Entity (D);
            Set_IR_Info_Spec;

            N := Register_IR_Info_Spec (E);
            Bind_FE_To_BE (FEN.Identifier (E), N, B_Register_IR_Info);
            Append_To (Visible_Part (Current_Package), N);

            D := First_Entity (Definitions (E));

            while Present (D) loop
               Visit (D);
               D := Next_Entity (D);
            end loop;

            N := IR_Function_Spec (E);
            Bind_FE_To_BE (FEN.Identifier (E), N, B_IR_Function);
            Append_To (Visible_Part (Current_Package), N);

            Pop_Entity;
         end if;
      end Visit_Module;

      ---------------------------------
      -- Visit_Operation_Declaration --
      ---------------------------------

      procedure Visit_Operation_Declaration (E : Node_Id) is
         N : Node_Id;
      begin
         --  We do not generate IR informations for operations that
         --  are expanded from IDL attributes

         if FEN.Loc (Identifier (E)) = No_Location then
            return;
         end if;

         N := IR_Function_Spec (E);
         Bind_FE_To_BE (FEN.Identifier (E), N, B_IR_Function);
         Append_To (Visible_Part (Current_Package), N);
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
         N := IR_Function_Spec (E);
         Bind_FE_To_BE (FEN.Identifier (E), N, B_IR_Function);
         Append_To (Visible_Part (Current_Package), N);
      end Visit_Structure_Type;

      ----------------------------
      -- Visit_Type_Declaration --
      ----------------------------

      procedure Visit_Type_Declaration (E : Node_Id) is
         D : Node_Id;
         N : Node_Id;
      begin
         D := First_Entity (Declarators (E));

         while Present (D) loop
            N := IR_Function_Spec (D);
            Bind_FE_To_BE (FEN.Identifier (D), N, B_IR_Function);
            Append_To (Visible_Part (Current_Package), N);

            D := Next_Entity (D);
         end loop;
      end Visit_Type_Declaration;

      ----------------------
      -- Visit_Union_Type --
      ----------------------

      procedure Visit_Union_Type (E : Node_Id) is
         N : Node_Id;
      begin
         N := IR_Function_Spec (E);
         Bind_FE_To_BE (FEN.Identifier (E), N, B_IR_Function);
         Append_To (Visible_Part (Current_Package), N);
      end Visit_Union_Type;

   end Package_Spec;

   ------------------
   -- Package_Body --
   ------------------

   package body Package_Body is

      procedure Visit_Attribute_Declaration (E : Node_Id);
      procedure Visit_Enumeration_Type (E : Node_Id);
      procedure Visit_Exception_Declaration (E : Node_Id);
      procedure Visit_Interface_Declaration (E : Node_Id);
      procedure Visit_Module (E : Node_Id);
      procedure Visit_Operation_Declaration (E : Node_Id);
      procedure Visit_Specification (E : Node_Id);
      procedure Visit_Structure_Type (E : Node_Id);
      procedure Visit_Type_Declaration (E : Node_Id);
      procedure Visit_Union_Type (E : Node_Id);

      function IR_Function_Body
        (E        : Node_Id;
         For_Attr : Boolean := False)
        return Node_Id;
      --  Create the subprogram body of the IR_<Name> function
      --  relative to the IDL entity E. If For_Attr is True and If E
      --  is a simple declarator, then assume a is a declarator of an
      --  attribute declaration.

      function Register_IR_Info_Body (E : Node_Id) return Node_Id;
      --  Create the 'Register_IR_Info' subprogram body that must be
      --  generated in the IR_Info package.

      ----------------------
      -- IR_Function_Body --
      ----------------------

      function IR_Function_Body
        (E        : Node_Id;
         For_Attr : Boolean := False)
        return Node_Id
      is
         Spec         : constant Node_Id := Ir_Function_Node
           (BE_Node (Identifier (E)));
         Declarations : constant List_Id := New_List;
         Statements   : constant List_Id := New_List;
         N            : Node_Id;

         procedure Interface_Body;
         procedure Attribute_Body;
         procedure Declarator_Body;
         procedure Operation_Declaration_Body;
         procedure Module_Body;
         procedure Enumeration_Type_Body;
         procedure Structure_Exception_Body;
         procedure Union_Type_Body;

         procedure Parent_Container_Declaration;
         --  Declaration of a Container_Ref corresponding to the
         --  container corresponding to E's parent scope.

         procedure Parent_Container_Lookup;
         --  Statement to look up E's name in the container object
         --  that describes its parent scope.

         function Standard_Create_Parameters return List_Id;
         --  Create a list of the actual parameters that are common to
         --  all create_* operations: id, name, and version.

         function IDL_Type
           (Type_Spec  : Node_Id;
            Declarator : Node_Id)
           return Node_Id;
         --  Creates an IDLType object reference corresponding to the
         --  entity declared by declarator Declarator with the type
         --  denoted by Type_Spec (note, for arrays Type_Spec is the
         --  element type.) If Declarator is No_Node, no array bounds
         --  are assumed.

         function Array_IR
           (Elt_Type_Spec : Node_Id;
            Declarator    : Node_Id)
           return Node_Id;
         --  Create an ArrayDef IRObject

         function Fixed_IR (Type_Spec : Node_Id) return Node_Id;
         --  Create a FixedDef IRObject

         function Sequence_IR (Type_Spec : Node_Id) return Node_Id;
         --  Create a SequenceDef IRObject

         --------------------
         -- Interface_Body --
         --------------------

         procedure Interface_Body is
            Inner_Dcl : constant List_Id := New_List;
            Inner_St  : constant List_Id := New_List;
            Profile   : constant List_Id := New_List;
            N         : Node_Id;
            P         : Node_Id;
         begin
            Parent_Container_Lookup;

            N := Make_Object_Declaration
              (Defining_Identifier => Make_Defining_Identifier
                 (PN (P_Base_Ifs)),
               Object_Definition   => RE (RE_InterfaceDefSeq));
            Append_To (Inner_Dcl, N);

            P := First_Entity (Interface_Spec (E));

            while Present (P) loop
               N := Make_Subprogram_Call
                 (RE (RE_To_Ref_14),
                  New_List
                  (Get_IR_Function_Node
                   (P)));

               N := Make_Subprogram_Call
                 (RE (RE_To_Forward_2), New_List (N));

               N := Make_Subprogram_Call
                 (RE (RE_Append),
                  New_List
                  (Make_Identifier (PN (P_Base_Ifs)), N));

               Append_To (Inner_St, N);

               P := Next_Entity (P);
            end loop;

            N := Make_Identifier (PN (P_Container_Ref));
            Append_To (Profile, N);

            N := Make_Subprogram_Call
              (RE (RE_To_CORBA_String),
               New_List
               (Make_Identifier (Map_Repository_Id_Name (E))));
            Append_To (Profile, N);

            N := Make_Subprogram_Call
              (RE (RE_To_CORBA_String),
               New_List
               (Make_Literal
                (New_String_Value
                 (IDL_Name (Identifier (E)), False))));
            Append_To (Profile, N);

            N := Make_Subprogram_Call
              (RE (RE_To_CORBA_String_2),
               New_List
               (Make_Literal
                (New_String_Value
                 (Map_Type_Version (E), False))));
            Append_To (Profile, N);

            N := Make_Identifier (PN (P_Base_Ifs));
            Append_To (Profile, N);

            N := Make_Literal (New_Boolean_Value (False));
            Append_To (Profile, N);

            N := Make_Subprogram_Call (RE (RE_Create_Interface), Profile);
            N := Make_Subprogram_Call (RE (RE_To_Ref_12), New_List (N));
            N := Make_Assignment_Statement
              (Make_Identifier (Map_Cached_IR_Name (E)), N);
            Append_To (Inner_St, N);

            N := Make_Return_Statement
              (Make_Identifier (Map_Cached_IR_Name (E)));
            Append_To (Inner_St, N);

            N := Make_Block_Statement
              (Declarative_Part => Inner_Dcl,
               Statements       => Inner_St);
            Append_To (Statements, N);
         end Interface_Body;

         ---------------------
         -- Declarator_Body --
         ---------------------

         procedure Declarator_Body is
            D                 : constant Node_Id := Declaration (E);
            Create_Parameters : constant List_Id := Standard_Create_Parameters;
            Profile           : constant List_Id := New_List;
            N                 : Node_Id;
         begin
            Parent_Container_Lookup;

            N := Make_Identifier (PN (P_Container_Ref));
            Append_To (Profile, N);

            Append_To (Profile, First_Node (Create_Parameters));

            N := Make_Parameter_Association
              (Make_Identifier (PN (P_Original_Type)),
               Make_Subprogram_Call
               (RE (RE_To_Forward),
                New_List
                (IDL_Type (Type_Spec (D), E))));
            Append_To (Profile, N);

            N := Make_Subprogram_Call
              (RE (RE_Create_Alias), Profile);
            N := Make_Subprogram_Call (RE (RE_To_Ref_12), New_List (N));
            N := Make_Assignment_Statement
              (Make_Identifier (Map_Cached_IR_Name (E)), N);
            Append_To (Statements, N);

            N := Make_Return_Statement
              (Make_Identifier (Map_Cached_IR_Name (E)));
            Append_To (Statements, N);
         end Declarator_Body;

         --------------------
         -- Attribute_Body --
         --------------------

         procedure Attribute_Body is
            A                 : constant Node_Id := Declaration (E);
            Create_Parameters : constant List_Id := Standard_Create_Parameters;
            Profile           : constant List_Id := New_List;
            N                 : Node_Id;
         begin
            Parent_Container_Lookup;

            N := Make_Subprogram_Call
              (RE (RE_To_Ref_14),
               New_List (Make_Identifier (PN (P_Container_Ref))));
            Append_To (Profile, N);

            Append_To (Profile, First_Node (Create_Parameters));

            N := Make_Parameter_Association
              (Make_Identifier (PN (P_IDL_Type)),
               IDL_Type (Type_Spec (A), No_Node));
            Append_To (Profile, N);

            if Is_Readonly (A) then
               N := RE (RE_ATTR_READONLY);
            else
               N := RE (RE_ATTR_NORMAL);
            end if;

            N := Make_Parameter_Association (Make_Identifier (PN (P_Mode)), N);
            Append_To (Profile, N);

            N := Make_Subprogram_Call
              (RE (RE_Create_Attribute), Profile);
            N := Make_Subprogram_Call (RE (RE_To_Ref_12), New_List (N));
            N := Make_Assignment_Statement
              (Make_Identifier (Map_Cached_IR_Name (E)), N);
            Append_To (Statements, N);

            N := Make_Return_Statement
              (Make_Identifier (Map_Cached_IR_Name (E)));
            Append_To (Statements, N);
         end Attribute_Body;

         --------------------------------
         -- Operation_Declaration_Body --
         --------------------------------

         procedure Operation_Declaration_Body is
            Inner_Dcl : constant List_Id := New_List;
            Inner_St  : constant List_Id := New_List;
            Create_Parameters : constant List_Id := Standard_Create_Parameters;
            Profile   : constant List_Id := New_List;
            Aggr      : List_Id;
            N         : Node_Id;
            P         : Node_Id;
            M         : Node_Id;
         begin
            Parent_Container_Lookup;

            N := Make_Object_Declaration
              (Defining_Identifier => Make_Defining_Identifier
                 (PN (P_Params)),
               Object_Definition   => RE (RE_ParDescriptionSeq));
            Append_To (Inner_Dcl, N);

            if not FEU.Is_Empty (Parameters (E)) then
               P := First_Entity (Parameters (E));

               while Present (P) loop
                  Aggr := New_List;

                  N := Make_Element_Association
                    (Make_Identifier (CN (C_Name)),
                     Make_Subprogram_Call
                     (RE (RE_To_CORBA_String),
                      New_List
                      (Make_Literal
                       (New_String_Value
                        (To_Ada_Name
                         (FEN.IDL_Name
                          (Identifier
                           (Declarator (P)))),
                         False)))));
                  Append_To (Aggr, N);

                  N := Make_Element_Association
                    (Make_Identifier (CN (C_IDL_Type)),
                     Get_TC_Node (Type_Spec (P)));
                  Append_To (Aggr, N);

                  N := Make_Element_Association
                    (Make_Identifier (CN (C_Type_Def)),
                     Make_Subprogram_Call
                     (RE (RE_To_Forward),
                      New_List
                      (IDL_Type (Type_Spec (P), E))));
                  Append_To (Aggr, N);

                  case FEN.Parameter_Mode (P) is
                     when Mode_In =>
                        M := RE (RE_PARAM_IN);
                     when Mode_Inout =>
                        M := RE (RE_PARAM_INOUT);
                     when Mode_Out =>
                        M := RE (RE_PARAM_OUT);
                  end case;

                  N := Make_Element_Association
                    (Make_Identifier (CN (C_Mode)), M);
                  Append_To (Aggr, N);

                  N := Make_Qualified_Expression
                    (RE (RE_ParameterDescription),
                     Make_Record_Aggregate (Aggr));

                  N := Make_Subprogram_Call
                    (RE (RE_Append),
                     New_List
                     (Make_Identifier (PN (P_Params)), N));
                  Append_To (Inner_St, N);

                  P := Next_Entity (P);
               end loop;
            end if;

            N := Make_Object_Declaration
              (Defining_Identifier => Make_Defining_Identifier
                 (PN (P_Exceptions)),
               Object_Definition   => RE (RE_ExceptionDefSeq));
            Append_To (Inner_Dcl, N);

            --  XXX TODO
--          if FEU.Is_Empty (Exceptions (E)) then
--             X := First_Entity (Exceptions (E));

--             while Present (X) loop

--                X := Next_Entity (X);
--             end loop;
--          end if;

            N := Make_Object_Declaration
              (Defining_Identifier => Make_Defining_Identifier
                 (PN (P_Contexts)),
               Object_Definition   => RE (RE_ContextIdSeq));
            Append_To (Inner_Dcl, N);

            --  XXX TODO
--          if FEU.Is_Empty (Contexts (E)) then
--             C := First_Entity (Exceptions (E));

--             while Present (C) loop

--                C := Next_Entity (C);
--             end loop;
--          end if;

            N := Make_Subprogram_Call
              (RE (RE_To_Ref_14),
               New_List (Make_Identifier (PN (P_Container_Ref))));
            Append_To (Profile, N);

            Append_To (Profile, First_Node (Create_Parameters));

            N := Make_Parameter_Association
              (Make_Identifier (PN (P_Result)),
               Make_Subprogram_Call
               (RE (RE_To_Ref_15),
                New_List
                (Get_IR_Function_Node
                 (Type_Spec (E)))));
            Append_To (Profile, N);

            if Is_Oneway (E) then
               M := RE (RE_OP_ONEWAY);
            else
               M := RE (RE_OP_NORMAL);
            end if;

            N := Make_Parameter_Association (Make_Identifier (PN (P_Mode)), M);
            Append_To (Profile, N);

            N := Make_Parameter_Association
              (Make_Identifier (PN (P_Params)),
               Make_Identifier (PN (P_Params)));
            Append_To (Profile, N);

            N := Make_Parameter_Association
              (Make_Identifier (PN (P_Exceptions)),
               Make_Identifier (PN (P_Exceptions)));
            Append_To (Profile, N);

            N := Make_Parameter_Association
              (Make_Identifier (PN (P_Contexts)),
               Make_Identifier (PN (P_Contexts)));
            Append_To (Profile, N);

            N := Make_Subprogram_Call (RE (RE_Create_Operation), Profile);
            N := Make_Subprogram_Call (RE (RE_To_Ref_12), New_List (N));
            N := Make_Assignment_Statement
              (Make_Identifier (Map_Cached_IR_Name (E)), N);
            Append_To (Inner_St, N);

            N := Make_Return_Statement
              (Make_Identifier (Map_Cached_IR_Name (E)));
            Append_To (Inner_St, N);

            N := Make_Block_Statement
              (Declarative_Part => Inner_Dcl,
               Statements       => Inner_St);
            Append_To (Statements, N);
         end Operation_Declaration_Body;

         -----------------
         -- Module_Body --
         -----------------

         procedure Module_Body is
            Profile : constant List_Id := New_List;
            N       : Node_Id;
         begin
            Parent_Container_Lookup;

            N := Make_Identifier (PN (P_Container_Ref));
            Append_To (Profile, N);

            N := Make_Subprogram_Call
              (RE (RE_To_CORBA_String),
               New_List
               (Make_Identifier (Map_Repository_Id_Name (E))));
            Append_To (Profile, N);

            N := Make_Subprogram_Call
              (RE (RE_To_CORBA_String),
               New_List
               (Make_Literal
                (New_String_Value
                 (IDL_Name (Identifier (E)), False))));
            Append_To (Profile, N);

            N := Make_Subprogram_Call
              (RE (RE_To_CORBA_String_2),
               New_List
               (Make_Literal
                (New_String_Value
                 (Map_Type_Version (E), False))));
            Append_To (Profile, N);

            N := Make_Subprogram_Call (RE (RE_Create_Module), Profile);
            N := Make_Subprogram_Call (RE (RE_To_Ref_12), New_List (N));
            N := Make_Assignment_Statement
              (Make_Identifier (Map_Cached_IR_Name (E)), N);
            Append_To (Statements, N);

            N := Make_Return_Statement
              (Make_Identifier (Map_Cached_IR_Name (E)));
            Append_To (Statements, N);
         end Module_Body;

         ---------------------------
         -- Enumeration_Type_Body --
         ---------------------------

         procedure Enumeration_Type_Body is
            Inner_Dcl  : constant List_Id := New_List;
            Inner_St   : constant List_Id := New_List;
            Profile    : constant List_Id := New_List;
            N          : Node_Id;
            Enumerator : Node_Id;
         begin
            Parent_Container_Lookup;

            N := Make_Object_Declaration
              (Defining_Identifier => Make_Defining_Identifier
                 (PN (P_Members)),
               Object_Definition   => RE (RE_EnumMemberSeq));
            Append_To (Inner_Dcl, N);

            Enumerator := First_Entity (Enumerators (E));

            while Present (Enumerator) loop
               N := Make_Subprogram_Call
                 (RE (RE_Append),
                  New_List
                  (Make_Identifier (PN (P_Members)),
                   Make_Subprogram_Call
                   (RE (RE_To_CORBA_String),
                    New_List
                    (Make_Literal
                     (New_String_Value
                      (To_Ada_Name
                       (IDL_Name
                        (Identifier
                         (Enumerator))),
                       False))))));
               Append_To (Inner_St, N);

               Enumerator := Next_Entity (Enumerator);
            end loop;

            N := Make_Identifier (PN (P_Container_Ref));
            Append_To (Profile, N);

            N := Make_Subprogram_Call
              (RE (RE_To_CORBA_String),
               New_List
               (Make_Identifier (Map_Repository_Id_Name (E))));
            Append_To (Profile, N);

            N := Make_Subprogram_Call
              (RE (RE_To_CORBA_String),
               New_List
               (Make_Literal
                (New_String_Value
                 (IDL_Name (Identifier (E)), False))));
            Append_To (Profile, N);

            N := Make_Subprogram_Call
              (RE (RE_To_CORBA_String_2),
               New_List
               (Make_Literal
                (New_String_Value
                 (Map_Type_Version (E), False))));
            Append_To (Profile, N);

            N := Make_Identifier (PN (P_Members));
            Append_To (Profile, N);

            N := Make_Subprogram_Call (RE (RE_Create_Enum), Profile);
            N := Make_Subprogram_Call (RE (RE_To_Ref_12), New_List (N));
            N := Make_Assignment_Statement
              (Make_Identifier (Map_Cached_IR_Name (E)), N);
            Append_To (Inner_St, N);

            N := Make_Return_Statement
              (Make_Identifier (Map_Cached_IR_Name (E)));
            Append_To (Inner_St, N);

            N := Make_Block_Statement
              (Declarative_Part => Inner_Dcl,
               Statements       => Inner_St);
            Append_To (Statements, N);
         end Enumeration_Type_Body;

         ------------------------------
         -- Structure_Exception_Body --
         ------------------------------

         procedure Structure_Exception_Body is
            Inner_Dcl  : constant List_Id := New_List;
            Inner_St   : constant List_Id := New_List;
            Create_Parameters : constant List_Id := Standard_Create_Parameters;
            Profile    : constant List_Id := New_List;
            Aggr       : List_Id;
            N          : Node_Id;
            Member     : Node_Id;
            Declarator : Node_Id;
            C          : Node_Id;
         begin
            Parent_Container_Lookup;

            N := Make_Object_Declaration
              (Defining_Identifier => Make_Defining_Identifier
                 (PN (P_Members)),
               Object_Definition   => RE (RE_StructMemberSeq));
            Append_To (Inner_Dcl, N);

            Member := First_Entity (Members (E));

            while Present (Member) loop
               Declarator := First_Entity (Declarators (Member));

               while Present (Declarator) loop
                  Aggr := New_List;

                  N := Make_Element_Association
                    (Make_Identifier (CN (C_Name)),
                     Make_Subprogram_Call
                     (RE (RE_To_CORBA_String),
                      New_List
                      (Make_Literal
                       (New_String_Value
                        (To_Ada_Name
                         (FEN.IDL_Name
                          (Identifier
                           (Declarator))),
                         False)))));
                  Append_To (Aggr, N);

                  N := Make_Element_Association
                    (Make_Identifier (CN (C_IDL_Type)),
                     Get_TC_Node (Type_Spec (Member)));
                  Append_To (Aggr, N);

                  N := Make_Element_Association
                    (Make_Identifier (CN (C_Type_Def)),
                     Make_Subprogram_Call
                     (RE (RE_To_Forward),
                      New_List
                      (IDL_Type (Type_Spec (Member), Declarator))));
                  Append_To (Aggr, N);

                  N := Make_Qualified_Expression
                    (RE (RE_StructMember),
                     Make_Record_Aggregate (Aggr));

                  N := Make_Subprogram_Call
                    (RE (RE_Append),
                     New_List
                     (Make_Identifier (PN (P_Members)), N));
                  Append_To (Inner_St, N);

                  Declarator := Next_Entity (Declarator);
               end loop;

               Member := Next_Entity (Member);
            end loop;

            N := Make_Identifier (PN (P_Container_Ref));
            Append_To (Profile, N);

            Append_To (Profile, First_Node (Create_Parameters));

            N := Make_Parameter_Association
              (Make_Identifier (PN (P_Members)),
               Make_Identifier (PN (P_Members)));
            Append_To (Profile, N);

            if FEN.Kind (E) = K_Structure_Type then
               C := RE (RE_Create_Struct);
            else
               C := RE (RE_Create_Exception);
            end if;

            N := Make_Subprogram_Call (C, Profile);
            N := Make_Subprogram_Call (RE (RE_To_Ref_12), New_List (N));
            N := Make_Assignment_Statement
              (Make_Identifier (Map_Cached_IR_Name (E)), N);
            Append_To (Inner_St, N);

            N := Make_Return_Statement
              (Make_Identifier (Map_Cached_IR_Name (E)));
            Append_To (Inner_St, N);

            N := Make_Block_Statement
              (Declarative_Part => Inner_Dcl,
               Statements       => Inner_St);
            Append_To (Statements, N);
         end Structure_Exception_Body;

         ---------------------
         -- Union_Type_Body --
         ---------------------

         procedure Union_Type_Body is
            Inner_Dcl      : constant List_Id := New_List;
            Inner_St       : constant List_Id := New_List;
            Create_Parameters : constant List_Id := Standard_Create_Parameters;
            Profile        : constant List_Id := New_List;
            Orig_Type      : constant Node_Id :=
              FEU.Get_Original_Type_Specifier (Switch_Type_Spec (E));
            Switch_Type    : Node_Id;
            Literal_Parent : Node_Id := No_Node;

            Aggr        : List_Id;
            N           : Node_Id;
            Alternative : Node_Id;
            Label       : Node_Id;
         begin
            if Is_Base_Type (Switch_Type_Spec (E)) then
               Switch_Type :=
                 RE (Convert (FEN.Kind (Switch_Type_Spec (E))));

            elsif FEN.Kind (Orig_Type) = K_Enumeration_Type then
               Switch_Type := Map_Expanded_Name (Switch_Type_Spec (E));
               Literal_Parent := Map_Expanded_Name
                 (Scope_Entity
                  (Identifier
                   (Orig_Type)));
            else
               Switch_Type := Map_Expanded_Name (Switch_Type_Spec (E));
            end if;

            Parent_Container_Lookup;

            N := Make_Object_Declaration
              (Defining_Identifier => Make_Defining_Identifier
                 (PN (P_Members)),
               Object_Definition   => RE (RE_UnionMemberSeq));
            Append_To (Inner_Dcl, N);

            Alternative := First_Entity (Switch_Type_Body (E));

            while Present (Alternative) loop
               Label := First_Entity (Labels (Alternative));

               while Present (Label) loop
                  Aggr := New_List;

                  N := Make_Element_Association
                    (Make_Identifier (CN (C_Name)),
                     Make_Subprogram_Call
                     (RE (RE_To_CORBA_String),
                      New_List
                      (Make_Literal
                       (New_String_Value
                        (To_Ada_Name
                         (FEN.IDL_Name
                          (Identifier
                           (Declarator
                            (Element
                             (Alternative))))),
                         False)))));
                  Append_To (Aggr, N);

                  if FEU.Expr_Value (Label) = No_Value then
                     N := Make_Subprogram_Call
                       (RE (RE_To_Any_0),
                        New_List
                        (Make_Qualified_Expression
                         (RE (RE_Octet),
                          Make_Literal (Int0_Val))));
                  else
                     N := Make_Subprogram_Call
                       (Get_To_Any_Node (Switch_Type_Spec (E)),
                        New_List
                        (Make_Qualified_Expression
                         (Switch_Type,
                          Make_Literal_With_Parent
                          (FEU.Expr_Value (Label),
                           Parent => Literal_Parent))));
                  end if;

                  N := Make_Parameter_Association
                    (Make_Identifier (PN (P_Label)), N);
                  Append_To (Aggr, N);

                  N := Make_Element_Association
                    (Make_Identifier (CN (C_IDL_Type)),
                     Get_TC_Node
                     (Type_Spec
                      (Element
                       (Alternative))));
                  Append_To (Aggr, N);

                  N := Make_Element_Association
                    (Make_Identifier (CN (C_Type_Def)),
                     Make_Subprogram_Call
                     (RE (RE_To_Forward),
                      New_List
                      (IDL_Type
                       (Type_Spec
                        (Element
                         (Alternative)),
                        Declarator
                        (Element
                         (Alternative))))));
                  Append_To (Aggr, N);

                  N := Make_Qualified_Expression
                    (RE (RE_UnionMember),
                     Make_Record_Aggregate (Aggr));

                  N := Make_Subprogram_Call
                    (RE (RE_Append),
                     New_List
                     (Make_Identifier (PN (P_Members)), N));
                  Append_To (Inner_St, N);

                  Label := Next_Entity (Label);
               end loop;

               Alternative := Next_Entity (Alternative);
            end loop;

            N := Make_Identifier (PN (P_Container_Ref));
            Append_To (Profile, N);

            Append_To (Profile, First_Node (Create_Parameters));

            N := Make_Parameter_Association
              (Make_Identifier (PN (P_Discriminator_Type)),
               Make_Subprogram_Call
               (RE (RE_To_Forward),
                New_List
                (Make_Subprogram_Call
                 (RE (RE_To_Ref_15),
                  New_List
                  (Get_IR_Function_Node
                   (Switch_Type_Spec (E)))))));
            Append_To (Profile, N);

            N := Make_Parameter_Association
              (Make_Identifier (PN (P_Members)),
               Make_Identifier (PN (P_Members)));
            Append_To (Profile, N);

            N := Make_Subprogram_Call (RE (RE_Create_Union), Profile);
            N := Make_Subprogram_Call (RE (RE_To_Ref_12), New_List (N));
            N := Make_Assignment_Statement
              (Make_Identifier (Map_Cached_IR_Name (E)), N);
            Append_To (Inner_St, N);

            N := Make_Return_Statement
              (Make_Identifier (Map_Cached_IR_Name (E)));
            Append_To (Inner_St, N);

            N := Make_Block_Statement
              (Declarative_Part => Inner_Dcl,
               Statements       => Inner_St);
            Append_To (Statements, N);
         end Union_Type_Body;

         ----------------------------------
         -- Parent_Container_Declaration --
         ----------------------------------

         procedure Parent_Container_Declaration is
            Parent_Scope      : constant Node_Id :=
              Scope_Entity (FEN.Identifier (E));
            Parent_Scope_Kind : constant FEN.Node_Kind :=
              FEN.Kind (Parent_Scope);
            N                 : Node_Id;
            Expression        : Node_Id;
         begin
            if Parent_Scope_Kind /= K_Interface_Declaration and then
              Parent_Scope_Kind /= K_Value_Declaration      and then
              Parent_Scope_Kind  /= K_Module
            then
               Expression := Make_Subprogram_Call
                 (RE (RE_To_Ref_13),
                  New_List
                  (RE (RE_Get_IR_Root)));
            else
               --  Parent_Scope corresponds to a container in the IR
               --  sense.

               Expression := Make_Subprogram_Call
                 (RE (RE_To_Ref_13),
                  New_List
                  (Get_IR_Function_Node
                   (Parent_Scope)));
            end if;

            N := Make_Object_Declaration
              (Defining_Identifier => Make_Defining_Identifier
                 (PN (P_Container_Ref)),
               Constant_Present    => True,
               Object_Definition   => RE (RE_Ref_13),
               Expression          => Expression);
            Append_To (Declarations, N);
         end Parent_Container_Declaration;

         -----------------------------
         -- Parent_Container_Lookup --
         -----------------------------

         procedure Parent_Container_Lookup is
            N : Node_Id;
         begin
            N := Make_Subprogram_Call
              (RE (RE_To_CORBA_String),
               New_List
               (Make_Literal
                (New_String_Value
                 (IDL_Name (Identifier (E)), False))));

            N := Make_Subprogram_Call
              (RE (RE_Lookup),
               New_List
               (Make_Identifier (PN (P_Container_Ref)), N));

            N := Make_Subprogram_Call
              (RE (RE_To_Ref_12), New_List (N));

            N := Make_Assignment_Statement
              (Make_Identifier (Map_Cached_IR_Name (E)), N);

            Append_To (Statements, N);

            N := Make_If_Statement
              (Condition       => Make_Subprogram_Call
                 (RE (RE_Is_Nil_12),
                  New_List
                  (Make_Identifier
                   (Map_Cached_IR_Name (E)))),
               Then_Statements => New_List
                 (Make_Return_Statement
                  (Make_Identifier
                   (Map_Cached_IR_Name (E)))));
            Append_To (Statements, N);
         end Parent_Container_Lookup;

         --------------------------------
         -- Standard_Create_Parameters --
         --------------------------------

         function Standard_Create_Parameters return List_Id is
            Result : constant List_Id := New_List;
         begin
            N := Make_Subprogram_Call
              (RE (RE_To_CORBA_String),
               New_List
               (Make_Literal
                (New_String_Value (Map_Repository_Id_Name (E), False))));
            N := Make_Parameter_Association
              (Make_Identifier (PN (P_Id)), N);
            Append_To (Result, N);

            N := Make_Subprogram_Call
              (RE (RE_To_CORBA_String),
               New_List
               (Make_Literal
                (New_String_Value
                 (IDL_Name (Identifier (E)), False))));
            N := Make_Parameter_Association
              (Make_Identifier (PN (P_Name)), N);
            Append_To (Result, N);

            N := Make_Subprogram_Call
              (RE (RE_To_CORBA_String_2),
               New_List
               (Make_Literal
                (New_String_Value
                 (Map_Type_Version (E), False))));
            N := Make_Parameter_Association
              (Make_Identifier (PN (P_Version)), N);
            Append_To (Result, N);

            return Result;
         end Standard_Create_Parameters;

         --------------
         -- IDL_Type --
         --------------

         function IDL_Type
           (Type_Spec  : Node_Id;
            Declarator : Node_Id)
           return Node_Id
         is
            Original_Type_Spec : constant Node_Id :=
              FEU.Get_Original_Type_Specifier (Type_Spec);
            Is_Array      : constant Boolean := Present (Declarator)
              and then FEN.Kind (Declarator) = K_Complex_Declarator;
         begin
            if Is_Array then
               return Array_IR
                 (Original_Type_Spec,
                  FEU.Get_Original_Type_Declarator (Declarator));
            else
               case FEN.Kind (Original_Type_Spec) is
                  when K_Fixed_Point_Type =>
                     return Fixed_IR (Original_Type_Spec);

                  when K_Sequence_Type =>
                     return Sequence_IR (Original_Type_Spec);

                  when others =>
                     return Make_Subprogram_Call
                       (RE (RE_To_Ref_15),
                        New_List
                        (Get_IR_Function_Node
                         (FEU.Get_Original_Type_Declarator
                          (Type_Spec))));
               end case;
            end if;
         end IDL_Type;

         --------------
         -- Array_IR --
         --------------

         function Array_IR
           (Elt_Type_Spec : Node_Id;
            Declarator    : Node_Id)
           return Node_Id
         is
            function Rec_Array_IR (Dim : Node_Id) return Node_Id;

            ------------------
            -- Rec_Array_IR --
            ------------------

            function Rec_Array_IR (Dim : Node_Id) return Node_Id is
               N       : Node_Id;
               Profile : constant List_Id := New_List;
               V       : Value_Type;
            begin
               N := Make_Subprogram_Call (RE (RE_Get_IR_Root), No_List);
               Append_To (Profile, N);

               V := FEU.Expr_Value (Dim);

               N := Make_Parameter_Association
                 (Make_Identifier (PN (P_Length)),
                  Make_Literal (New_Value (V)));
               Append_To (Profile, N);

               if Present (Next_Entity (Dim)) then
                  N := Rec_Array_IR (Next_Entity (Dim));
               else
                  N := Make_Subprogram_Call
                    (RE (RE_To_Ref_15),
                     New_List (Get_IR_Function_Node (Elt_Type_Spec)));
               end if;

               N := Make_Parameter_Association
                 (Make_Identifier (PN (P_Element_Type)), N);
               Append_To (Profile, N);

               N := Make_Subprogram_Call (RE (RE_Create_Array), Profile);
               N := Make_Subprogram_Call (RE (RE_To_Ref_15), New_List (N));

               return N;
            end Rec_Array_IR;

            L : constant List_Id := Array_Sizes (Declarator);
         begin
            return Rec_Array_IR (First_Entity (L));
         end Array_IR;

         --------------
         -- Fixed_IR --
         --------------

         function Fixed_IR (Type_Spec : Node_Id) return Node_Id is
            N       : Node_Id;
            Profile : constant List_Id := New_List;
         begin
            N := Make_Subprogram_Call (RE (RE_Get_IR_Root), No_List);
            Append_To (Profile, N);

            N := Make_Parameter_Association
              (Make_Identifier (PN (P_IDL_Digits)),
               Make_Literal
               (New_Integer_Value
                (Unsigned_Long_Long (FEN.N_Total (Type_Spec)), 1, 10)));
            Append_To (Profile, N);

            N := Make_Parameter_Association
              (Make_Identifier (PN (P_Scale)),
               Make_Literal
               (New_Integer_Value
                (Unsigned_Long_Long (FEN.N_Scale (Type_Spec)), 1, 10)));
            Append_To (Profile, N);

            N := Make_Subprogram_Call (RE (RE_Create_Fixed), Profile);
            N := Make_Subprogram_Call (RE (RE_To_Ref_15), New_List (N));

            return N;
         end Fixed_IR;

         -----------------
         -- Sequence_IR --
         -----------------

         function Sequence_IR (Type_Spec : Node_Id) return Node_Id is
            N        : Node_Id;
            Profile  : constant List_Id := New_List;
            Bounded  : constant Boolean := Present (Max_Size (Type_Spec));
            Elt_Type : constant Node_Id := FEN.Type_Spec (Type_Spec);
            V        : Value_Id;
         begin
            N := Make_Subprogram_Call (RE (RE_Get_IR_Root), No_List);
            Append_To (Profile, N);

            if Bounded then
               V := FEU.Expr_Value (Max_Size (Type_Spec));
            else
               V := Int0_Val;
            end if;

            N := Make_Parameter_Association
              (Make_Identifier (PN (P_Bound)), Make_Literal (V));
            Append_To (Profile, N);

            N := Make_Parameter_Association
              (Make_Identifier (PN (P_Element_Type)),
               IDL_Type (FEU.Get_Original_Type_Specifier (Elt_Type), No_Node));
            Append_To (Profile, N);

            N := Make_Subprogram_Call (RE (RE_Create_Sequence), Profile);
            N := Make_Subprogram_Call (RE (RE_To_Ref_15), New_List (N));

            return N;
         end Sequence_IR;
      begin
         --  Declare the Cached_IR_<Name> global variable

         N := Make_Object_Declaration
           (Defining_Identifier => Make_Defining_Identifier
              (Map_Cached_IR_Name (E)),
            Object_Definition   => RE (RE_Ref_12));
         Append_To (BEN.Statements (Current_Package), N);

         --  Common declarations and statements

         Parent_Container_Declaration;

         N := Make_If_Statement
           (Condition       => Make_Subprogram_Call
              (RE (RE_Is_Nil_12),
               New_List
               (Make_Identifier
                (Map_Cached_IR_Name (E)))),
            Then_Statements => New_List
              (Make_Return_Statement
               (Make_Identifier
                (Map_Cached_IR_Name (E)))));
         Append_To (Statements, N);

         case FEN.Kind (E) is
            when K_Interface_Declaration =>
               Interface_Body;

            when K_Simple_Declarator =>
               if For_Attr then
                  Attribute_Body;
               else
                  Declarator_Body;
               end if;

            when K_Complex_Declarator =>
               Declarator_Body;

            when K_Operation_Declaration =>
               Operation_Declaration_Body;

            when K_Module =>
               Module_Body;

            when K_Enumeration_Type =>
               Enumeration_Type_Body;

            when K_Structure_Type =>
               Structure_Exception_Body;

            when K_Union_Type =>
               Union_Type_Body;

            when K_Exception_Declaration =>
               Structure_Exception_Body;

            when others =>
               raise Program_Error;
         end case;

         --  Add the corresponding block statement to the body of
         --  Register_IR_Info.

         declare
            Register_IR_Info_St : constant List_Id := Get_GList
              (Package_Declaration (Current_Package),
               GL_Register_IR_Info);
            Dcl                 : constant List_Id := New_List;
         begin
            N := Make_Object_Declaration
              (Defining_Identifier => Make_Defining_Identifier (PN (P_Dummy)),
               Constant_Present    => True,
               Object_Definition   => Make_Attribute_Reference
                 (RE (RE_Ref_2), A_Class),
               Expression          => Get_IR_Function_Node (E));
            Append_To (Dcl, N);

            N := Make_Pragma
              (Pragma_Unreferenced,
               New_List (Make_Identifier (PN (P_Dummy))));
            Append_To (Dcl, N);

            N := Make_Block_Statement
              (Declarative_Part => Dcl,
               Statements       => New_List (Make_Null_Statement));
            Append_To (Register_IR_Info_St, N);
         end;

         N := Make_Subprogram_Body (Spec, Declarations, Statements);

         return N;
      end IR_Function_Body;

      ---------------------------
      -- Register_IR_Info_Body --
      ---------------------------

      function Register_IR_Info_Body (E : Node_Id) return Node_Id is
         Spec       : constant Node_Id := Register_Ir_Info_Node
           (BE_Node (Identifier (E)));
         Statements : constant List_Id := Get_GList
           (Package_Declaration (Current_Package),
            GL_Register_IR_Info);
      begin
         return Make_Subprogram_Body (Spec, No_List, Statements);
      end Register_IR_Info_Body;

      -----------
      -- Visit --
      -----------

      procedure Visit (E : Node_Id) is
      begin
         case FEN.Kind (E) is
            when K_Specification =>
               Visit_Specification (E);

            when K_Enumeration_Type =>
               Visit_Enumeration_Type (E);

            when K_Exception_Declaration =>
               Visit_Exception_Declaration (E);

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
         N : Node_Id;
         A : Node_Id;
      begin
         A := First_Entity (Declarators (E));

         while Present (A) loop
            N := IR_Function_Body (A, For_Attr => True);
            Append_To (Statements (Current_Package), N);

            A := Next_Entity (A);
         end loop;
      end Visit_Attribute_Declaration;

      ----------------------------
      -- Visit_Enumeration_Type --
      ----------------------------

      procedure Visit_Enumeration_Type (E : Node_Id) is
         N : Node_Id;
      begin
         N := IR_Function_Body (E);
         Append_To (Statements (Current_Package), N);
      end Visit_Enumeration_Type;

      ---------------------------------
      -- Visit_Exception_Declaration --
      ---------------------------------

      procedure Visit_Exception_Declaration (E : Node_Id) is
         N : Node_Id;
      begin
         N := IR_Function_Body (E);
         Append_To (Statements (Current_Package), N);
      end Visit_Exception_Declaration;

      ---------------------------------
      -- Visit_Interface_Declaration --
      ---------------------------------

      procedure Visit_Interface_Declaration (E : Node_Id) is
         N : Node_Id;
      begin
         N := BEN.Parent (Type_Def_Node (BE_Node (Identifier (E))));
         Push_Entity (BEN.IDL_Unit (Package_Declaration (N)));
         Set_IR_Info_Body;

         --  Statements  of the Register_IR_Info body

         Initialize_GList (Package_Declaration (Current_Package),
                           GL_Register_IR_Info);

         N := First_Entity (Interface_Body (E));
         while Present (N) loop
            Visit (N);
            N := Next_Entity (N);
         end loop;

         N := IR_Function_Body (E);
         Append_To (Statements (Current_Package), N);

         N := Register_IR_Info_Body (E);
         Append_To (Statements (Current_Package), N);

         Pop_Entity;
      end Visit_Interface_Declaration;

      ------------------
      -- Visit_Module --
      ------------------

      procedure Visit_Module (E : Node_Id) is
         D : Node_Id;
         N : Node_Id;
      begin
         if not Map_Particular_CORBA_Parts (E, PK_IR_Info_Body) then
            D := Stub_Node (BE_Node (Identifier (E)));
            Push_Entity (D);
            Set_IR_Info_Body;

            --  Statements  of the Register_IR_Info body

            Initialize_GList (Package_Declaration (Current_Package),
                              GL_Register_IR_Info);

            D := First_Entity (Definitions (E));

            while Present (D) loop
               Visit (D);
               D := Next_Entity (D);
            end loop;

            N := IR_Function_Body (E);
            Append_To (Statements (Current_Package), N);

            N := Register_IR_Info_Body (E);
            Append_To (Statements (Current_Package), N);

            Pop_Entity;
         end if;
      end Visit_Module;

      ---------------------------------
      -- Visit_Operation_Declaration --
      ---------------------------------

      procedure Visit_Operation_Declaration (E : Node_Id) is
         N : Node_Id;
      begin
         --  We do not generate IR informations for operations that
         --  are expanded from IDL attributes

         if FEN.Loc (Identifier (E)) = No_Location then
            return;
         end if;

         N := IR_Function_Body (E);
         Append_To (Statements (Current_Package), N);
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
         N := IR_Function_Body (E);
         Append_To (Statements (Current_Package), N);
      end Visit_Structure_Type;

      ----------------------------
      -- Visit_Type_Declaration --
      ----------------------------

      procedure Visit_Type_Declaration (E : Node_Id) is
         D : Node_Id;
         N : Node_Id;
      begin
         D := First_Entity (Declarators (E));

         while Present (D) loop
            N := IR_Function_Body (D);
            Append_To (Statements (Current_Package), N);

            D := Next_Entity (D);
         end loop;
      end Visit_Type_Declaration;

      ----------------------
      -- Visit_Union_Type --
      ----------------------

      procedure Visit_Union_Type (E : Node_Id) is
         N : Node_Id;
      begin
         N := IR_Function_Body (E);
         Append_To (Statements (Current_Package), N);
      end Visit_Union_Type;

   end Package_Body;

end Backend.BE_CORBA_Ada.IR_Infos;

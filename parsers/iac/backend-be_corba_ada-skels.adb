------------------------------------------------------------------------------
--                                                                          --
--                            POLYORB COMPONENTS                            --
--                                   IAC                                    --
--                                                                          --
--           B A C K E N D . B E _ C O R B A _ A D A . S K E L S            --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                        Copyright (c) 2005 - 2006                         --
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

with Namet;     use Namet;
with Values;    use Values;
with Locations; use Locations;

with Frontend.Nodes;  use Frontend.Nodes;
with Frontend.Nutils;

with Backend.BE_CORBA_Ada.Expand;      use Backend.BE_CORBA_Ada.Expand;
with Backend.BE_CORBA_Ada.IDL_To_Ada;  use Backend.BE_CORBA_Ada.IDL_To_Ada;
with Backend.BE_CORBA_Ada.Nodes;       use Backend.BE_CORBA_Ada.Nodes;
with Backend.BE_CORBA_Ada.Nutils;      use Backend.BE_CORBA_Ada.Nutils;
with Backend.BE_CORBA_Ada.Runtime;     use Backend.BE_CORBA_Ada.Runtime;

with GNAT.Perfect_Hash_Generators; use GNAT.Perfect_Hash_Generators;

package body Backend.BE_CORBA_Ada.Skels is
   package FEN renames Frontend.Nodes;
   package BEN renames Backend.BE_CORBA_Ada.Nodes;
   package FEU renames Frontend.Nutils;

   package body Package_Spec is

      procedure Visit_Interface_Declaration (E : Node_Id);
      procedure Visit_Module (E : Node_Id);
      procedure Visit_Specification (E : Node_Id);

      -----------
      -- Visit --
      -----------

      procedure Visit (E : Node_Id) is
      begin
         case FEN.Kind (E) is
            when K_Interface_Declaration =>
               Visit_Interface_Declaration (E);

            when K_Module =>
               Visit_Module (E);

            when K_Specification =>
               Visit_Specification (E);

            when others =>
               null;
         end case;
      end Visit;

      ---------------------------------
      -- Visit_Interface_Declaration --
      ---------------------------------

      procedure Visit_Interface_Declaration (E : Node_Id) is
         N : Node_Id;
      begin
         --  No Skel package is generated for an abstract or a local
         --  interface
         if FEN.Is_Abstract_Interface (E) or else
           FEN.Is_Local_Interface (E)
         then
            return;
         end if;

         N := BEN.Parent (Type_Def_Node (BE_Node (Identifier (E))));
         Push_Entity (BEN.IDL_Unit (Package_Declaration (N)));
         Set_Skeleton_Spec;
         N := Make_Subprogram_Call
           (Make_Defining_Identifier (GN (Pragma_Elaborate_Body)),
            No_List);
         N := Make_Pragma_Statement (N);
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
         if not Map_Particular_CORBA_Parts (E, PK_Skel_Spec) then
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

   end Package_Spec;

   package body Package_Body is

      Invoke_Then_Statements  : List_Id := No_List;
      Invoke_Elsif_Statements : List_Id := No_List;
      Package_Initialization  : List_Id := No_List;
      Choice_List             : List_Id := No_List;

      function Deferred_Initialization_Body (E : Node_Id) return Node_Id;
      function Gen_Invoke_Part (S : Node_Id) return Node_Id;
      function Invoke_Body
        (E              : Node_Id;
         Is_A_Invk_Part : Node_Id)
        return Node_Id;
      procedure Invoke_Declaration (L : List_Id);
      function Invoke_Spec return Node_Id;
      function Is_A_Invoke_Part return Node_Id;
      function Implicit_CORBA_Methods return List_Id;
      function Servant_Is_A_Body (Spec : Node_Id) return Node_Id;
      procedure Skeleton_Initialization (L : List_Id);
      function Non_User_Exception_Handler return Node_Id;

      procedure Visit_Interface_Declaration (E : Node_Id);
      procedure Visit_Module (E : Node_Id);
      procedure Visit_Operation_Declaration (E : Node_Id);
      procedure Visit_Specification (E : Node_Id);

      --  The entities below are used in case of optimization using
      --  minimal perfect hash functions
      N_Subprograms           : Unsigned_Long_Long;
      Register_Procedure_List : List_Id;
      Invoke_Subp_Bodies      : List_Id;
      Optim                   : Optimization;

      --  This function generates the name of the package that will
      --  contain the Hash function.
      function  Hash_Package_Name (E : Node_Id) return Name_Id;

      --  This procedure initialise the lists above. It initialises
      --  the GNAT Perfect_Hash generator.
      procedure Initialize_Hash_Function_Optimization;

      --  This procedure computes the Perfect Hash function generator,
      --  produces it in an additional package and finally finalizes
      --  the generator.
      procedure Achieve_Hash_Function_Optimization (E : Node_Id);

      --  This function inserts the name of the subprogram to the
      --  Perfect hash function generator. It produces also a
      --  "Register procedure" call statement which will be added to
      --  the Deferred_Initialization procedure statements.
      procedure Insert_And_Register_Statements
        (Subp_Name   : Name_Id);

      --  Generation of the Register_Procedure subprogram which is
      --  called to register a procedure in the hash table.
      function Register_Procedure_Spec return Node_Id;
      function Register_Procedure_Body (E : Node_Id) return Node_Id;

      ----------------------------------
      -- Deferred_Initialization_Body --
      ----------------------------------

      function Deferred_Initialization_Body (E : Node_Id) return Node_Id is
         N          : Node_Id;
         Profile    : constant List_Id := New_List (K_List_Id);
         Spec       : Node_Id;
         Statements : constant List_Id := New_List (K_List_Id);
      begin
         Spec := Make_Subprogram_Specification
           (Make_Defining_Identifier (SN (S_Deferred_Initialization)),
            No_List);
         N := Type_Def_Node (BE_Node (Identifier (E)));
         N := Next_Node (N);
         N := Make_Subprogram_Call
           (RE (RE_To_CORBA_String),
            Make_List_Id (Expand_Designator (N)));
         Append_Node_To_List (N, Profile);

         N := Make_Type_Attribute
           (Make_Designator (SN (S_Servant_Is_A)), A_Access);
         Append_Node_To_List (N, Profile);

         N := Make_Type_Attribute
           (Make_Designator (SN (S_Is_A)), A_Access);
         Append_Node_To_List (N, Profile);

         N := Make_Type_Attribute (Make_Designator (SN (S_Invoke)), A_Access);
         Append_Node_To_List (N, Profile);

         N := Make_Subprogram_Call
           (RE (RE_Register_Skeleton), Profile);
         Append_Node_To_List (N, Statements);

         --  In case of perfect hash function optimization, we
         --  register the Invoke_XXXX procedures at the package
         --  initialilzation
         if Use_Minimal_Hash_Function then
            Append_Node_To_List
              (First_Node (Register_Procedure_List),
               Statements);
         end if;

         N := Make_Subprogram_Implementation
           (Spec, No_List, Statements);

         return N;
      end Deferred_Initialization_Body;

      ---------------------
      -- Gen_Invoke_Part --
      ---------------------

      function Gen_Invoke_Part (S : Node_Id) return Node_Id is
         C                    : Node_Id;
         Count                : Natural;
         N                    : Node_Id;
         M                    : Node_Id;
         Declarative_Part     : constant List_Id := New_List (K_List_Id);
         Statements           : constant List_Id := New_List (K_List_Id);
         Param                : Node_Id;
         Param_Name           : Name_Id;
         Type_Node            : Node_Id;
         New_Name             : Name_Id;
         P                    : List_Id;
         Inv_Profile          : constant List_Id := New_List (K_List_Id);
         TC                   : Node_Id;
         From_Any_Helper      : Node_Id;
         To_Any_Helper        : Node_Id;
         FE                   : Node_Id;
         Impl_Id              : Node_Id;
         Operation_Name       : Name_Id := BEN.Name (Defining_Identifier (S));
         Discret_Choice_Value : Value_Id;
         Record_Node          : Node_Id;

         function Exception_Handler_Alternative
           (E : Node_Id)
           return Node_Id;
         --  Generation of an alternative in the exception handler

         -----------------------------------
         -- Exception_Handler_Alternative --
         -----------------------------------

         function Exception_Handler_Alternative
           (E : Node_Id)
           return Node_Id is
            Result     : Node_Id;
            Selector   : Node_Id;
            Expression : Node_Id;
            N          : Node_Id;
            D          : constant List_Id := New_List (K_List_Id);
            S          : constant List_Id := New_List (K_List_Id);
         begin
            --  Getting the Exception name

            N := Expand_Designator
              (Stub_Node
               (BE_Node
                (Identifier
                 (Reference (E)))));

            Selector := Make_Object_Declaration
              (Defining_Identifier =>
                 Make_Defining_Identifier (PN (P_E)),
               Object_Definition => N);

            --  Declaration of the Members variable Getting the node
            --  corresponding to the declaration of the
            --  "Excp_Name"_Members type.

            N := Type_Def_Node (BE_Node (Identifier (Reference (E))));

            N := Make_Object_Declaration
              (Defining_Identifier =>
                 Make_Defining_Identifier (PN (P_Members)),
               Object_Definition => Defining_Identifier (N));
            Append_Node_To_List (N, D);

            --  Getting the node corresponding to the declaration of
            --  the Get_Members procedure. This procedure is declared
            --  2 nodes after the member type definition.

            N := Type_Def_Node (BE_Node (Identifier (Reference (E))));
            N := Next_Node (Next_Node (N));

            N := Defining_Identifier (N);
            N := Make_Subprogram_Call
              (N,
               Make_List_Id
               (Make_Defining_Identifier (PN (P_E)),
                Make_Defining_Identifier (PN (P_Members))));
            Append_Node_To_List (N, S);

            --  Getting the node corresponding to the declaration of
            --  the To_Any procedure in the helper package.

            N := To_Any_Node (BE_Node (Identifier (Reference (E))));

            N := Make_Subprogram_Call
              (Expand_Designator (N),
               Make_List_Id
               (Make_Defining_Identifier (PN (P_Members))));

            N := Make_Subprogram_Call
              (RE (RE_Set_Exception),
               Make_List_Id
               (Make_Defining_Identifier (PN (P_Request)), N));
            Append_Node_To_List (N, S);

            N := Make_Return_Statement (No_Node);
            Append_Node_To_List (N, S);

            Expression := Make_Block_Statement
              (Declarative_Part => D,
               Statements       => S);

            Result := Make_Component_Association
              (Selector,
               Expression);
            return Result;
         end Exception_Handler_Alternative;

      begin

         --  Implementation.Object'Class (Self.All)'Access

         N := Implementation_Package (Current_Entity);
         N := First_Node (Visible_Part (Package_Specification (N)));
         N := Expand_Designator (N);
         N := Make_Type_Attribute (N, A_Class);
         C := Make_Designator
           (Designator => PN (P_Self),
            Is_All     => True);
         N := Make_Subprogram_Call
           (N, Make_List_Id (C));
         N := Make_Attribute_Designator
           (N, A_Access);
         Append_Node_To_List (N, Inv_Profile);

         Count := Length (Parameter_Profile (S));

         if Count > 1 then
            Param := First_Node (Parameter_Profile (S));
            Param := Next_Node (Param);
            loop
               P := Make_List_Id (Make_Designator (VN (V_Argument_List)));
               Param_Name := BEN.Name (Defining_Identifier (Param));

               Type_Node := Parameter_Type (Param);

               --  If the parameter type is a class-wide type, we
               --  remove the "'Class" attribute from the type name

               if BEN.Kind (Type_Node) = K_Attribute_Designator then
                  Type_Node := Prefix (Type_Node);
               end if;

               N :=  Make_Object_Declaration
                 (Defining_Identifier =>
                    Make_Defining_Identifier (Param_Name),
                  Object_Definition   => Type_Node);
               Append_Node_To_List (N, Declarative_Part);

               --  Adding the parameter to the profile of the
               --  implementation call

               Append_Node_To_List
                 (Make_Defining_Identifier (Param_Name),
                  Inv_Profile);

               --  The declaration below are not generated if the SII
               --  is used

               if not Use_SII then

                  --  Arg_Name_U_XXXX declaration

                  C := Make_Subprogram_Call
                    (Defining_Identifier   => RE (RE_To_CORBA_String),
                     Actual_Parameter_Part =>
                       Make_List_Id (Make_Literal
                                     (New_String_Value (Param_Name, False))));
                  New_Name := Add_Prefix_To_Name ("Arg_Name_U_", Param_Name);

                  Append_Node_To_List (Make_Designator (New_Name), P);

                  N := Make_Object_Declaration
                    (Defining_Identifier => Make_Defining_Identifier
                     (New_Name),
                     Constant_Present => True,
                     Object_Definition => RE (RE_Identifier_0),
                     Expression => C);
                  Append_Node_To_List (N, Declarative_Part);

                  --  Argument_U_XXXX declaration

                  TC := Get_TC_Node (BEN.FE_Node (Parameter_Type (Param)));

                  C :=  Make_Subprogram_Call
                    (Defining_Identifier   => RE (RE_Get_Empty_Any),
                     Actual_Parameter_Part =>
                       Make_List_Id (TC));
                  New_Name := Add_Prefix_To_Name ("Argument_U_", Param_Name);
                  Append_Node_To_List (Make_Designator (New_Name), P);
                  N := Make_Object_Declaration
                    (Defining_Identifier => Make_Defining_Identifier
                     (New_Name),
                     Constant_Present => False,
                     Object_Definition => RE (RE_Any),
                     Expression => C);
                  Append_Node_To_List (N, Declarative_Part);

                  --  The parameter mode is added to the Add_Item
                  --  profile

                  if BEN.Parameter_Mode (Param) = Mode_Out then
                     N := RE (RE_ARG_OUT_0);
                  elsif BEN.Parameter_Mode (Param) = Mode_In then
                     N := RE (RE_ARG_IN_0);
                  else
                     N := RE (RE_ARG_INOUT_0);
                  end if;

                  Append_Node_To_List (N, P);

                  --  Add_Item call

                  N := Make_Subprogram_Call
                    (RE (RE_Add_Item_0),
                     P);
                  Append_Node_To_List (N, Statements);
               end if;

               Param := Next_Node (Param);
               exit when No (Param);
            end loop;
         end if;

         --  In the SII mode, the request payload has to be set before
         --  the request is processed. The Argument procedure is not
         --  the same as DII

         if Use_SII then

            N := Expand_Designator
              (Type_Def_Node
               (BE_Node
                (FE_Node
                 (S))));

            N := Make_Object_Declaration
              (Defining_Identifier =>
                 Make_Defining_Identifier (PN (P_Arg_List)),
               Aliased_Present     => True,
               Object_Definition   => N);
            Append_Node_To_List (N, Declarative_Part);

            --  We need an Error Container

            N := Make_Object_Declaration
              (Defining_Identifier => Make_Defining_Identifier
               (PN (P_Error)),
               Object_Definition => RE (RE_Error_Container));
            Append_Node_To_List (N, Declarative_Part);

            N := Make_Subprogram_Call
              (RE (RE_Set_Type),
               Make_List_Id
               (Make_Designator (VN (V_Argument)), RE (RE_TC_Void)));
            Append_Node_To_List (N, Statements);

            --  Setting the Payload field

            Set_Str_To_Name_Buffer
              ("Setting the request Payload");
            Append_Node_To_List
              (Make_Ada_Comment (Name_Find),
               Statements);

            --  Getting the XXXX_Set_Args node

            N := Expand_Designator
              (Set_Args_Node
               (BE_Node
                (FE_Node
                 (S))));

            --  Getting the XXXX_Args_Type node

            C := Expand_Designator
              (Type_Def_Node
               (BE_Node
                (FE_Node
                 (S))));
            N := Make_Subprogram_Call
              (N,
               Make_List_Id
               (Make_Defining_Identifier (PN (P_Request)),
                Make_Object_Instanciation (C)));
            Append_Node_To_List (N, Statements);

            --  Calling Arguments

            Set_Str_To_Name_Buffer
              ("Processing request");
            Append_Node_To_List
              (Make_Ada_Comment (Name_Find),
               Statements);

            --  Converting the Request parameter type

            N := Make_Subprogram_Call
              (RE (RE_Request_Access),
               Make_List_Id
               (Make_Defining_Identifier (PN (P_Request))));

            --  Request payload

            C := Make_Designator
              (Designator => PN (P_Payload),
               Parent     => PN (P_Request));

            --  Call Arguments

            N := Make_Subprogram_Call
              (RE (RE_Arguments_2),
               Make_List_Id (N, C, Make_Defining_Identifier (PN (P_Error))));
            Append_Node_To_List (N, Statements);

            --  Handling error

            C := Make_Subprogram_Call
              (RE (RE_Found),
               Make_List_Id (Make_Defining_Identifier (PN (P_Error))));
            N := Make_Subprogram_Call
              (RE (RE_Raise_From_Error),
               Make_List_Id (Make_Defining_Identifier (PN (P_Error))));

            N := Make_If_Statement
              (Condition       => C,
               Then_Statements => Make_List_Id (N));

            Append_Node_To_List (N, Statements);
         else
            Set_Str_To_Name_Buffer
              ("Processing request");
            Append_Node_To_List
              (Make_Ada_Comment (Name_Find),
               Statements);

            N := Make_Subprogram_Call
              (RE (RE_Arguments_1),
               Make_List_Id
               (Make_Defining_Identifier (PN (P_Request)),
                Make_Defining_Identifier (VN (V_Argument_List))));
            Append_Node_To_List (N, Statements);
         end if;

         --  The bloc above implements the generation of :
         --  * The argument conversion from the "Any" type
         --  * The call of the corresponding method implemented by the
         --    programmer.
         --  * The handling of eventuals exceptions thrown by the method.

         --  If the method could potentially throw an exception, the
         --  generated code will be put in a statement bloc. Else, No
         --  additional statement bloc will be used.

         declare
            Inner_Statements  : List_Id := No_List;
            Inner             : Boolean := False;
            Exception_Handler : List_Id := No_List;
            Excp_Node         : Node_Id;
         begin

            --  Looking wether the operation throws exceptions and
            --  setting Inner_statement to the corresponding value

            N := Corresponding_Entity (FE_Node (S));
            if FEN.Kind (N) = K_Operation_Declaration and then
              not FEU.Is_Empty (Exceptions (N)) then
               Inner_Statements  := New_List (K_List_Id);
               Exception_Handler := New_List (K_List_Id);
               Inner             := True;

               --  Creating the exception handler statements

               Excp_Node := First_Entity (Exceptions (N));
               while Present (Excp_Node) loop
                  N := Exception_Handler_Alternative (Excp_Node);
                  Append_Node_To_List (N, Exception_Handler);
                  Excp_Node := Next_Entity (Excp_Node);
               end loop;
            else
               Inner_Statements := Statements;
            end if;

            --  Setting parameters

            Set_Str_To_Name_Buffer
              ("Setting Parameters");
            Append_Node_To_List
              (Make_Ada_Comment (Name_Find),
               Inner_Statements);

            --  In the case of SII, the parameter are set from the
            --  Args record

            if Use_SII then

               --  Creating Request.Payload.Args.all

               C := Make_Designator (PN (P_Args), Is_All => True);
               N := Make_Designator
                 (Designator => PN (P_Payload),
                  Parent     => PN (P_Request));
               Set_Correct_Parent_Unit_Name (C, N);

               --  Converting type

               N := Expand_Designator
                 (Type_Def_Node
                  (BE_Node
                   (FE_Node
                    (S))));
               Record_Node := Make_Subprogram_Call
                 (N, Make_List_Id (C));
            end if;

            if Count > 1 then
               Param := First_Node (Parameter_Profile (S));
               Param := Next_Node (Param);
               loop
                  if  BEN.Parameter_Mode (Param) = Mode_In
                    or else BEN.Parameter_Mode (Param) = Mode_Inout then

                     --  Getting the parameter name

                     Param_Name := BEN.Name (Defining_Identifier (Param));

                     --  Preparing the assigned value (SII or DII)

                     if Use_SII then

                        --  Getting the record field

                        C := Make_Defining_Identifier (Param_Name);
                        --  Here, we use directly Set_Parent_Unit_Name
                        --  and not Set_Correct_Parent_Unit_Name
                        --  because the parent node is not a defining
                        --  identifier nor a designator
                        Set_Parent_Unit_Name (C, Record_Node);

                     else
                        New_Name := Add_Prefix_To_Name
                          ("Argument_U_", Param_Name);

                        From_Any_Helper := Get_From_Any_Node
                          (BEN.FE_Node
                           (Parameter_Type
                            (Param)));
                        C :=  Make_Subprogram_Call
                          (From_Any_Helper,
                           Make_List_Id
                           (Make_Designator
                            (New_Name)));
                     end if;

                     N := Make_Assignment_Statement
                       (Make_Defining_Identifier (Param_Name),
                        C);
                     Append_Node_To_List (N, Inner_Statements);
                  end if;

                  Param := Next_Node (Param);
                  exit when No (Param);
               end loop;
            end if;

            --  Call Implementation

            Set_Str_To_Name_Buffer
              ("Call Implementation");
            Append_Node_To_List
              (Make_Ada_Comment (Name_Find),
               Inner_Statements);

            N := Corresponding_Entity (FE_Node (S));
            C := Impl_Node (BE_Node (FE_Node (S)));

            --  Re-adjusting the parent unit name of the
            --  operation. This is necessary in the case of operations
            --  or attributes inherited from the second until the last
            --  parent (multiple inheritence)

            Impl_Id := New_Node (K_Designator);
            Set_Defining_Identifier
              (Impl_Id,
               Copy_Node
               (Defining_Identifier (C)));
            Set_Correct_Parent_Unit_Name
              (Impl_Id,
               Defining_Identifier
               (Implementation_Package
                (Current_Entity)));

            C := Make_Subprogram_Call
              (Copy_Designator (Impl_Id),
               Inv_Profile);

            if Present (Return_Type (S)) then

               --  If the return type is a class-wide type, then the
               --  node kind is K_Attribute_Designator, we take only
               --  the prefix

               if BEN.Kind (Return_Type (S)) = K_Attribute_Designator then
                  Type_Node := Prefix (Return_Type (S));

                  --  We cast the result value

                  C := Make_Subprogram_Call
                    (Copy_Designator (Type_Node),
                     Make_List_Id (C));
               else
                  Type_Node := Return_Type (S);
               end if;

               N := Make_Object_Declaration
                 (Defining_Identifier =>
                    Make_Defining_Identifier (VN (V_Result)),
                  Object_Definition =>
                    Type_Node);
               Append_Node_To_List (N, Declarative_Part);

               C := Make_Assignment_Statement
                 (Make_Defining_Identifier (VN (V_Result)),
                  C);
            end if;

            Append_Node_To_List (C, Inner_Statements);

            if Inner then
               Append_Node_To_List
                 (Make_Block_Statement
                  (Declarative_Part => No_List,
                   Statements => Inner_Statements,
                   Exception_Handler => Exception_Handler),
                  Statements);
            end if;

         end;

         --  Set Result

         if Present (Return_Type (S)) then
            Set_Str_To_Name_Buffer
              ("Setting the result");
            Append_Node_To_List
              (Make_Ada_Comment (Name_Find),
               Statements);
            if Use_SII then
               C := Make_Defining_Identifier (PN (P_Returns));

               --  Here, we use directly Set_Parent_Unit_Name and not
               --  Set_Correct_Parent_Unit_Name because the parent
               --  node is not a defining identifier nor a designator

               --  Set_Parent_Unit_Name (C, Record_Node);
               Set_Parent_Unit_Name (C, Make_Designator (PN (P_Arg_List)));

               N := Make_Assignment_Statement
                 (C, Make_Designator (VN (V_Result)));
               Append_Node_To_List (N, Statements);
            else
               FE := FE_Node (Return_Type (S));

               To_Any_Helper := Get_To_Any_Node (FE);

               C := Make_Subprogram_Call
                 (To_Any_Helper,
                  Make_List_Id (Make_Designator (VN (V_Result))));
               N := Make_Subprogram_Call
                 (RE (RE_Set_Result),
                  Make_List_Id
                  (Make_Designator (PN (P_Request)),
                   C));
               Append_Node_To_List (N, Statements);
            end if;
         end if;

         --  Setting out arguments

         if Count > 1 then
            Param := First_Node (Parameter_Profile (S));
            Param := Next_Node (Param);
            loop
               if  BEN.Parameter_Mode (Param) = Mode_Out
                 or else BEN.Parameter_Mode (Param) = Mode_Inout then
                  Set_Str_To_Name_Buffer
                    ("Setting out argument");
                  Append_Node_To_List
                    (Make_Ada_Comment (Name_Find),
                     Statements);

                  Param_Name := BEN.Name (Defining_Identifier (Param));

                  if Use_SII then
                     C := Make_Defining_Identifier (Param_Name);
--                       C := Make_Designator
--                         (Designator => Param_Name,
--                          Parent     => PN (P_Arg_List));

                     --  Here, we use directly Set_Parent_Unit_Name
                     --  and not Set_Correct_Parent_Unit_Name because
                     --  the parent node is not a defining identifier
                     --  nor a designator

                     --  Set_Parent_Unit_Name (C, Record_Node);
                     Set_Parent_Unit_Name
                       (C, Make_Designator (PN (P_Arg_List)));
                     N := Make_Assignment_Statement
                       (C, Make_Designator (Param_Name));
                     Append_Node_To_List (N, Statements);
                  else
                     New_Name := Add_Prefix_To_Name
                       ("Argument_U_", Param_Name);

                     To_Any_Helper := Get_To_Any_Node
                       (BEN.FE_Node
                        (Parameter_Type
                         (Param)));

                     declare
                        Arg_List : constant List_Id := New_List (K_List_Id);
                     begin
                        Append_Node_To_List
                          (Make_Defining_Identifier (New_Name),
                           Arg_List);
                        Append_Node_To_List
                          (Make_Subprogram_Call
                           (To_Any_Helper,
                            Make_List_Id (Make_Designator (Param_Name))),
                           Arg_List);

                        N := Make_Subprogram_Call
                          (RE (RE_Move_Any_Value),
                           Arg_List);
                        Append_Node_To_List (N, Statements);
                     end;
                  end if;
               end if;
               Param := Next_Node (Param);
               exit when No (Param);
            end loop;
         end if;

         if Use_SII then
            declare
               Decl : constant List_Id := New_List (K_List_Id);
               Stat : constant List_Id := New_List (K_List_Id);
               --  Bool : Node_Id;
            begin
               --  Get the GIOP version

               --  M := Make_Designator
               --  (Designator => VN (V_Binding_Profile),
               --   Is_All     => True);
               --  N := Make_Assignment_Statement
               --  (Make_Designator (VN (V_Minor)),
               --   Make_Subprogram_Call
               --   (RE (RE_Get_GIOP_Version), Make_List_Id (M)));
               --  Append_Node_To_List (N, Statements);

               --  Not used for the moement
               --  Bool := Make_Expression
               --    (Make_Defining_Identifier (VN (V_Minor)),
               --     Op_Equal,
               --     Make_Literal (New_Integer_Value (2, 1, 10)));

               Set_Str_To_Name_Buffer
                 ("Create the Argument list");
               Append_Node_To_List
                 (Make_Ada_Comment (Name_Find),
                  Stat);

               C := Make_Subprogram_Call
                 (RE (RE_Create),
                  Make_List_Id
                  (Make_Defining_Identifier
                   (VN (V_Operation_Argument_List))));
               Append_Node_To_List (C, Stat);

               --  Put the created NVList in the request record

               N := Make_Assignment_Statement
                 (Make_Designator
                  (Designator => PN (P_Args),
                   Parent     => PN (P_Request)),
                  Make_Defining_Identifier
                  (VN (V_Operation_Argument_List)));
               Append_Node_To_List (N, Stat);

               --  Preparing the parameter list of the Add_Item Call

               P := Make_List_Id
                 (Make_Designator (VN (V_Operation_Argument_List)));

               Set_Str_To_Name_Buffer ("Buffer_Name");
               N := Make_Designator (Name_Find);
               Append_Node_To_List (N, P);

               N := Make_Designator (VN (V_Argument));

               N := Make_Subprogram_Call
                 (RE (RE_To_PolyORB_Any),
                  Make_List_Id (N));
               Append_Node_To_List (N, P);

               N := RE (RE_ARG_OUT_1);

               Append_Node_To_List (N, P);

               --  Call the Add_Item procedure

               N := Make_Subprogram_Call
                 (RE (RE_Add_Item_1),
                  P);
               Append_Node_To_List (N, Stat);

               --  the marshaller method
               C := Expand_Designator
                 (Marshaller_Node
                  (BE_Node
                   (FE_Node
                    (S))));

               P := New_List (K_List_Id);
               Append_Node_To_List
                 (RE (RE_Server_Entity), P);

               M := Make_Designator (PN (P_Arg_List));
               M := Make_Attribute_Designator (M, A_Access);

--                 N := Make_Designator
--                   (Designator => PN (P_Payload),
--                    Parent     => PN (P_Request));
--                 Set_Correct_Parent_Unit_Name (M, N);

               Append_Node_To_List
                 (M, P);

               Append_Node_To_List
                 (Make_Defining_Identifier (VN (V_Buffer)), P);
               M := Make_Designator
                 (Designator => VN (V_Component),
                  Is_All     => True);

               --  GIOP_Session is used to get the representation
               --  attribute (the declartif part the block)

               N := Make_Subprogram_Call
                 (RE (RE_GIOP_Session),
                  Make_List_Id (M));

               N := Make_Object_Declaration
                 (Defining_Identifier =>
                    Make_Defining_Identifier (VN (V_Session)),
                  Object_Definition   => RE (RE_GIOP_Session),
                  Renamed_Object      => N);
               Append_Node_To_List (N, Decl);

               N := Make_Subprogram_Call
                 (RE (RE_Get_Representation),
                  Make_List_Id (Make_Designator (VN (V_Session))));
               Append_Node_To_List (N, P);

               Append_Node_To_List
                 (Make_Literal (New_Integer_Value (1, 1, 10)), P);

               Append_Node_To_List
                 (Make_Defining_Identifier (PN (P_Error)), P);

               --  The marshaller method call

               N := Make_Subprogram_Call
                 (C, P);
               Append_Node_To_List (N, Stat);

               --  If any error we raise a program_error

               N := Make_Subprogram_Call
                 (RE (RE_Found),
                  Make_List_Id (Make_Designator (PN (P_Error))));

               N := Make_If_Statement
                 (Condition       => N,
                  Then_Statements =>
                    Make_List_Id (Make_Raise_Statement
                                  (Make_Designator (EN (E_Program_Error)))));
               Append_Node_To_List (N, Stat);

               N := Make_Block_Statement
                 (Declarative_Part => Decl,
                  Statements       => Stat);

               --  For the moment there is no implementation of
               --  SII/SSI invokation for GIOP 1.0 and 1.1 so we
               --  didn't have to make this conditionnel
               --  N := Make_If_Statement
               --    (Condition        => Bool,
               --     Then_Statements  => Make_List_Id (N));

               Append_Node_To_List (N, Statements);
            end;
         end if;

         --  For operations expanded from attributes, we add an
         --  underscore to the operation name

         if FEN.Loc (FE_Node (S)) = No_Location then
            Operation_Name := Add_Prefix_To_Name ("_", Operation_Name);
         end if;

         --  If no optimization is requested by the user, we generate
         --  an elsif statement. Else, we generate an case statement
         --  alternative

         if not Use_Minimal_Hash_Function then
            C := Make_Expression
              (Make_Defining_Identifier (VN (V_Operation)),
               Op_Equal,
               Make_Literal
               (New_String_Value
                (Operation_Name, False)));

            N := Make_Block_Statement
              (Declarative_Part => Declarative_Part,
               Statements       => Statements);
            N := Make_Elsif_Statement
              (C, Make_List_Id (N));
         else

            --  Insert the subprogram name into the hash function
            --  generator and add a call to Register_Procedure

            Insert_And_Register_Statements
              (Operation_Name);

            --  Prepare the case alternative
            --  * Discret Choice : value of N_Subprogram minus 1

            Discret_Choice_Value := New_Integer_Value
              (N_Subprograms - 1, 1, 10);

            N := Make_Block_Statement
              (Declarative_Part => Declarative_Part,
               Statements       => Statements);

            N := Make_Case_Statement_Alternative
              (Make_List_Id (Make_Literal (Discret_Choice_Value)),
               Make_List_Id (N));
         end if;
         return N;
      end Gen_Invoke_Part;

      -----------------
      -- Invoke_Body --
      -----------------

      function Invoke_Body
        (E              : Node_Id;
         Is_A_Invk_Part : Node_Id)
        return Node_Id
      is
         N                 : Node_Id;
         Spec              : Node_Id;
         D                 : constant List_Id := New_List (K_List_Id);
         C_1               : Node_Id;
         Else_Statements   : constant List_Id := New_List (K_List_Id);
         Invoke_Statements : constant List_Id := New_List (K_List_Id);
         Exception_Handler : Node_Id;
         Is_A_Lowered_Name : Name_Id;
      begin
         Spec := Invoke_Spec;

         --  The declarative part

         Invoke_Declaration (D);

         --  We don't create the request if the SII is used

         if not Use_SII then
            N := Make_Subprogram_Call
              (RE (RE_Create_List),
               Make_List_Id
               (Make_Literal (Int0_Val),
                Make_Defining_Identifier (VN (V_Argument_List))));
            Append_Node_To_List (N, Invoke_Statements);
         end if;

         if not Use_Minimal_Hash_Function then
            Append_Node_To_List (Is_A_Invk_Part, Invoke_Then_Statements);

            Set_Str_To_Name_Buffer ("_is_a");
            Is_A_Lowered_Name := Name_Find;

            C_1 := Make_Expression
              (Make_Defining_Identifier (VN (V_Operation)),
               Op_Equal,
               Make_Literal
               (New_String_Value
                (Is_A_Lowered_Name, False)));
         else
            Append_Node_To_List (Is_A_Invk_Part, Invoke_Subp_Bodies);

            N := Make_Defining_Identifier (SN (S_Hash));
            Set_Correct_Parent_Unit_Name
              (N, Make_Defining_Identifier (Hash_Package_Name (E)));

            --  Calculate the hash code of the operation

            N := Make_Subprogram_Call
              (N,
               Make_List_Id
               (Make_Defining_Identifier
                (VN (V_Operation))));
            N := Make_Assignment_Statement
              (Make_Defining_Identifier (VN (V_Index)),
               N);
            Append_Node_To_List (N, Invoke_Statements);

            --  Obtaining the operation name corresponding to the hash
            --  code

            N := Make_Subprogram_Call
              (Make_Defining_Identifier (PN (P_Invoke_Db)),
               Make_List_Id (Make_Defining_Identifier (VN (V_Index))));

            N := Make_Assignment_Statement
              (Make_Defining_Identifier (PN (P_Invoke_Name_Access)),
               N);
            Append_Node_To_List (N, Invoke_Statements);

            --  The condition

            N := Make_Designator
              (PN (P_Invoke_Name_Access),
               Is_All => True);

            C_1 := Make_Expression
              (Make_Defining_Identifier (VN (V_Operation)),
               Op_Equal,
               N);

            --  Generate the "case" statement afetr adding a "when
            --  others" clause

            N := Make_Raise_Statement (Make_Designator (EN (E_Program_Error)));
            N := Make_Case_Statement_Alternative
              (No_List,
               Make_List_Id (N));
            Append_Node_To_List (N, Invoke_Subp_Bodies);

            N := Make_Case_Statement
              (Make_Designator (VN (V_Index)),
               Invoke_Subp_Bodies);
            Append_Node_To_List (N, Invoke_Then_Statements);

         end if;

         N := Make_Subprogram_Call
           (RE (RE_Raise_Bad_Operation),
            Make_List_Id (RE (RE_Default_Sys_Member)));
         Append_Node_To_List (N, Else_Statements);

         N := Make_If_Statement
           (C_1,
            Invoke_Then_Statements,
            Invoke_Elsif_Statements,
            Else_Statements);

         Exception_Handler := Non_User_Exception_Handler;

         N := Make_Block_Statement
           (Declarative_Part  => No_List,
            Statements        =>
              Make_List_Id (N),
            Exception_Handler =>
              Make_List_Id (Exception_Handler));
         Append_Node_To_List (N, Invoke_Statements);

         --  Generation of the Invoke Procedure

         N := Make_Subprogram_Implementation
           (Spec, D, Invoke_Statements);

         return N;
      end Invoke_Body;

      ------------------------
      -- Invoke_Declaration --
      ------------------------

      procedure Invoke_Declaration (L : List_Id) is
         N : Node_Id;
      begin
         N := Make_Designator
           (Designator => PN (P_Request),
            Is_All     => True);
         N := Make_Subprogram_Call
           (RE (RE_Operation),
            Make_List_Id (N));
         N := Make_Subprogram_Call
           (RE (RE_To_Standard_String),
            Make_List_Id (N));
         N := Make_Object_Declaration
           (Defining_Identifier =>
              Make_Defining_Identifier (VN (V_Operation)),
            Constant_Present    => True,
            Object_Definition   => RE (RE_String_2),
            Expression          => N);
         Append_Node_To_List (N, L);

         N := Make_Object_Declaration
           (Defining_Identifier =>
              Make_Defining_Identifier (VN (V_Argument_List)),
            Object_Definition   => RE (RE_Ref_4));
         Append_Node_To_List (N, L);

         if Use_SII then
            declare
               C         : Node_Id;
               M         : Node_Id;
               R         : Name_Id;
               Cast_Node : Node_Id;
            begin
               N := Make_Object_Declaration
                 (Defining_Identifier =>
                    Make_Defining_Identifier (VN (V_Operation_Argument_List)),
                  Constant_Present    => False,
                  Object_Definition   => RE (RE_Ref_3),
                  Expression          => No_Node);
               Append_Node_To_List (N, L);

               N := Make_Subprogram_Call
                 (RE (RE_Request_Access),
                  Make_List_Id
                  (Make_Defining_Identifier (PN (P_Request))));

               N := Make_Object_Declaration
                 (Defining_Identifier =>
                    Make_Defining_Identifier (VN (V_Request)),
                  Constant_Present    => True,
                  Object_Definition   => RE (RE_Request_Access),
                  Expression          => N);
               Append_Node_To_List (N, L);

               C := Make_Designator
                 (Designator => PN (P_Dependent_Binding_Object),
                  Parent     => VN (V_Request));

               --  Request binding object

               N := Make_Object_Declaration
                 (Defining_Identifier =>
                    Make_Defining_Identifier (VN (V_Binding_Object)),
                  Constant_Present    => True,
                  Object_Definition   => RE (RE_Ref_10),
                  Expression          => C);
               Append_Node_To_List (N, L);

               --  The GIOP Session is the Component attribut
               --  Dependent_Binding_Object

               C := Make_Subprogram_Call
                 (RE (RE_Get_Component),
                  Make_List_Id (Make_Designator (VN (V_Binding_Object))));

               N := Make_Object_Declaration
                 (Defining_Identifier =>
                    Make_Defining_Identifier (VN (V_Component)),
                  Constant_Present    => True,
                  Object_Definition   => RE (RE_Component_Access),
                  Expression          => C);
               Append_Node_To_List (N, L);

--                 --  The binding profile

--                 C := Make_Subprogram_Call
--                   (RE (RE_Entity_Of),
--                    Make_List_Id (Make_Designator (VN (V_Binding_Object))));

--                 C := Make_Subprogram_Call
--                   (RE (RE_Binding_Object_Access),
--                    Make_List_Id (C));

--                 C := Make_Subprogram_Call
--                   (RE (RE_Get_Profile),
--                    Make_List_Id (C));

--                 N := Make_Object_Declaration
--                   (Defining_Identifier =>
--                      Make_Defining_Identifier (VN (V_Binding_Profile)),
--                    Constant_Present    => True,
--                    Object_Definition   => RE (RE_Profile_Access),
--                    Expression          => C);
--                 Append_Node_To_List (N, L);

               --  Buffer for marshalling the arguments

               C := Make_Object_Instanciation (RE (RE_Buffer_Type));

               N := Make_Object_Declaration
                 (Defining_Identifier =>
                    Make_Defining_Identifier (VN (V_Buffer)),
                  Constant_Present => True,
                  Object_Definition => RE (RE_Buffer_Access),
                  Expression => C);
               Append_Node_To_List (N, L);

               Set_Str_To_Name_Buffer ("Args_Buffer");
               R := Name_Find;
               C := Make_Subprogram_Call
                 (Defining_Identifier   => RE (RE_To_PolyORB_String),
                  Actual_Parameter_Part =>
                    Make_List_Id (Make_Literal (New_String_Value (R, False))));

               Set_Str_To_Name_Buffer ("Buffer_Name");
               R := Name_Find;
               N := Make_Object_Declaration
                 (Defining_Identifier => Make_Defining_Identifier (R),
                  Constant_Present    => False,
                  Object_Definition   => RE (RE_Identifier),
                  Expression          => C);
               Append_Node_To_List (N, L);

               --  We cast the buffer pointer to U_Long

               Cast_Node := Make_Subprogram_Call
                 (Defining_Identifier =>
                    RE (RE_Buff_Access_To_Ulong),
                  Actual_Parameter_Part =>
                    Make_List_Id (Make_Designator (VN (V_Buffer))));

               M := Make_Subprogram_Call
                 (Defining_Identifier   => RE (RE_To_Any_0),
                  Actual_Parameter_Part => Make_List_Id (Cast_Node));

               N := Make_Object_Declaration
                 (Defining_Identifier =>
                    Make_Defining_Identifier (VN (V_Argument)),
                  Constant_Present    => False,
                  Object_Definition   => RE (RE_Any),
                  Expression          => M);
               Append_Node_To_List (N, L);

               --  N := Make_Object_Declaration
               --  (Defining_Identifier =>
               --     Make_Defining_Identifier (VN (V_Minor)),
               --   Object_Definition   => RE (RE_Natural));
               --  Append_Node_To_List (N, L);
            end;
         end if;

         if Use_Minimal_Hash_Function then
            N := Make_Object_Declaration
              (Defining_Identifier => Make_Defining_Identifier
               (VN (V_Index)),
               Object_Definition   => RE (RE_Natural));
            Append_Node_To_List (N, L);

            N := Make_Object_Declaration
              (Defining_Identifier => Make_Defining_Identifier
               (PN (P_Invoke_Name_Access)),
               Object_Definition   => Make_Defining_Identifier
               (TN (T_String_Ptr)));
            Append_Node_To_List (N, L);

         end if;
      end Invoke_Declaration;

      -----------------
      -- Invoke_Spec --
      -----------------

      function Invoke_Spec return Node_Id is
         N       : Node_Id;
         Param   : Node_Id;
         Profile : List_Id;

      begin
         Profile := New_List (K_List_Id);
         Param   := Make_Parameter_Specification
           (Make_Defining_Identifier (PN (P_Self)),
            RE (RE_Servant));
         Append_Node_To_List (Param, Profile);
         Param := Make_Parameter_Specification
           (Make_Defining_Identifier (PN (P_Request)),
            RE (RE_Object_Ptr));
         Append_Node_To_List (Param, Profile);
         N := Make_Subprogram_Specification
           (Make_Defining_Identifier (SN (S_Invoke)),
            Profile,
            No_Node);
         return N;
      end Invoke_Spec;

      ----------------------
      -- Is_A_Invoke_Part --
      ----------------------

      function Is_A_Invoke_Part return Node_Id is
         N                    : Node_Id;
         Statements           : constant List_Id := New_List (K_List_Id);
         Discret_Choice_Value : Value_Id;
         Profile : constant List_Id := New_List (K_List_Id);
      begin

         N := Make_Designator (PN (P_Request));
         Append_Node_To_List (N, Profile);

         N := Make_Designator (VN (V_Argument_List));
         Append_Node_To_List (N, Profile);

         N := Make_Type_Attribute
           (Make_Designator (SN (S_Is_A)), A_Access);
         Set_Correct_Parent_Unit_Name
           (N,
            Expand_Designator
            (Main_Package
             (Current_Entity)));
         Append_Node_To_List (N, Profile);

         N := Make_Subprogram_Call
           (RE (RE_Handle_Is_A),
            Profile);

         Append_Node_To_List (N, Statements);

         --  If no optimization is requested by the user, we generate
         --  an elsif statement. Else, we generate a case alternative
         --  statement

         if not Use_Minimal_Hash_Function then
            N := Make_Block_Statement
              (Declarative_Part => No_List,
               Statements       => Statements);
         else
            --  Insert the subprogram name into the hash function
            --  generator and add a call to Register_Procedure

            Set_Str_To_Name_Buffer ("_is_a");
            Insert_And_Register_Statements
              (Name_Find);

            --  Prepare the case alternative
            --  * Discret Choice : value of N_Subprogram minus 1

            Discret_Choice_Value := New_Integer_Value
              (N_Subprograms - 1, 1, 10);

            N := Make_Block_Statement
              (Declarative_Part => No_List,
               Statements       => Statements);

            N := Make_Case_Statement_Alternative
              (Make_List_Id (Make_Literal (Discret_Choice_Value)),
               Make_List_Id (N));

         end if;
         return N;
      end Is_A_Invoke_Part;

      ----------------------------
      -- Implicit_CORBA_Methods --
      ----------------------------

      function Implicit_CORBA_Methods return List_Id
      is

         Result_List : constant List_Id := New_List (K_List_Id);

         --  To make the addition (or the removal) of an implicit
         --  CORBA method easier, we use the
         --  'Add_Implicit_CORBA_Method' subprogram.  This subprogram
         --  takes the method name, the corresponding Handle_XX
         --  procedure and the profile of this procedure.

         procedure Add_Implicit_CORBA_Method
           (Method_Name : String;
            Method_Handle : Node_Id;
            Method_Profile : List_Id);

         -------------------------------
         -- Add_Implicit_CORBA_Method --
         -------------------------------

         procedure Add_Implicit_CORBA_Method
           (Method_Name : String;
            Method_Handle : Node_Id;
            Method_Profile : List_Id)
         is
            N                    : Node_Id;
            Discret_Choice_Value : Value_Id;
            Op_Name              : Name_Id;
            C                    : Node_Id;
         begin
            N := Make_Subprogram_Call
              (Method_Handle,
               Method_Profile);

            --  If no optimization is requested by the user, we
            --  generate an elsif statement. Else, we generate a case
            --  alternative statement

            if not Use_Minimal_Hash_Function then

               Set_Str_To_Name_Buffer (Method_Name);
               Op_Name := Name_Find;

               C := Make_Expression
                 (Make_Defining_Identifier (VN (V_Operation)),
                  Op_Equal,
                  Make_Literal
                  (New_String_Value
                   (Op_Name, False)));

               N := Make_Block_Statement
                 (Declarative_Part => No_List,
                  Statements       => Make_List_Id (N));

               N := Make_Elsif_Statement
                 (C, Make_List_Id (N));
            else
               --  Insert the subprogram name into the hash function
               --  generator and add a call to Register_Procedure

               Set_Str_To_Name_Buffer (Method_Name);
               Insert_And_Register_Statements
                 (Name_Find);

               --  Prepare the case alternative
               --  * Discret Choice : value of N_Subprogram minus 1

               Discret_Choice_Value := New_Integer_Value
                 (N_Subprograms - 1, 1, 10);

               N := Make_Block_Statement
                 (Declarative_Part => No_List,
                  Statements       => Make_List_Id (N));

               N := Make_Case_Statement_Alternative
                 (Make_List_Id (Make_Literal (Discret_Choice_Value)),
                  Make_List_Id (N));

            end if;

            Append_Node_To_List (N, Result_List);

         end Add_Implicit_CORBA_Method;

         Profile : List_Id;
         N       : Node_Id;
      begin
         --  For each implicit CORBA Method, add a similar block

         --  The "Interface" implicit method

         Profile := New_List (K_List_Id);

         N := Make_Designator (PN (P_Request));
         Append_Node_To_List (N, Profile);

         N := Make_Designator (VN (V_Argument_List));
         Append_Node_To_List (N, Profile);

         N := Make_Designator (PN (P_Repository_Id));
         Append_Node_To_List (N, Profile);

         Add_Implicit_CORBA_Method
           ("_interface",
            RE (RE_Handle_Interface),
            Profile);

         --  The Domain_Managers implicit method

         Profile := New_List (K_List_Id);

         N := Make_Designator (PN (P_Self));
         Append_Node_To_List (N, Profile);

         N := Make_Designator (PN (P_Request));
         Append_Node_To_List (N, Profile);

         N := Make_Designator (VN (V_Argument_List));
         Append_Node_To_List (N, Profile);

         Add_Implicit_CORBA_Method
           ("_domain_managers",
            RE (RE_Handle_Domain_Managers),
            Profile);

         --  The Non_Existent implicit method

         Profile := New_List (K_List_Id);

         N := Make_Designator (PN (P_Request));
         Append_Node_To_List (N, Profile);

         N := Make_Designator (VN (V_Argument_List));
         Append_Node_To_List (N, Profile);

         Add_Implicit_CORBA_Method
           ("_non_existent",
            RE (RE_Handle_Non_Existent),
            Profile);

         Add_Implicit_CORBA_Method
           ("_not_existent",
            RE (RE_Handle_Non_Existent),
            Profile);

         return Result_List;
      end Implicit_CORBA_Methods;

      -----------------------
      -- Servant_Is_A_Body --
      -----------------------

      function Servant_Is_A_Body (Spec : Node_Id) return Node_Id is
         Statements : constant List_Id := New_List (K_List_Id);
         N          : Node_Id;
      begin
         N := Implementation_Package (Current_Entity);
         N := First_Node
           (Visible_Part (Package_Specification (N)));
         N := Expand_Designator (N);
         N := Make_Type_Attribute (N, A_Class);
         N := Make_Expression
           (Make_Designator
            (Designator => PN (P_Obj),
             Is_All     => True),
            Op_In,
            N);
         N := Make_Return_Statement (N);
         Append_Node_To_List (N, Statements);

         return  Make_Subprogram_Implementation
           (Spec, No_List, Statements);
      end Servant_Is_A_Body;

      -----------------------------
      -- Skeleton_Initialization --
      -----------------------------

      procedure Skeleton_Initialization (L : List_Id) is
         N         : Node_Id;
         V         : Value_Id;
         Aggregates : List_Id;
      begin
         Aggregates := New_List (K_List_Id);
         N := Defining_Identifier
           (Package_Declaration (Current_Package));
         V := New_String_Value
           (Fully_Qualified_Name (N), False);
         N := Make_Subprogram_Call
           (RE (RE_Add),
            Make_List_Id (Make_Literal (V)));
         N := Make_Component_Association
           (Selector_Name  =>
              Make_Defining_Identifier (PN (P_Name)),
            Expression          =>
              N);
         Append_Node_To_List (N, Aggregates);

         N := Make_Component_Association
           (Selector_Name  =>
              Make_Defining_Identifier (PN (P_Conflicts)),
            Expression          =>
              RE (RE_Empty));
         Append_Node_To_List (N, Aggregates);

         N := Make_Component_Association
           (Selector_Name  =>
              Make_Defining_Identifier (PN (P_Depends)),
            Expression          =>
              RE (RE_Empty));
         Append_Node_To_List (N, Aggregates);

         N := Make_Component_Association
           (Selector_Name  =>
              Make_Defining_Identifier (PN (P_Provides)),
            Expression          =>
              RE (RE_Empty));
         Append_Node_To_List (N, Aggregates);

         N := Make_Component_Association
           (Selector_Name  =>
              Make_Defining_Identifier (PN (P_Implicit)),
            Expression          =>
              RE (RE_False));
         Append_Node_To_List (N, Aggregates);

         N := Make_Component_Association
           (Selector_Name  =>
              Make_Defining_Identifier (PN (P_Init)),
            Expression          =>
              Make_Type_Attribute
            (Make_Designator (SN (S_Deferred_Initialization)),
             A_Access));
         Append_Node_To_List (N, Aggregates);

         N := Make_Record_Aggregate
           (Aggregates);

         N := Make_Qualified_Expression
           (Subtype_Mark => RE (RE_Module_Info),
            Aggregate    => N);

         N := Make_Subprogram_Call
           (RE (RE_Register_Module),
            Make_List_Id (N));
         Append_Node_To_List (N, L);
      end Skeleton_Initialization;

      --------------------------------
      -- Non_User_Exception_Handler --
      --------------------------------

      function Non_User_Exception_Handler return Node_Id is
         Result     : Node_Id;
         Selector   : Node_Id;
         Expression : Node_Id;
         N          : Node_Id;
         D          : constant List_Id := New_List (K_List_Id);
         S          : constant List_Id := New_List (K_List_Id);
      begin
         --  Generation of the "E : others" statement

         Selector := Make_Object_Declaration
           (Defining_Identifier =>
              Make_Defining_Identifier (PN (P_E)),
            Object_Definition => No_Node);

         --  Body of the exception handler

         N := Make_Subprogram_Call
           (RE (RE_System_Exception_To_Any),
            Make_List_Id
            (Make_Defining_Identifier (PN (P_E))));

         N := Make_Subprogram_Call
           (RE (RE_To_CORBA_Any),
            Make_List_Id (N));

         N := Make_Subprogram_Call
           (RE (RE_Set_Exception),
            Make_List_Id
            (Make_Defining_Identifier (PN (P_Request)), N));
         Append_Node_To_List (N, S);

         N := Make_Return_Statement (No_Node);
         Append_Node_To_List (N, S);

         Expression := Make_Block_Statement
           (Declarative_Part => D,
            Statements       => S);

         Result := Make_Component_Association
           (Selector,
            Expression);
         return Result;
      end Non_User_Exception_Handler;

      -----------------------
      -- Hash_Package_Name --
      -----------------------

      function  Hash_Package_Name (E : Node_Id) return Name_Id is
         pragma Assert (FEN.Kind (E) = K_Interface_Declaration);
      begin
         Get_Name_String
           (FEU.Fully_Qualified_Name
            (FEN.Identifier (E),
             Separator => "_"));
         Add_Str_To_Name_Buffer ("_Hash");
         return Name_Find;
      end Hash_Package_Name;

      -------------------------------------------
      -- Initialize_Hash_Function_Optimization --
      -------------------------------------------

      procedure Initialize_Hash_Function_Optimization is
      begin
         --  Checking wether the user chose to optimize memory space
         --  or CPU Time

         if Optimize_CPU and then not Optimize_Memory then
            Optim := CPU_Time;
         elsif Optimize_Memory and then not Optimize_CPU then
            Optim := Memory_Space;
         else
            raise Program_Error;
         end if;

         --  Initialize the lists and the number of subprograms

         N_Subprograms           := 0;
         Register_Procedure_List := New_List (K_List_Id);
         Invoke_Subp_Bodies      := New_List (K_List_Id);

      end Initialize_Hash_Function_Optimization;

      ----------------------------------------
      -- Achieve_Hash_Function_Optimization --
      ----------------------------------------

      procedure Achieve_Hash_Function_Optimization (E : Node_Id) is
         N          : Node_Id;
         V          : Natural;

         --  This is the random seed used in the generation
         --  algorithm. Since we don't need the random aspect in IAC,
         --  we fix the seed

         Seed    : constant Natural := 4321;

         --  The ratio of the algorithm

         K_2_V   : Float;
      begin
         --  We add a "with" clause to be able to use the "Hash"
         --  function

         Add_With_Package (Make_Designator (Hash_Package_Name (E)));

         --  Declaration of the total number of subprograms

         N := Make_Literal (New_Integer_Value (N_Subprograms, 1, 10));
         N := Make_Object_Declaration
           (Defining_Identifier => Make_Defining_Identifier
            (PN (P_N_Operations)),
            Constant_Present    => True,
            Object_Definition   => RE (RE_Natural),
            Expression          => N);
         Append_Node_To_List (N, Statements (Current_Package));

         --  Definition of a string access type

         N := Make_Full_Type_Declaration
           (Defining_Identifier => Make_Defining_Identifier
            (TN (T_String_Ptr)),
            Type_Definition     => Make_Access_Type_Definition
            (RE (RE_String_2)));
         Append_Node_To_List (N, Statements (Current_Package));

         --  Declaration of the hash table. The hash table size is
         --  equal to the number of subprograms

         N := New_Node (K_Range_Constraint);
         Set_First (N, Make_Literal (Int0_Val));
         Set_Last
           (N,
            Make_Expression
            (Make_Defining_Identifier (PN (P_N_Operations)),
             Op_Minus,
             Make_Literal (Int1_Val)));

         N := Make_Object_Declaration
           (Defining_Identifier => Make_Defining_Identifier
            (PN (P_Invoke_Db)),
            Object_Definition   => Make_Array_Type_Definition
            (Make_List_Id (N),
             Make_Defining_Identifier (TN (T_String_Ptr))),
            Expression          => Make_Record_Aggregate
            (Make_List_Id
             (Make_Component_Association
              (Selector_Name => No_Node, --  'others'
               Expression    => Make_Null_Statement))));
         Append_Node_To_List (N, Statements (Current_Package));

         --  Insert the spec and the body of the Register_Procedure
         --  procedure

         N := Register_Procedure_Spec;
         Append_Node_To_List (N, Statements (Current_Package));
         N := Register_Procedure_Body (E);
         Append_Node_To_List (N, Statements (Current_Package));

         --  Compute the hash function generator, we use all positions
         --  In the case of CPU time optimization, the algorithm
         --  should succeed from the first iteration. For the Memory
         --  space optimization the algorithm may fail, wo we
         --  increment the number of the graph vertexes until it
         --  succeeds. We are sure that for V >= 257, the algorithm
         --  will succeed.

         V := 2 * Natural (N_Subprograms) + 1;
         loop
            K_2_V := Float (V) / Float (N_Subprograms);
            Initialize
              (Seed   => Seed,
               K_To_V => K_2_V,
               Optim  => Optim);
            begin
               Compute;
               exit;
            exception when others =>
               if Optim = CPU_Time then
                  raise;
               end if;
               V := V + 1;
            end;
         end loop;

         --  Produce the package containing the Hash function

         Get_Name_String (Hash_Package_Name (E));
         Produce (Name_Buffer (1 .. Name_Len));

         --  Finalize the generator

         Finalize;
      end Achieve_Hash_Function_Optimization;

      ------------------------------------
      -- Insert_And_Register_Statements --
      ------------------------------------

      procedure Insert_And_Register_Statements
        (Subp_Name   : Name_Id)
      is
         Profile : constant List_Id := New_List (K_List_Id);
         N       : Node_Id;
      begin
         --  First of all, we increment the number of subprograms

         N_Subprograms := N_Subprograms + 1;

         --  Insert the subprogram name into the perfect hash table
         --  generator

         Get_Name_String (Subp_Name);
         Insert (Name_Buffer (1 .. Name_Len));

         --  Generate the call to Register_Procedure, which put an
         --  access to the Invoke_XXXX in the right place into the
         --  hash table.

         N := Make_Literal
           (New_String_Value
            (Subp_Name, False));
         Append_Node_To_List (N, Profile);

         N := Make_Subprogram_Call
           (Make_Defining_Identifier (SN (S_Register_Procedure)),
            Profile);
         Append_Node_To_List (N, Register_Procedure_List);

      end Insert_And_Register_Statements;

      -----------------------------
      -- Register_Procedure_Spec --
      -----------------------------

      function Register_Procedure_Spec return Node_Id is
         N       : Node_Id;
         Profile : constant List_Id := New_List (K_List_Id);
      begin
         N := Make_Parameter_Specification
           (Defining_Identifier => Make_Defining_Identifier
            (PN (P_Operation_Name)),
            Subtype_Mark        => RE (RE_String_2));
         Append_Node_To_List (N, Profile);

         N := Make_Subprogram_Specification
           (Defining_Identifier => Make_Defining_Identifier
            (SN (S_Register_Procedure)),
            Parameter_Profile   => Profile,
            Return_Type         => No_Node);
         return N;
      end Register_Procedure_Spec;

      -----------------------------
      -- Register_Procedure_Body --
      -----------------------------

      function Register_Procedure_Body (E : Node_Id) return Node_Id is
         Spec             : Node_Id;
         Declarative_Part : constant List_Id := New_List (K_List_Id);
         Statements       : constant List_Id := New_List (K_List_Id);
         N                : Node_Id;
      begin
         Spec := Register_Procedure_Spec;

         --  Declarative part

         N := Make_Object_Declaration
           (Defining_Identifier => Make_Defining_Identifier
            (VN (V_Index)),
            Object_Definition   => RE (RE_Natural));
         Append_Node_To_List (N, Declarative_Part);

         N := Make_Object_Declaration
           (Defining_Identifier => Make_Defining_Identifier
            (PN (P_Invoke_Name_Access)),
            Object_Definition   => Make_Defining_Identifier
            (TN (T_String_Ptr)));
         Append_Node_To_List (N, Declarative_Part);

         --  Statements part

         N := Make_Defining_Identifier (SN (S_Hash));
         Set_Correct_Parent_Unit_Name
           (N, Make_Defining_Identifier (Hash_Package_Name (E)));

         N := Make_Subprogram_Call
           (N,
            Make_List_Id
            (Make_Defining_Identifier
             (PN (P_Operation_Name))));

         N := Make_Assignment_Statement
           (Make_Defining_Identifier (VN (V_Index)),
            N);
         Append_Node_To_List (N, Statements);

         --  Test if the hash code was already found in which case
         --  raise a program error

         N := Make_Subprogram_Call
           (Make_Defining_Identifier (PN (P_Invoke_Db)),
            Make_List_Id (Make_Defining_Identifier (VN (V_Index))));

         N := Make_Expression
           (N,
            Op_Not_Equal,
            Make_Null_Statement);

         N := Make_If_Statement
           (Condition       => N,
            Then_Statements => Make_List_Id
            (Make_Raise_Statement
             (Make_Defining_Identifier
              (EN (E_Program_Error)))));
         Append_Node_To_List (N, Statements);

         --  Assigning the procedure actual name

         N :=  Make_Defining_Identifier (PN (P_Invoke_Name_Access));
         N := Make_Assignment_Statement
           (N,
            Make_Object_Instanciation
            (Make_Qualified_Expression
             (Subtype_Mark => RE (RE_String_2),
              Aggregate    => Make_Record_Aggregate
              (Make_List_Id
               (Make_Defining_Identifier
                (PN (P_Operation_Name)))))));
         Append_Node_To_List (N, Statements);

         --  Update the hash table

         N := Make_Subprogram_Call
           (Make_Defining_Identifier (PN (P_Invoke_Db)),
            Make_List_Id (Make_Defining_Identifier (VN (V_Index))));
         N := Make_Assignment_Statement
           (N,
            Make_Defining_Identifier (PN (P_Invoke_Name_Access)));
         Append_Node_To_List (N, Statements);

         N := Make_Subprogram_Implementation
           (Specification => Spec,
            Declarations  => Declarative_Part,
            Statements    => Statements);

         return N;
      end Register_Procedure_Body;

      -----------
      -- Visit --
      -----------

      procedure Visit (E : Node_Id) is
      begin
         case FEN.Kind (E) is

            when K_Interface_Declaration =>
               Visit_Interface_Declaration (E);

            when K_Module =>
               Visit_Module (E);

            when K_Operation_Declaration =>
               Visit_Operation_Declaration (E);

            when K_Specification =>
               Visit_Specification (E);

            when others =>
               null;
         end case;
      end Visit;

      ---------------------------------
      -- Visit_Interface_Declaration --
      ---------------------------------

      procedure Visit_Interface_Declaration (E : Node_Id) is
         N              : Node_Id;
         Param          : Node_Id;
         Profile        : constant List_Id := New_List (K_List_Id);
         Invk_Spec      : Node_Id;
         Invk_Body      : Node_Id;
         Is_A_Invk_Part : Node_Id;
         Implicit_CORBA : List_Id;

      begin
         --  No Skel package is generated for an abstract or a local
         --  interface

         if FEN.Is_Abstract_Interface (E) or else
           FEN.Is_Local_Interface (E)
         then
            return;
         end if;

         N := BEN.Parent (Type_Def_Node (BE_Node (Identifier (E))));
         Push_Entity (BEN.IDL_Unit (Package_Declaration (N)));

         Set_Skeleton_Body;

         Invoke_Then_Statements := New_List (K_List_Id);

         Package_Initialization  := New_List (K_List_Id);

         --  If the user chose to generate optimised skeletons, we
         --  initialise the optimization related lists

         if Use_Minimal_Hash_Function then
            Initialize_Hash_Function_Optimization;
            Choice_List := Invoke_Subp_Bodies;
         else
            Invoke_Elsif_Statements := New_List (K_List_Id);
            Choice_List := Invoke_Elsif_Statements;
         end if;

         N := First_Entity (Interface_Body (E));
         while Present (N) loop
            Visit (N);
            N := Next_Entity (N);
         end loop;

         --  In case of multiple inheritence, generate the mappings
         --  for the operations and attributes of the parent interface
         --  including the first one.

         Map_Inherited_Entities_Bodies
           (Current_interface    => E,
            Visit_Operation_Subp => Visit_Operation_Declaration'Access,
            Skel                 => True);

         --  We make a difference between the Is_A Method and the rest of
         --  implicit CORBA methods for two reasons:
         --  * Is_A is not implicit since it is declared in the stub.
         --  * in case of non-optimisation the _is_a test of the operation is
         --    always put at the beginning of the if .. elsif .. elsif
         --    statement to make the code generation of operation code simpler.

         Is_A_Invk_Part := Is_A_Invoke_Part;

         --  Here, we assign the list of the the implicit CORBA
         --  methods It's important to do this before the finalization
         --  of the hash function generator (in case of optimisation)
         --  so that all the hash keys could be inserted before the
         --  computation pahse of the algorithm.

         Implicit_CORBA := Implicit_CORBA_Methods;

         --  At this point, all operations and attributes are
         --  visited. We achive the perfect hash function generation
         --  and the building of the conditional structure which
         --  handles the request.

         if Use_Minimal_Hash_Function then
            Achieve_Hash_Function_Optimization (E);
         end if;

         --  Here, we append the implicit CORBA methods either to the
         --  elsif statements or to the case statement depending on
         --  the optimization mode chosen by the developer

         Append_Node_To_List
           (First_Node (Implicit_CORBA),
            Choice_List);

         --  Build the Invoke procedure

         Invk_Spec := Invoke_Spec;
         Invk_Body := Invoke_Body (E, Is_A_Invk_Part);

         --  Add the Invoke procedure Spec

         Append_Node_To_List (Invk_Spec, Statements (Current_Package));

         --  Add the Invoke procedure Body

         Append_Node_To_List (Invk_Body, Statements (Current_Package));

         --  Generation of the Servant_Is_A function

         Param := Make_Parameter_Specification
           (Make_Defining_Identifier (PN (P_Obj)),
            RE (RE_Servant));
         Append_Node_To_List (Param, Profile);
         N := Make_Subprogram_Specification
           (Make_Defining_Identifier (SN (S_Servant_Is_A)),
            Profile,
            RE (RE_Boolean_0));
         Append_Node_To_List (N, Statements (Current_Package));

         N := Servant_Is_A_Body (N);
         Append_Node_To_List (N, Statements (Current_Package));

         --  Generation of the Deferred_Initialization procedure

         N := Deferred_Initialization_Body (E);
         Append_Node_To_List (N, Statements (Current_Package));

         Skeleton_Initialization (Package_Initialization);
         Set_Package_Initialization (Current_Package, Package_Initialization);

         Pop_Entity;
      end Visit_Interface_Declaration;

      ------------------
      -- Visit_Module --
      ------------------

      procedure Visit_Module (E : Node_Id) is
         D : Node_Id;
      begin
         if not Map_Particular_CORBA_Parts (E, PK_Skel_Body) then
            Push_Entity (Stub_Node (BE_Node (Identifier (E))));
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

      procedure Visit_Operation_Declaration (E : Node_Id) is
         Spec   : Node_Id;
         N      : Node_Id;
      begin
         Spec := Stub_Node (BE_Node (Identifier (E)));
         N := Gen_Invoke_Part (Spec);
         Append_Node_To_List (N, Choice_List);
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

   end Package_Body;
end Backend.BE_CORBA_Ada.Skels;
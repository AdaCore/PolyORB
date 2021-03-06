------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--            B A C K E N D . B E _ C O R B A _ A D A . C D R S             --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2005-2013, Free Software Foundation, Inc.          --
--                                                                          --
-- This is free software;  you can redistribute it  and/or modify it  under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for  more details.                                               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
--                  PolyORB is maintained by AdaCore                        --
--                     (email: sales@adacore.com)                           --
--                                                                          --
------------------------------------------------------------------------------

with Namet;     use Namet;
with Values;    use Values;

with Frontend.Nodes;  use Frontend.Nodes;
with Frontend.Nutils;

with Backend.BE_CORBA_Ada.Nodes;       use Backend.BE_CORBA_Ada.Nodes;
with Backend.BE_CORBA_Ada.Nutils;      use Backend.BE_CORBA_Ada.Nutils;
with Backend.BE_CORBA_Ada.IDL_To_Ada;  use Backend.BE_CORBA_Ada.IDL_To_Ada;
with Backend.BE_CORBA_Ada.Runtime;     use Backend.BE_CORBA_Ada.Runtime;

with Backend.BE_CORBA_Ada.Common;      use Backend.BE_CORBA_Ada.Common;

package body Backend.BE_CORBA_Ada.CDRs is

   package FEN renames Frontend.Nodes;
   package FEU renames Frontend.Nutils;
   package BEN renames Backend.BE_CORBA_Ada.Nodes;
   package BEU renames Backend.BE_CORBA_Ada.Nutils;
   package BEA renames Backend.BE_CORBA_Ada;

   package body Package_Spec is

      function Args_Type_Record (E : Node_Id) return Node_Id;
      --  Builds a record type declaration. The members of the record
      --  type are the operation arguments and result.

      function Marshaller_Spec (E : Node_Id) return Node_Id;
      --  Builds the spec of the static marshaller subprogram

      function Unmarshaller_Spec (E : Node_Id) return Node_Id;
      --  Builds the spec of the static unmarshaller subprogram

      procedure Visit_Attribute_Declaration (E : Node_Id);
      procedure Visit_Interface_Declaration (E : Node_Id);
      procedure Visit_Module (E : Node_Id);
      procedure Visit_Operation_Declaration (E : Node_Id);
      procedure Visit_Specification (E : Node_Id);

      ----------------------
      -- Args_Type_Record --
      ----------------------

      function Args_Type_Record (E : Node_Id) return Node_Id is
         pragma Assert (FEN.Kind (E) = K_Operation_Declaration);
         Spec       : constant Node_Id := Stub_Node
           (BE_Node (Identifier (E)));
         P          : constant List_Id := Parameter_Profile (Spec);
         T          : constant Node_Id := Return_Type (Spec);
         Components : List_Id;
         Component  : Node_Id;
         Parameter  : Node_Id;
         Args_Type  : Node_Id := No_Node;
         Par_Type   : Node_Id;
      begin
         Components := New_List;

         --  For each parameter in the subprogram profile, a member
         --  with the same name and the same type is generated in the
         --  record

         if not BEU.Is_Empty (P) then

            --  Skip the first parameter corresponding to 'Self'

            Parameter := Next_Node (First_Node (P));
            while Present (Parameter) loop

               --  If the parameter type is a class-wide type, we
               --  remove the "'Class" attribute from the type name.

               Par_Type := Parameter_Type (Parameter);

               if BEN.Kind (Par_Type) = K_Attribute_Reference then
                  Par_Type := Prefix (Par_Type);
               end if;

               Component := Make_Component_Declaration
                 (Defining_Identifier => Defining_Identifier (Parameter),
                  Subtype_Indication  => Par_Type);
               Append_To (Components, Component);
               Parameter := Next_Node (Parameter);
            end loop;
         end if;

         --  If the subprogram is a function, we add an additional
         --  member corresponding to the result of the function.

         if Present (T) then

            --  If the return type is a class-wide type, we remove the
            --  "'Class" attribute from the type name

            Par_Type := T;

            if BEN.Kind (Par_Type) = K_Attribute_Reference then
               Par_Type := Prefix (Par_Type);
            end if;

            Component := Make_Component_Declaration
              (Defining_Identifier => Make_Defining_Identifier
               (PN (P_Returns)),
               Subtype_Indication  => Par_Type);
            Append_To (Components, Component);
         end if;

         --  Type Declaration

         Args_Type := Make_Full_Type_Declaration
           (Defining_Identifier => Map_Args_Type_Identifier
              (Defining_Identifier (Spec)),
            Type_Definition     => Make_Record_Definition
              (Components));

         return Args_Type;
      end Args_Type_Record;

      ---------------------
      -- Marshaller_Spec --
      ---------------------

      function Marshaller_Spec (E : Node_Id) return Node_Id is
         pragma Assert (FEN.Kind (E) = K_Operation_Declaration);

         Spec       : constant Node_Id := Stub_Node
           (BE_Node (Identifier (E)));
         Profile   : List_Id;
         Parameter : Node_Id;
         S         : Node_Id;
      begin
         Profile  := New_List;

         --  'Role' parameter

         Parameter := Make_Parameter_Specification
           (Defining_Identifier => Make_Defining_Identifier
            (PN (P_Role)),
            Subtype_Mark        => RE (RE_Boolean_0),
            Parameter_Mode      => Mode_In);
         Append_To (Profile, Parameter);

         --  'Args' parameter

         Parameter := Make_Parameter_Specification
           (Defining_Identifier => Make_Defining_Identifier
            (PN (P_Args)),
            Subtype_Mark        => Make_Access_Type_Definition
            (Expand_Designator (Type_Def_Node (BE_Node (Identifier (E))))),
            Parameter_Mode      => Mode_In);
         Append_To (Profile, Parameter);

         --  'Buffer' parameter

         Parameter := Make_Parameter_Specification
           (Defining_Identifier => Make_Defining_Identifier
            (PN (P_Buffer)),
            Subtype_Mark        => RE (RE_Buffer_Access),
            Parameter_Mode      => Mode_In);
         Append_To (Profile, Parameter);

         --  'Representation' parameter

         Parameter := Make_Parameter_Specification
           (Defining_Identifier => Make_Defining_Identifier
            (PN (P_Representation)),
            Subtype_Mark        => Make_Attribute_Reference
            (RE (RE_CDR_Representation), A_Class),
            Parameter_Mode      => Mode_In);
         Append_To (Profile, Parameter);

         --  'First_Arg_Alignment' parameter

         Parameter := Make_Parameter_Specification
           (Defining_Identifier => Make_Defining_Identifier
            (PN (P_First_Arg_Alignment)),
            Subtype_Mark        => RE (RE_Alignment_Type),
            Parameter_Mode      => Mode_In);
         Append_To (Profile, Parameter);

         --  'Error' parameter

         Parameter := Make_Parameter_Specification
           (Defining_Identifier => Make_Defining_Identifier
            (PN (P_Error)),
            Subtype_Mark        => RE (RE_Error_Container),
            Parameter_Mode      => Mode_Inout);
         Append_To (Profile, Parameter);

         --  Subprogram Specification

         S := Make_Subprogram_Specification
           (Map_Marshaller_Identifier (Defining_Identifier (Spec)),
            Profile,
            No_Node);

         return S;
      end Marshaller_Spec;

      -----------------------
      -- Unmarshaller_Spec --
      -----------------------

      function Unmarshaller_Spec (E : Node_Id) return Node_Id is
         pragma Assert (FEN.Kind (E) = K_Operation_Declaration);

         Spec       : constant Node_Id := Stub_Node
           (BE_Node (Identifier (E)));
         Profile   : List_Id;
         Parameter : Node_Id;
         S         : Node_Id;
      begin
         Profile  := New_List;

         --  'Role' parameter

         Parameter := Make_Parameter_Specification
           (Defining_Identifier => Make_Defining_Identifier
            (PN (P_Role)),
            Subtype_Mark        => RE (RE_Boolean_0),
            Parameter_Mode      => Mode_In);
         Append_To (Profile, Parameter);

         --  'Args' parameter

         Parameter := Make_Parameter_Specification
           (Defining_Identifier => Make_Defining_Identifier
            (PN (P_Args)),
            Subtype_Mark        => Make_Access_Type_Definition
            (Expand_Designator (Type_Def_Node (BE_Node (Identifier (E))))),
            Parameter_Mode      => Mode_In);
         Append_To (Profile, Parameter);

         --  'Buffer' parameter

         Parameter := Make_Parameter_Specification
           (Defining_Identifier => Make_Defining_Identifier
            (PN (P_Buffer)),
            Subtype_Mark        => RE (RE_Buffer_Access),
            Parameter_Mode      => Mode_In);
         Append_To (Profile, Parameter);

         --  'Representation' parameter

         Parameter := Make_Parameter_Specification
           (Defining_Identifier => Make_Defining_Identifier
            (PN (P_Representation)),
            Subtype_Mark        => Make_Attribute_Reference
            (RE (RE_CDR_Representation), A_Class),
            Parameter_Mode      => Mode_In);
         Append_To (Profile, Parameter);

         --  'First_Arg_Alignment' parameter

         Parameter := Make_Parameter_Specification
           (Defining_Identifier => Make_Defining_Identifier
            (PN (P_First_Arg_Alignment)),
            Subtype_Mark        => RE (RE_Alignment_Type),
            Parameter_Mode      => Mode_In);
         Append_To (Profile, Parameter);

         --  'Error' parameter

         Parameter := Make_Parameter_Specification
           (Defining_Identifier => Make_Defining_Identifier
            (PN (P_Error)),
            Subtype_Mark        => RE (RE_Error_Container),
            Parameter_Mode      => Mode_Inout);
         Append_To (Profile, Parameter);

         --  Subprogram Specification

         S := Make_Subprogram_Specification
           (Map_Unmarshaller_Identifier (Defining_Identifier (Spec)),
            Profile,
            No_Node);

         return S;
      end Unmarshaller_Spec;

      -----------
      -- Visit --
      -----------

      procedure Visit (E : Node_Id) is
      begin
         case FEN.Kind (E) is

            when K_Attribute_Declaration =>
               Visit_Attribute_Declaration (E);

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
      -- Visit_Attribute_Declaration --
      ---------------------------------

      procedure Visit_Attribute_Declaration (E : Node_Id) is
         N    : Node_Id;
         D    : Node_Id;
      begin
         Set_CDR_Spec;

         D := First_Entity (Declarators (E));
         while Present (D) loop

            --  Explaining comment

            Set_Str_To_Name_Buffer ("Attribute : ");
            Get_Name_String_And_Append (IDL_Name (Identifier (D)));
            N := Make_Ada_Comment (Name_Find);
            Append_To (Visible_Part (Current_Package), N);

            D := Next_Entity (D);
         end loop;
      end Visit_Attribute_Declaration;

      ---------------------------------
      -- Visit_Interface_Declaration --
      ---------------------------------

      procedure Visit_Interface_Declaration (E : Node_Id) is
         N : Node_Id;
      begin
         --  No CDR package is generated for a local interface

         if FEN.Is_Local_Interface (E) then
            return;
         end if;

         N := BEN.Parent (Type_Def_Node (BE_Node (Identifier (E))));
         Push_Entity (BEN.IDL_Unit (Package_Declaration (N)));
         Set_CDR_Spec;

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
         if not Map_Particular_CORBA_Parts (E, PK_CDR_Spec) then
            Push_Entity (Stub_Node (BE_Node (Identifier (E))));
            D := First_Entity (Definitions (E));

            while Present (D) loop
               Visit (D);
               D := Next_Entity (D);
            end loop;

            Pop_Entity;
         end if;
      end  Visit_Module;

      ---------------------------------
      -- Visit_Operation_Declaration --
      ---------------------------------

      procedure Visit_Operation_Declaration (E : Node_Id) is
         N : Node_Id;
      begin
         Set_CDR_Spec;

         --  Explaining comment

         Set_Str_To_Name_Buffer ("Operation : ");
         Get_Name_String_And_Append (IDL_Name (Identifier (E)));
         N := Make_Ada_Comment (Name_Find);
         Append_To (Visible_Part (Current_Package), N);

         --  Generating the 'Operation_Name'_Args_Type declaration

         N := Args_Type_Record (E);
         Append_To (Visible_Part (Current_Package), N);
         Bind_FE_To_BE (Identifier (E), N, B_Type_Def);

         --  Generating the 'Operation_Name'_Marshaller spec

         N := Marshaller_Spec (E);
         Append_To (Visible_Part (Current_Package), N);
         Bind_FE_To_BE (Identifier (E), N, B_Marshaller);

         --  Generating the 'Operation_Name'_Unmarshaller spec

         N := Unmarshaller_Spec (E);
         Append_To (Visible_Part (Current_Package), N);
         Bind_FE_To_BE (Identifier (E), N, B_Unmarshaller);
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
   end Package_Spec;

   package body Package_Body is

      function Marshaller_Body (E : Node_Id) return Node_Id;
      function Unmarshaller_Body (E : Node_Id) return Node_Id;

      --  The functions Get_... and the variables ..._Number return
      --  new variable names. They are used to avoid conflicts.

      function Get_Element_Name return Name_Id;
      function Get_Index_Name return Name_Id;
      function Get_Length_Name return Name_Id;
      function Get_Union_Name return Name_Id;
      Element_Number : Nat := 0;
      Index_Number   : Nat := 0;
      Length_Number  : Nat := 0;
      Union_Number   : Nat := 0;

      function Storage_Variable_Declaration
        (Var_Name : Name_Id; Var_Type : Node_Id)
        return Node_Id;
      --  This function builds a variable declaration. The variable
      --  corresponds to an operation parameter or an operation
      --  result. The variable type is the PolyORB type corresponding
      --  to the Var_Node node

      function Do_Marshall
        (Var_Node : Node_Id;
         Var_Type : Node_Id;
         Buff     : Name_Id)
        return Node_Id;
      --  This function builds the marshalling statements to the
      --  buffer from the variable Var_Node

      function Do_Unmarshall
        (Var_Node : Node_Id;
         Var_Type : Node_Id;
         Buff     : Name_Id)
        return Node_Id;
      --  This function builds the unmarshalling statements from the
      --  buffer into the variable Var_Node

      procedure Visit_Attribute_Declaration (E : Node_Id);
      procedure Visit_Interface_Declaration (E : Node_Id);
      procedure Visit_Module (E : Node_Id);
      procedure Visit_Operation_Declaration (E : Node_Id);
      procedure Visit_Specification (E : Node_Id);

      ---------------------
      -- Marshaller_Body --
      ---------------------

      function Marshaller_Body (E : Node_Id) return Node_Id is
         pragma Assert (FEN.Kind (E) = K_Operation_Declaration);

         Subp_Spec         : Node_Id;
         Subp_Statements   : constant List_Id := New_List;
         Subp_Declarations : constant List_Id := New_List;

         P                 : constant List_Id := Parameters (E);
         T                 : constant Node_Id := Type_Spec (E);

         Client_Case       : constant List_Id := New_List
           (RE (RE_True));
         Client_Statements : constant List_Id := New_List;

         Server_Case       : constant List_Id := New_List
           (RE (RE_False));
         Server_Statements : constant List_Id := New_List;

         Case_Alternatives : constant List_Id := New_List;

         Alignment_Const   : Boolean := True;

         Args_Id           : Node_Id;
         Parameter         : Node_Id;
         Parameter_Name    : Name_Id;
         Parameter_Mode    : Mode_Id;
         Rewinded_Type     : Node_Id;
         N                 : Node_Id;
         M                 : Node_Id;

         --  The global structure of the generated XXXX_Marshaller
         --  function is:

         --  case Role is
         --     when Client_Entity =>
         --        <Marshall IN and INOUT Arguments> (if any)
         --     when Server_Entity =>
         --        <Marshall Result> (if any)
         --        <Marshall OUT and INOUT Arguments> (if any)
         --  end case;

      begin

         Subp_Spec := Marshaller_Node (BE_Node (Identifier (E)));
         Args_Id   := Map_Args_Identifier
           (Defining_Identifier
            (Stub_Node
             (BE_Node
              (Identifier
               (E)))));

         --  If buffers pre-allocation option is enabled

         if BEA.Use_Optimized_Buffers_Allocation then
            declare
               Method_Buffer_Size : Node_Id;
               M                  : Node_Id;
            begin
               Method_Buffer_Size := Expand_Designator
                 (Buffer_Size_Node
                  (BE_Node
                   (FE_Node
                    (Subp_Spec))));

               --  Call of the method_name_buffer_size method

               M := Make_Subprogram_Call
                 (Method_Buffer_Size,
                  New_List
                  (Make_Defining_Identifier (PN (P_Role)),
                   Make_Defining_Identifier (PN (P_Args)),
                   Make_Defining_Identifier (PN (P_Buffer)),
                   Make_Defining_Identifier (PN (P_Data_Alignment))));

               if (Present (T) and then FEN.Kind (T) /= K_Void)
                    or else
                  Contains_Out_Parameters (E)
               then
                  Append_To (Server_Statements, M);
               end if;

               M := Make_Subprogram_Call
                 (Method_Buffer_Size,
                  New_List
                  (Make_Defining_Identifier (PN (P_Role)),
                   Make_Defining_Identifier (PN (P_Args)),
                   Make_Defining_Identifier (PN (P_Buffer)),
                    Make_Defining_Identifier (PN (P_Data_Alignment))));

               if not FEU.Is_Empty (P)
                 and then Contains_In_Parameters (E)
               then
                  Append_To (Client_Statements, M);
               end if;
            end;
         end if;

         if Present (T) and then FEN.Kind (T) /= K_Void then

            Rewinded_Type := FEU.Get_Original_Type_Specifier (T);

            --  Explaining comment

            Set_Str_To_Name_Buffer ("Marshalling Result : ");
            Get_Name_String_And_Append  (PN (P_Returns));
            Add_Str_To_Name_Buffer (" => ");
            Add_Str_To_Name_Buffer
              (FEN.Node_Kind'Image
               (FEN.Kind
                (Rewinded_Type)));
            N := Make_Ada_Comment (Name_Find);

            Append_To (Server_Statements, N);

            --  Aligning CDR position in Buffer

            N := Make_Subprogram_Call
              (RE (RE_Pad_Align),
               New_List
               (Make_Identifier (PN (P_Buffer)),
                Make_Identifier (PN (P_Data_Alignment))));
            Append_To (Server_Statements, N);

            --  No need to align if the operation has no OUT or IN OUT formals

            if Contains_Out_Parameters (E) then
               N := Make_Assignment_Statement
                 (Make_Defining_Identifier (PN (P_Data_Alignment)),
                  Make_Literal (Int1_Val));
               Append_To (Server_Statements, N);
               Alignment_Const := False;
            end if;

            --  Marshalling the result and handling the error

            N := Make_Selected_Component
              (Copy_Node (Args_Id),
               Make_Defining_Identifier (PN (P_Returns)));

            N := Do_Marshall (N, T, PN (P_Buffer));
            Append_To (Server_Statements, N);

         end if;

         --  Handling parameters

         if not FEU.Is_Empty (P) then

            --  Aligning CDR position in Buffer in client and server
            --  parts.

            if Contains_Out_Parameters (E) then
               N := Make_Subprogram_Call
                 (RE (RE_Pad_Align),
                  New_List
                  (Make_Identifier (PN (P_Buffer)),
                   Make_Identifier (PN (P_Data_Alignment))));
               Append_To (Server_Statements, N);
            end if;

            if Contains_In_Parameters (E) then
               N := Make_Subprogram_Call
                 (RE (RE_Pad_Align),
                  New_List
                  (Make_Identifier (PN (P_Buffer)),
                   Make_Identifier (PN (P_Data_Alignment))));
               Append_To (Client_Statements, N);
            end if;

            Parameter := First_Entity (P);
            while Present (Parameter) loop

               Rewinded_Type  := FEU.Get_Original_Type_Specifier
                 (Type_Spec
                  (Parameter));
               Parameter_Name := To_Ada_Name
                 (IDL_Name
                  (Identifier
                   (Declarator
                    (Parameter))));
               Parameter_Mode := FEN.Parameter_Mode (Parameter);

               --  The IN    parameters are marshalled by client
               --  The OUT   parameters are marshalled by            server
               --  The INOUT parameters are marshalled by client and server

               --  Explaining comment

               Set_Str_To_Name_Buffer ("Marshall Parameter : ");
               Get_Name_String_And_Append (Parameter_Name);
               Add_Str_To_Name_Buffer (" => ");
               Add_Str_To_Name_Buffer
                 (FEN.Node_Kind'Image
                  (FEN.Kind
                   (FEU.Get_Original_Type_Specifier
                    (Type_Spec
                     (Parameter)))));

               if Is_In (Parameter_Mode) then
                  N := Make_Ada_Comment (Name_Find);
                  Append_To (Client_Statements, N);
               end if;

               if Is_Out (Parameter_Mode) then
                  N := Make_Ada_Comment (Name_Find);
                  Append_To (Server_Statements, N);
               end if;

               --  Marshalling the parameter and handling the error

               if Is_In (Parameter_Mode) then
                  N := Make_Selected_Component
                    (Copy_Node (Args_Id),
                     Make_Defining_Identifier (Parameter_Name));

                  N := Do_Marshall
                    (N,
                     Type_Spec (Parameter),
                     PN (P_Buffer));
                  Append_To (Client_Statements, N);
               end if;

               if Is_Out (Parameter_Mode) then
                  N := Make_Selected_Component
                    (Copy_Node (Args_Id),
                     Make_Defining_Identifier (Parameter_Name));

                  N := Do_Marshall
                    (N,
                     Type_Spec (Parameter),
                     PN (P_Buffer));
                  Append_To (Server_Statements, N);
               end if;

               Parameter := Next_Entity (Parameter);
            end loop;
         end if;

         --  The declarative part of the subprogram :

         if BEU.Is_Empty (Client_Statements)
           and then BEU.Is_Empty (Server_Statements)
         then
            declare
               Unref_Entities : constant array (Positive range <>) of Name_Id
                 := (PN (P_Role),
                     PN (P_Args),
                     PN (P_Buffer),
                     PN (P_Representation),
                     PN (P_First_Arg_Alignment),
                     PN (P_Error));
            begin
               for Index in Unref_Entities'Range loop
                  N := Make_Pragma
                    (Pragma_Unreferenced,
                     New_List (Make_Identifier (Unref_Entities (Index))));
                  Append_To (Subp_Declarations, N);
               end loop;
            end;
         else
            declare
               --  It's complicated to determine whether the
               --  parameters 'Error' and 'Representation' are or
               --  aren't referenced (depending) on the types
               --  handled. So we ignore warnings raised about these
               --  two parameters

               W_Off_Entities : constant array (Positive range <>) of Name_Id
                 := (PN (P_Representation),
                     PN (P_Error));
            begin
               for Index in W_Off_Entities'Range loop
                  N := Make_Pragma
                    (Pragma_Warnings,
                     New_List (RE (RE_Off),
                                   Make_Identifier (W_Off_Entities (Index))));
                  Append_To (Subp_Declarations, N);
               end loop;

               --  Common declarations

               --  1/ Data_Alignment : This variable modified when there are
               --     OUT or INOUT parameters in order to avoid the alignment
               --     of buffer more than one time.

               N := Make_Object_Declaration
                 (Defining_Identifier => Make_Defining_Identifier
                    (PN (P_Data_Alignment)),
                  Object_Definition   => RE (RE_Alignment_Type),
                  Constant_Present    => Alignment_Const,
                  Expression          => Make_Identifier
                    (PN (P_First_Arg_Alignment)));
               Append_To (Subp_Declarations, N);

               --  2/ This is the record that contains the operation
               --  parameters.

               N := Expand_Designator
                 (Type_Def_Node
                  (BE_Node
                   (Identifier
                    (E))));
               M := Make_Explicit_Dereference
                 (Make_Identifier
                    (PN (P_Args)));
               N := Make_Object_Declaration
                 (Defining_Identifier => Args_Id,
                  Object_Definition   => N,
                  Constant_Present    => True,
                  Expression          => M);
               Append_To (Subp_Declarations, N);
            end;
         end if;

         --  If the subprogram is a procedure without arguments, we
         --  add a null statement to the subprogram statements, else
         --  we build a switch case.

         if BEU.Is_Empty (Client_Statements)
           and then BEU.Is_Empty (Server_Statements)
         then
            Append_To (Subp_Statements, Make_Null_Statement);
         else
            --  Building the case statement

            if BEU.Is_Empty (Client_Statements) then
               Append_To (Client_Statements, Make_Null_Statement);
            end if;
            N := Make_Case_Statement_Alternative
              (Client_Case, Client_Statements);
            Append_To (Case_Alternatives, N);

            if BEU.Is_Empty (Server_Statements) then
               Append_To (Server_Statements, Make_Null_Statement);
            end if;
            N := Make_Case_Statement_Alternative
              (Server_Case, Server_Statements);
            Append_To (Case_Alternatives, N);

            N := Make_Case_Statement
              (Make_Identifier (PN (P_Role)), Case_Alternatives);
            Append_To (Subp_Statements, N);
         end if;

         --  Building the subprogram implementation

         N := Make_Subprogram_Body
           (Specification => Subp_Spec,
            Declarations  => Subp_Declarations,
            Statements    => Subp_Statements);

         return N;
      end Marshaller_Body;

      -----------------------
      -- Unmarshaller_Body --
      -----------------------

      function Unmarshaller_Body (E : Node_Id) return Node_Id is
         pragma Assert (FEN.Kind (E) = K_Operation_Declaration);

         Subp_Spec         : Node_Id;
         Subp_Statements   : constant List_Id := New_List;
         Subp_Declarations : constant List_Id := New_List;

         P                 : constant List_Id := Parameters (E);
         T                 : constant Node_Id := Type_Spec (E);

         Client_Case       : constant List_Id := New_List
           (RE (RE_True));
         Client_Statements : constant List_Id := New_List;

         Server_Case       : constant List_Id := New_List
           (RE (RE_False));
         Server_Statements : constant List_Id := New_List;

         Case_Alternatives : constant List_Id := New_List;

         Alignment_Const   : Boolean := True;

         Args_Id           : Node_Id;
         Parameter         : Node_Id;
         Parameter_Name    : Name_Id;
         Parameter_Mode    : Mode_Id;
         Rewinded_Type     : Node_Id;
         N                 : Node_Id;
         M                 : Node_Id;

         --  The global structure of the generated XXXX_Unmarshaller
         --  function is:

         --  case Role is
         --     when Client_Entity =>
         --        <Unmarshall Result> (if any)
         --        <Unmarshall OUT and INOUT Arguments> (if any)
         --     when Server_Entity =>
         --        <Unmarshall IN and INOUT Arguments> (if any)
         --  end case;

      begin
         Subp_Spec := Unmarshaller_Node (BE_Node (Identifier (E)));
         Args_Id   := Map_Args_Identifier
           (Defining_Identifier
            (Stub_Node
             (BE_Node
              (Identifier
               (E)))));

         --  We reset the variable index used to avoid name conflicts
         --  between arrays.

         Element_Number := 0;
         Index_Number   := 0;
         Union_Number   := 0;

         --  The declarative part generation of the subprogram is
         --  postponed after the handling of the arguments and the
         --  result because it depends on the result of this handling.

         --  If the subprogram is a function, we handle the result

         if Present (T) and then FEN.Kind (T) /= K_Void then

            Rewinded_Type := FEU.Get_Original_Type_Specifier (T);

            --  Explaining comment

            Set_Str_To_Name_Buffer ("Unmarshalling Result : ");
            Get_Name_String_And_Append (PN (P_Returns));
            Add_Str_To_Name_Buffer (" => ");
            Add_Str_To_Name_Buffer
              (FEN.Node_Kind'Image
               (FEN.Kind
                (Rewinded_Type)));
            N := Make_Ada_Comment (Name_Find);
            Append_To (Client_Statements, N);

            --  Aligning CDR position in Buffer

            N := Make_Subprogram_Call
              (RE (RE_Align_Position),
               New_List
               (Make_Identifier (PN (P_Buffer)),
                Make_Identifier (PN (P_Data_Alignment))));
            Append_To (Client_Statements, N);

            --  the operation does not have out or INOUT parameters,
            --  there is no need to this

            if Contains_Out_Parameters (E) then
               N := Make_Assignment_Statement
                 (Make_Defining_Identifier (PN (P_Data_Alignment)),
                  Make_Literal (Int1_Val));
               Append_To (Client_Statements, N);
               Alignment_Const := False;
            end if;

            --  Declaring the storage variable

            N := Storage_Variable_Declaration
              (PN (P_Returns), T);
            Append_To (Subp_Declarations, N);

            --  Unmarshalling the result and handling the error

            N := Do_Unmarshall
              (Make_Identifier (PN (P_Returns)), T, PN (P_Buffer));
            Append_To (Client_Statements, N);

            --  Updating the record field

            N := Make_Selected_Component
              (Copy_Node (Args_Id),
               Make_Defining_Identifier (PN (P_Returns)));

            N := Make_Assignment_Statement
              (N,
               Cast_Variable_From_PolyORB_Type
               (PN (P_Returns), T));
            Append_To (Client_Statements, N);

         end if;

         --  Handling parameters

         if not FEU.Is_Empty (P) then

            --  Aligning CDR position in Buffer in client and server parts

            if Contains_Out_Parameters (E) then
               N := Make_Subprogram_Call
                 (RE (RE_Align_Position),
                  New_List
                  (Make_Identifier (PN (P_Buffer)),
                   Make_Identifier (PN (P_Data_Alignment))));
               Append_To (Client_Statements, N);
            end if;

            if Contains_In_Parameters (E) then
               N := Make_Subprogram_Call
                 (RE (RE_Align_Position),
                  New_List
                  (Make_Identifier (PN (P_Buffer)),
                   Make_Identifier (PN (P_Data_Alignment))));
               Append_To (Server_Statements, N);
            end if;

            Parameter := First_Entity (P);

            while Present (Parameter) loop

               Rewinded_Type  := FEU.Get_Original_Type_Specifier
                 (Type_Spec
                  (Parameter));
               Parameter_Name := To_Ada_Name
                 (IDL_Name
                  (Identifier
                   (Declarator
                    (Parameter))));
               Parameter_Mode := FEN.Parameter_Mode (Parameter);

               --  The IN    parameters are unmarshalled by            server
               --  The OUT   parameters are unmarshalled by client
               --  The INOUT parameters are unmarshalled by client and server

               --  Explaining comment

               Set_Str_To_Name_Buffer ("Unmarshall Parameter : ");
               Get_Name_String_And_Append (Parameter_Name);
               Add_Str_To_Name_Buffer (" => ");
               Add_Str_To_Name_Buffer
                 (FEN.Node_Kind'Image
                  (FEN.Kind
                   (FEU.Get_Original_Type_Specifier
                    (Type_Spec
                     (Parameter)))));

               if Is_In (Parameter_Mode) then
                  N := Make_Ada_Comment (Name_Find);
                  Append_To (Server_Statements, N);
               end if;
               if Is_Out (Parameter_Mode) then
                  N := Make_Ada_Comment (Name_Find);
                  Append_To (Client_Statements, N);
               end if;

               --  Declaring the storage variable

               N := Storage_Variable_Declaration
                 (Parameter_Name, Type_Spec (Parameter));
               Append_To (Subp_Declarations, N);

               --  Unmarshalling the parameter and handling the error

               if Is_In (Parameter_Mode) then
                  N := Do_Unmarshall
                    (Make_Identifier (Parameter_Name),
                     Type_Spec (Parameter),
                     PN (P_Buffer));
                  Append_To (Server_Statements, N);
               end if;

               if Is_Out (Parameter_Mode) then
                  N := Do_Unmarshall
                    (Make_Identifier (Parameter_Name),
                     Type_Spec (Parameter),
                     PN (P_Buffer));
                  Append_To (Client_Statements, N);
               end if;

               --  Updating the record field

               if Is_In (Parameter_Mode) then
                  N := Make_Selected_Component
                    (Copy_Node (Args_Id),
                     Make_Defining_Identifier (Parameter_Name));

                  N := Make_Assignment_Statement
                    (N,
                     Cast_Variable_From_PolyORB_Type
                     (Parameter_Name, Type_Spec (Parameter)));
                  Append_To (Server_Statements, N);
               end if;

               if Is_Out (Parameter_Mode) then
                  N := Make_Selected_Component
                    (Copy_Node (Args_Id),
                     Make_Defining_Identifier (Parameter_Name));

                  N := Make_Assignment_Statement
                    (N,
                     Cast_Variable_From_PolyORB_Type
                     (Parameter_Name, Type_Spec (Parameter)));
                  Append_To (Client_Statements, N);
               end if;

               Parameter := Next_Entity (Parameter);
            end loop;
         end if;

         --  The declarative part of the subprogram :

         if BEU.Is_Empty (Client_Statements)
           and then BEU.Is_Empty (Server_Statements)
         then
            declare
               Unref_Entities : constant array (Positive range <>) of Name_Id
                 := (PN (P_Role),
                     PN (P_Args),
                     PN (P_Buffer),
                     PN (P_Representation),
                     PN (P_First_Arg_Alignment));
            begin
               for Index in Unref_Entities'Range loop
                  N := Make_Pragma
                    (Pragma_Unreferenced,
                     New_List (Make_Identifier (Unref_Entities (Index))));
                  Append_To (Subp_Declarations, N);
               end loop;
            end;
         else
            declare
               --  It's complicated to determine whether the
               --  parameters 'Error' and 'Representation' are or
               --  aren't referenced (depending) on the types
               --  handled. So we ignore warnings raised about these
               --  two parameters

               W_Off_Entities : constant array (Positive range <>) of Name_Id
                 := (PN (P_Representation),
                     PN (P_Error));

            begin
               for Index in W_Off_Entities'Range loop
                  N := Make_Pragma
                    (Pragma_Warnings,
                     New_List (RE (RE_Off),
                                   Make_Identifier (W_Off_Entities (Index))));
                  Append_To (Subp_Declarations, N);
               end loop;

               --  Common declarations

               --  1/ Data_Alignment : This variable modified when
               --  there are OUT or INOUT parameters in order to avoid
               --  the alignment of buffer more than one time.

               N := Make_Object_Declaration
                 (Defining_Identifier => Make_Defining_Identifier
                    (PN (P_Data_Alignment)),
                  Object_Definition   => RE (RE_Alignment_Type),
                  Constant_Present    => Alignment_Const,
                  Expression          => Make_Identifier
                    (PN (P_First_Arg_Alignment)));
               Append_To (Subp_Declarations, N);

               --  2/ This is the record that contains the operation
               --  parameters.

               N := Expand_Designator
                 (Type_Def_Node
                  (BE_Node
                   (Identifier
                    (E))));
               M := Make_Explicit_Dereference
                 (Make_Identifier
                    (PN (P_Args)));
               N := Make_Object_Declaration
                 (Defining_Identifier => Args_Id,
                  Object_Definition   => N,
                  Expression          => Make_Subprogram_Call
                    (N, New_List (M)));
               Append_To (Subp_Declarations, N);
            end;
         end if;

         --  If the subprogram is a procedure without arguments, we
         --  add a null statement to the subprogram statements, else
         --  we build a switch case.

         if BEU.Is_Empty (Client_Statements)
           and then BEU.Is_Empty (Server_Statements)
         then
            Append_To (Subp_Statements, Make_Null_Statement);
         else
            --  Building the case statement

            if BEU.Is_Empty (Client_Statements) then
               Append_To (Client_Statements, Make_Null_Statement);
            end if;

            N := Make_Case_Statement_Alternative
              (Client_Case, Client_Statements);
            Append_To (Case_Alternatives, N);

            if BEU.Is_Empty (Server_Statements) then
               Append_To (Server_Statements, Make_Null_Statement);
            end if;

            N := Make_Case_Statement_Alternative
              (Server_Case, Server_Statements);
            Append_To (Case_Alternatives, N);

            N := Make_Case_Statement
              (Make_Identifier (PN (P_Role)), Case_Alternatives);
            Append_To (Subp_Statements, N);

            --  Updating the argument list when needed

            Set_Str_To_Name_Buffer ("Update the argument list");
            N := Make_Ada_Comment (Name_Find);
            Append_To (Subp_Statements, N);

            M := Make_Explicit_Dereference
              (Make_Identifier
                 (PN (P_Args)));
            N := Make_Assignment_Statement (M, Copy_Node (Args_Id));
            Append_To (Subp_Statements, N);
         end if;

         --  Add an exception handler to the unmarshalling functions
         --  in case something wrong happens, e.g. a Constraint_Error
         --  exception is raised.

         declare
            Result     : Node_Id;
            M          : Node_Id;
            S          : constant List_Id := New_List;
            Excep_St   : List_Id := New_List;
            Profile    : constant List_Id := New_List;
         begin
            --  Body of the exception handler

            --  Set the exception informations

            M := Make_Identifier (PN (P_Error));
            Append_To (Profile, M);

            M := RE (RE_Marshal_E);
            Append_To (Profile, M);

            M := Make_Qualified_Expression
              (RE (RE_System_Exception_Members),
               Make_Record_Aggregate
               (New_List
                  (Make_Component_Association
                     (Make_Identifier (CN (C_Minor)),
                      Make_Literal (Values.New_Integer_Value (4, 1, 10))),
                   Make_Component_Association
                     (Make_Identifier (CN (C_Completed)),
                      RE (RE_Completed_No)))));
            Append_To (Profile, M);

            M := Make_Subprogram_Call (RE (RE_Throw), Profile);
            Append_To (S, M);

            Result := Make_Case_Statement_Alternative (No_List, S);

            Excep_St := New_List (Result);

            N := Make_Block_Statement
              (Declarative_Part => No_List,
               Statements       => Subp_Statements,
               Exception_Handler => Excep_St);
         end;

         --  Building the subprogram implementation

         N := Make_Subprogram_Body
           (Specification => Subp_Spec,
            Declarations  => Subp_Declarations,
            Statements    => New_List (N));

         return N;
      end Unmarshaller_Body;

      ----------------------------------
      -- Storage_Variable_Declaration --
      ----------------------------------

      function Storage_Variable_Declaration
        (Var_Name : Name_Id; Var_Type : Node_Id)
        return Node_Id
      is
         N         : Node_Id;
         Orig_Type : Node_Id;
      begin

         Orig_Type := FEU.Get_Original_Type_Specifier (Var_Type);

         case FEN.Kind (Orig_Type) is
            when K_Long =>
               N := Make_Object_Declaration
                 (Defining_Identifier => Make_Defining_Identifier (Var_Name),
                  Object_Definition   => RE (RE_Long_1));

            when K_Unsigned_Long | K_Enumeration_Type =>
               N := Make_Object_Declaration
                 (Defining_Identifier => Make_Defining_Identifier (Var_Name),
                  Object_Definition   => RE (RE_Unsigned_Long_1));

            when K_Long_Long =>
               N := Make_Object_Declaration
                 (Defining_Identifier => Make_Defining_Identifier (Var_Name),
                  Object_Definition   => RE (RE_Long_Long_1));

            when K_Unsigned_Long_Long =>
               N := Make_Object_Declaration
                 (Defining_Identifier => Make_Defining_Identifier (Var_Name),
                  Object_Definition   => RE (RE_Unsigned_Long_Long_1));

            when K_Short =>
               N := Make_Object_Declaration
                 (Defining_Identifier => Make_Defining_Identifier (Var_Name),
                  Object_Definition   => RE (RE_Short_1));

            when K_Unsigned_Short =>
               N := Make_Object_Declaration
                 (Defining_Identifier => Make_Defining_Identifier (Var_Name),
                  Object_Definition   => RE (RE_Unsigned_Short_1));

            when K_Float =>
               N := Make_Object_Declaration
                 (Defining_Identifier => Make_Defining_Identifier (Var_Name),
                  Object_Definition   => RE (RE_Float_1));

            when K_Double =>
               N := Make_Object_Declaration
                 (Defining_Identifier => Make_Defining_Identifier (Var_Name),
                  Object_Definition   => RE (RE_Double_1));

            when K_Long_Double =>
               N := Make_Object_Declaration
                 (Defining_Identifier => Make_Defining_Identifier (Var_Name),
                  Object_Definition   => RE (RE_Long_Double_1));

            when K_Char =>
               N := Make_Object_Declaration
                 (Defining_Identifier => Make_Defining_Identifier (Var_Name),
                  Object_Definition   => RE (RE_Char_1));

            when K_Wide_Char =>
               N := Make_Object_Declaration
                 (Defining_Identifier => Make_Defining_Identifier (Var_Name),
                  Object_Definition   => RE (RE_Wchar_1));

            when K_String
              | K_String_Type =>
               N := Make_Object_Declaration
                 (Defining_Identifier => Make_Defining_Identifier (Var_Name),
                  Object_Definition   => RE (RE_String_1));

            when K_Wide_String
              | K_Wide_String_Type =>
               N := Make_Object_Declaration
                 (Defining_Identifier => Make_Defining_Identifier (Var_Name),
                  Object_Definition   => RE (RE_Wide_String_1));

            when K_Octet =>
               N := Make_Object_Declaration
                 (Defining_Identifier => Make_Defining_Identifier (Var_Name),
                  Object_Definition   => RE (RE_Octet_1));

            when K_Boolean =>
               N := Make_Object_Declaration
                 (Defining_Identifier => Make_Defining_Identifier (Var_Name),
                  Object_Definition   => RE (RE_Boolean_1));

            when K_Object =>
               N := Make_Object_Declaration
                 (Defining_Identifier => Make_Defining_Identifier (Var_Name),
                  Object_Definition   => RE (RE_Ref_9));

            when K_Interface_Declaration
              | K_Forward_Interface_Declaration =>
               --  Check whether we are dealing with a TypeCode

               if Get_Predefined_CORBA_Entity (Orig_Type) = RE_Object then
                  N := Make_Object_Declaration
                    (Defining_Identifier => Make_Defining_Identifier
                       (Var_Name),
                     Object_Definition   => RE (RE_Local_Ref_1));
               else
                  N := Make_Object_Declaration
                    (Defining_Identifier => Make_Defining_Identifier
                       (Var_Name),
                     Object_Definition   => RE (RE_Ref_9));
               end if;

            when K_Fixed_Point_Type =>
               declare
                  FP_Type_Node     : Node_Id;
               begin
                  --  Getting the fixed point type

                  FP_Type_Node := Expand_Designator
                    (Type_Def_Node (BE_Node (Orig_Type)));

                  N := Make_Object_Declaration
                    (Defining_Identifier => Make_Defining_Identifier
                       (Var_Name),
                     Object_Definition   => FP_Type_Node);
               end;

            when K_Complex_Declarator
              | K_Structure_Type
              | K_Union_Type =>
               declare
                  Direct_Type    : constant Node_Id := Expand_Designator
                    (Type_Def_Node
                     (BE_Node
                      (Identifier
                       (Orig_Type))));
               begin
                  N := Make_Object_Declaration
                    (Defining_Identifier => Make_Defining_Identifier
                       (Var_Name),
                     Object_Definition   => Direct_Type);
               end;

            when K_Sequence_Type =>
               declare
                  Seq_Package_Node : Node_Id;
                  Seq_Exp          : Node_Id;
               begin
                  --  Getting the instantiated package node

                  Seq_Package_Node := Expand_Designator
                    (Instantiation_Node (BE_Node (Orig_Type)));

                  --  Sequence type

                  N := Make_Selected_Component
                    (Seq_Package_Node,
                     Make_Identifier (TN (T_Sequence)));

                  --  null sequence

                  Seq_Exp := Make_Selected_Component
                    (Seq_Package_Node,
                     Make_Identifier (PN (P_Null_Sequence)));

                  --  Variable declaration

                  N := Make_Object_Declaration
                    (Defining_Identifier => Make_Defining_Identifier
                     (Var_Name),
                     Object_Definition   => N,
                     Expression          => Seq_Exp);
               end;

            when K_Any =>
               N := Make_Object_Declaration
                 (Defining_Identifier => Make_Defining_Identifier (Var_Name),
                  Object_Definition   => RE (RE_Any_1));

            when others =>
               raise Program_Error with
                 "Storage_Variable_Declaration: "
                 & FEN.Node_Kind'Image (FEN.Kind (Orig_Type))
                 & ": Not Yet Implemented!";
         end case;

         return N;
      end Storage_Variable_Declaration;

      -----------------
      -- Do_Marshall --
      -----------------

      function Do_Marshall
        (Var_Node : Node_Id;
         Var_Type : Node_Id;
         Buff     : Name_Id)
        return Node_Id
      is
         Block_Dcl        : constant List_Id := New_List;
         Block_St         : constant List_Id := New_List;
         N                : Node_Id;
         Type_Spec_Node   : Node_Id;
         Direct_Type_Node : Node_Id;
      begin
         --  Getting the original type

         Type_Spec_Node := FEU.Get_Original_Type_Specifier (Var_Type);

         if FEN.Kind (Var_Type) = K_Simple_Declarator
           or else FEN.Kind (Var_Type) = K_Complex_Declarator
         then
            Direct_Type_Node := Type_Spec (Declaration (Var_Type));
         else
            Direct_Type_Node := Var_Type;
         end if;

         case FEN.Kind (Type_Spec_Node) is

            when K_Boolean
              | K_Double
              | K_Float
              | K_Long
              | K_Long_Double
              | K_Long_Long
              | K_Octet
              | K_Short
              | K_Unsigned_Long
              | K_Unsigned_Long_Long
              | K_Unsigned_Short
              | K_Enumeration_Type
              | K_Object =>

               N := Make_Subprogram_Call
                 (RE (RE_Marshall_2),
                  New_List
                  (Make_Identifier (Buff),
                   Cast_Variable_To_PolyORB_Type
                   (Var_Node, Direct_Type_Node)));
               Append_To (Block_St, N);

            when K_Fixed_Point_Type =>
               declare
                  FP_Type_Node     : Node_Id;
               begin

                  --  Getting the fixed point type

                  FP_Type_Node := Expand_Designator
                    (Type_Def_Node (BE_Node (Type_Spec_Node)));

                  --  Instantiate the package:
                  --  PolyORB.Representations.CDR.Common.Fixed_Point.

                  N := Make_Package_Instantiation
                    (Make_Defining_Identifier (VN (V_Fixed_Point)),
                     RU (RU_PolyORB_Representations_CDR_Common_Fixed_Point),
                     New_List (FP_Type_Node));
                  Append_To (Block_Dcl, N);

                  --  Marshall

                  N := Make_Selected_Component
                    (VN (V_Fixed_Point),
                     SN (S_Marshall));

                  N := Make_Subprogram_Call
                    (N,
                     New_List
                     (Make_Identifier (Buff),
                      Cast_Variable_To_PolyORB_Type
                      (Var_Node, Direct_Type_Node)));
                  Append_To (Block_St, N);
               end;

            when K_Char
              | K_String
              | K_String_Type
              | K_Wide_Char
              | K_Wide_String
              | K_Wide_String_Type =>
               declare
                  Profile : constant List_Id := New_List;
               begin
                  N := Make_Identifier (PN (P_Representation));
                  Append_To (Profile, N);

                  N := Make_Identifier (Buff);
                  Append_To (Profile, N);

                  Append_To (Profile,
                    Cast_Variable_To_PolyORB_Type
                      (Var_Node, Direct_Type_Node));

                  N := Make_Identifier (PN (P_Error));
                  Append_To (Profile, N);

                  N := Make_Subprogram_Call (RE (RE_Marshall_1), Profile);
                  Append_To (Block_St, N);

                  --  Handling the error

                  N := Make_Subprogram_Call
                    (RE (RE_Found),
                     New_List (Make_Identifier (PN (P_Error))));
                  N := Make_If_Statement
                    (Condition       => N,
                     Then_Statements => New_List
                     (Make_Return_Statement (No_Node)));
                  Append_To (Block_St, N);
               end;

            when K_Interface_Declaration
              | K_Forward_Interface_Declaration =>
               --  Check whether we are dealing with a TypeCode

               if Get_Predefined_CORBA_Entity (Type_Spec_Node) = RE_Object then
                  declare
                     Profile : constant List_Id := New_List;
                  begin
                     N := Make_Identifier (Buff);
                     Append_To (Profile, N);

                     N := Make_Attribute_Reference
                       (Make_Type_Conversion
                        (Make_Attribute_Reference
                         (RE (RE_CDR_Representation), A_Class),
                         Make_Identifier (PN (P_Representation))),
                        A_Unrestricted_Access);
                     Append_To (Profile, N);

                     Append_To (Profile,
                       Cast_Variable_To_PolyORB_Type
                         (Var_Node, Direct_Type_Node));

                     N := Make_Identifier (PN (P_Error));
                     Append_To (Profile, N);

                     N := Make_Subprogram_Call (RE (RE_Marshall_1), Profile);
                     Append_To (Block_St, N);

                     --  Handling the error

                     N := Make_Subprogram_Call
                       (RE (RE_Found),
                        New_List (Make_Identifier (PN (P_Error))));
                     N := Make_If_Statement
                       (Condition       => N,
                        Then_Statements => New_List
                          (Make_Return_Statement (No_Node)));
                     Append_To (Block_St, N);
                  end;
               else
                  N := Make_Subprogram_Call
                    (RE (RE_Marshall_2),
                     New_List
                     (Make_Identifier (Buff),
                      Cast_Variable_To_PolyORB_Type
                      (Var_Node, Direct_Type_Node)));
                  Append_To (Block_St, N);
               end if;

            when K_Sequence_Type =>
               declare
                  Seq_Package_Node : Node_Id;
                  Seq_Element      : Node_Id;
                  Index_Node       : Node_Id;
                  Range_Constraint : Node_Id;
                  Index_Name       : constant Name_Id := Get_Index_Name;
                  Seq_Length       : constant Name_Id := Get_Length_Name;
                  For_Statements   : constant List_Id := New_List;
               begin
                  --  Getting the instantiated package node

                  Seq_Package_Node := Expand_Designator
                    (Instantiation_Node (BE_Node (Type_Spec_Node)));

                  --  Getting the sequence length

                  N := Make_Object_Declaration
                    (Defining_Identifier => Make_Defining_Identifier
                     (Seq_Length),
                     Object_Definition   => RE (RE_Unsigned_Long_1));
                  Append_To (Block_Dcl, N);

                  N := Make_Selected_Component
                    (Seq_Package_Node,
                     Make_Identifier (SN (S_Length)));

                  N := Make_Subprogram_Call
                    (N,
                     New_List
                     (Cast_Variable_To_PolyORB_Type
                      (Var_Node, Direct_Type_Node)));
                  N := Make_Subprogram_Call
                    (RE (RE_Unsigned_Long_1),
                     New_List (N));
                  N := Make_Assignment_Statement
                    (Make_Defining_Identifier (Seq_Length), N);
                  Append_To (Block_St, N);

                  --  Marshalling the sequence length (Unsigned_Long)

                  N := Make_Subprogram_Call
                    (RE (RE_Marshall_2),
                     New_List
                     (Make_Identifier (Buff),
                      Make_Defining_Identifier (Seq_Length)));
                  Append_To (Block_St, N);

                  --  Marshalling the sequence elements

                  Index_Node := Make_Defining_Identifier (Index_Name);

                  --    Creating the range constraint

                  Range_Constraint := Make_Range_Constraint
                    (Make_Literal (Int1_Val),
                     Make_Defining_Identifier (Seq_Length));

                  --    Getting the sequence element

                  N := Make_Selected_Component
                    (Seq_Package_Node,
                     Get_Base_Identifier (RE (RE_Get_Element)));

                  Seq_Element := Make_Subprogram_Call
                    (N,
                     New_List
                     (Cast_Variable_To_PolyORB_Type
                      (Var_Node, Direct_Type_Node),
                      Make_Subprogram_Call
                      (RE (RE_Positive),
                       New_List (Index_Node))));

                  --    Marshalling the sequence element

                  N := Do_Marshall
                    (Var_Node => Seq_Element,
                     Var_Type => Type_Spec (Type_Spec_Node),
                     Buff     => Buff);
                  Append_To (For_Statements, N);

                  --    Building the loop

                  N := Make_For_Statement
                    (Index_Node,
                     Range_Constraint,
                     For_Statements);
                  Append_To (Block_St, N);
               end;

            when K_Complex_Declarator =>
               declare
                  I                    : Nat := 0;
                  Sizes                : constant List_Id :=
                    Range_Constraints
                    (Type_Definition
                     (Type_Def_Node
                      (BE_Node
                       (Identifier
                        (Type_Spec_Node)))));
                  Dim                  : Node_Id;
                  Loop_Statements      : List_Id := No_List;
                  Enclosing_Statements : List_Id;
                  Index_List           : constant List_Id :=
                    New_List;
                  Index_Node           : Node_Id := No_Node;
                  Index_Name           : constant Name_Id :=
                    Get_Index_Name;

               begin
                  --  Building the nested loops

                  Dim := First_Node (Sizes);
                  loop
                     Get_Name_String (Index_Name);
                     Add_Char_To_Name_Buffer ('_');
                     Add_Nat_To_Name_Buffer (I);
                     Index_Node := Make_Defining_Identifier
                       (Add_Suffix_To_Name (Unique_Suffix, Name_Find));
                     Append_To (Index_List, Index_Node);
                     Enclosing_Statements := Loop_Statements;
                     Loop_Statements := New_List;
                     N := Make_For_Statement
                       (Index_Node, Dim, Loop_Statements);

                     if I > 0 then
                        Append_To (Enclosing_Statements, N);
                     else
                        Append_To (Block_St, N);
                     end if;

                     I := I + 1;
                     Dim := Next_Node (Dim);
                     exit when No (Dim);
                  end loop;

                  --  Filling the statements of the deepest loop by
                  --  the marshalling of the corresponding array
                  --  element.

                  N := Make_Subprogram_Call (Var_Node, Index_List);
                  N := Do_Marshall
                    (Var_Node => N,
                     Var_Type => Type_Spec (Declaration (Type_Spec_Node)),
                     Buff     => Buff);
                  Append_To (Loop_Statements, N);

               end;

            when K_Structure_Type =>
               declare
                  Member       : Node_Id;
                  Declarator   : Node_Id;
                  Dcl_Ada_Name : Name_Id;
                  Dcl_Ada_Node : Node_Id;
               begin
                  Member := First_Entity (Members (Type_Spec_Node));

                  while Present (Member) loop
                     Declarator := First_Entity (FEN.Declarators (Member));

                     while Present (Declarator) loop

                        --  Getting the record field name

                        Dcl_Ada_Name := To_Ada_Name
                          (IDL_Name
                           (Identifier
                            (Declarator)));

                        Dcl_Ada_Node := Make_Selected_Component
                          (Var_Node,
                           Make_Identifier (Dcl_Ada_Name));

                        --  Marshalling the record field

                        N := Do_Marshall
                          (Var_Node => Dcl_Ada_Node,
                           Var_Type => Declarator,
                           Buff     => Buff);
                        Append_To (Block_St, N);

                        Declarator := Next_Entity (Declarator);
                     end loop;
                     Member := Next_Entity (Member);
                  end loop;
               end;

            when K_Union_Type =>
               declare
                  Switch_Node         : Node_Id;
                  Switch_Alternatives : List_Id;
                  Switch_Case         : Node_Id;
                  Has_Default         : Boolean := False;
                  Choices             : List_Id;
                  Literal_Parent      : Node_Id := No_Node;
                  Switch_Statements   : List_Id;
                  Switch_Type         : Node_Id;
                  Dcl_Ada_Name        : Name_Id;
                  Dcl_Ada_Node        : Node_Id;
                  Declarator          : Node_Id;
               begin
                  --  1/ Marshall the union switch

                  Switch_Node := Make_Selected_Component
                    (Var_Node,
                     Make_Identifier (FEN.Switch_Name (Type_Spec_Node)));

                  N := Do_Marshall
                    (Var_Node => Switch_Node,
                     Var_Type => Switch_Type_Spec (Type_Spec_Node),
                     Buff     => Buff);
                  Append_To (Block_St, N);

                  --  2/ Depending on the switch value, marshall the
                  --  corresponding flag.

                  Switch_Type := FEU.Get_Original_Type_Specifier
                    (Switch_Type_Spec
                     (Type_Spec_Node));
                  if FEN.Kind (Switch_Type) = K_Enumeration_Type then
                     Literal_Parent := Map_Expanded_Name
                       (Scope_Entity
                        (Identifier
                         (Switch_Type)));
                  end if;

                  Switch_Alternatives := New_List;
                  Switch_Case := First_Entity
                    (Switch_Type_Body
                     (Type_Spec_Node));

                  while Present (Switch_Case) loop
                     Map_Choice_List
                       (Labels (Switch_Case),
                        Literal_Parent,
                        Choices,
                        Has_Default);

                     Switch_Statements := New_List;

                     --  Getting the field name

                     Declarator := FEN.Declarator
                       (Element
                        (Switch_Case));

                     Dcl_Ada_Name := To_Ada_Name
                       (IDL_Name
                        (Identifier
                         (Declarator)));

                     Dcl_Ada_Node := Make_Selected_Component
                       (Var_Node,
                        Make_Identifier (Dcl_Ada_Name));

                     --  Marshalling the record field

                     N := Do_Marshall
                       (Var_Node => Dcl_Ada_Node,
                        Var_Type => Declarator,
                        Buff     => Buff);
                     Append_To (Switch_Statements, N);

                     --  Building the switch alternative

                     Append_To (Switch_Alternatives,
                       Make_Case_Statement_Alternative
                         (Choices, Switch_Statements));

                     Switch_Case := Next_Entity (Switch_Case);
                  end loop;

                  --  Add an empty when others clause to keep the compiler
                  --  happy.

                  if not Has_Default then
                     Append_To (Switch_Alternatives,
                        Make_Case_Statement_Alternative (No_List, No_List));
                  end if;

                  N := Make_Case_Statement
                    (Switch_Node,
                     Switch_Alternatives);
                  Append_To (Block_St, N);

               end;

            when K_Any =>
               declare
                  Profile : constant List_Id := New_List;
               begin
                  N := Make_Identifier (Buff);
                  Append_To (Profile, N);

                  N := Make_Attribute_Reference
                    (Make_Type_Conversion
                     (Make_Attribute_Reference
                      (RE (RE_CDR_Representation), A_Class),
                      Make_Identifier (PN (P_Representation))),
                     A_Unrestricted_Access);
                  Append_To (Profile, N);

                  Append_To (Profile,
                    Cast_Variable_To_PolyORB_Type
                      (Var_Node, Direct_Type_Node));

                  N := Make_Subprogram_Call (RE (RE_Marshall_1), Profile);
                  Append_To (Block_St, N);
               end;

            when others =>
               raise Program_Error with
                 "Do_Marshall: "
                 & FEN.Node_Kind'Image (FEN.Kind (Type_Spec_Node))
                 & ": Not Yet Implemented!";
         end case;

         N := Make_Block_Statement
           (Declarative_Part => Block_Dcl,
            Statements       => Block_St);

         return N;
      end Do_Marshall;

      -------------------
      -- Do_Unmarshall --
      -------------------

      function Do_Unmarshall
        (Var_Node : Node_Id;
         Var_Type : Node_Id;
         Buff     : Name_Id)
        return Node_Id
      is
         Block_Dcl        : constant List_Id := New_List;
         Block_St         : constant List_Id := New_List;
         N                : Node_Id;
         Type_Spec_Node   : Node_Id;
         Direct_Type_Node : Node_Id;
      begin
         --  Getting the original type

         Type_Spec_Node := FEU.Get_Original_Type_Specifier (Var_Type);

         if FEN.Kind (Var_Type) = K_Simple_Declarator
           or else FEN.Kind (Var_Type) = K_Complex_Declarator
         then
            Direct_Type_Node := Type_Spec (Declaration (Var_Type));
         else
            Direct_Type_Node := Var_Type;
         end if;

         case FEN.Kind (Type_Spec_Node) is

            when K_Boolean
              | K_Double
              | K_Float
              | K_Long
              | K_Long_Double
              | K_Long_Long
              | K_Octet
              | K_Short
              | K_Unsigned_Long
              | K_Unsigned_Long_Long
              | K_Unsigned_Short
              | K_Enumeration_Type
              | K_Object =>

               begin
                  N := Make_Subprogram_Call
                    (RE (RE_Unmarshall_2),
                     New_List
                     (Make_Identifier (Buff)));
                  N := Make_Assignment_Statement (Var_Node, N);
                  Append_To (Block_St, N);
               end;

            when K_Fixed_Point_Type =>
               declare
                  FP_Type_Node     : Node_Id;
               begin

                  --  Getting the fixed point type

                  FP_Type_Node := Expand_Designator
                    (Type_Def_Node (BE_Node (Type_Spec_Node)));

                  --  Instantiate the package :
                  --  PolyORB.Representations.CDR.Common.Fixed_Point.

                  N := Make_Package_Instantiation
                    (Make_Defining_Identifier (VN (V_Fixed_Point)),
                     RU (RU_PolyORB_Representations_CDR_Common_Fixed_Point),
                     New_List (FP_Type_Node));
                  Append_To (Block_Dcl, N);

                  --  Unmarshall

                  N := Make_Selected_Component
                    (VN (V_Fixed_Point),
                     SN (S_Unmarshall));

                  N := Make_Subprogram_Call
                    (N,
                     New_List
                     (Make_Identifier (Buff)));
                  N := Make_Assignment_Statement (Var_Node, N);
                  Append_To (Block_St, N);
               end;

            when K_Char
              | K_String
              | K_String_Type
              | K_Wide_Char
              | K_Wide_String
              | K_Wide_String_Type =>
               declare
                  Profile : constant List_Id := New_List;
               begin
                  N := Make_Identifier (PN (P_Representation));
                  Append_To (Profile, N);

                  N := Make_Identifier (Buff);
                  Append_To (Profile, N);

                  Append_To (Profile, Var_Node);

                  N := Make_Identifier (PN (P_Error));
                  Append_To (Profile, N);

                  N := Make_Subprogram_Call (RE (RE_Unmarshall_1), Profile);
                  Append_To (Block_St, N);

                  --  Handling the error

                  N := Make_Subprogram_Call
                    (RE (RE_Found),
                     New_List (Make_Identifier (PN (P_Error))));
                  N := Make_If_Statement
                    (Condition       => N,
                     Then_Statements => New_List
                     (Make_Return_Statement (No_Node)));
                  Append_To (Block_St, N);
               end;

            when K_Interface_Declaration
              | K_Forward_Interface_Declaration =>
               --  Check whether we are dealing with a TypeCode

               if Get_Predefined_CORBA_Entity (Type_Spec_Node) = RE_Object then
                  declare
                     Profile : constant List_Id := New_List;
                  begin
                     N := Make_Identifier (Buff);
                     Append_To (Profile, N);

                     N := Make_Attribute_Reference
                       (Make_Type_Conversion
                        (Make_Attribute_Reference
                         (RE (RE_CDR_Representation), A_Class),
                         Make_Identifier (PN (P_Representation))),
                        A_Unrestricted_Access);
                     Append_To (Profile, N);

                     Append_To (Profile, Var_Node);

                     N := Make_Identifier (PN (P_Error));
                     Append_To (Profile, N);

                     N := Make_Subprogram_Call (RE (RE_Unmarshall_1), Profile);
                     Append_To (Block_St, N);

                     --  Handling the error

                     N := Make_Subprogram_Call
                       (RE (RE_Found),
                        New_List (Make_Identifier (PN (P_Error))));
                     N := Make_If_Statement
                       (Condition       => N,
                        Then_Statements => New_List
                          (Make_Return_Statement (No_Node)));
                     Append_To (Block_St, N);
                  end;
               else
                  N := Make_Subprogram_Call
                    (RE (RE_Unmarshall_2),
                     New_List
                     (Make_Identifier (Buff)));
                  N := Make_Assignment_Statement (Var_Node, N);
                  Append_To (Block_St, N);
               end if;

            when K_Sequence_Type =>
               declare
                  Seq_Package_Node : Node_Id;
                  Index_Node       : Node_Id;
                  Range_Constraint : Node_Id;
                  Element_Dcl      : Node_Id;
                  Index_Name       : constant Name_Id := Get_Index_Name;
                  Seq_Element_Name : constant Name_Id := Get_Element_Name;
                  Seq_Length       : constant Name_Id := Get_Length_Name;
                  For_Statements   : constant List_Id := New_List;
               begin
                  --  Getting the instantiated package node

                  Seq_Package_Node := Expand_Designator
                    (Instantiation_Node (BE_Node (Type_Spec_Node)));

                  --  Getting the sequence length

                  N := Make_Object_Declaration
                    (Defining_Identifier => Make_Defining_Identifier
                     (Seq_Length),
                     Object_Definition   => RE (RE_Unsigned_Long_1));
                  Append_To (Block_Dcl, N);

                  --  Unmarshalling the sequence length

                  N := Make_Subprogram_Call
                    (RE (RE_Unmarshall_2),
                     New_List
                     (Make_Identifier (Buff)));
                  N := Make_Assignment_Statement
                    (Make_Identifier (Seq_Length), N);
                  Append_To (Block_St, N);

                  --  Unmarshalling the sequence elements

                  Index_Node := Make_Defining_Identifier (Index_Name);

                  --    Creating the range constraint

                  Range_Constraint := Make_Range_Constraint
                    (Make_Literal (Int1_Val),
                     Make_Defining_Identifier (Seq_Length));

                  --    Declaring the element variable

                  Element_Dcl := Storage_Variable_Declaration
                    (Seq_Element_Name, Type_Spec (Type_Spec_Node));
                  Append_To (Block_Dcl, Element_Dcl);

                  --    Unmarshalling the sequence element

                  N := Do_Unmarshall
                    (Var_Node => Make_Identifier (Seq_Element_Name),
                     Var_Type => Type_Spec (Type_Spec_Node),
                     Buff     => Buff);
                  Append_To (For_Statements, N);

                  --    Appending the sequence element

                  N := Make_Selected_Component
                    (Seq_Package_Node,
                     Make_Identifier (SN (S_Append)));

                  N := Make_Subprogram_Call
                    (N,
                     New_List
                     (Var_Node,
                      Cast_Variable_From_PolyORB_Type
                      (Seq_Element_Name, Type_Spec (Type_Spec_Node))));
                  Append_To (For_Statements, N);

                  --  If we deal with nested sequences, we must
                  --  purge the sequence element for the next
                  --  unmarshalling iteration.

                  if FEN.Kind
                    (FEU.Get_Original_Type_Specifier
                     (Type_Spec (Type_Spec_Node))) = K_Sequence_Type
                  then
                     N := Make_Assignment_Statement
                       (Make_Identifier (Seq_Element_Name),
                        Copy_Expanded_Name (BEN.Expression (Element_Dcl)));
                     Append_To (For_Statements, N);
                  end if;

                  --  Building the loop

                  N := Make_For_Statement
                    (Index_Node,
                     Range_Constraint,
                     For_Statements);
                  Append_To (Block_St, N);
               end;

            when K_Complex_Declarator =>
               declare
                  I                    : Nat := 0;
                  Sizes                : constant List_Id :=
                    Range_Constraints
                    (Type_Definition
                     (Type_Def_Node
                      (BE_Node
                       (Identifier
                        (Type_Spec_Node)))));
                  Dim                  : Node_Id;
                  Loop_Statements      : List_Id := No_List;
                  Enclosing_Statements : List_Id;
                  Index_List           : constant List_Id :=
                    New_List;
                  Index_Node           : Node_Id := No_Node;
                  Array_Element        : constant Name_Id :=
                    Get_Element_Name;
                  Index_Name           : constant Name_Id :=
                    Get_Index_Name;

               begin
                  --  Building the nested loops

                  Dim := First_Node (Sizes);
                  loop
                     Get_Name_String (Index_Name);
                     Add_Char_To_Name_Buffer ('_');
                     Add_Nat_To_Name_Buffer (I);
                     Index_Node := Make_Defining_Identifier
                       (Add_Suffix_To_Name (Unique_Suffix, Name_Find));
                     Append_To (Index_List, Index_Node);
                     Enclosing_Statements := Loop_Statements;
                     Loop_Statements := New_List;
                     N := Make_For_Statement
                       (Index_Node, Dim, Loop_Statements);

                     if I > 0 then
                        Append_To (Enclosing_Statements, N);
                     else
                        Append_To (Block_St, N);
                     end if;

                     I := I + 1;
                     Dim := Next_Node (Dim);
                     exit when No (Dim);
                  end loop;

                  --  Filling the statements of the deepest loop by
                  --  the marshalling of the corresponding array
                  --  element.

                  --  Declaring the element variable

                  N := Storage_Variable_Declaration
                    (Array_Element, Type_Spec (Declaration (Type_Spec_Node)));
                  Append_To (Block_Dcl, N);

                  --  Unmarshalling the element and handling the error

                  N := Do_Unmarshall
                    (Var_Node => Make_Identifier (Array_Element),
                     Var_Type => Type_Spec (Declaration (Type_Spec_Node)),
                     Buff     => Buff);
                  Append_To (Loop_Statements, N);

                  --  Updating the array element

                  N := Make_Subprogram_Call (Var_Node, Index_List);
                  N := Make_Assignment_Statement
                    (N,
                     Cast_Variable_From_PolyORB_Type
                     (Array_Element,
                      Type_Spec (Declaration (Type_Spec_Node))));
                  Append_To (Loop_Statements, N);

               end;

            when K_Structure_Type =>
               declare
                  Member         : Node_Id;
                  Declarator     : Node_Id;
                  Dcl_Ada_Name   : Name_Id;
                  Dcl_Ada_Node   : Node_Id;
                  Struct_Element : Name_Id;
               begin

                  Member := First_Entity (Members (Type_Spec_Node));
                  while Present (Member) loop
                     Declarator := First_Entity (FEN.Declarators (Member));
                     while Present (Declarator) loop

                        --  Getting an element name

                        Struct_Element := Get_Element_Name;

                        --  Declaring the element variable

                        N := Storage_Variable_Declaration
                          (Struct_Element, Declarator);
                        Append_To (Block_Dcl, N);

                        --  Unmarshalling the element

                        N := Do_Unmarshall
                          (Var_Node => Make_Identifier (Struct_Element),
                           Var_Type => Declarator,
                           Buff     => Buff);
                        Append_To (Block_St, N);

                        --  Getting the record field name

                        Dcl_Ada_Name := To_Ada_Name
                          (IDL_Name
                           (Identifier
                            (Declarator)));

                        Dcl_Ada_Node := Make_Selected_Component
                          (Var_Node,
                           Make_Identifier (Dcl_Ada_Name));

                        --  Updating the struct field

                        N := Make_Assignment_Statement
                          (Dcl_Ada_Node,
                           Cast_Variable_From_PolyORB_Type
                           (Struct_Element,
                            Declarator));
                        Append_To (Block_St, N);

                        Declarator := Next_Entity (Declarator);
                     end loop;
                     Member := Next_Entity (Member);
                  end loop;
               end;

            when K_Union_Type =>
               declare
                  Switch_Alternatives : List_Id;
                  Switch_Case         : Node_Id;
                  Has_Default         : Boolean := False;
                  Choices             : List_Id;
                  Literal_Parent      : Node_Id := No_Node;
                  Switch_Statements   : List_Id;
                  Switch_Type         : Node_Id;
                  Dcl_Ada_Name        : Name_Id;
                  Dcl_Ada_Node        : Node_Id;
                  Declarator          : Node_Id;
                  Switch_Element      : Name_Id;
                  Union_Element       : Name_Id;
               begin

                  --  1/ Unmarshall the union switch

                  --    Getting an element name

                  Switch_Element := Get_Element_Name;

                  --    Declaring the switch variable

                  N := Storage_Variable_Declaration
                    (Switch_Element, Switch_Type_Spec (Type_Spec_Node));
                  Append_To (Block_Dcl, N);

                  --    Unmarshall the switch

                  N := Do_Unmarshall
                    (Var_Node => Make_Identifier (Switch_Element),
                     Var_Type => Switch_Type_Spec (Type_Spec_Node),
                     Buff     => Buff);
                  Append_To (Block_St, N);

                  --  2/ Build the union: we cannot build the union by
                  --  the means of a record aggregate. The solution is
                  --  to declare an intermediary variable with the
                  --  correct union type and then to assign the union
                  --  this variable by means of a qualified
                  --  expression.

                  declare
                     Inner_Dcl : constant List_Id := New_List;
                     Inner_St  : constant List_Id := New_List;
                     Intermed_Name : constant Name_Id := Get_Union_Name;
                  begin
                     --  Intermediary variable with the correct type

                     N := Make_Subprogram_Call
                       (Map_Expanded_Name (Direct_Type_Node),
                        New_List
                        (Cast_Variable_From_PolyORB_Type
                         (Switch_Element,
                          Switch_Type_Spec (Type_Spec_Node))));
                     N := Make_Object_Declaration
                       (Defining_Identifier => Make_Defining_Identifier
                          (Intermed_Name),
                        Object_Definition => N);
                     Append_To (Inner_Dcl, N);

                     --  Disable warning because the variable is not
                     --  assigned.

                     N := Make_Pragma
                       (Pragma_Warnings,
                        New_List (RE (RE_Off),
                                      Make_Defining_Identifier
                                      (Intermed_Name)));
                     Append_To (Inner_Dcl, N);

                     --  Qualified expression

                     N := Make_Qualified_Expression
                       (Subtype_Mark => Map_Expanded_Name
                          (Direct_Type_Node),
                        Operand      => Make_Identifier (Intermed_Name));

                     N := Make_Assignment_Statement (Var_Node, N);
                     Append_To (Inner_St, N);

                     --  Add the new block statements

                     N := Make_Block_Statement
                       (Declarative_Part => Inner_Dcl,
                        Statements       => Inner_St);
                     Append_To (Block_St, N);
                  end;

                  --  3/ Depending on the switch value, unmarshall the
                  --  corresponding flag.

                  Switch_Type := FEU.Get_Original_Type_Specifier
                    (Switch_Type_Spec
                     (Type_Spec_Node));
                  if FEN.Kind (Switch_Type) = K_Enumeration_Type then
                     Literal_Parent := Map_Expanded_Name
                       (Scope_Entity
                        (Identifier
                         (Switch_Type)));
                  end if;

                  Switch_Alternatives := New_List;
                  Switch_Case := First_Entity
                    (Switch_Type_Body
                     (Type_Spec_Node));
                  while Present (Switch_Case) loop
                     Map_Choice_List
                       (Labels (Switch_Case),
                        Literal_Parent,
                        Choices,
                        Has_Default);

                     Switch_Statements := New_List;

                     Declarator := FEN.Declarator
                       (Element
                        (Switch_Case));

                     --    Getting an element name

                     Union_Element := Get_Element_Name;

                     --    Declaring the element variable

                     N := Storage_Variable_Declaration
                       (Union_Element, Declarator);
                     Append_To (Block_Dcl, N);

                     --    Unmarshalling the element

                     N := Do_Unmarshall
                       (Var_Node => Make_Identifier (Union_Element),
                        Var_Type => Declarator,
                        Buff     => Buff);
                     Append_To (Switch_Statements, N);

                     --  Getting the field name

                     Dcl_Ada_Name := To_Ada_Name
                       (IDL_Name
                        (Identifier
                         (Declarator)));

                     Dcl_Ada_Node := Make_Selected_Component
                       (Var_Node,
                        Make_Identifier (Dcl_Ada_Name));

                     N := Make_Assignment_Statement
                       (Dcl_Ada_Node,
                        Cast_Variable_From_PolyORB_Type
                        (Union_Element,
                         Declarator));
                     Append_To (Switch_Statements, N);

                     --  Building the switch alternative

                     Append_To (Switch_Alternatives,
                       Make_Case_Statement_Alternative
                         (Choices, Switch_Statements));

                     Switch_Case := Next_Entity (Switch_Case);
                  end loop;

                  --  Add an empty when others clause to keep the compiler
                  --  happy.

                  if not Has_Default then
                     Append_To (Switch_Alternatives,
                       Make_Case_Statement_Alternative (No_List, No_List));
                  end if;

                  N := Make_Case_Statement
                    (Cast_Variable_From_PolyORB_Type
                     (Switch_Element,
                      Switch_Type_Spec (Type_Spec_Node)),
                     Switch_Alternatives);
                  Append_To (Block_St, N);

               end;

            when K_Any =>
               declare
                  Profile : constant List_Id := New_List;
               begin
                  N := Make_Identifier (Buff);
                  Append_To (Profile, N);

                  N := Make_Attribute_Reference
                    (Make_Type_Conversion
                     (Make_Attribute_Reference
                      (RE (RE_CDR_Representation), A_Class),
                      Make_Identifier (PN (P_Representation))),
                     A_Unrestricted_Access);
                  Append_To (Profile, N);

                  N := Make_Subprogram_Call (RE (RE_Unmarshall_1), Profile);
                  N := Make_Assignment_Statement (Var_Node, N);
                  Append_To (Block_St, N);
               end;

            when others =>
               raise Program_Error with
                 "Do_Unmarshall: "
                 & FEN.Node_Kind'Image (FEN.Kind (Type_Spec_Node))
                 & ": Not Yet Implemented!";
         end case;

         N := Make_Block_Statement
           (Declarative_Part => Block_Dcl,
            Statements       => Block_St);

         return N;
      end Do_Unmarshall;

      ----------------------
      -- Get_Element_Name --
      ----------------------

      function Get_Element_Name return Name_Id is
         Element : Name_Id;
      begin
         Set_Str_To_Name_Buffer ("Element_");
         Element_Number := Element_Number + 1;
         Add_Nat_To_Name_Buffer (Element_Number);
         Element := Add_Suffix_To_Name (Unique_Suffix, Name_Find);
         return Element;
      end Get_Element_Name;

      --------------------
      -- Get_Index_Name --
      --------------------

      function Get_Index_Name return Name_Id is
         Index : Name_Id;
      begin
         Set_Str_To_Name_Buffer ("Index_");
         Index_Number := Index_Number + 1;
         Add_Nat_To_Name_Buffer (Index_Number);
         Index := Name_Find;
         return Index;
      end Get_Index_Name;

      ---------------------
      -- Get_Length_Name --
      ---------------------

      function Get_Length_Name return Name_Id is
         Length : Name_Id;
      begin
         Set_Str_To_Name_Buffer ("Length_");
         Length_Number := Length_Number + 1;
         Add_Nat_To_Name_Buffer (Length_Number);
         Length := Name_Find;
         return Length;
      end Get_Length_Name;

      --------------------
      -- Get_Union_Name --
      --------------------

      function Get_Union_Name return Name_Id is
         U : Name_Id;
      begin
         Set_Str_To_Name_Buffer ("Union_");
         Union_Number := Union_Number + 1;
         Add_Nat_To_Name_Buffer (Union_Number);
         U := Name_Find;
         return U;
      end Get_Union_Name;

      -----------
      -- Visit --
      -----------

      procedure Visit (E : Node_Id) is
      begin
         case FEN.Kind (E) is

            when K_Attribute_Declaration =>
               Visit_Attribute_Declaration (E);

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
      -- Visit_Attribute_Declaration --
      ---------------------------------

      procedure Visit_Attribute_Declaration (E : Node_Id) is
         N    : Node_Id;
         D    : Node_Id;
      begin
         Set_CDR_Body;

         D := First_Entity (Declarators (E));
         while Present (D) loop
            Set_Str_To_Name_Buffer ("Attribute : ");
            Get_Name_String_And_Append (IDL_Name (Identifier (D)));
            N := Make_Ada_Comment (Name_Find);
            Append_To (Statements (Current_Package), N);

            D := Next_Entity (D);
         end loop;
      end Visit_Attribute_Declaration;

      ---------------------------------
      -- Visit_Interface_Declaration --
      ---------------------------------

      procedure Visit_Interface_Declaration (E : Node_Id) is
         N : Node_Id;
      begin
         --  No CDR package is generated for a local interface

         if FEN.Is_Local_Interface (E) then
            return;
         end if;

         N := BEN.Parent (Type_Def_Node (BE_Node (Identifier (E))));
         Push_Entity (BEN.IDL_Unit (Package_Declaration (N)));
         Set_CDR_Body;

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
         if not Map_Particular_CORBA_Parts (E, PK_CDR_Body) then
            Push_Entity (Stub_Node (BE_Node (Identifier (E))));
            D := First_Entity (Definitions (E));

            while Present (D) loop
               Visit (D);
               D := Next_Entity (D);
            end loop;

            Pop_Entity;
         end if;
      end  Visit_Module;

      ---------------------------------
      -- Visit_Operation_Declaration --
      ---------------------------------

      procedure Visit_Operation_Declaration (E : Node_Id) is
         N     : Node_Id;
      begin
         Set_CDR_Body;

         --  Explaining comment

         Set_Str_To_Name_Buffer ("Operation : ");
         Get_Name_String_And_Append (IDL_Name (Identifier (E)));
         N := Make_Ada_Comment (Name_Find);
         Append_To (Statements (Current_Package), N);

         --  Generating the 'Operation_Name'_Marshaller Body

         N := Marshaller_Body (E);
         Append_To (Statements (Current_Package), N);

         --  Generating the 'Operation_Name'_Unmarshaller Body

         N := Unmarshaller_Body (E);
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
   end Package_Body;
end Backend.BE_CORBA_Ada.CDRs;

------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--         B A C K E N D . B E _ C O R B A _ A D A . B U F F E R S          --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2006-2009, Free Software Foundation, Inc.          --
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

with Namet;   use Namet;
with Values;

with Frontend.Nodes;  use Frontend.Nodes;
with Frontend.Nutils;

with Backend.BE_CORBA_Ada.Nodes;       use Backend.BE_CORBA_Ada.Nodes;
with Backend.BE_CORBA_Ada.Nutils;      use Backend.BE_CORBA_Ada.Nutils;
with Backend.BE_CORBA_Ada.IDL_To_Ada;  use Backend.BE_CORBA_Ada.IDL_To_Ada;
with Backend.BE_CORBA_Ada.Runtime;     use Backend.BE_CORBA_Ada.Runtime;

with Backend.BE_CORBA_Ada.Common;      use Backend.BE_CORBA_Ada.Common;
package body Backend.BE_CORBA_Ada.Buffers is

   package FEN renames Frontend.Nodes;
   package FEU renames Frontend.Nutils;
   package BEN renames Backend.BE_CORBA_Ada.Nodes;
   package BEU renames Backend.BE_CORBA_Ada.Nutils;

   package body Package_Spec is
      function Buffer_Size_Spec (E : Node_Id) return Node_Id;
      --  Builds the spec of the static buffer size subprogram

      procedure Visit_Attribute_Declaration (E : Node_Id);
      procedure Visit_Interface_Declaration (E : Node_Id);
      procedure Visit_Module (E : Node_Id);
      procedure Visit_Operation_Declaration (E : Node_Id);
      procedure Visit_Specification (E : Node_Id);

      ----------------------
      -- Buffer_Size_Spec --
      ----------------------

      function Buffer_Size_Spec (E : Node_Id) return Node_Id is
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

         --  'First_Arg_Alignment' parameter

         Parameter := Make_Parameter_Specification
           (Defining_Identifier => Make_Defining_Identifier
            (PN (P_First_Arg_Alignment)),
            Subtype_Mark        => RE (RE_Alignment_Type),
            Parameter_Mode      => Mode_In);
         Append_To (Profile, Parameter);

         --  Subprogram Specification

         S := Make_Subprogram_Specification
           (Make_Selected_Component
            (Defining_Identifier (Buffers_Package (Current_Entity)),
             Map_Buffer_Size_Identifier (Defining_Identifier (Spec))),
            Profile,
            No_Node);

         return S;
      end Buffer_Size_Spec;

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
         Set_Buffers_Spec;

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
         --  No buffers package is generated for a local interface

         if FEN.Is_Local_Interface (E) then
            return;
         end if;

         N := BEN.Parent (Type_Def_Node (BE_Node (Identifier (E))));
         Push_Entity (BEN.IDL_Unit (Package_Declaration (N)));
         Set_Buffers_Spec;

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
         if not Map_Particular_CORBA_Parts (E, PK_Buffers_Spec) then
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
         N    : Node_Id;
         Attribute_Name : Name_Id;
      begin
         Set_Buffers_Spec;

         --  Explaining comment

         Set_Str_To_Name_Buffer ("Operation : ");
         Get_Name_String_And_Append (IDL_Name (Identifier (E)));
         N := Make_Ada_Comment (Name_Find);
         Append_To (Visible_Part (Current_Package), N);

         --  Generating the 'Operation_Name'_Buffer_Size spec

         N := Buffer_Size_Spec (E);
         Append_To (Visible_Part (Current_Package), N);

         Bind_FE_To_BE (Identifier (E), N, B_Buffer_Size);

         --  Variables to store buffers size

         Attribute_Name := Add_Suffix_To_Name
           ("_Client_Size", IDL_Name (Identifier (E)));

         N := Make_Object_Declaration
           (Defining_Identifier => Make_Defining_Identifier
            (Attribute_Name),

            Object_Definition   => RE (RE_Stream_Element_Count),
            Constant_Present    => False,
            Expression          => Make_Literal (Int0_Val));

         Append_To (Visible_Part (Current_Package), N);

         Attribute_Name := Add_Suffix_To_Name
           ("_Server_Size", IDL_Name (Identifier (E)));

         N := Make_Object_Declaration
           (Defining_Identifier => Make_Defining_Identifier
            (Attribute_Name),

            Object_Definition   => RE (RE_Stream_Element_Count),
            Constant_Present    => False,
            Expression          => Make_Literal (Int0_Val));

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
   end Package_Spec;

   package body Package_Body is
      use Values;

      Args_Declared       : Boolean := False;
      Fixed_Client_Buffer : Boolean := True;
      Fixed_Server_Buffer : Boolean := True;
      Variable_Buffer     : Boolean := False;

      function Buffer_Size_Body (E : Node_Id) return Node_Id;

      function Get_Index_Name return Name_Id;
      Index_Number : Nat := 0;

      procedure Visit_Attribute_Declaration (E : Node_Id);
      procedure Visit_Interface_Declaration (E : Node_Id);
      procedure Visit_Module (E : Node_Id);
      procedure Visit_Operation_Declaration (E : Node_Id);
      procedure Visit_Specification (E : Node_Id);

      function Compute_Size
        (Var_Node : Node_Id;
         Var_Type : Node_Id;
         Subp_Dec : List_Id;
         Subp_Nod : Node_Id)
        return Node_Id;

      function Parameter_Size (N : Node_Id) return Value_Id;

      procedure Declare_Args (Subp_Dec : List_Id; Subp_Nod : Node_Id);

      ----------------------
      -- Buffer_Size_Body --
      ----------------------

      function Buffer_Size_Body (E : Node_Id) return Node_Id is
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
         L                 : Node_Id;
         Cl_Buffer_Size    : Node_Id;
         Sr_Buffer_Size    : Node_Id;
         Bool_Exp1         : Node_Id;
         Bool_Exp2         : Node_Id;
      begin
         Args_Declared       := False;
         Fixed_Client_Buffer := True;
         Fixed_Server_Buffer := True;
         Variable_Buffer     := False;

         --  generate instructions to allocate the buffer needed to
         --  marshall the body message.

         --  The declarative part generation of the subprogram is
         --  postponed after the handling of the arguments and the
         --  result because it depends on the result of this handling.

         --  Subprogram specification

         Subp_Spec := Buffer_Size_Node (BE_Node (Identifier (E)));
         Args_Id   := Map_Args_Identifier
           (Defining_Identifier
            (Stub_Node
             (BE_Node
              (Identifier
               (E)))));

         --  We do not recompute buffer size if there is no need
         --  bounded type (client side).

         if Contains_In_Parameters (E) then
            Cl_Buffer_Size := Make_Identifier
              (Add_Suffix_To_Name
               ("_Client_Size",
                IDL_Name
                (Identifier
                 (E))));

            Bool_Exp1 := Make_Expression
              (Cl_Buffer_Size,
               Op_Greater,
               Make_Literal (New_Integer_Value (512, 1, 10)));

            Bool_Exp2 := Make_Expression
              (Cl_Buffer_Size,
               Op_Not_Equal,
               Make_Literal (New_Integer_Value (0, 1, 10)));

            N := Make_Subprogram_Call
              (RE (RE_Preallocate_Buffer),
               New_List
               (Make_Identifier (PN (P_Buffer)),
                Cl_Buffer_Size));

            M := Make_Return_Statement (No_Node);

            L := Make_Elsif_Statement
              (Condition       => Bool_Exp2,
               Then_Statements => New_List (M));

            L := Make_If_Statement
              (Condition        => Bool_Exp1,
               Then_Statements  => New_List (N, M),
               Elsif_Statements => New_List (L));

            Append_To (Client_Statements, L);
         end if;

         --  We do not recompute buffer size if there is no need
         --  bounded type (server side).

         if Contains_Out_Parameters (E)
           or else
           (Present (T) and then
            FEN.Kind (T) /= K_Void)
         then
            Sr_Buffer_Size := Make_Identifier
              (Add_Suffix_To_Name
               ("_Server_Size",
                IDL_Name
                (Identifier
                 (E))));

            Bool_Exp1 := Make_Expression
              (Sr_Buffer_Size,
               Op_Greater,
               Make_Literal (New_Integer_Value (512, 1, 10)));

            Bool_Exp2 := Make_Expression
              (Sr_Buffer_Size,
               Op_Not_Equal,
               Make_Literal (New_Integer_Value (0, 1, 10)));

            N := Make_Subprogram_Call
              (RE (RE_Preallocate_Buffer),
               New_List
               (Make_Identifier (PN (P_Buffer)),
                Sr_Buffer_Size));

            M := Make_Return_Statement (No_Node);

            L := Make_Elsif_Statement
              (Condition       => Bool_Exp2,
               Then_Statements => New_List (M));

            L := Make_If_Statement
              (Condition        => Bool_Exp1,
               Then_Statements  => New_List (N, M),
               Elsif_Statements  => New_List (L));

            Append_To (Server_Statements, L);
         end if;

         --  If the subprogram is a function, we handle the result

         if Present (T) and then FEN.Kind (T) /= K_Void then

            Rewinded_Type := FEU.Get_Original_Type_Specifier (T);

            --  Explaining comment

            Set_Str_To_Name_Buffer ("padding for Result : ");
            Get_Name_String_And_Append  (PN (P_Returns));
            Add_Str_To_Name_Buffer (" => ");
            Add_Str_To_Name_Buffer
              (FEN.Node_Kind'Image
               (FEN.Kind
                (Rewinded_Type)));
            N := Make_Ada_Comment (Name_Find);
            Append_To (Server_Statements, N);

            --  Body alignment

            N := Make_Subprogram_Call
              (RE (RE_Pad_Compute),
               New_List
               (Make_Identifier (VN (V_CDR_Position)),
                Make_Identifier (VN (V_Buffer_Size)),
                Make_Identifier (PN (P_Data_Alignment))));
            Append_To (Server_Statements, N);

            --  Initialize body alignment to "1"

            if  Contains_Out_Parameters (E) then
               N := Make_Assignment_Statement
                 (Make_Defining_Identifier (PN (P_Data_Alignment)),
                  Make_Literal (Int1_Val));
               Append_To (Server_Statements, N);
               Alignment_Const := False;
            end if;

            --  Compute memory needed for result marshalling

            N := Make_Selected_Component
              (Copy_Node (Args_Id),
               Make_Defining_Identifier (PN (P_Returns)));
            N := Compute_Size (N, T, Subp_Declarations, E);
            Append_To (Server_Statements, N);

            --  If return type is unbounded we must recompute the
            --  server buffer size each time.

            if Variable_Buffer then
               Fixed_Server_Buffer := False;
               Variable_Buffer := False;
            end if;
         end if;

         --  Handling parameters

         if not FEU.Is_Empty (P) then
            --  Body alignment

            if  Contains_Out_Parameters (E) then
               N := Make_Subprogram_Call
                 (RE (RE_Pad_Compute),
                  New_List
                  (Make_Identifier (VN (V_CDR_Position)),
                   Make_Identifier (VN (V_Buffer_Size)),
                   Make_Identifier (PN (P_Data_Alignment))));

               Append_To (Server_Statements, N);
            end if;

            if  Contains_In_Parameters (E) then
               N := Make_Subprogram_Call
                 (RE (RE_Pad_Compute),
                  New_List
                  (Make_Identifier (VN (V_CDR_Position)),
                   Make_Identifier (VN (V_Buffer_Size)),
                   Make_Identifier (PN (P_Data_Alignment))));

               Append_To (Client_Statements, N);
            end if;

            --  Parameters

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

               Set_Str_To_Name_Buffer ("padding for Parameter : ");
               Get_Name_String_And_Append (Parameter_Name);
               Add_Str_To_Name_Buffer (" => ");
               Add_Str_To_Name_Buffer
                 (FEN.Node_Kind'Image
                  (FEN.Kind
                   (FEU.Get_Original_Type_Specifier
                    (Type_Spec
                     (Parameter)))));

               if  Is_In (Parameter_Mode) then
                  N := Make_Ada_Comment (Name_Find);
                  Append_To (Client_Statements, N);
               end if;

               if  Is_Out (Parameter_Mode) then
                  N := Make_Ada_Comment (Name_Find);
                  Append_To (Server_Statements, N);
               end if;

               --  Compute parameter size

               if  Is_In (Parameter_Mode) then
                  N := Make_Selected_Component
                    (Copy_Node (Args_Id),
                     Make_Defining_Identifier (Parameter_Name));

                  N := Compute_Size
                    (N,
                     Type_Spec (Parameter),
                     Subp_Declarations,
                     E);
                  Append_To (Client_Statements, N);

                  --  If parameter type is unbounded we must recompute
                  --  the client buffer size each time.

                  if Variable_Buffer then
                     Fixed_Client_Buffer := False;
                     Variable_Buffer := False;
                  end if;
               end if;

               if  Is_Out (Parameter_Mode) then
                  N := Make_Selected_Component
                    (Copy_Node (Args_Id),
                     Make_Defining_Identifier (Parameter_Name));

                  N := Compute_Size
                    (N,
                     Type_Spec (Parameter),
                     Subp_Declarations,
                     E);
                  Append_To (Server_Statements, N);

                  --  If parameter type is unbounded we must recompute
                  --  the server buffer size each time.

                  if Variable_Buffer then
                     Fixed_Server_Buffer := False;
                     Variable_Buffer := False;
                  end if;
               end if;
               Parameter := Next_Entity (Parameter);
            end loop;

         end if;

         --  Allocate Buffer_Size octets

         if not BEU.Is_Empty (Client_Statements) then
            if Fixed_Client_Buffer then
               N := Make_Assignment_Statement
                 (Cl_Buffer_Size,
                  Make_Identifier (VN (V_Buffer_Size)));
               Append_To (Client_Statements, N);
            else
               N := Make_Assignment_Statement
                 (Cl_Buffer_Size,
                  Make_Literal (Int0_Val));
               Append_To (Client_Statements, N);
            end if;

            N := Make_Subprogram_Call
              (RE (RE_Preallocate_Buffer),
               New_List
               (Make_Identifier (PN (P_Buffer)),
                Make_Identifier (VN (V_Buffer_Size))));

            Append_To (Client_Statements, N);
         end if;

         if not BEU.Is_Empty (Server_Statements) then
            if Fixed_Server_Buffer then
               N := Make_Assignment_Statement
                 (Sr_Buffer_Size,
                  Make_Identifier (VN (V_Buffer_Size)));
               Append_To (Server_Statements, N);
            else
               N := Make_Assignment_Statement
                 (Sr_Buffer_Size,
                  Make_Literal (Int0_Val));
               Append_To (Server_Statements, N);
            end if;

            N := Make_Subprogram_Call
              (RE (RE_Preallocate_Buffer),
               New_List
               (Make_Identifier (PN (P_Buffer)),
                Make_Identifier (VN (V_Buffer_Size))));

            Append_To (Server_Statements, N);
         end if;

         --  The declarative part of the subprogram:

         if BEU.Is_Empty (Client_Statements)
           and then BEU.Is_Empty (Server_Statements)
         then
            declare
               Unref_Entities : constant array (Positive range <>) of Name_Id
                 := (PN (P_Role),
                     PN (P_Args),
                     PN (P_Buffer),
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
            --  It's complicated to determine if the parameters 'Args'
            --  is or isn't referenced (depending) on the types
            --  handled. So we ignore warnings raised about these
            --  parameter.

            N := Make_Pragma
              (Pragma_Warnings,
               New_List (RE (RE_Off), Make_Identifier (PN (P_Args))));
            Append_To (Subp_Declarations, N);

            --  Common declarations

            --  1/ Data_Alignment : This variable modified when there
            --  are OUT or INOUT parameters in order to avoid the
            --  alignment of buffer more than one time.

            N := Make_Object_Declaration
              (Defining_Identifier => Make_Defining_Identifier
               (PN (P_Data_Alignment)),
               Object_Definition   => RE (RE_Alignment_Type),
               Constant_Present    => Alignment_Const,
               Expression          => Make_Identifier
               (PN (P_First_Arg_Alignment)));
            Append_To (Subp_Declarations, N);

            --  Use type instruction for arithmetic operation on
            --  Buffer_Size and CDR_Position.

            N := Make_Used_Type
              (Make_Identifier
               (Fully_Qualified_Name
                (RE (RE_Unsigned_Long_1))));

            Append_To (Subp_Declarations, N);

            N := Make_Used_Type
              (Make_Identifier
               (Fully_Qualified_Name
                (RE (RE_Stream_Element_Count))));

            Append_To (Subp_Declarations, N);

            --  Buffer_Size declaration and initialization

            N := Make_Object_Declaration
              (Defining_Identifier => Make_Defining_Identifier
               (VN (V_Buffer_Size)),
               Object_Definition   => RE (RE_Stream_Element_Count),
               Constant_Present    => False,
               Expression          =>

                 Make_Expression
               (Make_Subprogram_Call
                (RE (RE_CDR_Position),
                 New_List (Make_Identifier (PN (P_Buffer)))),
                Op_Minus,
                Make_Subprogram_Call
                (RE (RE_Length),
                 New_List (Make_Identifier (PN (P_Buffer))))));

            Append_To (Subp_Declarations, N);

            --  CDR_Position declaration and initialization

            N := Make_Object_Declaration
              (Defining_Identifier => Make_Defining_Identifier
               (VN (V_CDR_Position)),
               Object_Definition   => RE (RE_Stream_Element_Count),
               Constant_Present    => False,
               Expression          =>
                 Make_Subprogram_Call
               (RE (RE_CDR_Position),
                New_List (Make_Identifier (PN (P_Buffer)))));

            Append_To (Subp_Declarations, N);
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
      end Buffer_Size_Body;

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
         Set_Buffers_Body;

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
         --  No buffers package is generated for a local interface

         if FEN.Is_Local_Interface (E) then
            return;
         end if;

         N := BEN.Parent (Type_Def_Node (BE_Node (Identifier (E))));
         Push_Entity (BEN.IDL_Unit (Package_Declaration (N)));
         Set_Buffers_Body;

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
         if not Map_Particular_CORBA_Parts (E, PK_Buffers_Body) then
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
         Set_Buffers_Body;

         Set_Str_To_Name_Buffer ("Operation : ");
         Get_Name_String_And_Append (IDL_Name (Identifier (E)));
         N := Make_Ada_Comment (Name_Find);
         Append_To (Statements (Current_Package), N);

         --  Generating the 'Operation_Name'_Buffer_Size Body

         N := Buffer_Size_Body (E);
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

      -----------------------
      --  Compute_Size  --
      -----------------------

      function Compute_Size
        (Var_Node : Node_Id;
         Var_Type : Node_Id;
         Subp_Dec : List_Id;
         Subp_Nod : Node_Id)
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
            when K_Object
              | K_Interface_Declaration =>
               declare
                  Padding_Value : Value_Id;
               begin
                  Padding_Value := Parameter_Size (Type_Spec_Node);

                  --  We send an IOR so we make a padding on 4 octets

                  N := Make_Subprogram_Call
                    (RE (RE_Pad_Compute),
                     New_List
                     (Make_Identifier (VN (V_CDR_Position)),
                      Make_Identifier (VN (V_Buffer_Size)),
                      Make_Literal (New_Integer_Value (4, 1, 10))));

                  Append_To (Block_St, N);

                  N := Make_Assignment_Statement
                    (Make_Defining_Identifier (VN (V_Buffer_Size)),
                     Make_Expression
                     (Make_Literal (Padding_Value),
                      Op_Plus,
                      Make_Defining_Identifier (VN (V_Buffer_Size))));

                  Append_To (Block_St, N);

                  N := Make_Assignment_Statement
                    (Make_Defining_Identifier (VN (V_CDR_Position)),
                     Make_Expression
                     (Make_Literal (Padding_Value),
                      Op_Plus,
                      Make_Defining_Identifier (VN (V_CDR_Position))));

                  Append_To (Block_St, N);
               end;
            when K_Wide_Char =>
               declare
                  M : Node_Id;
               begin
                  --  The padding of Wchar depend on the GIOP version
                  --  and the Code Set so we make a padding for the
                  --  worst case (ex :GIOP 1.1 with ISO 10646
                  --  UCS-4CS).

                  N := Make_Assignment_Statement
                    (Make_Defining_Identifier (VN (V_Buffer_Size)),
                     Make_Expression
                     (Make_Identifier (VN (V_Buffer_Size)),
                      Op_Plus,
                      Make_Literal (New_Integer_Value (4, 1, 10))));

                  Append_To (Block_St, N);

                  N := Make_Assignment_Statement
                    (Make_Defining_Identifier (VN (V_CDR_Position)),
                     Make_Expression
                     (Make_Identifier (VN (V_CDR_Position)),
                      Op_Plus,
                      Make_Literal (New_Integer_Value (4, 1, 10))));

                  Append_To (Block_St, N);

                  --  Wchar size

                  M := Make_Subprogram_Call
                    (RE (RE_Type_Size),
                     New_List
                     (Cast_Variable_To_PolyORB_Type
                       (Var_Node, Direct_Type_Node)));

                  N := Make_Assignment_Statement
                    (Make_Defining_Identifier (VN (V_Buffer_Size)),
                     Make_Expression
                     (Make_Subprogram_Call
                      (RE (RE_Stream_Element_Count),
                       New_List (M)),
                      Op_Plus,
                      Make_Defining_Identifier (VN (V_Buffer_Size))));

                  Append_To (Block_St, N);

                  N := Make_Assignment_Statement
                    (Make_Defining_Identifier (VN (V_CDR_Position)),
                     Make_Expression
                     (Make_Subprogram_Call
                      (RE (RE_Stream_Element_Count),
                       New_List (M)),
                      Op_Plus,
                      Make_Defining_Identifier (VN (V_CDR_Position))));

                  Append_To (Block_St, N);

                  if not Args_Declared then
                     Declare_Args (Subp_Dec, Subp_Nod);
                  end if;
                  Variable_Buffer := True;
               end;
            when K_Boolean
              | K_Double
              | K_Float
              | K_Long
              | K_Long_Long
              | K_Octet
              | K_Short
              | K_Unsigned_Long
              | K_Unsigned_Long_Long
              | K_Unsigned_Short
              | K_Enumeration_Type
              | K_Char =>

               declare
                  Padding_Value  : Value_Id;
               begin
                  --  Getting the parameter size

                  Padding_Value := Parameter_Size (Type_Spec_Node);

                  --  Padding

                  N := Make_Subprogram_Call
                    (RE (RE_Pad_Compute),
                     New_List
                     (Make_Identifier (VN (V_CDR_Position)),
                      Make_Identifier (VN (V_Buffer_Size)),
                      Make_Literal (Padding_Value)));

                  Append_To (Block_St, N);

                  --  Update Buffer_Size and CDR_Position

                  N := Make_Assignment_Statement
                    (Make_Defining_Identifier (VN (V_Buffer_Size)),
                     Make_Expression
                     (Make_Literal (Padding_Value),
                      Op_Plus,
                      Make_Defining_Identifier (VN (V_Buffer_Size))));

                  Append_To (Block_St, N);

                  N := Make_Assignment_Statement
                    (Make_Defining_Identifier (VN (V_CDR_Position)),
                     Make_Expression
                     (Make_Literal (Padding_Value),
                      Op_Plus,
                      Make_Defining_Identifier (VN (V_CDR_Position))));

                  Append_To (Block_St, N);
               end;

            when K_String_Type
              | K_Wide_String_Type =>
               declare
                  Padding_Value : Value_Id;
               begin
                  --  Getting the string length

                  Padding_Value := Parameter_Size (Type_Spec_Node);

                  --  Padding for the string length

                  N := Make_Subprogram_Call
                    (RE (RE_Pad_Compute),
                     New_List
                     (Make_Identifier (VN (V_CDR_Position)),
                      Make_Identifier (VN (V_Buffer_Size)),
                      Make_Literal (New_Integer_Value (4, 1, 10))));

                  Append_To (Block_St, N);

                  --  Update Buffer_Size and CDR_Position

                  N := Make_Assignment_Statement
                    (Make_Defining_Identifier (VN (V_Buffer_Size)),
                     Make_Expression
                     (Make_Literal (New_Integer_Value (4, 1, 10)),
                      Op_Plus,
                      Make_Identifier (VN (V_Buffer_Size))));

                  Append_To (Block_St, N);

                  N := Make_Assignment_Statement
                    (Make_Defining_Identifier (VN (V_CDR_Position)),
                     Make_Expression
                     (Make_Literal (New_Integer_Value (4, 1, 10)),
                      Op_Plus,
                      Make_Identifier (VN (V_CDR_Position))));

                  Append_To (Block_St, N);

                  --  Add the string length to Buffer_Size and
                  --  CDR_Position.

                  N := Make_Assignment_Statement
                    (Make_Defining_Identifier (VN (V_Buffer_Size)),
                     Make_Expression
                     (Make_Literal (Padding_Value),
                      Op_Plus,
                      Make_Defining_Identifier (VN (V_Buffer_Size))));

                  Append_To (Block_St, N);

                  N := Make_Assignment_Statement
                    (Make_Defining_Identifier (VN (V_CDR_Position)),
                     Make_Expression
                     (Make_Literal (Padding_Value),
                      Op_Plus,
                      Make_Defining_Identifier (VN (V_CDR_Position))));

                  Append_To (Block_St, N);
               end;

            when K_Fixed_Point_Type =>
               declare
                  M            : Node_Id;
               begin
                  --  Getting the fixed_point type

                  N := Expand_Designator
                    (Type_Def_Node (BE_Node (Type_Spec_Node)));

                  --  Instantiate the package:
                  --  PolyORB.Buffers.Optimization.Fixed_Point

                  N := Make_Package_Instantiation
                    (Make_Defining_Identifier (VN (V_FXS)),
                     RU (RU_PolyORB_Buffers_Optimization_Fixed_Point),
                     New_List
                     (N));
                  Append_To (Block_Dcl, N);

                  N := Make_Selected_Component
                    (VN (V_FXS),
                     SN (S_Type_Size));

                  M := Make_Subprogram_Call
                    (RE (RE_Stream_Element_Count),
                     (New_List
                      (Make_Subprogram_Call
                       (N,
                        New_List
                        (Cast_Variable_To_PolyORB_Type
                         (Var_Node, Direct_Type_Node))))));

                  --  Update Buffer_Size and CDR_Position

                  N := Make_Assignment_Statement
                    (Make_Defining_Identifier (VN (V_Buffer_Size)),
                     Make_Expression
                     (M,
                      Op_Plus,
                      Make_Defining_Identifier (VN (V_Buffer_Size))));
                  Append_To (Block_St, N);

                  N := Make_Assignment_Statement
                    (Make_Defining_Identifier (VN (V_CDR_Position)),
                     Make_Expression
                     (M,
                      Op_Plus,
                      Make_Defining_Identifier (VN (V_CDR_Position))));
                  Append_To (Block_St, N);

                  --  Indicate the use of method_name_args variable

                  if not Args_Declared then
                     Declare_Args (Subp_Dec, Subp_Nod);
                  end if;
                  Variable_Buffer := True;
               end;

            when K_Long_Double =>
               declare
                  Padding_Value  : Value_Id;
               begin
                  --  Alignment for Long Double is not equal to his
                  --  size (/= 16).

                  Padding_Value := Parameter_Size (Type_Spec_Node);

                  N := Make_Subprogram_Call
                    (RE (RE_Pad_Compute),
                     New_List
                     (Make_Identifier (VN (V_CDR_Position)),
                      Make_Identifier (VN (V_Buffer_Size)),
                      Make_Literal (New_Integer_Value (8, 1, 10))));

                  Append_To (Block_St, N);

                  --  Update Buffer_Size and CDR_Position

                  N := Make_Assignment_Statement
                    (Make_Defining_Identifier (VN (V_Buffer_Size)),
                     Make_Expression
                     (Make_Literal (Padding_Value),
                      Op_Plus,
                      Make_Defining_Identifier (VN (V_Buffer_Size))));

                  Append_To (Block_St, N);

                  N := Make_Assignment_Statement
                    (Make_Defining_Identifier (VN (V_CDR_Position)),
                     Make_Expression
                     (Make_Literal (Padding_Value),
                      Op_Plus,
                      Make_Defining_Identifier (VN (V_CDR_Position))));

                  Append_To (Block_St, N);
               end;

            when K_String
              | K_Wide_String =>
               declare
                  M : Node_Id;
               begin
                  --  Padding for string length

                  N := Make_Subprogram_Call
                    (RE (RE_Pad_Compute),
                     New_List
                     (Make_Identifier (VN (V_CDR_Position)),
                      Make_Identifier (VN (V_Buffer_Size)),
                      Make_Literal (New_Integer_Value (4, 1, 10))));

                  Append_To (Block_St, N);

                  --  Update Buffer_Size and CDR_Position for the
                  --  marshalling of string length.

                  N := Make_Assignment_Statement
                    (Make_Defining_Identifier (VN (V_Buffer_Size)),
                     Make_Expression
                     (Make_Identifier (VN (V_Buffer_Size)),
                      Op_Plus,
                      Make_Literal (New_Integer_Value (4, 1, 10))));

                  Append_To (Block_St, N);

                  N := Make_Assignment_Statement
                    (Make_Defining_Identifier (VN (V_CDR_Position)),
                     Make_Expression
                     (Make_Identifier (VN (V_CDR_Position)),
                      Op_Plus,
                      Make_Literal (New_Integer_Value (4, 1, 10))));

                  Append_To (Block_St, N);

                  --  Call of Type_Size subprogram

                  M := Make_Subprogram_Call
                    (RE (RE_Type_Size),
                     New_List
                     (Cast_Variable_To_PolyORB_Type
                       (Var_Node, Direct_Type_Node)));

                  N := Make_Assignment_Statement
                    (Make_Defining_Identifier (VN (V_Buffer_Size)),
                     Make_Expression
                     (Make_Subprogram_Call
                      (RE (RE_Stream_Element_Count),
                       New_List (M)),
                      Op_Plus,
                      Make_Defining_Identifier (VN (V_Buffer_Size))));

                  Append_To (Block_St, N);

                  N := Make_Assignment_Statement
                    (Make_Defining_Identifier (VN (V_CDR_Position)),
                     Make_Expression
                     (Make_Subprogram_Call
                      (RE (RE_Stream_Element_Count),
                       New_List (M)),
                      Op_Plus,
                      Make_Defining_Identifier (VN (V_CDR_Position))));

                  Append_To (Block_St, N);

                  --  Indicate the use of method_name_args variable

                  if not Args_Declared then
                     Declare_Args (Subp_Dec, Subp_Nod);
                  end if;

                  Variable_Buffer := True;
               end;
            when K_Sequence_Type =>
               declare
                  Seq_Package_Node : Node_Id;
                  Seq_Element      : Node_Id;
                  Index_Node       : Node_Id;
                  Range_Constraint : Node_Id;
                  Padding_Value    : Value_Id;
                  For_Statements   : constant List_Id := New_List;
               begin
                  --  padding for sequence length

                  N := Make_Subprogram_Call
                    (RE (RE_Pad_Compute),
                     New_List
                     (Make_Identifier (VN (V_CDR_Position)),
                      Make_Identifier (VN (V_Buffer_Size)),
                      Make_Literal (New_Integer_Value (4, 1, 10))));

                  Append_To (Block_St, N);

                  --  updating Buffer_Size and CDR_Position

                  N := Make_Assignment_Statement
                    (Make_Defining_Identifier (VN (V_Buffer_Size)),
                     Make_Expression
                     (Make_Identifier (VN (V_Buffer_Size)),
                      Op_Plus,
                      Make_Literal (New_Integer_Value (4, 1, 10))));
                  Append_To (Block_St, N);

                  N := Make_Assignment_Statement
                    (Make_Defining_Identifier (VN (V_CDR_Position)),
                     Make_Expression
                     (Make_Identifier (VN (V_CDR_Position)),
                      Op_Plus,
                      Make_Literal (New_Integer_Value (4, 1, 10))));
                  Append_To (Block_St, N);

                  --  Getting the instantiated package node

                  Seq_Package_Node := Defining_Identifier
                    (Instantiation_Node (BE_Node (Type_Spec_Node)));

                  --  Getting the sequence length

                  N := Make_Object_Declaration
                    (Defining_Identifier => Make_Defining_Identifier
                     (VN (V_Seq_Len)),
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
                    (Make_Defining_Identifier (VN (V_Seq_Len)), N);

                  Append_To (Block_St, N);

                  N := Make_Selected_Component
                    (Seq_Package_Node,
                     Get_Base_Identifier (RE (RE_Get_Element)));

                  --  Verify if the element type is complex

                  Padding_Value := Parameter_Size (Type_Spec (Type_Spec_Node));

                  if Padding_Value = Int0_Val then
                     --  Sequence element type is complex

                     Index_Node := Make_Defining_Identifier (VN (V_Index));

                     --  Creating the range constraint

                     Range_Constraint := Make_Range_Constraint
                       (Make_Literal (Int1_Val),
                        Make_Defining_Identifier (VN (V_Seq_Len)));

                     --  Getting the sequence element

                     Seq_Element := Make_Subprogram_Call
                       (N,
                        New_List
                        (Cast_Variable_To_PolyORB_Type
                         (Var_Node, Direct_Type_Node),
                         Make_Subprogram_Call
                         (RE (RE_Positive),
                          New_List (Index_Node))));

                     N := Compute_Size
                       (Var_Node => Seq_Element,
                        Var_Type => Type_Spec (Type_Spec_Node),
                        Subp_Dec => Subp_Dec,
                        Subp_Nod => Subp_Nod);

                     Append_To (For_Statements, N);

                     --  Building the loop

                     N := Make_For_Statement
                       (Index_Node,
                        Range_Constraint,
                        For_Statements);
                     Append_To (Block_St, N);

                  else
                     --  Sequence element type is simple so we can
                     --  compute sequence size without a loop.

                     declare
                        M : Node_Id;
                     begin
                        --  Padding for the first element

                        N := Make_Subprogram_Call
                          (RE (RE_Pad_Compute),
                           New_List
                           (Make_Identifier (VN (V_CDR_Position)),
                            Make_Identifier (VN (V_Buffer_Size)),
                            Make_Literal (Padding_Value)));

                        Append_To (Block_St, N);

                        --  Multiply 'Len' by sequence element size

                        M := Make_Expression
                          (Make_Defining_Identifier (VN (V_Seq_Len)),
                           Op_Asterisk,
                           Make_Literal (Padding_Value));

                        --  Update buffer_size and CDR_Position

                        N := Make_Assignment_Statement
                          (Make_Defining_Identifier (VN (V_Buffer_Size)),

                           Make_Expression
                           (Make_Subprogram_Call
                            (RE (RE_Stream_Element_Count),
                             New_List (M)),
                            Op_Plus,
                            Make_Defining_Identifier (VN (V_Buffer_Size))));
                        Append_To (Block_St, N);

                        N := Make_Assignment_Statement
                          (Make_Defining_Identifier (VN (V_CDR_Position)),
                           Make_Expression
                           (Make_Subprogram_Call
                            (RE (RE_Stream_Element_Count),
                             New_List (M)),
                            Op_Plus,
                            Make_Defining_Identifier (VN (V_CDR_Position))));
                        Append_To (Block_St, N);
                     end;
                  end if;

                  if not Args_Declared then
                     Declare_Args (Subp_Dec, Subp_Nod);
                  end if;
                  Variable_Buffer := True;
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
                  Padding_Value        : Value_Id;
                  M                    : Node_Id;
                  Loop_Range           : Value_Id;
                  Type_Param           : Node_Id;
               begin
                  Type_Param := Type_Spec (Declaration (Type_Spec_Node));
                  Padding_Value := Parameter_Size (Type_Param);

                  --  If element type is simple

                  if Padding_Value /= Int0_Val then
                     --  Compute the number of element of the array

                     Dim := First_Node (Sizes);
                     M := Make_Literal (Padding_Value);
                     loop
                        Loop_Range := New_Integer_Value
                          (Unsigned_Long_Long'Value
                           (Values.Image (Get_Value (Last (Dim)))) + 1,
                           1,
                           10);

                        M := Make_Expression
                          (Make_Literal (Loop_Range),
                           Op_Asterisk,
                           M);

                        Dim := Next_Node (Dim);
                        exit when No (Dim);
                     end loop;

                     N := Make_Subprogram_Call
                       (RE (RE_Pad_Compute),
                        New_List
                        (Make_Identifier (VN (V_CDR_Position)),
                         Make_Identifier (VN (V_Buffer_Size)),
                         Make_Literal (Padding_Value)));
                     Append_To (Block_St, N);

                     --  Update Buffer_Size and CDR_Position

                     N := Make_Assignment_Statement
                       (Make_Defining_Identifier (VN (V_Buffer_Size)),
                        Make_Expression
                        (Make_Literal (Padding_Value),
                         Op_Plus,
                         M));
                     Append_To (Block_St, N);

                     N := Make_Assignment_Statement
                       (Make_Defining_Identifier (VN (V_CDR_Position)),
                        Make_Expression
                        (Make_Literal (Padding_Value),
                         Op_Plus,
                         M));
                     Append_To (Block_St, N);
                  else
                     --  Element type is complex, Building the nested
                     --  loops

                     Dim := First_Node (Sizes);
                     loop
                        Get_Name_String (Index_Name);
                        Add_Char_To_Name_Buffer ('_');
                        Add_Nat_To_Name_Buffer (I);
                        Index_Node := Make_Defining_Identifier
                          (Add_Suffix_To_Name (Var_Suffix, Name_Find));
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
                     --  the making padding for the corresponding
                     --  array element.

                     N := Make_Subprogram_Call (Var_Node, Index_List);

                     N := Compute_Size
                       (Var_Node => N,
                        Var_Type => Type_Spec (Declaration (Type_Spec_Node)),
                        Subp_Dec => Subp_Dec,
                        Subp_Nod => Subp_Nod);
                     Append_To (Loop_Statements, N);
                  end if;
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

                        N := Compute_Size
                          (Var_Node => Dcl_Ada_Node,
                           Var_Type => Declarator,
                           Subp_Dec => Subp_Dec,
                           Subp_Nod => Subp_Nod);
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
                     Make_Identifier (CN (C_Switch)));

                  N := Compute_Size
                    (Var_Node => Switch_Node,
                     Var_Type => Switch_Type_Spec (Type_Spec_Node),
                     Subp_Dec => Subp_Dec,
                     Subp_Nod => Subp_Nod);

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

                     N := Compute_Size
                       (Var_Node => Dcl_Ada_Node,
                        Var_Type => Declarator,
                        Subp_Dec => Subp_Dec,
                        Subp_Nod => Subp_Nod);

                     Append_To (Switch_Statements, N);

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

                  Append_To (Block_St,
                    Make_Case_Statement (Switch_Node, Switch_Alternatives));

                  if not Args_Declared then
                     Declare_Args (Subp_Dec, Subp_Nod);
                  end if;
                  Variable_Buffer := True;
               end;

            when others =>
               Append_To (Block_St, Make_Null_Statement);
         end case;

         N := Make_Block_Statement
           (Declarative_Part => Block_Dcl,
            Statements       => Block_St);
         return N;
      end Compute_Size;

      ----------------------
      --  Parameter_Size  --
      ----------------------

      function Parameter_Size (N : Node_Id)
                              return Value_Id
      is
         Type_Spec_Node : Node_Id;
      begin

         Type_Spec_Node := FEU.Get_Original_Type_Specifier (N);

         case FEN.Kind (Type_Spec_Node) is

            when K_Boolean
              | K_Char =>
               return New_Integer_Value (1, 1, 10);

            when K_Short
              | K_Unsigned_Short =>
               return New_Integer_Value (2, 1, 10);

            when K_Unsigned_Long
              | K_Float
              | K_Enumeration_Type
              | K_Long =>
               return New_Integer_Value (4, 1, 10);

            when K_Long_Long
              | K_Unsigned_Long_Long
              | K_Double =>
               return New_Integer_Value (8, 1, 10);

            when K_Long_Double =>
               return New_Integer_Value (16, 1, 10);

            when K_Octet =>
               return New_Integer_Value (1, 1, 10);

            when K_String_Type
              | K_Wide_String_Type =>
               declare
                  String_Size : Unsigned_Long_Long;
               begin
                  String_Size := Unsigned_Long_Long'Value
                    (Values.Image
                     (FEU.Expr_Value (Max_Size (Type_Spec_Node))));
                  return New_Integer_Value (String_Size, 1, 10);
               end;

            when K_Object
              | K_Interface_Declaration =>
               --  Just an estimation IOR hasn't a fixed length

               return New_Integer_Value (1024, 1, 10);

            when others =>
               return Int0_Val;
         end case;
      end Parameter_Size;

      procedure Declare_Args
        (Subp_Dec : List_Id;
         Subp_Nod : Node_Id)
      is
         Args_Id : Node_Id;
         M       : Node_Id;
         N       : Node_Id;
      begin
         Args_Declared := True;
         N := Expand_Designator
           (Type_Def_Node
            (BE_Node
             (Identifier
              (Subp_Nod))));

         Args_Id := Map_Args_Identifier
           (Defining_Identifier
            (Stub_Node
             (BE_Node
              (Identifier
               (Subp_Nod)))));

         M := Make_Explicit_Dereference
           (Make_Identifier
              (PN (P_Args)));

         N := Make_Object_Declaration
           (Defining_Identifier => Args_Id,
            Object_Definition   => N,
            Expression          => Make_Subprogram_Call
            (N, New_List (M)));
         Append_To (Subp_Dec, N);
      end Declare_Args;
   end Package_Body;
end Backend.BE_CORBA_Ada.Buffers;

------------------------------------------------------------------------------
--                                                                          --
--                            GLADE COMPONENTS                              --
--                                                                          --
--                             X E _ P A R S E                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision$                             --
--                                                                          --
--         Copyright (C) 1996,1997 Free Software Foundation, Inc.           --
--                                                                          --
-- GNATDIST is  free software;  you  can redistribute  it and/or  modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software  Foundation;  either version 2,  or  (at your option) any later --
-- version. GNATDIST is distributed in the hope that it will be useful, but --
-- WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHANTABI- --
-- LITY or FITNESS  FOR A PARTICULAR PURPOSE.  See the  GNU General  Public --
-- License  for more details.  You should  have received a copy of the  GNU --
-- General Public License distributed with  GNATDIST; see file COPYING.  If --
-- not, write to the Free Software Foundation, 59 Temple Place - Suite 330, --
-- Boston, MA 02111-1307, USA.                                              --
--                                                                          --
--                 GLADE  is maintained by ACT Europe.                      --
--                 (email: glade-report@act-europe.fr)                      --
--                                                                          --
------------------------------------------------------------------------------

with Namet;            use Namet;
with Osint;            use Osint;
with Output;           use Output;
with Table;
with XE;               use XE;
with XE_Scan;          use XE_Scan;
with XE_Utils;         use XE_Utils;
with XE_Stdcnf;        use XE_Stdcnf;

package body XE_Parse is

   procedure Write_Conflict_Error
     (SLOC  : in Location_Type;
      Name  : in Name_Id);

   procedure Write_Declaration_Error
     (SLOC  : in Location_Type;
      Name  : in Name_Id);

   procedure Write_Error_Message
     (SLOC  : in Location_Type;
      Mesg1 : in String  := "";
      Name1 : in Name_Id := No_Name;
      Mesg2 : in String  := "";
      Name2 : in Name_Id := No_Name);

   procedure Write_Type_Error
     (SLOC  : in Location_Type;
      Name  : in Name_Id);

   ------------------------
   -- Check_Not_Declared --
   ------------------------

   procedure Check_Not_Declared
     (Declaration_Name : in Name_Id;
      Declaration_Sloc : in Location_Type) is
      Node : Node_Id;
   begin
      Search_Declaration (Declaration_Name, Node);
      if Node /= Null_Node then
         Write_Conflict_Error (Declaration_Sloc, Declaration_Name);
      end if;
   end Check_Not_Declared;

   ---------------------
   -- Declare_Literal --
   ---------------------

   procedure Declare_Literal
     (Literal_Name : in  Name_Id;
      Literal_Type : in  Type_Id;
      Literal_Sloc : in  Location_Type;
      Literal_Node : out Variable_Id) is
      L : Variable_Id;
   begin

      --  A literal is a variable which is not linked into the
      --  configuration declaration list.
      Create_Variable    (L, Literal_Name);
      Set_Variable_Type  (L, Literal_Type);
      Set_Node_Location  (Node_Id (L), Literal_Sloc);
      Literal_Node := L;

   end Declare_Literal;

   ----------------------------
   -- Declare_Procedure_Call --
   ----------------------------

   procedure Declare_Procedure_Call
     (Subprogram_Node : in Subprogram_Id) is
      New_Statement  : Statement_Id;
      Old_Subprogram : Subprogram_Id;
      New_Subprogram : Subprogram_Id;
      Old_Parameter  : Parameter_Id;
      New_Parameter  : Parameter_Id;
   begin

      Old_Subprogram := Subprogram_Node;

      --  Parser naming convention: ISN_Proc_Call indicates a procedure call.

      Create_Statement (New_Statement, ISN_Proc_Call);

      --  Make a entire copy of subprogram node.

      Create_Subprogram
        (New_Subprogram, Get_Node_Name (Node_Id (Old_Subprogram)));
      Subprogram_Is_A_Procedure
        (New_Subprogram, Is_Subprogram_A_Procedure (Old_Subprogram));
      Set_Pragma_Kind
        (New_Subprogram, Get_Pragma_Kind (Old_Subprogram));

      --  Make a copy of parameters.

      First_Subprogram_Parameter (Old_Subprogram, Old_Parameter);
      while Old_Parameter /= Null_Parameter loop

         Declare_Subprogram_Parameter
           (Get_Node_Name (Node_Id (Old_Parameter)),
            Get_Parameter_Type (Old_Parameter),
            New_Subprogram,
            Null_Location,
            New_Parameter);

         --  Assign the formal parameters as they were during the parameter
         --  matching phase.

         Duplicate_Variable
           (Get_Variable_Value (Variable_Id (Old_Parameter)),
            Variable_Id (New_Parameter));

         Parameter_Is_Initialized  (New_Parameter, True);
         Next_Subprogram_Parameter (Old_Parameter);

      end loop;

      Set_Subprogram_Call (New_Statement, New_Subprogram);

      Add_Configuration_Declaration
        (Configuration_Node, Node_Id (New_Statement));

   end Declare_Procedure_Call;

   ------------------------
   -- Declare_Subprogram --
   ------------------------

   procedure Declare_Subprogram
     (Subprogram_Name  : in  Name_Id;
      Pragma_Kind      : in  Pragma_Type;
      Is_A_Procedure   : in  Boolean;
      Subprogram_Sloc  : in  Location_Type;
      Subprogram_Node  : out Subprogram_Id) is
      Node : Subprogram_Id;
      Unit : Variable_Id;
   begin

      if Pragma_Kind = Pragma_Unknown then

         --  An ada unit node should be defined and its value holds the
         --  subprogram node. This way, function or procedure are handled
         --  as normal ada units.

         Search_Variable (Subprogram_Name, Ada_Unit_Type_Node, Unit);
         if Unit = Null_Variable then
            Declare_Variable
              (Subprogram_Name,
               Ada_Unit_Type_Node,
               Subprogram_Sloc,
               Unit);

         elsif Get_Variable_Value (Unit) /= Null_Variable then

            --  In this case, the ada unit is already declared, but already
            --  holds a function or procedure node.
            Write_Conflict_Error (Subprogram_Sloc, Subprogram_Name);

         end if;
      end if;

      Create_Subprogram             (Node, Subprogram_Name);
      Set_Node_Location             (Node_Id (Node), Subprogram_Sloc);
      Subprogram_Is_A_Procedure     (Node, Is_A_Procedure);
      Set_Pragma_Kind               (Node, Pragma_Kind);
      Subprogram_Node := Node;

      if Pragma_Kind = Pragma_Unknown then

         --  If it is an ada unit (variable) then it is already linked into
         --  the configuration declaration list.
         Set_Variable_Value (Unit, Variable_Id (Node));

      else
         Add_Configuration_Declaration (Configuration_Node, Node_Id (Node));
      end if;

   end Declare_Subprogram;

   ----------------------------------
   -- Declare_Subprogram_Parameter --
   ----------------------------------

   procedure Declare_Subprogram_Parameter
     (Parameter_Name  : in  Name_Id;
      Para_Type_Node  : in  Type_Id;
      Subprogram_Node : in  Subprogram_Id;
      Parameter_Sloc  : in  Location_Type;
      Parameter_Node  : out Parameter_Id) is
      Node : Parameter_Id;
   begin

      Create_Parameter           (Node, Parameter_Name);
      Set_Parameter_Type         (Node, Para_Type_Node);
      Add_Subprogram_Parameter   (Subprogram_Node, Node);
      Set_Node_Location          (Node_Id (Node), Parameter_Sloc);
      Parameter_Node := Node;

   end Declare_Subprogram_Parameter;

   ------------------
   -- Declare_Type --
   ------------------

   procedure Declare_Type
     (Type_Name : in  Name_Id;
      Type_Kind : in  Predefined_Type;
      List_Size : in  Int;
      Comp_Type : in  Type_Id;
      Is_Frozen : in  Boolean;
      Type_Sloc : in  Location_Type;
      Type_Node : out Type_Id) is
      T : Type_Id;
   begin

      Check_Not_Declared (Type_Name, Get_Token_Location);
      Create_Type (T, Type_Name);
      if Comp_Type /= Null_Type then
         Set_Array_Component_Type (T, Comp_Type);
      end if;
      if 0 /= List_Size then
         Set_Component_List_Size (T, List_Size);
      end if;
      Type_Is_Frozen      (T, Is_Frozen);
      Set_Type_Kind       (T, Type_Kind);
      Set_Node_Location   (Node_Id (T), Type_Sloc);
      Type_Node := T;

      Add_Configuration_Declaration  (Configuration_Node, Node_Id (T));

   end Declare_Type;

   ----------------------------
   -- Declare_Type_Attribute --
   ----------------------------

   procedure Declare_Type_Attribute
     (Type_Node          : in Type_Id;
      Attribute_Name     : in Name_Id;
      Attr_Type_Node     : in Type_Id;
      Attribute_Kind     : in Attribute_Type;
      Attribute_Sloc     : in Location_Type;
      Attribute_Node     : out Attribute_Id) is
      A : Attribute_Id;
   begin

      Declare_Type_Component
        (Type_Node,
         Attribute_Prefix & Attribute_Name,
         Attr_Type_Node,
         Attribute_Sloc,
         Component_Id (A));
      Set_Attribute_Kind        (Component_Id (A), Attribute_Kind);
      Attribute_Node := A;

   end Declare_Type_Attribute;

   ----------------------------
   -- Declare_Type_Component --
   ----------------------------

   procedure Declare_Type_Component
     (Type_Node          : in Type_Id;
      Component_Name     : in Name_Id;
      Comp_Type_Node     : in Type_Id;
      Component_Sloc     : in Location_Type;
      Component_Node     : out Component_Id) is
      C : Component_Id;
   begin

      Create_Component          (C, Component_Name);
      Set_Component_Type        (C, Comp_Type_Node);
      Set_Attribute_Kind        (C, Attribute_Unknown);
      Add_Type_Component        (Type_Node, C);
      Set_Node_Location         (Node_Id (C), Component_Sloc);
      Component_Node            := C;

   end Declare_Type_Component;

   ----------------------
   -- Declare_Variable --
   ----------------------

   procedure Declare_Variable
     (Variable_Name : in  Name_Id;
      Variable_Type : in  Type_Id;
      Variable_Sloc : in  Location_Type;
      Variable_Node : out Variable_Id) is
      V : Variable_Id;
      C : Component_Id;
      X : Component_Id;
      S : Int;
   begin

      Check_Not_Declared (Variable_Name, Variable_Sloc);

      Create_Variable    (V, Variable_Name);
      Set_Variable_Type  (V, Variable_Type);

      --  This type is a structure, duplicate the structure (but not
      --  attributes).

      S := Get_Component_List_Size (Variable_Type);
      if S /= 0 and then S /= Unbounded then
         First_Type_Component (Variable_Type, C);
         while C /= Null_Component loop
            if Get_Attribute_Kind (C) = Attribute_Unknown then
               Duplicate_Component (C, X);
               Add_Variable_Component (V, X);
               S := S - 1;
            end if;
            Next_Type_Component (C);
         end loop;
         pragma Assert (S = 0);
      end if;

      Set_Node_Location  (Node_Id (V), Variable_Sloc);
      Variable_Node := V;

      Add_Configuration_Declaration (Configuration_Node, Node_Id (V));

   end Declare_Variable;

   --------------------------------
   -- Declare_Variable_Component --
   --------------------------------

   procedure Declare_Variable_Component
     (Variable_Node      : in Variable_Id;
      Component_Name     : in Name_Id;
      Component_Type     : in Type_Id;
      Component_Value    : in Variable_Id;
      Attribute_Kind     : in Attribute_Type;
      Component_Sloc     : in Location_Type;
      Component_Node     : out Component_Id) is
      C : Component_Id;
   begin

      Create_Component          (C, Component_Name);
      Set_Component_Type        (C, Component_Type);
      Set_Attribute_Kind        (C, Attribute_Kind);
      Set_Component_Value       (C, Node_Id (Component_Value));
      Add_Variable_Component    (Variable_Node, C);
      Set_Node_Location         (Node_Id (C), Component_Sloc);
      Component_Node            := C;

   end Declare_Variable_Component;

   -------------------------
   -- Duplicate_Component --
   -------------------------

   procedure Duplicate_Component
     (Source : in Component_Id;
      Target : out Component_Id) is
      C : Component_Id;
      N : Name_Id;
      T : Type_Id;
      A : Attribute_Type;
   begin

      N := Get_Node_Name (Node_Id (Source));
      T := Get_Component_Type (Source);
      A := Get_Attribute_Kind (Source);
      Create_Component   (C, N);
      Set_Component_Type (C, T);
      Set_Attribute_Kind (C, A);
      Component_Is_Initialized (C, False);
      Target := C;

   end Duplicate_Component;

   ------------------------
   -- Duplicate_Variable --
   ------------------------

   procedure Duplicate_Variable
     (Source, Target : in Variable_Id) is
      C : Component_Id;
      T : Type_Id;
      X : Component_Id;
      V : Variable_Id;
   begin

      T := Get_Variable_Type (Source);
      pragma Assert (Get_Variable_Type (Target) = T);

      if Get_Array_Component_Type (T) /= Null_Type then

         --  This is a list assignment.
         First_Variable_Component (Source, C);
         while C /= Null_Component loop
            if Get_Attribute_Kind (C) = Attribute_Unknown then
               Duplicate_Component (C, X);
               Add_Variable_Component (Target, X);
               V := Variable_Id (Get_Component_Value (C));
               Set_Component_Value (X, Node_Id (V));
            end if;
            Next_Variable_Component (C);
         end loop;

      else

         --  Assign a single element.
         Set_Variable_Value (Target, Source);

      end if;

   end Duplicate_Variable;

   ---------------------------
   -- Exit_On_Parsing_Error --
   ---------------------------

   procedure Exit_On_Parsing_Error is
   begin
      Print;
      raise Parsing_Error;
   end Exit_On_Parsing_Error;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin

      Attribute_Prefix := Str_To_Id ("attr__");
      Pragma_Prefix    := Str_To_Id ("pragma__");
      Type_Prefix      := Str_To_Id ("type__");

      ISN_List_Comp  := Str_To_Id ("_list_component");
      ISN_Array_Comp := Str_To_Id ("array_component");
      ISN_Proc_Main  := Str_To_Id ("_main_procedure");
      ISN_Appl_Main  := Str_To_Id ("_appl_main_proc");
      ISN_Return_Par := Str_To_Id ("_returned_param");
      ISN_Subpro_Par := Str_To_Id ("_sub_prog_param");
      ISN_Proc_Call  := Str_To_Id ("_procedure_call");
      ISN_Proc_Call  := Str_To_Id ("_procedure_unit");

      XE_Stdcnf.Initialize;

      Print;

   end Initialize;

   ---------------------------
   -- Is_Expression_Of_Type --
   ---------------------------

   function Is_Expression_Of_Type
     (Expr_Node : in Node_Id;
      Type_Node : in Type_Id)
      return Boolean is
      P : Parameter_Id;
      C : Component_Id;
      V : Variable_Id;
   begin

      --  If variable, check the variable type with the given type.

      if Is_Variable (Expr_Node) then

         V := Variable_Id (Expr_Node);
         if Get_Variable_Type (V) = Ada_Unit_Type_Node then

            --  When this is an ada unit, check its value.
            V := Get_Variable_Value (V);
            return Is_Expression_Of_Type (Node_Id (V), Type_Node);

         else
            return Get_Variable_Type (V) = Type_Node;
         end if;

      elsif Is_Subprogram (Expr_Node) then

         --  If Expr_Node is a function, check returned parameter.

         if not Is_Subprogram_A_Procedure (Subprogram_Id (Expr_Node)) and then
           Get_Type_Kind (Type_Node) /= Pre_Type_Function then
            Search_Function_Returned_Parameter (Subprogram_Id (Expr_Node), P);
            return Get_Parameter_Type (P) = Type_Node;

         end if;

         --  Is the expression a function when the type is a function type
         --  or a procedure when the type is a procedure type ?

         if (Is_Subprogram_A_Procedure (Subprogram_Id (Expr_Node)) and then
             Get_Type_Kind (Type_Node) /= Pre_Type_Procedure) or else
            (not Is_Subprogram_A_Procedure (Subprogram_Id (Expr_Node)) and then
             Get_Type_Kind (Type_Node) /= Pre_Type_Function) then
            return False;
         end if;

         if Get_Array_Component_Type (Type_Node) /= Null_Type then

            --  Check also type of parameters ...

            First_Type_Component (Type_Node, C);
            First_Subprogram_Parameter (Subprogram_Id (Expr_Node), P);
            while C /= Null_Component and then P /= Null_Parameter loop
               if Get_Component_Type (C) /= Get_Parameter_Type (P) then
                  return False;
               end if;
               Next_Type_Component (C);
               Next_Subprogram_Parameter (P);
            end loop;

         else
            return True;
         end if;

         --  Check we have the same number of parameters ...

         return C = Null_Component and P = Null_Parameter;

      else
         return False;
      end if;

   end Is_Expression_Of_Type;

   -----------
   -- Match --
   -----------

   function Match (L : Token_List_Type) return Boolean is
   begin
      for Index in L'Range loop
         if L (Index) = Token then
            return True;
         end if;
      end loop;
      return False;
   end Match;

   ------------------------------
   -- Match_Actual_With_Formal --
   ------------------------------

   procedure Match_Actual_With_Formal
     (Subprogram_Node : in Subprogram_Id) is
      Convention     : Convention_Type;
      Actual_Name    : Name_Id;
      Formal_Name    : Name_Id;
      Actual_Node    : Variable_Id;
      Formal_Node    : Parameter_Id;
      Formal_Type    : Type_Id;
      N_Parameter    : Int;
      Location       : Location_Type;
   begin

      --  Look for the matching (marked) parameters. When a formal
      --  parameter has an associated actual parameter, mark the
      --  formal parameter and set the formal parameter value to
      --  the actual parameter.

      N_Parameter := 0;
      First_Subprogram_Parameter (Subprogram_Node, Formal_Node);
      while Formal_Node /= Null_Parameter loop
         Parameter_Is_Initialized (Formal_Node, False);
         N_Parameter := N_Parameter + 1;
         Next_Subprogram_Parameter (Formal_Node);
      end loop;

      --  At the beginning, convention is unknown.

      if N_Parameter <= 0 then
         return;
      end if;

      T_Left_Paren;

      --  What is the the convention used here.

      Take_Token ((Tok_Identifier, Tok_String_Literal));
      Location := Get_Token_Location;
      Take_Token ((Tok_Arrow, Tok_Comma, Tok_Right_Paren));
      if Token = Tok_Arrow then
         Convention := Named;
      else
         Convention := Positional;
      end if;
      Set_Token_Location (Location);

      loop
         Location := Get_Token_Location;

         if Convention = Named then
            T_Identifier;
            Formal_Name := Token_Name;
            T_Arrow;
         end if;

         --  If convention = named, check that such a formal parameter
         --     belongs to the subprogram parameter list.
         --  If convention = positional, retrieve the first unmarked
         --     (unmatched) parameter (name and node).

         Search_Matching_Parameter
           (Subprogram_Node,
            Convention,
            Formal_Name,
            Formal_Type,
            Formal_Node);

         if Formal_Node = Null_Parameter then
            Write_Error_Message (Location, "formal parameter mismatch");
         end if;

         Take_Token ((Tok_Identifier, Tok_String_Literal));
         Location    := Get_Token_Location;
         Actual_Name := Token_Name;

         if Token = Tok_String_Literal then

            if Formal_Type /= String_Type_Node then
               Write_Error_Message (Location, "actual parameter mismatch");
            end if;

            --  Create a dummy declaration that contains the literal.

            Declare_Literal
              (Actual_Name,
               String_Type_Node,
               Location,
               Actual_Node);

         else

            --  Does this actual parameter really exist ?

            Search_Actual_Parameter (Actual_Name, Formal_Type, Actual_Node);

            if Actual_Node = Null_Variable then
               Write_Error_Message (Location, "actual parameter mismatch");
            end if;

         end if;

         --  Mark the matching parameter and set its value to actual
         --  parameter value.

         Duplicate_Variable (Actual_Node, Variable_Id (Formal_Node));

         --  There is one less parameter to match.

         Parameter_Is_Initialized (Formal_Node, True);
         N_Parameter := N_Parameter - 1;

         Take_Token ((Tok_Comma, Tok_Right_Paren));

         if Token = Tok_Right_Paren then
            exit when N_Parameter = 0;
            Write_Error_Message (Get_Token_Location, "missing parameters");
         elsif Token /= Tok_Right_Paren and then N_Parameter = 0 then
            Write_Error_Message (Get_Token_Location, "too many parameters");
         end if;

      end loop;

      T_Semicolon;

   end Match_Actual_With_Formal;

   -----------------------------
   -- P_Aggregate_Assignement --
   -----------------------------

   procedure P_Aggregate_Assignement
     (Variable_Node : in Variable_Id) is
      Expression_Name   : Name_Id;
      Expression_Node   : Variable_Id;
      Expression_Sloc   : Location_Type;
      Variable_Type     : Type_Id;
      List_Element_Node : Component_Id;
      List_Element_Type : Type_Id;
      Comp_List_Size    : Int;
   begin

      --  Only aggregates are allowed at this point.
      Variable_Type  := Get_Variable_Type (Variable_Node);
      Comp_List_Size := Get_Component_List_Size (Variable_Type);
      T_Left_Paren;

      if Comp_List_Size = 0 then
         Write_Error_Message
           (Get_Token_Location, "only aggregate are allowed");
      end if;

      if Comp_List_Size = Unbounded then
         List_Element_Type := Get_Array_Component_Type (Variable_Type);
      end if;

      loop

         Take_Token ((Tok_Identifier, Tok_Right_Paren));
         exit when Token = Tok_Right_Paren;

         Expression_Sloc := Get_Token_Location;

         if Comp_List_Size /= Unbounded then

            Search_Uninitialized_Component
              (Variable_Node, Null_Type, List_Element_Node);
            if List_Element_Node = Null_Component then
               Write_Error_Message
                 (Expression_Sloc, "too many components for record aggregate");
            end if;

            List_Element_Type := Get_Component_Type (List_Element_Node);
         end if;

         --  Ada unit names are allowed.

         P_Full_Ada_Identifier;
         Expression_Name := Token_Name;

         Search_Variable (Expression_Name, Expression_Node);

         --  Has this variable already been declared.

         if Expression_Node /= Null_Variable then

            if Get_Variable_Type (Expression_Node) /= List_Element_Type then
               Write_Conflict_Error (Expression_Sloc, Expression_Name);
            end if;

         elsif Is_Type_Frozen (List_Element_Type) then
            Write_Declaration_Error (Expression_Sloc, Expression_Name);

         else

            --  Increase the enumeration type.

            Declare_Variable
              (Expression_Name,
               List_Element_Type,
               Expression_Sloc,
               Expression_Node);

         end if;

         if Get_Component_List_Size (Variable_Type) = Unbounded then

            --  As a naming convention, we use the keyword Component_Unit
            --  as a anonymous component name.

            Declare_Variable_Component
              (Variable_Node,
               ISN_Array_Comp,
               List_Element_Type,
               Expression_Node,
               Attribute_Unknown,
               Expression_Sloc,
               List_Element_Node);

         else

            Set_Component_Value (List_Element_Node, Node_Id (Expression_Node));

         end if;

         Take_Token ((Tok_Comma, Tok_Right_Paren));
         exit when Token = Tok_Right_Paren;

      end loop;

      T_Semicolon;

   end P_Aggregate_Assignement;

   --------------------------
   -- P_Configuration_Body --
   --------------------------

   procedure P_Configuration_Body is
      Name : Name_Id;
   begin
      loop
         Take_Token ((Tok_Identifier, Tok_Null, Tok_End));
         if Token = Tok_Identifier then

            --  This is an assignement. Includes a list of ada units
            --  into a partition.

            declare
               Variable_Node : Variable_Id;
            begin
               Name := Token_Name;
               Search_Variable (Name, Variable_Node);
               if Variable_Node = Null_Variable then
                  Write_Declaration_Error (Get_Token_Location, Name);
               end if;
               T_Colon_Equal;

               --  Read the ada units aggregate.

               P_Aggregate_Assignement (Variable_Node);

            end;
         elsif Token = Tok_End  then

            P_Configuration_End;
            exit;

         end if;

      end loop;

   end P_Configuration_Body;

   ---------------------------------
   -- P_Configuration_Declaration --
   ---------------------------------

   procedure P_Configuration_Declaration is
      Conf_Name : Name_Id;
      Conf_Sloc : Location_Type;
      Conf_Node : Configuration_Id;
   begin

      --  Use "standard" configuration to start.

      T_Configuration;
      T_Identifier;
      Conf_Name := Token_Name;
      Conf_Sloc := Get_Token_Location;

      --  We have the real configuration node. Let's use this one.

      Create_Configuration (Conf_Node, Conf_Name);
      Set_Node_Location    (Node_Id (Conf_Node), Conf_Sloc);

      --  Append the "standard" root configuration to the new one.

      Add_Configuration_Declaration (Conf_Node, Node_Id (Configuration_Node));

      --  Now, the new configuration is the root configuration.

      Configuration_Node := Conf_Node;

      T_Is;

   end P_Configuration_Declaration;

   -------------------------
   -- P_Configuration_End --
   -------------------------

   procedure P_Configuration_End is
   begin
      Take_Token ((Tok_Identifier, Tok_Semicolon));

      --  Check that the configuration name is matching the current
      --  configuration name.

      if Token = Tok_Identifier then
         if Get_Node_Name (Node_Id (Configuration_Node)) /= Token_Name then
            Write_Error_Message (Get_Token_Location, "name mismatch");
         end if;
         T_Semicolon;
      end if;

   end P_Configuration_End;

   ---------------------------
   -- P_Full_Ada_Identifier --
   ---------------------------

   procedure P_Full_Ada_Identifier is
      Identifier : Name_Id := Token_Name;
      Location   : Location_Type;
   begin
      loop
         Next_Token;
         Location := Get_Token_Location;

         --  If token is '.' then continue ...

         if Token = Tok_Dot then
            T_Identifier;
            Identifier := Identifier & Dot_Sep_Id & Token_Name;

         --  If not, then this is the identifier end.

         else
            Set_Token_Location (Location);
            Token_Name := Identifier;
            Token      := Tok_Identifier;
            exit;
         end if;
      end loop;

   end P_Full_Ada_Identifier;

   ----------------------------
   -- P_Function_Declaration --
   ----------------------------

   procedure P_Function_Declaration is
      Function_Name  : Name_Id;
      Function_Sloc  : Location_Type;
      Function_Node  : Subprogram_Id;
      Parameter_Name : Name_Id;
      Parameter_Sloc : Location_Type;
      Parameter_Node : Parameter_Id;
      Para_Type_Name : Name_Id;
      Para_Type_Sloc : Location_Type;
      Para_Type_Node : Type_Id;
      Para_Type_Kind : Predefined_Type;
   begin

      --  The following is the only allowed signature :
      --     function <F> (<X> : String) return String;
      --  where <F> and <X> are to be defined.

      --  Token FUNCTION has already been parsed.

      T_Identifier;
      Function_Name := Token_Name;
      Function_Sloc := Get_Token_Location;

      Declare_Subprogram
        (Function_Name,
         Pragma_Unknown,
         False,
         Function_Sloc,
         Function_Node);

      T_Left_Paren;

      --  Get parameter <X>.

      T_Identifier;
      Parameter_Name := Token_Name;
      Parameter_Sloc := Get_Token_Location;

      T_Colon;

      --  Get parameter type.

      T_Identifier;
      Para_Type_Name := Token_Name;
      Para_Type_Sloc := Get_Token_Location;

      Search_Type (Para_Type_Name, Para_Type_Kind, Para_Type_Node);

      --  String is the only expected type.

      if Para_Type_Node /= String_Type_Node then
         Write_Error_Message
           (Para_Type_Sloc, """",
            Para_Type_Name, """ is not the expected type");
      end if;

      --  Declare <X> as a formal parameter.

      Declare_Subprogram_Parameter
        (Parameter_Name,
         Para_Type_Node,
         Function_Node,
         Parameter_Sloc,
         Parameter_Node);

      T_Right_Paren;
      T_Return;

      --  Get returned parameter type.

      T_Identifier;
      Para_Type_Name := Token_Name;
      Para_Type_Sloc := Get_Token_Location;

      Search_Type (Para_Type_Name, Para_Type_Kind, Para_Type_Node);

      --  String is the only type allowed at this level.

      if Para_Type_Node /= String_Type_Node then
         Write_Type_Error (Para_Type_Sloc, Para_Type_Name);
      end if;

      --  Declare returned parameter type. As a naming convention
      --  we use keyword Returned_Param as the anonymous parameter.

      Declare_Subprogram_Parameter
        (ISN_Return_Par,
         Para_Type_Node,
         Function_Node,
         Null_Location,
         Parameter_Node);

      T_Semicolon;

   end P_Function_Declaration;

   --------------
   -- P_Pragma --
   --------------

   procedure P_Pragma is
      Pragma_Kind : Pragma_Type;
      Pragma_Node : Subprogram_Id;
      Pragma_Name : Name_Id;
   begin

      --  Token PRAGMA has already been parsed.

      T_Identifier;

      --  Known pragmas are prefixed by Pragma_Prefix.

      Pragma_Name := Pragma_Prefix & Token_Name;

      --  Is this pragma a legal pragma.

      Search_Pragma (Pragma_Name, Pragma_Kind, Pragma_Node);
      if Pragma_Node = Null_Subprogram then
         Write_Error_Message
           (Get_Token_Location, "unrecognized pragma """, Token_Name, """");
      end if;

      --  Parse a pragma as a procedure call.

      Match_Actual_With_Formal (Pragma_Node);

      --  When successful, declare the procedure call node.

      Declare_Procedure_Call (Pragma_Node);

   end P_Pragma;

   -----------------------------
   -- P_Procedure_Declaration --
   -----------------------------

   procedure P_Procedure_Declaration is
      Ada_Unit_Node  : Variable_Id;
      Constant_True  : Variable_Id;
      Partition_Name : Name_Id;
      Partition_Node : Variable_Id;
      Partition_Sloc : Location_Type;
      Procedure_Sloc : Location_Type;
      Procedure_Name : Name_Id;
      Procedure_Node : Subprogram_Id;
      Component_Node : Component_Id;
   begin

      --  Token PROCEDURE has already been parsed.

      T_Identifier;

      Procedure_Name := Token_Name;
      Procedure_Sloc := Get_Token_Location;

      Declare_Subprogram
        (Procedure_Name,
         Pragma_Unknown,
         True,
         Procedure_Sloc,
         Procedure_Node);

      Take_Token ((Tok_Is, Tok_Semicolon));

      if Token = Tok_Is then

         T_In;

         --  This should be an already declared variable.

         T_Identifier;
         Partition_Name := Token_Name;
         Partition_Sloc := Get_Token_Location;
         Search_Variable (Partition_Name, Partition_Node);

         --  This variable has to be already declared. Its type has to be
         --  of the predefined type Partition_Type_Node.

         if Partition_Node = Null_Variable or else
           Get_Variable_Type (Partition_Node) /= Partition_Type_Node then
            Write_Conflict_Error (Partition_Sloc, Partition_Name);
         end if;

         Search_Variable (Procedure_Name, Ada_Unit_Node);

         Declare_Variable_Component
           (Variable_Node      => Partition_Node,
            Component_Name     => Attribute_Prefix & Str_To_Id ("main"),
            Component_Type     => Ada_Unit_Type_Node,
            Component_Value    => Ada_Unit_Node,
            Attribute_Kind     => Attribute_Main,
            Component_Sloc     => Procedure_Sloc,
            Component_Node     => Component_Node);

         Search_Variable (Str_To_Id ("true"), Constant_True);

         Declare_Variable_Component
           (Variable_Node      => Partition_Node,
            Component_Name     => Attribute_Prefix & Str_To_Id ("_leader"),
            Component_Type     => Boolean_Type_Node,
            Component_Value    => Constant_True,
            Attribute_Kind     => Attribute_Leader,
            Component_Sloc     => Procedure_Sloc,
            Component_Node     => Component_Node);

         T_Semicolon;

      end if;

   end P_Procedure_Declaration;

   -----------------------------
   -- P_Representation_Clause --
   -----------------------------

   procedure P_Representation_Clause is
      Direct_Name : Name_Id;
      Direct_Node : Node_Id;
      Direct_Type : Type_Id;
      Attr_Name   : Name_Id;
      Attr_Sloc   : Location_Type;
      Attr_Type   : Type_Id;
      Attr_Node   : Component_Id;
      Expr_Name   : Name_Id;
      Expr_Node   : Node_Id;
      Expr_Sloc   : Location_Type;
      Is_A_Type   : Boolean;
   begin

      --  Token FOR has already been parsed.

      T_Identifier;
      Direct_Name := Token_Name;
      Search_Declaration (Direct_Name, Direct_Node);

      --  This identifier has to be already declared.

      if Direct_Node /= Null_Node then

         --  If legal, retrieve variable Direct_Node type.

         if Is_Variable (Direct_Node) then
            Direct_Type := Get_Variable_Type (Variable_Id (Direct_Node));
            Is_A_Type := False;

         elsif Is_Type (Direct_Node) then
            Direct_Type := Type_Id (Direct_Node);
            Is_A_Type := True;

         --  Only variables and types are subject to representation clause.

         else
            Write_Error_Message
              (Get_Token_Location,
               "attribute cannot be given to ", Direct_Name);
         end if;

      else
         Write_Declaration_Error (Get_Token_Location, Direct_Name);
      end if;

      T_Apostrophe;

      --  Get the attribute name.

      T_Identifier;
      Attr_Name := Token_Name;
      Attr_Sloc := Get_Token_Location;

      --  Attributes are always prefixed by Attribute_Prefix.

      Search_Component
        (Attribute_Prefix & Attr_Name,
         Direct_Type,
         Attr_Node);

      --  Check that this attribute is a legal attribute for the given type.

      if Attr_Node = Null_Component or else
        Get_Attribute_Kind (Attr_Node) = Attribute_Unknown then
         Write_Error_Message
           (Get_Token_Location, "unrecognized attribute """, Attr_Name, """");
      end if;

      Attr_Name := Attribute_Prefix & Attr_Name;

      T_Use;
      Take_Token ((Tok_Identifier, Tok_String_Literal));
      Expr_Name := Token_Name;
      Expr_Sloc := Get_Token_Location;

      --  If string literal, declare an anonymous variable.

      if Token = Tok_String_Literal then
         Declare_Literal
           (Expr_Name,
            String_Type_Node,
            Expr_Sloc,
            Variable_Id (Expr_Node));

      --  Otherwise, retrieve the declaration.

      else
         Search_Declaration (Expr_Name, Expr_Node);
         if Expr_Node = Null_Node then
            Write_Declaration_Error (Expr_Sloc, Expr_Name);
         end if;
      end if;

      Attr_Type := Get_Component_Type (Attr_Node);

      --  Check that the expression has the correct type.

      if not Is_Expression_Of_Type (Expr_Node, Attr_Type) then
         Write_Error_Message
           (Get_Token_Location, """",
            Get_Node_Name (Node_Id (Expr_Node)),
            """ is an invalid expression here");
      end if;

      if Is_A_Type then

         --  Set attribute to the given value.
         Set_Component_Value (Attr_Node, Expr_Node);

      else

         Declare_Variable_Component
           (Variable_Node      => Variable_Id (Direct_Node),
            Component_Name     => Attr_Name,
            Component_Type     => Attr_Type,
            Component_Value    => Variable_Id (Expr_Node),
            Attribute_Kind     => Get_Attribute_Kind (Attr_Node),
            Component_Sloc     => Attr_Sloc,
            Component_Node     => Attr_Node);

      end if;

      T_Semicolon;

   end P_Representation_Clause;

   ---------------------------------
   -- P_Variable_List_Declaration --
   ---------------------------------

   procedure P_Variable_List_Declaration
     (Previous_Name : in Name_Id;
      Previous_Sloc : in Location_Type) is
      Previous_Node : Variable_Id;
      Variable_Name : Name_Id;
      Variable_Node : Variable_Id;
      Variable_Sloc : Location_Type;
      Var_Type_Name : Name_Id;
      Var_Type_Node : Type_Id;
      Var_Type_Kind : Predefined_Type;
      Var_Type_Sloc : Location_Type;
   begin

      Take_Token ((Tok_Comma, Tok_Colon));

      --  Is it a list of identifiers ?

      if Token = Tok_Comma then

         T_Identifier;
         Variable_Name := Token_Name;
         Variable_Sloc := Get_Token_Location;

         --  Declare a temporary variable of any type.

         --  XXXXX: Should not use partition_type.
         Declare_Variable
           (Previous_Name,
            Partition_Type_Node,
            Previous_Sloc,
            Previous_Node);

         --  Call recursively P_Variable_List_Declaration until the
         --  end of list. Variable_Node is a node to the next
         --  declared variable.

         P_Variable_List_Declaration (Variable_Name, Variable_Sloc);

         --  Variable can now be fully described.

         Search_Variable (Variable_Name, Variable_Node);
         Set_Variable_Type (Previous_Node, Get_Variable_Type (Variable_Node));

         --  If previous variable has been initialized, initialize
         --  this newly declared variable as well.

         Duplicate_Variable (Variable_Node, Previous_Node);

      else

         --  The following identifier is a type.

         T_Identifier;
         Var_Type_Name := Token_Name;
         Var_Type_Sloc := Get_Token_Location;

         --  Has this type been declared ?

         Search_Type
           (Var_Type_Name,
            Var_Type_Kind,
            Var_Type_Node);

         if Var_Type_Node = Null_Type then
            Write_Type_Error (Var_Type_Sloc, Var_Type_Name);
         end if;

         --  Declare this new variable of type Var_Type_Node.

         Declare_Variable
           (Previous_Name,
            Var_Type_Node,
            Previous_Sloc,
            Previous_Node);

         Take_Token ((Tok_Semicolon, Tok_Colon_Equal));

         --  Is there an initialization ?

         if Token = Tok_Colon_Equal then
            P_Aggregate_Assignement (Previous_Node);
         end if;

      end if;

   end P_Variable_List_Declaration;

   -----------
   -- Parse --
   -----------

   procedure Parse is
   begin  --  Parse

      Load_File (Configuration_File);

      P_Configuration_Declaration;

      loop

         Take_Token
           ((Tok_Identifier,
             Tok_Procedure,
             Tok_Function,
             Tok_For,
             Tok_Pragma,
             Tok_Begin,
             Tok_End));

         case Token is

            when Tok_Function   =>
               P_Function_Declaration;

            when Tok_Procedure  =>
               P_Procedure_Declaration;

            when Tok_For        =>
               P_Representation_Clause;

            when Tok_Pragma     =>
               P_Pragma;

            when Tok_Identifier =>
               P_Variable_List_Declaration (Token_Name, Get_Token_Location);

            when Tok_Begin      =>
               P_Configuration_Body;
               exit;

            when Tok_End        =>
               P_Configuration_End;
               exit;

            when others         => null;
         end case;

      end loop;

      T_EOF;

      Print;

   end Parse;

   -----------
   -- Print --
   -----------

   procedure Print
     (Node : in Variable_Id;
      Head : in String) is
      T : Type_Id;
      S : Int;
      C : Component_Id;
      H : String (1 .. Head'Length + 6) := (others => ' ');
   begin
      Write_Str  (Head);
      Write_Str  ("Type : ");
      T := Get_Variable_Type (Node);
      Write_Name (Get_Node_Name (Node_Id (T)));
      Write_Eol;
      Write_Str  (Head);
      S := Get_Component_List_Size (T);
      if S = 0 then
         if Get_Variable_Value (Node) /= Null_Variable then
            Write_Str  (Head);
            Write_Str  ("    Data :");
            Write_Eol;
            Write_Str  (Head);
            Write_Str  ("       ");
            Write_Name (Get_Node_Name (Node_Id (Get_Variable_Value (Node))));
            Write_Eol;
         else
            Write_Str  (Head);
            Write_Str  ("Val  : ");
            Write_Int (Get_Scalar_Value (Node));
            Write_Eol;
         end if;
      else
         Write_Str  (Head);
         if Get_Array_Component_Type (T) /= Null_Type then
            Write_Str  ("   Array :");
         else
            Write_Str  ("   Record :");
         end if;
         Write_Eol;
         First_Variable_Component (Node, C);
         while C /= Null_Component loop
            Print (C, H, False);
            Next_Variable_Component (C);
         end loop;
         Write_Str  (Head);
         Write_Str  ("    Attr :");
         Write_Eol;
         First_Variable_Component (Node, C);
         while C /= Null_Component loop
            Print (C, H, True);
            Next_Variable_Component (C);
         end loop;
      end if;
   end Print;

   -----------
   -- Print --
   -----------

   procedure Print
     (Node : in Type_Id;
      Head : in String) is
      C : Component_Id;
      S : Int;
      T : Type_Id;
      H : String (1 .. Head'Length + 6) := (others => ' ');
   begin
      S := Get_Component_List_Size (Node);
      if S /= 0 then
         if S = Unbounded then
            Write_Str  (Head);
            Write_Str  ("   Size : unbounded");
            Write_Eol;
            T := Get_Array_Component_Type (Node);
            Write_Str  ("   Kind : ");
            Write_Int  (Convert (Get_Type_Kind (T)));
            Write_Eol;
         else
            Write_Str  (Head);
            Write_Str  ("   Size : ");
            Write_Int  (S);
            Write_Eol;
            First_Type_Component (Node, C);
            if C /= Null_Component then
               Write_Str  (Head);
               Write_Str  ("   Data : ");
               Write_Eol;
               while C /= Null_Component loop
                  Print (C, H, False);
                  Next_Type_Component (C);
               end loop;
            end if;
         end if;
         First_Type_Component (Node, C);
         if C /= Null_Component then
            Write_Str  (Head);
            Write_Str  ("   Attr : ");
            Write_Eol;
            while C /= Null_Component loop
               Print (C, H, True);
               Next_Type_Component (C);
            end loop;
         end if;
      end if;
   end Print;

   -----------
   -- Print --
   -----------

   procedure Print
     (Node : in Component_Id;
      Head : in String;
      Attr : in Boolean) is
      Item  : Node_Id;
      Value : Node_Id;
   begin
      if (Get_Attribute_Kind (Node) /= Attribute_Unknown) = Attr then
         Write_Str  (Head);
         Write_Name (Get_Node_Name (Node_Id (Node)));
         Write_Str  (" : ");
         Item := Node_Id (Get_Component_Type (Node));
         Write_Name (Get_Node_Name (Item));
         if Is_Component_Initialized (Node) then
            Value := Get_Component_Value (Node);
            Write_Eol;
            Write_Str (Head);
            Write_Str ("   = ");
            Write_Name (Get_Node_Name (Node_Id (Value)));
         else
            Write_Eol;
            Write_Str (Head);
            Write_Str ("   = ???");
         end if;
         Write_Eol;
      end if;
   end Print;

   -----------
   -- Print --
   -----------

   procedure Print
     (Node : in Parameter_Id;
      Head : in String) is
      Item  : Type_Id;
   begin
      Write_Str  (Head);
      Write_Name (Get_Node_Name (Node_Id (Node)));
      Write_Str  (" : ");
      Item := Get_Parameter_Type (Node);
      Write_Name (Get_Node_Name (Node_Id (Item)));
      if Is_Parameter_Initialized (Node) then
         declare
            H : String (1 .. Head'Length + 3) := (others => ' ');
            V : Variable_Id;
         begin
            V := Get_Variable_Value (Variable_Id (Node));
            Write_Str (" := ");
            Write_Name (Get_Node_Name (Node_Id (V)));
         end;
      end if;
      Write_Eol;
   end Print;

   -----------
   -- Print --
   -----------

   procedure Print
     (Node : in Subprogram_Id;
      Head : in String) is
      H : String (1 .. Head'Length + 6) := (others => ' ');
      P : Parameter_Id;
   begin
      Write_Str (Head);
      Write_Str  ("    Mark : ");
      Write_Int  (Int (Get_Pragma_Kind (Node)));
      Write_Eol;
      Write_Str  (Head);
      Write_Str  ("    Proc : ");
      if Is_Subprogram_A_Procedure (Node) then
         Write_Str ("true");
      else
         Write_Str ("false");
      end if;
      Write_Eol;
      Write_Str  (Head);
      Write_Str  ("    Para :");
      Write_Eol;
      First_Subprogram_Parameter (Node, P);
      while P /= Null_Parameter loop
         Print (P, H);
         Next_Subprogram_Parameter (P);
      end loop;
   end Print;

   -----------
   -- Print --
   -----------

   procedure Print
     (Node : in Statement_Id;
      Head : in String) is
      S : Subprogram_Id;
   begin
      S := Get_Subprogram_Call (Node);
      Write_Str  (Head);
      Write_Str  ("Call : ");
      Write_Name (Get_Node_Name (Node_Id (S)));
      Write_Eol;
      Print (S, Head);
   end Print;

   -----------
   -- Print --
   -----------

   procedure Print
     (Node : in Configuration_Id;
      Head : in String) is
   begin
      null;
   end Print;

   -----------
   -- Print --
   -----------

   procedure Print
     (Node : in Node_Id;
      Head : in String) is
   begin
      Write_Str  (Head);
      Write_Str  ("Name : ");
      Write_Name (Get_Node_Name (Node));
      Write_Eol;
      Write_Str (Head);
      Write_Str ("Node : ");
      if Is_Variable (Node) then
         Write_Str ("variable");
         Write_Eol;
         Print (Variable_Id (Node), Head);
      elsif Is_Type (Node) then
         Write_Str ("type");
         Write_Eol;
         Print (Type_Id (Node), Head);
      elsif Is_Subprogram (Node) then
         Write_Str ("subprogram");
         Write_Eol;
         Print (Subprogram_Id (Node), Head);
      elsif Is_Configuration (Node) then
         Write_Str ("configuration");
         Write_Eol;
         Print (Configuration_Id (Node), Head);
      elsif Is_Statement (Node) then
         Write_Str ("statement");
         Write_Eol;
         Print (Statement_Id (Node), Head);
      else
         Write_Str ("unknown");
         Write_Eol;
      end if;
      Write_Eol;
   end Print;

   -----------
   -- Print --
   -----------

   procedure Print is
      Node : Node_Id;
   begin
      if Debug_Mode then
         Write_Eol;
         Write_Str ("configuration");
         Write_Eol;
         Write_Str ("=============");
         Write_Eol;
         Write_Eol;
         First_Configuration_Declaration (Configuration_Node, Node);
         while Node /= Null_Node loop
            Print (Node, "");
            Next_Configuration_Declaration (Node);
         end loop;
      end if;
   end Print;

   -----------------------------
   -- Search_Actual_Parameter --
   -----------------------------

   procedure Search_Actual_Parameter
     (Actual_Name : in  Name_Id;
      Actual_Type : in  Type_Id;
      Actual_Node : out Variable_Id) is
      Actual : Node_Id;
   begin

      --  Scan the configuration to retrieve a possible variable of
      --  name Actual_Name ...

      First_Configuration_Declaration (Configuration_Node, Actual);
      while Actual /= Null_Node loop
         if Is_Variable       (Actual) and then
            Get_Node_Name     (Actual) = Actual_Name and then
            Get_Variable_Type (Variable_Id (Actual)) = Actual_Type then
            Actual_Node := Variable_Id (Actual);
            return;
         end if;
         Next_Configuration_Declaration (Actual);
      end loop;

      Write_Declaration_Error (Get_Token_Location, Actual_Name);

   end Search_Actual_Parameter;

   ----------------------
   -- Search_Component --
   ----------------------

   procedure Search_Component
     (Component_Name : in  Name_Id;
      Type_Node      : in  Type_Id;
      Component_Node : out Component_Id) is
      C : Component_Id;
   begin

      First_Type_Component (Type_Node, C);
      while C /= Null_Component loop
         exit when Get_Node_Name (Node_Id (C)) = Component_Name;
         Next_Type_Component (C);
      end loop;
      Component_Node := C;

   end Search_Component;

   ----------------------
   -- Search_Component --
   ----------------------

   procedure Search_Component
     (Component_Name : in  Name_Id;
      Variable_Node  : in  Variable_Id;
      Component_Node : out Component_Id) is
      C : Component_Id;
   begin

      First_Variable_Component (Variable_Node, C);
      while C /= Null_Component loop
         exit when Get_Node_Name (Node_Id (C)) = Component_Name;
         Next_Variable_Component (C);
      end loop;
      Component_Node := C;

   end Search_Component;

   ------------------------
   -- Search_Declaration --
   ------------------------

   procedure Search_Declaration
     (Declaration_Name : in Name_Id;
      Declaration_Node : out Node_Id) is
      Node : Node_Id;
      Name : Name_Id;
   begin

      First_Configuration_Declaration (Configuration_Node, Node);
      while Node /= Null_Node loop
         Name := Get_Node_Name (Node);
         exit when Name = Declaration_Name;
         Next_Configuration_Declaration (Node);
      end loop;
      Declaration_Node := Node;

   end Search_Declaration;

   ----------------------------------------
   -- Search_Function_Returned_Parameter --
   ----------------------------------------

   procedure Search_Function_Returned_Parameter
     (Function_Node  : in Subprogram_Id;
      Parameter_Node : out Parameter_Id) is
      Prev, Next : Parameter_Id;
   begin
      pragma Assert (not Is_Subprogram_A_Procedure (Function_Node));

      --  Basically, look for the last parameter.
      Prev := Null_Parameter;
      First_Subprogram_Parameter (Function_Node, Next);
      while Next /= Null_Parameter loop
         Prev := Next;
         Next_Subprogram_Parameter (Next);
      end loop;
      Parameter_Node := Parameter_Id (Prev);

   end Search_Function_Returned_Parameter;

   -------------------------------
   -- Search_Matching_Parameter --
   -------------------------------

   procedure Search_Matching_Parameter
     (Subprogram_Node : in Subprogram_Id;
      Convention      : in Convention_Type;
      Formal_Name     : in out Name_Id;
      Formal_Type     : in out Type_Id;
      Parameter_Node  : in out Parameter_Id) is
   begin

      First_Subprogram_Parameter (Subprogram_Node, Parameter_Node);
      while Parameter_Node /= Null_Parameter loop
         Formal_Type := Get_Variable_Type (Variable_Id (Parameter_Node));
         case Convention is

            --  If Positional, find the first unmarked parameter.
            when Positional =>
               if not Is_Parameter_Initialized (Parameter_Node) then
                  Formal_Name := Get_Node_Name (Node_Id (Parameter_Node));
                  return;
               end if;

            --  If Named, use Formal_Name to return format parameter node.
            when Named =>
               if Get_Node_Name (Node_Id (Parameter_Node)) = Formal_Name then
                  return;
               end if;

         end case;
         Next_Subprogram_Parameter (Parameter_Node);
      end loop;

      Write_Error_Message (Get_Token_Location, "no matching parameter");

   end Search_Matching_Parameter;

   -------------------
   -- Search_Pragma --
   -------------------

   procedure Search_Pragma
     (Pragma_Name : in  Name_Id;
      Pragma_Kind : out Pragma_Type;
      Pragma_Node : out Subprogram_Id) is
      Node : Subprogram_Id;
   begin

      Search_Subprogram (Pragma_Name, Node);
      if Node /= Null_Subprogram then
         Pragma_Kind := Get_Pragma_Kind (Node);
      end if;
      Pragma_Node := Node;

   end Search_Pragma;

   -----------------------
   -- Search_Subprogram --
   -----------------------

   procedure Search_Subprogram
     (Subprogram_Name : in  Name_Id;
      Subprogram_Node : out Subprogram_Id) is
      Node : Node_Id;
   begin

      Search_Declaration (Subprogram_Name, Node);
      if Node /= Null_Node and then
         not Is_Subprogram (Node) then
         Node := Null_Node;
      end if;
      Subprogram_Node := Subprogram_Id (Node);

   end Search_Subprogram;

   -----------------
   -- Search_Type --
   -----------------

   procedure Search_Type
     (Type_Name : in  Name_Id;
      Type_Kind : out Predefined_Type;
      Type_Node : out Type_Id) is
      Node : Node_Id;
   begin

      Search_Declaration (Type_Name, Node);
      if Node /= Null_Node and then
         not Is_Type (Node) then
         Node := Null_Node;
      end if;
      Type_Node := Type_Id (Node);
      if Node /= Null_Node then
         Type_Kind := Get_Type_Kind (Type_Id (Node));
      end if;

   end Search_Type;

   ------------------------------------
   -- Search_Uninitialized_Component --
   ------------------------------------

   procedure Search_Uninitialized_Component
     (Variable_Node  : in  Variable_Id;
      Component_Type : in  Type_Id;
      Component_Node : out Component_Id) is
      C : Component_Id;
      T : Type_Id;
   begin

      First_Variable_Component (Variable_Node, C);
      while C /= Null_Component loop
         T := Get_Component_Type (C);
         exit when (Component_Type = T or else Component_Type = Null_Type)
           and then not Is_Component_Initialized (C);
         Next_Variable_Component (C);
      end loop;
      Component_Node := C;

   end Search_Uninitialized_Component;

   ---------------------
   -- Search_Variable --
   ---------------------

   procedure Search_Variable
     (Variable_Name : in  Name_Id;
      Variable_Node : out Variable_Id) is
      Node : Node_Id;
   begin

      Search_Declaration (Variable_Name, Node);
      if Node /= Null_Node  and then
         not Is_Variable (Node) then
         Node := Null_Node;
      end if;
      Variable_Node := Variable_Id (Node);

   end Search_Variable;

   ---------------------
   -- Search_Variable --
   ---------------------

   procedure Search_Variable
     (Variable_Name : in  Name_Id;
      Variable_Type : in Type_Id;
      Variable_Node : out Variable_Id) is
      Node : Node_Id;
   begin

      Search_Declaration (Variable_Name, Node);
      if Node /= Null_Node  and then
         not Is_Variable (Node) then
         Node := Null_Node;
      end if;
      Variable_Node := Variable_Id (Node);

   end Search_Variable;

   -----------------------
   -- Set_Node_Location --
   -----------------------

   procedure Set_Node_Location
     (Node     : in Node_Id;
      Location : in Location_Type) is
      X, Y : Int;
   begin
      Location_To_XY (Location, X, Y);
      Set_Node_SLOC  (Node, X, Y);
   end Set_Node_Location;

   ------------------
   -- T_Apostrophe --
   ------------------

   procedure T_Apostrophe is
   begin
      Take_Token (Tok_Apostrophe);
   end T_Apostrophe;

   -------------
   -- T_Arrow --
   -------------

   procedure T_Arrow is
   begin
      Take_Token (Tok_Arrow);
   end T_Arrow;

   -------------
   -- T_Colon --
   -------------

   procedure T_Colon is
   begin
      Take_Token (Tok_Colon);
   end T_Colon;

   -------------------
   -- T_Colon_Equal --
   -------------------

   procedure T_Colon_Equal is
   begin
      Take_Token (Tok_Colon_Equal);
   end T_Colon_Equal;

   -------------
   -- T_Comma --
   -------------

   procedure T_Comma is
   begin
      Take_Token (Tok_Comma);
   end T_Comma;

   ---------------------
   -- T_Configuration --
   ---------------------

   procedure T_Configuration is
   begin
      Take_Token (Tok_Configuration);
   end T_Configuration;

   -----------
   -- T_Dot --
   -----------

   procedure T_Dot is
   begin
      Take_Token (Tok_Dot);
   end T_Dot;

   -----------
   -- T_End --
   -----------

   procedure T_End is
   begin
      Take_Token (Tok_End);
   end T_End;

   -----------
   -- T_EOF --
   -----------

   procedure T_EOF is
   begin
      Take_Token (Tok_EOF);
   end T_EOF;

   -----------
   -- T_For --
   -----------

   procedure T_For is
   begin
      Take_Token (Tok_For);
   end T_For;

   ----------------
   -- T_Function --
   ----------------

   procedure T_Function is
   begin
      Take_Token (Tok_Function);
   end T_Function;

   ------------------
   -- T_Identifier --
   ------------------

   procedure T_Identifier is
   begin
      Take_Token (Tok_Identifier);
   end T_Identifier;

   ----------
   -- T_In --
   ----------

   procedure T_In is
   begin
      Take_Token (Tok_In);
   end T_In;

   ----------
   -- T_Is --
   ----------

   procedure T_Is is
   begin
      Take_Token (Tok_Is);
   end T_Is;

   ------------------
   -- T_Left_Paren --
   ------------------

   procedure T_Left_Paren is
   begin
      Take_Token (Tok_Left_Paren);
   end T_Left_Paren;

   --------------
   -- T_Pragma --
   --------------

   procedure T_Pragma is
   begin
      Take_Token (Tok_Pragma);
   end T_Pragma;

   -----------------
   -- T_Procedure --
   -----------------

   procedure T_Procedure is
   begin
      Take_Token (Tok_Procedure);
   end T_Procedure;

   --------------
   -- T_Return --
   --------------

   procedure T_Return is
   begin
      Take_Token (Tok_Return);
   end T_Return;

   -------------------
   -- T_Right_Paren --
   -------------------

   procedure T_Right_Paren is
   begin
      Take_Token (Tok_Right_Paren);
   end T_Right_Paren;

   -----------------
   -- T_Semicolon --
   -----------------

   procedure T_Semicolon is
   begin
      Take_Token (Tok_Semicolon);
   end T_Semicolon;

   ----------------------
   -- T_String_Literal --
   ----------------------

   procedure T_String_Literal is
   begin
      Take_Token (Tok_String_Literal);
   end T_String_Literal;

   -----------
   -- T_Use --
   -----------

   procedure T_Use is
   begin
      Take_Token (Tok_Use);
   end T_Use;

   ----------------
   -- Take_Token --
   ----------------

   procedure Take_Token (T : Token_Type) is
   begin
      Next_Token;
      if T /= Token then
         Write_Location (Get_Token_Location);
         Write_Token (T);
         Write_Str (" was expected");
         Write_Eol;
         Exit_On_Parsing_Error;
      end if;
   end Take_Token;

   ----------------
   -- Take_Token --
   ----------------

   procedure Take_Token (L : Token_List_Type) is
   begin
      Next_Token;
      if Match (L) then
         return;
      end if;
      Write_Location (Get_Token_Location);
      Write_Token (L (L'First));
      for Index in L'First + 1 .. L'Last loop
         Write_Str (" or ");
         Write_Token (L (Index));
      end loop;
      Write_Str (" was expected");
      Write_Eol;
      Exit_On_Parsing_Error;
   end Take_Token;

   --------------------------
   -- Write_Conflict_Error --
   --------------------------

   procedure Write_Conflict_Error
     (SLOC  : in Location_Type;
      Name  : in Name_Id) is
   begin
      Write_Error_Message
        (SLOC, """", Name, """ conflicts with a previous declaration");
   end Write_Conflict_Error;

   -----------------------------
   -- Write_Declaration_Error --
   -----------------------------

   procedure Write_Declaration_Error
     (SLOC  : in Location_Type;
      Name  : in Name_Id) is
   begin
      Write_Error_Message
        (SLOC, """", Name, """ is undefined");
   end Write_Declaration_Error;

   -------------------------
   -- Write_Error_Message --
   -------------------------

   procedure Write_Error_Message
     (SLOC  : in Location_Type;
      Mesg1 : in String  := "";
      Name1 : in Name_Id := No_Name;
      Mesg2 : in String  := "";
      Name2 : in Name_Id := No_Name) is
   begin
      Write_Location (SLOC);
      if Mesg1 /= "" then
         Write_Str (Mesg1);
      end if;
      if Name1 /= No_Name then
         Write_Name (Name1);
      end if;
      if Mesg2 /= "" then
         Write_Str (Mesg2);
      end if;
      if Name2 /= No_Name then
         Write_Name (Name2);
      end if;
      Write_Eol;
      Exit_On_Parsing_Error;
   end Write_Error_Message;

   ----------------------
   -- Write_Type_Error --
   ----------------------

   procedure Write_Type_Error
     (SLOC  : in Location_Type;
      Name  : in Name_Id) is
   begin
      Write_Error_Message (SLOC, """", Name, """ is not the expected type");
   end Write_Type_Error;

end XE_Parse;

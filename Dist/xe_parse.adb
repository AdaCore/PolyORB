------------------------------------------------------------------------------
--                                                                          --
--                            GLADE COMPONENTS                              --
--                                                                          --
--                             X E _ P A R S E                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision$
--                                                                          --
--         Copyright (C) 1996-1999 Free Software Foundation, Inc.           --
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
with Output;           use Output;
with Types;            use Types;
with XE;               use XE;
with XE_Scan;          use XE_Scan;
with XE_Utils;         use XE_Utils;

package body XE_Parse is

   subtype Node_Id is XE.Node_Id;

   Indent : constant String := "   ";

   --  Set Fatal_Error to False to allow overloading. In this case, if
   --  a litteral does not match the expected type, no error message
   --  is printed, an exception is raised and handled in order to try
   --  another matching.

   Fatal_Error    : Boolean := True;

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

   procedure Write_Indent
     (Many : in Int := 1;
      Mesg : in String := "");

   procedure Write_Type_Error
     (SLOC  : in Location_Type;
      Name  : in Name_Id);

   ------------------------
   -- Check_Not_Declared --
   ------------------------

   procedure Check_Not_Declared
     (Declaration_Name : in Name_Id;
      Declaration_Sloc : in Location_Type)
   is
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
      Literal_Node : out Variable_Id)
   is
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
     (Subprogram_Node : in Subprogram_Id;
      Subprogram_Sloc : in Location_Type)
   is
      New_Statement  : Statement_Id;
      Old_Subprogram : Subprogram_Id;
      New_Subprogram : Subprogram_Id;
      Old_Parameter  : Parameter_Id;
      New_Parameter  : Parameter_Id;
   begin
      Old_Subprogram := Subprogram_Node;

      --  Parser naming convention: Procedure_Name_Id indicates a
      --  procedure call.

      Create_Statement (New_Statement, Procedure_Name_Id);
      Set_Node_Location
        (Node_Id (New_Statement), Subprogram_Sloc);

      --  Make a copy of subprogram node.

      Create_Subprogram
        (New_Subprogram, Get_Node_Name (Node_Id (Old_Subprogram)));
      Subprogram_Is_A_Procedure
        (New_Subprogram, Is_Subprogram_A_Procedure (Old_Subprogram));
      Set_Pragma_Kind
        (New_Subprogram, Get_Pragma_Kind (Old_Subprogram));
      Set_Node_Location
        (Node_Id (New_Subprogram), Subprogram_Sloc);

      --  Make a copy of parameters.

      First_Subprogram_Parameter (Old_Subprogram, Old_Parameter);
      while Old_Parameter /= Null_Parameter loop

         Declare_Subprogram_Parameter
           (Get_Node_Name (Node_Id (Old_Parameter)),
            Get_Parameter_Type (Old_Parameter),
            New_Subprogram,
            Null_Location,
            New_Parameter);

         --  Assign the (actual) parameters of subprogram execution
         --  to the value of the formal parameters computed during
         --  the parameter matching phase.

         Set_Variable_Value
           (Variable_Id (New_Parameter),
            Get_Variable_Value (Variable_Id (Old_Parameter)));

         Parameter_Is_Initialized  (New_Parameter, True);
         Next_Subprogram_Parameter (Old_Parameter);

      end loop;

      Set_Subprogram_Call (New_Statement, New_Subprogram);

      Add_Configuration_Declaration
        (Configuration_Node,
         Node_Id (New_Statement));
   end Declare_Procedure_Call;

   ------------------------
   -- Declare_Subprogram --
   ------------------------

   procedure Declare_Subprogram
     (Subprogram_Name  : in  Name_Id;
      Pragma_Kind      : in  Pragma_Type;
      Is_A_Procedure   : in  Boolean;
      Subprogram_Sloc  : in  Location_Type;
      Subprogram_Node  : out Subprogram_Id)
   is
      Node : Subprogram_Id;
      Unit : Variable_Id;
   begin

      --  A pragma is handled like a subprogram execution. When parameter
      --  Pragma_Kind is different from Pragma_Unkown, we have a pragma
      --  declaration.

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

         elsif Is_Variable_Initialized (Unit) then

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

         --  If it is an ada unit (variable) then it is already linked
         --  into the configuration declaration list.
         Set_Variable_Value      (Unit, Variable_Id (Node));

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
      Parameter_Node  : out Parameter_Id)
   is
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
      Composite : in  Boolean;
      Array_Len : in  Int;
      Comp_Type : in  Type_Id;
      Type_Sloc : in  Location_Type;
      Type_Node : out Type_Id)
   is
      T : Type_Id;
   begin
      pragma Assert
        (not Composite
         or else Array_Len = 0
         or else Comp_Type /= Null_Type);

      Check_Not_Declared (Type_Name, Get_Token_Location);
      Create_Type (T, Type_Name);

      Type_Is_Composite (T, Composite);
      if Composite then
         Set_Array_Length (T, Array_Len);
         if Array_Len /= 0 then
            Set_Array_Component_Type (T, Comp_Type);
         end if;
      end if;

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
      Attribute_Node     : out Attribute_Id)
   is
      A : Attribute_Id;
   begin
      Declare_Type_Component
        (Type_Node,
         Attribute_Prefix & Attribute_Name,
         Attr_Type_Node,
         Attribute_Sloc,
         Component_Id (A));
      Set_Attribute_Kind (Component_Id (A), Attribute_Kind);
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
      Component_Node     : out Component_Id)
   is
      C : Component_Id;
   begin
      Create_Component          (C, Component_Name);
      Set_Component_Type        (C, Comp_Type_Node);
      Set_Attribute_Kind        (C, Attribute_Unknown);
      Component_Is_Initialized  (C, False);
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
      Variable_Node : out Variable_Id)
   is
      TV : Variable_Id;
      SC : Component_Id;
      TC : Component_Id;
   begin
      Check_Not_Declared (Variable_Name, Variable_Sloc);
      Create_Variable    (TV, Variable_Name);
      Set_Variable_Type  (TV, Variable_Type);

      --  This type is a record, allocate the record (but not
      --  attributes).

      if Is_Type_Composite (Variable_Type)
        and then Get_Array_Length (Variable_Type) = 0
      then
         First_Type_Component (Variable_Type, SC);
         while SC /= Null_Component loop
            if Get_Attribute_Kind (SC) = Attribute_Unknown then
               Declare_Variable_Component
                 (TV, Get_Node_Name (Node_Id (SC)),
                  Get_Component_Type (SC),
                  Attribute_Unknown,
                  Null_Location, TC);
            end if;
            Next_Type_Component (SC);
         end loop;
      end if;

      Set_Node_Location  (Node_Id (TV), Variable_Sloc);
      Variable_Node := TV;

      Add_Configuration_Declaration (Configuration_Node, Node_Id (TV));
   end Declare_Variable;

   --------------------------------
   -- Declare_Variable_Component --
   --------------------------------

   procedure Declare_Variable_Component
     (Variable_Node      : in Variable_Id;
      Component_Name     : in Name_Id;
      Component_Type     : in Type_Id;
      Attribute_Kind     : in Attribute_Type;
      Component_Sloc     : in Location_Type;
      Component_Node     : out Component_Id)
   is
      VC : Component_Id;
      VT : Type_Id;
   begin
      Create_Component         (VC, Component_Name);
      Set_Component_Type       (VC, Component_Type);
      Set_Attribute_Kind       (VC, Attribute_Kind);
      Component_Is_Initialized (VC, False);
      Add_Variable_Component   (Variable_Node, VC);
      Set_Node_Location        (Node_Id (VC), Component_Sloc);

      --  If we add a new component to an array, then increment the
      --  number of components.

      VT := Get_Variable_Type (Variable_Node);
      if Attribute_Kind = Attribute_Unknown
        and then Get_Array_Length (VT) /= 0
      then
         Set_Array_Length
           (Variable_Node,
            Get_Array_Length (Variable_Node) + 1);
      end if;
      Component_Node           := VC;
   end Declare_Variable_Component;

   ------------------------
   -- Duplicate_Variable --
   ------------------------

   procedure Duplicate_Variable
     (Source, Target : in Variable_Id)
   is
      SC : Component_Id;
      VT : Type_Id;
      TC : Component_Id;
   begin
      VT := Get_Variable_Type (Source);
      pragma Assert (Get_Variable_Type (Target) = VT);

      --  For a non-composite type, just set the variable value if
      --  needed.

      if not Is_Type_Composite (VT) then
         if Is_Variable_Initialized (Source) then
            Set_Variable_Value (Target, Get_Variable_Value (Source));
         end if;

      else

         --  If it is an array, then copy the number of components for
         --  the target.

         if Get_Array_Length (VT) /= 0 then
            Set_Array_Length (Target, Get_Array_Length (Source));
         end if;

         --  Allocate and initialize only non_attribute components;

         First_Variable_Component (Source, SC);
         while SC /= Null_Component loop
            if Get_Attribute_Kind (SC) = Attribute_Unknown then
               Declare_Variable_Component
                 (Target,
                  Get_Node_Name (Node_Id (SC)),
                  Get_Component_Type (SC),
                  Attribute_Unknown,
                  Null_Location, TC);
               if Is_Component_Initialized (SC) then
                  Set_Component_Value (TC, Get_Component_Value (SC));
               end if;
            end if;
            Next_Variable_Component (SC);
         end loop;
      end if;
   end Duplicate_Variable;

   -------------------
   -- Exit_On_Error --
   -------------------

   procedure Exit_On_Error is
   begin
      if Fatal_Error then
         Print;
         raise Parsing_Error;
      else
         raise Matching_Error;
      end if;
   end Exit_On_Error;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      Attribute_Prefix  := Str_To_Id ("attribute ");
      Pragma_Prefix     := Str_To_Id ("pragma ");
      Type_Prefix       := Str_To_Id ("type ");

      Function_Name_Id  := Str_To_Id ("function");
      Procedure_Name_Id := Str_To_Id ("procedure");
      Return_Name_Id    := Str_To_Id ("return");
   end Initialize;

   ------------------------------
   -- Match_Actual_With_Formal --
   ------------------------------

   procedure Match_Actual_With_Formal
     (Subprogram_Node : in Subprogram_Id)
   is
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

      --  Look forward to find the convention.

      Take_Token ((Tok_Identifier, Tok_String_Literal, Tok_Left_Paren));
      Location := Get_Token_Location;

      Convention := Positional;
      if Token = Tok_Identifier then
         Next_Token;
         if Token = Tok_Arrow then
            Convention := Named;
         end if;
      end if;
      Set_Token_Location (Location);

      --  Do the real matching once the convention is known.

      Formal_Name := No_Name;
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

         Take_Token ((Tok_Identifier, Tok_String_Literal, Tok_Left_Paren));
         Location    := Get_Token_Location;
         Actual_Name := Token_Name;

         if Token = Tok_String_Literal then

            if Formal_Type /= String_Type_Node then
               Write_Error_Message (Location, "actual parameter mismatch");
            end if;

            --  Create a declaration that contains the literal.
            Declare_Literal
              (Actual_Name,
               String_Type_Node,
               Location,
               Actual_Node);

         elsif Token = Tok_Identifier then

            --  Does this actual parameter really exist ?

            Search_Actual_Parameter (Actual_Name, Formal_Type, Actual_Node);
            if Actual_Node = Null_Variable then
               Write_Error_Message (Location, "actual parameter mismatch");
            end if;

         else

            --  This is a literal aggregate.

            Declare_Variable
              (New_Variable_Name,
               Formal_Type,
               Location,
               Actual_Node);

            --  Reset the location to read the first left parenthesis.

            Set_Token_Location (Location);
            P_Aggregate_Assignment (Actual_Node);
         end if;

         --  Mark the matching parameter and set its value to actual
         --  parameter value.

         Set_Variable_Value (Variable_Id (Formal_Node), Actual_Node);
         N_Parameter := N_Parameter - 1;

         exit when N_Parameter = 0;

         Next_Token;
         if Token /= Tok_Comma then
            Write_Error_Message (Get_Token_Location, "missing parameters");
         end if;
      end loop;
   end Match_Actual_With_Formal;

   -----------------------------
   -- P_Aggregate_Assignment --
   -----------------------------

   procedure P_Aggregate_Assignment
     (Variable_Node : in Variable_Id)
   is
      Expression_Name   : Name_Id;
      Expression_Node   : Variable_Id;
      Expression_Sloc   : Location_Type;
      Variable_Type     : Type_Id;
      Component_Node    : Component_Id;
      Component_Type    : Type_Id;
      Array_Length      : Int;
   begin

      --  Only aggregates are allowed at this point.
      Variable_Type := Get_Variable_Type (Variable_Node);
      if not Is_Type_Composite (Variable_Type) then
         Write_Error_Message
           (Get_Token_Location, "only aggregate are allowed");
         return;
      end if;

      Array_Length  := Get_Array_Length  (Variable_Type);
      T_Left_Paren;

      if Array_Length /= 0 then
         Component_Type := Get_Array_Component_Type (Variable_Type);
      end if;

      loop
         if Array_Length = 0 then
            Search_Uninitialized_Component
              (Variable_Node, Null_Type, Component_Node);
            if Component_Node = Null_Component then
               Write_Error_Message
                 (Get_Token_Location,
                  "too many components for record aggregate");
            end if;

            Component_Type := Get_Component_Type (Component_Node);
         end if;

         if Is_Type_Composite (Component_Type) then
            Take_Token ((Tok_Identifier,
                         Tok_Left_Paren,
                         Tok_Right_Paren,
                         Tok_String_Literal,
                         Tok_Numeric_Literal));
         else
            Take_Token ((Tok_Identifier,
                         Tok_Right_Paren,
                         Tok_String_Literal,
                         Tok_Numeric_Literal));
         end if;

         exit when Token = Tok_Right_Paren;

         Expression_Sloc := Get_Token_Location;

         if Token = Tok_Identifier then

            --  Ada unit names are allowed.

            P_Full_Ada_Identifier;
            Expression_Name := Token_Name;

            Search_Variable (Expression_Name, Expression_Node);
            if Expression_Node = Null_Variable then
               Declare_Variable
                 (Expression_Name,
                  Component_Type,
                  Expression_Sloc,
                  Expression_Node);
            end if;

         --  Tok_String_Literal.

         elsif Token = Tok_String_Literal then

            Declare_Literal
              (Token_Name,
               String_Type_Node,
               Expression_Sloc,
               Expression_Node);

         --  Tok_Numeric_Literal.

         elsif Token = Tok_Numeric_Literal then

            Declare_Literal
              (Token_Name,
               Integer_Type_Node,
               Expression_Sloc,
               Expression_Node);

         else
            Declare_Variable
              (New_Variable_Name,
               Component_Type,
               Expression_Sloc,
               Expression_Node);

            --  Reset the location to read the first left parenthesis.

            Set_Token_Location (Expression_Sloc);
            P_Aggregate_Assignment (Expression_Node);
         end if;

         --  Do this variable have the appropriate type.

         if Get_Variable_Type (Expression_Node) /= Component_Type then
            Write_Conflict_Error (Expression_Sloc, Expression_Name);
         end if;

         --  ???

         --  Variable_Is_Initialized (Expression_Node, True);

         if Array_Length /= 0 then

            --  We declare a component with an anonymous name.

            Declare_Variable_Component
              (Variable_Node,
               New_Component_Name (Variable_Node),
               Component_Type,
               Attribute_Unknown,
               Expression_Sloc,
               Component_Node);
         end if;

         Set_Component_Value (Component_Node, Expression_Node);

         Take_Token ((Tok_Comma, Tok_Right_Paren));
         exit when Token = Tok_Right_Paren;

      end loop;
      Variable_Is_Initialized (Variable_Node, True);
   end P_Aggregate_Assignment;

   --------------------------
   -- P_Configuration_Body --
   --------------------------

   procedure P_Configuration_Body
   is
      Name : Name_Id;
   begin
      if not Quiet_Mode then
         Write_Location (Get_Token_Location);
         Write_Str ("a configuration body is an obsolete feature");
         Write_Eol;
         Write_Location (Get_Token_Location);
         Write_Str ("this code should be moved in the declarative part");
         Write_Eol;
      end if;
      loop
         Take_Token ((Tok_Identifier, Tok_Null, Tok_End));
         if Token = Tok_Identifier then

            --  This is an assignment. Includes a list of ada units
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

               P_Aggregate_Assignment (Variable_Node);
               T_Semicolon;

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

   procedure P_Configuration_Declaration
   is
      Conf_Name : Name_Id;
      Conf_Sloc : Location_Type;
      Conf_Node : Configuration_Id;
   begin

      --  Use "private" configuration to start.

      T_Configuration;
      T_Identifier;
      Conf_Name := Token_Name;
      Conf_Sloc := Get_Token_Location;

      Check_Not_Declared   (Conf_Name, Conf_Sloc);

      --  We have the real configuration node. Let's use this one.

      Create_Configuration (Conf_Node, Conf_Name);
      Set_Node_Location    (Node_Id (Conf_Node), Conf_Sloc);

      --  Append the "private" configuration to the new one.

      Add_Configuration_Declaration
        (Conf_Node, Node_Id (Configuration_Node));

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

   procedure P_Full_Ada_Identifier
   is
      Identifier : Name_Id := Token_Name;
      Location   : Location_Type;
   begin
      loop
         Next_Token;
         Location := Get_Token_Location;

         --  If token is '.' then continue ...

         if Token = Tok_Dot then
            T_Identifier;
            Get_Name_String (Identifier);
            Add_Char_To_Name_Buffer ('.');
            Get_Name_String_And_Append (Token_Name);
            Identifier := Name_Find;

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

   procedure P_Function_Declaration
   is
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
      --  we use keyword Return_Name_Id as the anonymous parameter.

      Declare_Subprogram_Parameter
        (Return_Name_Id,
         Para_Type_Node,
         Function_Node,
         Para_Type_Sloc,
         Parameter_Node);

      T_Semicolon;
   end P_Function_Declaration;

   --------------
   -- P_Pragma --
   --------------

   procedure P_Pragma
   is
      Pragma_Kind : Pragma_Type;
      Pragma_Node : Subprogram_Id;
      Pragma_Name : Name_Id;
      Pragma_Sloc : Location_Type;
      Invoke_Sloc : Location_Type;
      Context     : Context_Type;
   begin

      --  Token PRAGMA has already been parsed.

      T_Identifier;

      --  Known pragmas are prefixed by Pragma_Prefix.

      Pragma_Name := Token_Name;
      Pragma_Sloc := Get_Token_Location;

      --  Is this pragma a known pragma.

      Search_Pragma (Pragma_Prefix & Pragma_Name, Pragma_Kind, Pragma_Node);
      if Pragma_Node = Null_Subprogram then
         Write_Error_Message
           (Get_Token_Location, "unrecognized pragma """, Pragma_Name, """");
      end if;

      declare
         Next_Node : Subprogram_Id := Pragma_Node;
      begin
         Search_Next_Pragma (Pragma_Prefix & Pragma_Name, Next_Node);
         Fatal_Error := (Next_Node = Null_Subprogram);
      end;

      --  Save the context. Try to find a matching pragma (some
      --  pragmas are overloaded). If the attempt fails, then reset
      --  the context and try another pragma. If this pragma is not
      --  overloaded, then a failure is a fatal error and errors have
      --  to be printed.

      loop
         begin
            Save_Context (Configuration_Node, Context);
            T_Left_Paren;
            Invoke_Sloc := Get_Token_Location;

            --  Parse a pragma as a procedure call.

            Match_Actual_With_Formal (Pragma_Node);

            --  There is a match. Any error is now fatal.
            Fatal_Error := True;

            Next_Token;
            if Token /= Tok_Right_Paren then
               Write_Error_Message (Get_Token_Location, "too many parameters");
            end if;
            exit;

         exception when Matching_Error =>

            --  Reset context and location
            Jump_Context (Context);
            Set_Token_Location (Invoke_Sloc);

            --  Find another overloaded pragma.
            Search_Next_Pragma (Pragma_Prefix & Pragma_Name, Pragma_Node);
            if Pragma_Node = Null_Subprogram then
               Fatal_Error := True;
               Write_Error_Message
                 (Invoke_Sloc, "invalid """, Pragma_Name, """ parameter list");
            end if;
         end;
      end loop;

      Fatal_Error := True;

      --  When successful, declare the procedure call node.

      Declare_Procedure_Call (Pragma_Node, Pragma_Sloc);
      T_Semicolon;
   end P_Pragma;

   -----------------------------
   -- P_Procedure_Declaration --
   -----------------------------

   procedure P_Procedure_Declaration
   is
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

      Take_Token ((Tok_Is, Tok_Semicolon, Tok_Dot));

      Search_Variable (Procedure_Name, Ada_Unit_Type_Node, Ada_Unit_Node);

      if Token = Tok_Dot then
         Write_Error_Message
           (Get_Token_Location,
            "main subprogram cannot be a child subprogram");
      end if;

      --  This procedure has to be declared when this statement is
      --  a declaration or when it has not been already declared.

      if Token = Tok_Semicolon or else Ada_Unit_Node = Null_Variable then
         Declare_Subprogram
           (Procedure_Name,
            Pragma_Unknown,
            True,
            Procedure_Sloc,
            Procedure_Node);
      end if;

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

         if Ada_Unit_Node = Null_Variable then
            Declare_Variable
              (Procedure_Name,
               Ada_Unit_Type_Node,
               Procedure_Sloc,
               Ada_Unit_Node);
         end if;

         Declare_Variable_Component
           (Variable_Node      => Partition_Node,
            Component_Name     => Attribute_Prefix & "main",
            Component_Type     => Ada_Unit_Type_Node,
            Attribute_Kind     => Attribute_Main,
            Component_Sloc     => Procedure_Sloc,
            Component_Node     => Component_Node);
         Set_Component_Value
           (Component_Node,
            Variable_Id (Ada_Unit_Node));

         Search_Variable (Str_To_Id ("true"), Constant_True);

         Declare_Variable_Component
           (Variable_Node      => Partition_Node,
            Component_Name     => Attribute_Prefix & "is boot partition",
            Component_Type     => Boolean_Type_Node,
            Attribute_Kind     => Attribute_Leader,
            Component_Sloc     => Procedure_Sloc,
            Component_Node     => Component_Node);
         Set_Component_Value
           (Component_Node,
            Variable_Id (Constant_True));

         T_Semicolon;
      end if;
   end P_Procedure_Declaration;

   -----------------------------
   -- P_Representation_Clause --
   -----------------------------

   procedure P_Representation_Clause
   is
      Direct_Name : Name_Id;
      Direct_Node : Node_Id;
      Direct_Type : Type_Id;
      Attr_Name   : Name_Id;
      Attr_Sloc   : Location_Type;
      Attr_Type   : Type_Id;
      Attr_Node   : Component_Id;
      Expr_Name   : Name_Id;
      Expr_Node   : Variable_Id;
      Expr_Type   : Type_Id;
      Expr_Sloc   : Location_Type;
      Is_A_Type   : Boolean;
      Context     : Context_Type;
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
        (Attribute_Prefix & Attr_Name, Direct_Type, Attr_Node);

      --  Check that this attribute is a legal attribute for the
      --  given type.

      if Attr_Node = Null_Component then
         Write_Error_Message
           (Attr_Sloc, "unrecognized attribute """, Attr_Name, """");
      end if;

      --  Attributes may be overloaded. If it is the case, then we
      --  will have to perform several attempts. In this case, an
      --  error is not a fatal error.

      declare
         Next_Node : Component_Id := Attr_Node;
      begin
         Search_Next_Component
           (Attribute_Prefix & Attr_Name,
            Next_Node);
         Fatal_Error := (Next_Node = Null_Component);
      end;

      T_Use;

      --  Save the context. Try to find a matching attribute (some
      --  attributes are overloaded). If the attempt fails, then reset
      --  the context and try another aattribute. If this attribute is
      --  not overloaded, then a failure is a fatal error and errors
      --  have to be printed.

      loop
         begin
            Save_Context (Configuration_Node, Context);
            Take_Token ((Tok_Identifier, Tok_String_Literal, Tok_Left_Paren));
            Expr_Name := Token_Name;
            Expr_Sloc := Get_Token_Location;
            Attr_Type := Get_Component_Type (Attr_Node);

            --  If string literal, declare an anonymous variable.

            if Token = Tok_String_Literal then
               Declare_Literal
                 (Expr_Name,
                  String_Type_Node,
                  Expr_Sloc,
                  Variable_Id (Expr_Node));

            --  If aggregate literal, declare an anonymous variable.

            elsif Token = Tok_Left_Paren then
               if not Is_Type_Composite (Attr_Type) then
                  Write_Type_Error (Expr_Sloc, Expr_Name);
               end if;

               Declare_Variable
                 (New_Variable_Name,
                  Attr_Type,
                  Expr_Sloc,
                  Variable_Id (Expr_Node));

               --  Reset the location to read the first left parenthesis.

               Set_Token_Location (Expr_Sloc);
               P_Aggregate_Assignment (Expr_Node);

            --  Otherwise, retrieve the declaration.

            else
               Search_Variable (Expr_Name, Expr_Node);
               if Expr_Node = Null_Variable then
                  Write_Declaration_Error (Expr_Sloc, Expr_Name);
               end if;
            end if;

            --  Check that the expression has the correct type
            Expr_Type := Get_Variable_Type (Expr_Node);

            --  Special case for functions and procedures
            if Expr_Type = Ada_Unit_Type_Node
              and then Is_Variable_Initialized (Expr_Node)
            then
               declare
                  S : Subprogram_Id;
                  P : Parameter_Id;
               begin
                  S := Subprogram_Id (Get_Variable_Value (Expr_Node));
                  if Is_Subprogram_A_Procedure (S) then

                     --  ??? Very ugly kludge
                     Expr_Type := Main_Procedure_Type_Node;

                  else
                     Search_Function_Returned_Parameter (S, P);
                     Expr_Type := Get_Parameter_Type (P);
                  end if;
               end;
            end if;

            --  Is this the expected type ?
            if Expr_Type /= Attr_Type then
               Write_Type_Error
                 (Get_Token_Location,
                  Get_Node_Name (Node_Id (Expr_Type)));
            end if;

            if Is_A_Type then
               --  When the attribute applies to a type, the attribute
               --  component does already exist.
               Set_Component_Value (Attr_Node, Expr_Node);

            else
               --  When the attribute applies to a variable, the
               --  attribute has to be created.
               Declare_Variable_Component
                 (Variable_Id (Direct_Node),
                  Attribute_Prefix & Attr_Name,
                  Attr_Type,
                  Get_Attribute_Kind (Attr_Node),
                  Attr_Sloc,
                  Attr_Node);
               Set_Component_Value (Attr_Node, Expr_Node);
            end if;
            exit;

         exception when Matching_Error =>

            --  Reset context and location
            Jump_Context (Context);
            Set_Token_Location (Expr_Sloc);


            --  Find another overloaded attribute.
            Search_Next_Component
              (Attribute_Prefix & Attr_Name,
               Attr_Node);
            if Attr_Node = Null_Component then
               Fatal_Error := True;
               Write_Error_Message
                 (Expr_Sloc, "expression type does not match """,
                  Attr_Name, """ attribute type");
            end if;
         end;
      end loop;

      Fatal_Error := True;
      T_Semicolon;
   end P_Representation_Clause;

   ---------------------------------
   -- P_Variable_List_Declaration --
   ---------------------------------

   procedure P_Variable_List_Declaration
     (Previous_Name : in Name_Id;
      Previous_Sloc : in Location_Type)
   is
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

         --  Declare a temporary variable of any type
         --  ??? Should not use partition_type

         Declare_Variable
           (Previous_Name,
            Partition_Type_Node,
            Previous_Sloc,
            Previous_Node);

         --  Call recursively P_Variable_List_Declaration until the
         --  end of list. Variable_Node is a node for the next
         --  declared variable.

         P_Variable_List_Declaration (Variable_Name, Variable_Sloc);

         --  Variables can now be fully described.

         Search_Variable (Variable_Name, Variable_Node);
         Set_Variable_Type (Previous_Node, Get_Variable_Type (Variable_Node));

         --  If previous variable has been initialized, initialize
         --  this newly declared variable with the same value.

         Duplicate_Variable (Variable_Node, Previous_Node);

      else

         --  The following identifier is a type.

         T_Identifier;
         Var_Type_Name := Token_Name;
         Var_Type_Sloc := Get_Token_Location;

         --  Has this type been declared ?

         Search_Type (Var_Type_Name, Var_Type_Kind, Var_Type_Node);

         if Var_Type_Node = Null_Type then
            Write_Type_Error (Var_Type_Sloc, Var_Type_Name);
         end if;

         --  Declare this new variable of type Var_Type_Node.

         Declare_Variable
           (Previous_Name, Var_Type_Node, Previous_Sloc, Previous_Node);

         Take_Token ((Tok_Semicolon, Tok_Colon_Equal));

         --  Is there an initialization ?

         if Token = Tok_Colon_Equal then
            P_Aggregate_Assignment (Previous_Node);
            T_Semicolon;
         end if;

      end if;
   end P_Variable_List_Declaration;

   -----------
   -- Parse --
   -----------

   procedure Parse is
   begin
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
   is
      Node : Node_Id;
      X, Y : Int;
      C    : Character;
   begin
      if not Debug_Mode then
         return;
      end if;
      Write_Eol;
      Write_Str ("configuration");
      Write_Eol;
      Write_Str ("=============");
      Write_Eol;
      Write_Eol;
      First_Configuration_Declaration (Configuration_Node, Node);
      while Node /= Null_Node loop
         if Is_Variable (Node) then
            C := 'V';
            Write_Str ("variable <");
         elsif Is_Type (Node) then
            C := 'T';
            Write_Str ("type <");
         elsif Is_Subprogram (Node) then
            C := 'S';
            Write_Str ("subprogram <");
         elsif Is_Statement (Node) then
            C := 'I';
            Write_Str ("invoke <");
         elsif Is_Configuration (Node) then
            C := 'C';
            Write_Str ("configuration <");
         end if;
         Write_Name (Get_Node_Name (Node));
         Write_Str  ("> (");
         Write_Int  (Int (Node));
         Write_Str  (" at ");
         Get_Node_SLOC (Node, X, Y);
         Write_Int  (X);
         Write_Str  (":");
         Write_Int  (Y);
         Write_Str  (")");
         Write_Eol;
         case C is
            when 'V' =>
               Print_Variable (Variable_Id (Node), 1);
            when 'T' =>
               Print_Type (Type_Id (Node), 1);
            when 'S' =>
               Print_Subprogram (Subprogram_Id (Node), 1);
            when 'I' =>
               Print_Statement (Statement_Id (Node), 1);
            when others =>
               null;
         end case;
         Write_Eol;
         Next_Configuration_Declaration (Node);
      end loop;
      Write_Eol;
      Write_Str ("=============");
      Write_Eol;
   end Print;

   ---------------------
   -- Print_Component --
   ---------------------

   procedure Print_Component
     (Node : in Component_Id;
      Many : in Int)
   is
      T : Type_Id;
      N : Variable_Id;
   begin
      T := Get_Component_Type (Node);
      Write_Indent (Many, "");
      Write_Name (Get_Component_Name (Node));
      Write_Str  (" : ");
      Write_Name (Get_Node_Name (Node_Id (T)));
      if Is_Component_Initialized (Node) then
         N := Get_Component_Value (Node);
         Write_Str  (" := ");
         Write_Name (Get_Variable_Name (N));
      end if;
      Write_Eol;
   end Print_Component;

   ---------------------
   -- Print_Parameter --
   ---------------------

   procedure Print_Parameter
     (Node : in Parameter_Id;
      Many : in Int)
   is
      T : Type_Id;
      V : Variable_Id;
   begin
      T := Get_Parameter_Type (Node);
      Write_Indent (Many, "");
      Write_Name (Get_Variable_Name (Variable_Id (Node)));
      Write_Str  (" : ");
      Write_Name (Get_Node_Name (Node_Id (T)));
      if Is_Variable_Initialized (Variable_Id (Node)) then
         V := Get_Parameter_Value (Node);
         Write_Str  (" := ");
         Write_Name (Get_Variable_Name (V));
      end if;
      Write_Eol;
   end Print_Parameter;

   ---------------------
   -- Print_Statement --
   ---------------------

   procedure Print_Statement
     (Node : in Statement_Id;
      Many : in Int)
   is
      S : Subprogram_Id;
   begin
      S := Get_Subprogram_Call (Node);
      Write_Indent (Many, "");
      Write_Name (Get_Node_Name (Node_Id (S)));
      Write_Eol;
      Print_Subprogram (S, Many);
   end Print_Statement;

   ----------------------
   -- Print_Subprogram --
   ----------------------

   procedure Print_Subprogram
     (Node : in Subprogram_Id;
      Many : in Int)
   is
      P : Parameter_Id;
   begin
      First_Subprogram_Parameter (Node, P);
      while P /= Null_Parameter loop
         Print_Parameter (P, Many + 1);
         Next_Subprogram_Parameter (P);
      end loop;
   end Print_Subprogram;

   ----------------
   -- Print_Type --
   ----------------

   procedure Print_Type
     (Node : in Type_Id;
      Many : in Int)
   is
      C : Component_Id;
      S : Int;
      T : Type_Id;
   begin
      if not Is_Type_Composite (Node) then
         return;
      end if;
      S := Get_Array_Length (Node);
      if S /= 0 then
         Write_Indent (Many, "array (");
         if S = Infinite then
            Write_Str ("<>");
         else
            Write_Str ("0 .. ");
            Write_Int (S - 1);
         end if;
         T := Get_Array_Component_Type (Node);
         Write_Str    (") of ");
         Write_Name   (Get_Node_Name (Node_Id (T)));
         Write_Eol;
      else
         First_Type_Component (Node, C);
         while C /= Null_Component loop
            if Get_Attribute_Kind (C) = Attribute_Unknown then
               Print_Component (C, Many + 1);
            end if;
            Next_Type_Component (C);
         end loop;
      end if;
      First_Type_Component (Node, C);
      while C /= Null_Component loop
         if Get_Attribute_Kind (C) /= Attribute_Unknown then
            Print_Component (C, Many + 1);
         end if;
         Next_Type_Component (C);
      end loop;
   end Print_Type;

   --------------------
   -- Print_Variable --
   --------------------

   procedure Print_Variable
     (Node : in Variable_Id;
      Many : in Int)
   is
      T : Type_Id;
      S : Int;
      C : Component_Id;
   begin
      T := Get_Variable_Type (Node);
      Write_Indent (Many, " : ");
      Write_Name (Get_Node_Name (Node_Id (T)));
      if not Is_Type_Composite (T) then
         if Is_Variable_Initialized (Node) then
            Write_Str (" := ");
            if T = String_Type_Node then
               Write_Name (Get_Variable_Name (Node));
            else
               Write_Int  (Get_Scalar_Value (Node));
            end if;
         end if;
         Write_Eol;
      else
         S := Get_Array_Length (T);
         if S > 0 then
            Write_Str  (" (0 .. ");
            Write_Int  (Get_Array_Length (Node) - 1);
            Write_Str  (") of ");
            T := Get_Array_Component_Type (T);
            Write_Name (Get_Node_Name (Node_Id (T)));
         end if;
         Write_Eol;
         First_Variable_Component (Node, C);
         while C /= Null_Component loop
            if Get_Attribute_Kind (C) = Attribute_Unknown then
               Print_Component (C, Many + 1);
            end if;
            Next_Type_Component (C);
         end loop;
         First_Variable_Component (Node, C);
         while C /= Null_Component loop
            if Get_Attribute_Kind (C) /= Attribute_Unknown then
               Print_Component (C, Many + 1);
            end if;
            Next_Type_Component (C);
         end loop;
      end if;
   end Print_Variable;

   -----------------------------
   -- Search_Actual_Parameter --
   -----------------------------

   procedure Search_Actual_Parameter
     (Actual_Name : in  Name_Id;
      Actual_Type : in  Type_Id;
      Actual_Node : out Variable_Id)
   is
      Actual : Node_Id;
   begin

      --  Scan the configuration to find variable Actual_Name.

      First_Configuration_Declaration (Configuration_Node, Actual);
      while Actual /= Null_Node loop
         if Is_Variable       (Actual)
           and then Get_Node_Name     (Actual) = Actual_Name
           and then Get_Variable_Type (Variable_Id (Actual)) = Actual_Type
         then
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
      Component_Node : out Component_Id)
   is
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
      Component_Node : out Component_Id)
   is
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
      Declaration_Node : out Node_Id)
   is
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
      Parameter_Node : out Parameter_Id)
   is
      Prev, Next : Parameter_Id;
   begin
      pragma Assert (not Is_Subprogram_A_Procedure (Function_Node));

      --  As it is a function, get the last parameter.
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
         Formal_Type := Get_Parameter_Type (Parameter_Node);
         case Convention is

            --  If Positional, find the first uninitialized parameter.
            when Positional =>
               if not Is_Parameter_Initialized (Parameter_Node) then
                  Formal_Name := Get_Node_Name (Node_Id (Parameter_Node));
                  return;
               end if;

            --  If Named, use Formal_Name to return format parameter node.
            when Named =>
               if Get_Node_Name
                 (Node_Id (Parameter_Node)) = Formal_Name then
                  return;
               end if;

         end case;
         Next_Subprogram_Parameter (Parameter_Node);
      end loop;
      Write_Error_Message (Get_Token_Location, "no matching parameter");
   end Search_Matching_Parameter;

   ---------------------------
   -- Search_Next_Component --
   ---------------------------

   procedure Search_Next_Component
     (Component_Name : in     Name_Id;
      Component_Node : in out Component_Id) is
   begin
      Next_Type_Component (Component_Node);
      while Component_Node /= Null_Component
        and then Get_Node_Name (Node_Id (Component_Node)) /= Component_Name
      loop
         Next_Type_Component (Component_Node);
      end loop;
   end Search_Next_Component;

   -----------------------------
   -- Search_Next_Declaration --
   -----------------------------

   procedure Search_Next_Declaration
     (Declaration_Name : in Name_Id;
      Declaration_Node : in out Node_Id)
   is
      Node : Node_Id;
      Name : Name_Id;
   begin
      Node := Node_Id (Declaration_Node);
      Next_Configuration_Declaration (Node);
      while Node /= Null_Node loop
         Name := Get_Node_Name (Node);
         exit when Name = Declaration_Name;
         Next_Configuration_Declaration (Node);
      end loop;
      Declaration_Node := Node;
   end Search_Next_Declaration;

   ------------------------
   -- Search_Next_Pragma --
   ------------------------

   procedure Search_Next_Pragma
     (Pragma_Name : in     Name_Id;
      Pragma_Node : in out Subprogram_Id) is
   begin
      Search_Next_Subprogram (Pragma_Name, Pragma_Node);
   end Search_Next_Pragma;

   ----------------------------
   -- Search_Next_Subprogram --
   ----------------------------

   procedure Search_Next_Subprogram
     (Subprogram_Name : in     Name_Id;
      Subprogram_Node : in out Subprogram_Id)
   is
      Node : Node_Id := Node_Id (Subprogram_Node);
   begin
      Search_Next_Declaration (Subprogram_Name, Node);
      while Node /= Null_Node
        and then not Is_Subprogram (Node)
      loop
         Search_Next_Declaration (Subprogram_Name, Node);
      end loop;
      Subprogram_Node := Subprogram_Id (Node);
   end Search_Next_Subprogram;

   -------------------
   -- Search_Pragma --
   -------------------

   procedure Search_Pragma
     (Pragma_Name : in  Name_Id;
      Pragma_Kind : out Pragma_Type;
      Pragma_Node : out Subprogram_Id)
   is
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
      Subprogram_Node : out Subprogram_Id)
   is
      Node : Node_Id;
   begin
      Search_Declaration (Subprogram_Name, Node);
      if Node /= Null_Node
        and then not Is_Subprogram (Node)
      then
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
      Type_Node : out Type_Id)
   is
      Node : Node_Id;
   begin
      Search_Declaration (Type_Name, Node);
      if Node /= Null_Node
        and then not Is_Type (Node)
      then
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
      Component_Node : out Component_Id)
   is
      C : Component_Id;
      T : Type_Id;
   begin

      --  If Component_Type is Null_Type, find the first uninitialized
      --  component, otherwise, try to match also the type.

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
      Variable_Node : out Variable_Id)
   is
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
      Variable_Node : out Variable_Id)
   is
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
      Location : in Location_Type)
   is
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
         if Fatal_Error then
            Write_Location (Get_Token_Location);
            Write_Token (T);
            Write_Str (" was expected");
            Write_Eol;
         end if;
         Exit_On_Error;
      end if;
   end Take_Token;

   ----------------
   -- Take_Token --
   ----------------

   procedure Take_Token (L : Token_List_Type) is
   begin
      Next_Token;
      for Index in L'Range loop
         if L (Index) = Token then
            return;
         end if;
      end loop;
      if Fatal_Error then
         Write_Location (Get_Token_Location);
         Write_Token (L (L'First));
         for Index in L'First + 1 .. L'Last loop
            Write_Str (" or ");
            Write_Token (L (Index));
         end loop;
         Write_Str (" was expected");
         Write_Eol;
      end if;
      Exit_On_Error;
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
      if Fatal_Error then
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
      end if;
      Exit_On_Error;
   end Write_Error_Message;

   ------------------
   -- Write_Indent --
   ------------------

   procedure Write_Indent
     (Many : in Int := 1;
      Mesg : in String := "") is
   begin
      for I in 1 .. Many loop
         Write_Str (Indent);
      end loop;
      Write_Str (Mesg);
   end Write_Indent;

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

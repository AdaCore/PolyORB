------------------------------------------------------------------------------
--                                                                          --
--                          ADABROKER COMPONENTS                            --
--                                                                          --
--               A D A _ B E . I D L 2 A D A . H E L P E R                  --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1999-2000 ENST Paris University, France.          --
--                                                                          --
-- AdaBroker is free software; you  can  redistribute  it and/or modify it  --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. AdaBroker  is distributed  in the hope that it will be  useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with AdaBroker; see file COPYING. If  --
-- not, write to the Free Software Foundation, 59 Temple Place - Suite 330, --
-- Boston, MA 02111-1307, USA.                                              --
--                                                                          --
--             AdaBroker is maintained by ENST Paris University.            --
--                     (email: broker@inf.enst.fr)                          --
--                                                                          --
------------------------------------------------------------------------------

with Idl_Fe.Types;          use Idl_Fe.Types;
with Idl_Fe.Tree;           use Idl_Fe.Tree;
with Idl_Fe.Tree.Synthetic; use Idl_Fe.Tree.Synthetic;

with Ada_Be.Identifiers;    use Ada_Be.Identifiers;
with Ada_Be.Temporaries;    use Ada_Be.Temporaries;
with Ada_Be.Debug;

with Utils;                 use Utils;
with Errors;                use Errors;

package body Ada_Be.Idl2Ada.Helper is

   Flag : constant Natural := Ada_Be.Debug.Is_Active ("ada_be.idl2ada.helper");
   procedure O is new Ada_Be.Debug.Output (Flag);

   ----------------------------------------
   -- Specialised generation subprograms --
   ----------------------------------------

   procedure Gen_From_Any_Profile
     (CU        : in out Compilation_Unit;
      Type_Node : in     Node_Id);
   --  Generate the profile for the From_Any function of a type

   procedure Gen_To_Any_Profile
     (CU        : in out Compilation_Unit;
      Type_Node : in     Node_Id);
   --  Generate the profile for the To_Any function of a type

   procedure Gen_Array_TC
     (CU        : in out Compilation_Unit;
      Node      : in     Node_Id;
      Decl_Node : in     Node_Id);
   --  generate lines to fill in an array typecode
   --  only used in the type_declarator part of gen_node_body

   function Ada_TC_Name (Node : Node_Id)
                         return String;
   --  The name of the typecode corresponding to an Ada type

   ----------------------------------------------
   -- End of internal subprograms declarations --
   ----------------------------------------------

   ---------------------
   --  Gen_Scope_Spec --
   ---------------------
   procedure Gen_Node_Spec
     (CU   : in out Compilation_Unit;
      Node : Node_Id) is
   begin
      case Kind (Node) is

         when K_Interface =>
            --  Unchecked_To_<reference>
            declare
               Short_Type_Name : constant String
                 := Ada_Type_Defining_Name (Node);
               Type_Name : constant String
                 := Ada_Type_Name (Node);
            begin
               Add_With (CU, "CORBA.Object");
               NL (CU);
               PL (CU, "function Unchecked_To_" & Short_Type_Name);
               PL (CU, "  (The_Ref : in CORBA.Object.Ref'Class)");
               PL (CU, "  return " & Type_Name & ";");
               PL (CU, "function To_" & Short_Type_Name);
               PL (CU, "  (The_Ref : in CORBA.Object.Ref'Class)");
               PL (CU, "  return " & Type_Name & ";");
            end;

            --  TypeCode
            NL (CU);
            PL (CU, Ada_TC_Name (Node)
                & " : CORBA.TypeCode.Object := ");
            II (CU);
            PL (CU, "CORBA.TypeCode.TC_ObjRef;");
            DI (CU);

            --  From_Any
            NL (CU);
            Gen_From_Any_Profile (CU, Node);
            PL (CU, ";");

            --  To_Any
            NL (CU);
            Gen_To_Any_Profile (CU, Node);
            PL (CU, ";");

            --  to fill in the typecode TC_<name of the type>
            Add_Elaborate_Body (CU);
            Add_With (CU, "CORBA");

         when K_Enum =>
            --  TypeCode
            NL (CU);
            PL (CU, Ada_TC_Name (Node)
                & " : CORBA.TypeCode.Object := ");
            II (CU);
            PL (CU, "CORBA.TypeCode.TC_Enum;");
            DI (CU);

            --  From_Any
            NL (CU);
            Gen_From_Any_Profile (CU, Node);
            PL (CU, ";");

            --  To_Any
            NL (CU);
            Gen_To_Any_Profile (CU, Node);
            PL (CU, ";");

            --  to fill in the typecode TC_<name of the type>
            Add_Elaborate_Body (CU);
            Add_With (CU, "CORBA");

         when K_Type_Declarator =>
            declare
               It   : Node_Iterator;
               Decl_Node : Node_Id;
            begin
               Init (It, Declarators (Node));
               --  get the first node of the list
               Get_Next_Node (It, Decl_Node);
               --  if it's an array, its the only one. If not, there's
               --  no array declaration in the list.
               if Is_Empty (Array_Bounds (Decl_Node)) then
                  return;
               end if;

               --  TypeCode
               NL (CU);
               pragma Debug (O ("gen_node_spec : about to call Ada_TC_Name"));
               PL (CU, Ada_TC_Name (Decl_Node)
                   & " : CORBA.TypeCode.Object := ");
               pragma Debug (O ("gen_node_spec : Ada_TC_Name "
                                & "successfully called"));
               II (CU);
               PL (CU, "CORBA.TypeCode.TC_Array;");
               DI (CU);

               --  From_Any
               NL (CU);
               Gen_From_Any_Profile (CU, Decl_Node);
               PL (CU, ";");

               --  To_Any
               NL (CU);
               Gen_To_Any_Profile (CU, Decl_Node);
               PL (CU, ";");

               --  to fill in the typecode TC_<name of the type>
               Add_Elaborate_Body (CU);
               Add_With (CU, "CORBA");
            end;

         when K_Struct =>
            --  typecode generation
            NL (CU);
            PL (CU, Ada_TC_Name (Node)
                & " : CORBA.TypeCode.Object := ");
            II (CU);
            PL (CU, "CORBA.TypeCode.TC_Struct;");
            DI (CU);

            --  from_any function
            NL (CU);
            Gen_From_Any_Profile (CU, Node);
            PL (CU, ";");

            --  to_any function
            NL (CU);
            Gen_To_Any_Profile (CU, Node);
            PL (CU, ";");

            --  to fill in the typecode TC_<name of the type>
            Add_Elaborate_Body (CU);
            Add_With (CU, "CORBA");

         when others =>
            null;

      end case;
   end Gen_Node_Spec;

   ---------------------
   --  Gen_Scope_Body --
   ---------------------
   procedure Gen_Node_Body
     (CU   : in out Compilation_Unit;
      Node : Node_Id) is
   begin
      case Kind (Node) is

         when K_Interface =>
            --  Unchecked_To_<reference>
            declare
               Short_Type_Name : constant String
                 := Ada_Type_Defining_Name (Node);
               Type_Name : constant String
                 := Ada_Type_Name (Node);
            begin
               Add_With (CU, "Broca.Refs");
               Add_With (CU, "Broca.Exceptions");

               NL (CU);
               PL (CU, "function Unchecked_To_" & Short_Type_Name);
               PL (CU, "  (The_Ref : in CORBA.Object.Ref'Class)");
               PL (CU, "  return " & Type_Name);
               PL (CU, "is");
               II (CU);
               PL (CU, "Result : " & Type_Name & ";");
               DI (CU);
               PL (CU, "begin");
               II (CU);
               PL (CU, "Set (Result,");
               PL (CU,
                   "     CORBA.Object.Get (The_Ref));");
               PL (CU, "return Result;");
               DI (CU);
               PL (CU, "end Unchecked_To_" & Short_Type_Name & ";");

               --  To_<reference>
               NL (CU);
               PL (CU, "function To_" & Short_Type_Name);
               PL (CU, "  (The_Ref : in CORBA.Object.Ref'Class)");
               PL (CU, "  return " & Type_Name);
               PL (CU, "is");
               II (CU);
               PL (CU, "Result : " & Type_Name & ";");
               DI (CU);
               PL (CU, "begin");
               II (CU);
               PL (CU,
                   "Result := Unchecked_To_"
                   & Short_Type_Name
                   & " (The_Ref);");
               PL (CU, "if Is_A (Result, "
                   & T_Repository_Id & ") then");
               II (CU);
               PL (CU, "return Result;");
               DI (CU);
               PL (CU, "else");
               II (CU);
               PL (CU, "Broca.Exceptions.Raise_Bad_Param;");
               DI (CU);
               PL (CU, "end if;");
               DI (CU);
               PL (CU, "end To_" & Short_Type_Name & ";");
            end;

            --  From_Any
            Add_With (CU, "CORBA.Object.Helper");
            NL (CU);
            Gen_From_Any_Profile (CU, Node);
            PL (CU, " is");
            PL (CU, "begin");
            II (CU);
            PL (CU, "return To_"
                & Ada_Type_Defining_Name (Node)
                & " (CORBA.Object.Helper."
                & "From_Any (Item));");
            DI (CU);
            PL (CU, "end From_Any;");

            --  To_Any
            Add_With (CU, "CORBA.Object.Helper");
            NL (CU);
            Gen_To_Any_Profile (CU, Node);
            PL (CU, " is");
            PL (CU, "begin");
            II (CU);
            PL (CU, "return CORBA.Object.Helper.To_Any "
                & "(CORBA.Object.Ref (Item));");
            DI (CU);
            PL (CU, "end To_Any;");

            --  to fill in the typecode TC_<name of the type>
            Divert (CU, Elaboration);
            NL (CU);
            PL (CU, "declare");
            II (CU);
            PL (CU, "Name : CORBA.String := CORBA.To_CORBA_String ("""
                & Ada_Name (Node)
                & """);");
            PL (CU, "Id : CORBA.String := CORBA.To_CORBA_String ("""
                & Idl_Repository_Id (Node)
                & """);");
            DI (CU);
            PL (CU, "begin");
            II (CU);
            PL (CU, "CORBA.TypeCode.Add_Parameter ("
                & Ada_TC_Name (Node)
                & ", CORBA.To_Any (Name));");
            PL (CU, "CORBA.TypeCode.Add_Parameter ("
                & Ada_TC_Name (Node)
                & ", CORBA.To_Any (Id));");
            DI (CU);
            PL (CU, "end;");
            Divert (CU, Visible_Declarations);

         when K_Enum =>
            --  From_Any
            NL (CU);
            Gen_From_Any_Profile (CU, Node);
            PL (CU, " is");
            II (CU);
            PL (CU, "Index : CORBA.Any :=");
            II (CU);
            PL (CU, "CORBA.Get_Aggregate_Element (Item,");
            PL (CU, "                             "
                & "CORBA.TC_Unsigned_Long,");
            PL (CU, "                             "
                & "CORBA.Unsigned_Long (0));");
            DI (CU);
            PL (CU, "Position : CORBA.Unsigned_Long "
                & ":= CORBA.From_Any (Index);");
            DI (CU);
            PL (CU, "begin");
            II (CU);
            PL (CU, "return "
                & Ada_Name (Node)
                & "'Val (Position);");
            DI (CU);
            PL (CU, "end From_Any;");

            --  To_Any
            NL (CU);
            Gen_To_Any_Profile (CU, Node);
            PL (CU, " is");
            II (CU);
            PL (CU, "Result : CORBA.Any := ");
            II (CU);
            PL (CU, "CORBA.Get_Empty_Any_Aggregate ("
                & Ada_TC_Name (Node)
                & ");");
            DI (CU);
            DI (CU);
            PL (CU, "begin");
            II (CU);
            PL (CU, "CORBA.Add_Aggregate_Element");
            II (CU);
            PL (CU, "(Result,");
            PL (CU, " CORBA.To_Any (CORBA.Unsigned_Long ("
                & Ada_Name (Node)
                & "'Pos (Item))));");
            DI (CU);
            PL (CU, "return Result;");
            DI (CU);
            PL (CU, "end To_Any;");

            --  to fill in the typecode TC_<name of the type>
            Divert (CU, Elaboration);
            PL (CU, "declare");
            II (CU);
            PL (CU, "Name : CORBA.String := CORBA.To_CORBA_String ("""
                & Ada_Name (Node)
                & """);");
            PL (CU, "Id : CORBA.String := CORBA.To_CORBA_String ("""
                & Idl_Repository_Id (Node)
                & """);");
            DI (CU);
            PL (CU, "begin");
            II (CU);
            PL (CU, "CORBA.TypeCode.Add_Parameter ("
                & Ada_TC_Name (Node)
                & ", CORBA.To_Any (Name));");
            PL (CU, "CORBA.TypeCode.Add_Parameter ("
                & Ada_TC_Name (Node)
                & ", CORBA.To_Any (Id));");
            declare
               It   : Node_Iterator;
               E_Node : Node_Id;
            begin
               Init (It, Enumerators (Node));
               while not Is_End (It) loop
                  Get_Next_Node (It, E_Node);
                  PL (CU, "CORBA.TypeCode.Add_Parameter ("
                      & Ada_TC_Name (Node)
                      & ", To_Any ("
                      & Ada_Name (E_Node)
                      & "));");
               end loop;
            end;
            DI (CU);
            PL (CU, "end;");
            Divert (CU, Visible_Declarations);

         when K_Type_Declarator =>
            declare
               It   : Node_Iterator;
               Decl_Node : Node_Id;
            begin
               Add_With (CU, "CORBA", Use_It => True);
               Init (It, Declarators (Node));
               --  get the first node of the list
               Get_Next_Node (It, Decl_Node);
               --  if it's an array, its the only one. If not, there's
               --  no array declaration in the list.
               if Is_Empty (Array_Bounds (Decl_Node)) then
                  return;
               end if;
               pragma Assert (Is_End (It));

               --  From_Any
               NL (CU);
               Gen_From_Any_Profile (CU, Decl_Node);
               PL (CU, " is");
               II (CU);
               PL (CU, "Result : "
                   & Ada_Type_Name (Decl_Node)
                   & ";");
               DI (CU);
               PL (CU, "begin");
               II (CU);
               declare
                  Bounds_It : Node_Iterator;
                  Bound_Node : Node_Id;
                  Number : Natural := 0;
               begin
                  Init (Bounds_It, Array_Bounds (Decl_Node));
                  while not Is_End (Bounds_It) loop
                     Get_Next_Node (Bounds_It, Bound_Node);
                     Put (CU, "for I"
                         & Img (Number)
                         & " in 0 .. ");
                     Gen_Node_Stubs_Spec (CU, Bound_Node);
                     PL (CU, " - 1 loop");
                     Number := Number + 1;
                     II (CU);
                  end loop;
                  Put (CU, "Result (I0");
                  for I in 1 .. Number - 1 loop
                     Put (CU, ", I" & Img (I));
                  end loop;
                  PL (CU, ") := From_Any");
                  II (CU);
                  PL (CU, "(CORBA.Get_Aggregate_Element (Item,");
                  PL (CU, "                              "
                      & Ada_TC_Name (T_Type (Node))
                      & ",");
                  Put (CU, "                             "
                       & " CORBA.Unsigned_Long (");
                  declare
                     Index : Natural := 0;
                     First_Bound : Boolean := True;
                  begin
                     Init (Bounds_It, Array_Bounds (Decl_Node));
                     while not Is_End (Bounds_It) loop
                        Get_Next_Node (Bounds_It, Bound_Node);
                        if First_Bound then
                           First_Bound := False;
                        else
                           Put (CU, " + ");
                        end if;
                        Put (CU, "I" & Img (Index));
                        for J in Index + 1 .. Number - 1 loop
                           Put (CU, " * ");
                           Gen_Node_Stubs_Spec (CU, Bound_Node);
                        end loop;
                        Index := Index + 1;
                     end loop;
                  end;
                  PL (CU, ")));");
                  DI (CU);
                  for I in 1 .. Number loop
                     DI (CU);
                     PL (CU, "end loop;");
                  end loop;
               end;
               PL (CU, "return Result;");
               DI (CU);
               PL (CU, "end From_Any;");

               --  To_Any
               NL (CU);
               Gen_To_Any_Profile (CU, Decl_Node);
               PL (CU, " is");
               II (CU);
               PL (CU, "Result : CORBA.Any := ");
               II (CU);
               PL (CU, "CORBA.Get_Empty_Any_Aggregate ("
                   & Ada_TC_Name (Decl_Node)
                   & ");");
               DI (CU);
               DI (CU);
               PL (CU, "begin");
               II (CU);
               declare
                  Bounds_It : Node_Iterator;
                  Bound_Node : Node_Id;
                  Number : Natural := 0;
               begin
                  Init (Bounds_It, Array_Bounds (Decl_Node));
                  while not Is_End (Bounds_It) loop
                     Get_Next_Node (Bounds_It, Bound_Node);
                     Put (CU, "for I"
                         & Img (Number)
                         & " in 0 .. ");
                     Gen_Node_Stubs_Spec (CU, Bound_Node);
                     PL (CU, " - 1 loop");
                     Number := Number + 1;
                     II (CU);
                  end loop;
                  PL (CU, "CORBA.Add_Aggregate_Element (Result,");
                  Put (CU, "                             "
                      & "To_Any (Item (I0");
                  for I in 1 .. Number - 1 loop
                     Put (CU, ", I" & Img (I));
                  end loop;
                  PL (CU, ")));");
                  for I in 1 .. Number loop
                     DI (CU);
                     PL (CU, "end loop;");
                  end loop;
               end;
               PL (CU, "return Result;");
               DI (CU);
               PL (CU, "end To_Any;");

               --  to fill in the typecode TC_<name of the type>
               Divert (CU, Elaboration);
               NL (CU);
               if Length (Array_Bounds (Decl_Node)) > 1 then
                  PL (CU, "declare");
                  II (CU);
                  for I in 1 .. Length (Array_Bounds (Decl_Node)) - 1 loop
                     PL (CU, "TC_"
                         & Img (I)
                         & " : CORBA.TypeCode.Object := "
                         & "CORBA.TypeCode.TC_Array;");
                  end loop;
                  DI (CU);
                  PL (CU, "begin");
                  II (CU);
               end if;
               Gen_Array_TC (CU, Node, Decl_Node);
               if Length (Array_Bounds (Decl_Node)) > 1 then
                  DI (CU);
                  PL (CU, "end;");
               end if;
               Divert (CU, Visible_Declarations);
            end;

         when K_Struct =>
            --  from_any function
            Add_With (CU, "CORBA", Use_It => True);
            NL (CU);
            Gen_From_Any_Profile (CU, Node);
            PL (CU, " is");
            II (CU);
            PL (CU, "Index : CORBA.Any;");
            declare
               It   : Node_Iterator;
               Member_Node : Node_Id;
            begin
               Init (It, Members (Node));
               while not Is_End (It) loop
                  Get_Next_Node (It, Member_Node);
                  declare
                     It2   : Node_Iterator;
                     Decl_Node : Node_Id;
                  begin
                     Init (It2, Decl (Member_Node));
                     while not Is_End (It2) loop
                        Get_Next_Node (It2, Decl_Node);
                        PL (CU, "Result_"
                            & Ada_Name (Decl_Node)
                            & " : "
                            & Ada_Type_Name (M_Type (Member_Node))
                            & ";");
                     end loop;
                  end;
               end loop;
            end;
            DI (CU);
            PL (CU, "begin");
            II (CU);
            declare
               It   : Node_Iterator;
               Member_Node : Node_Id;
               I : Integer := 0;
            begin
               Init (It, Members (Node));
               while not Is_End (It) loop
                  Get_Next_Node (It, Member_Node);
                  declare
                     It2   : Node_Iterator;
                     Decl_Node : Node_Id;
                  begin
                     Init (It2, Decl (Member_Node));
                     while not Is_End (It2) loop
                        Get_Next_Node (It2, Decl_Node);
                        PL (CU,
                            "Index := CORBA.Get_Aggregate_Element (Item,");
                        PL (CU,
                            "                                      "
                            & Ada_TC_Name (M_Type (Member_Node))
                            & ",");
                        PL (CU,
                            "                                      "
                            & "CORBA.Unsigned_Long ("
                            & Integer'Image (I)
                            &"));");
                        PL (CU, "Result_"
                            & Ada_Name (Decl_Node)
                            & " := From_Any (Index);");
                        I := I + 1;
                     end loop;
                  end;
               end loop;
            end;
            PL (CU, "return");
            II (CU);
            declare
               First_Member : Boolean := True;
               Begin_Of_Line : String (1 .. 1) := "(";
               End_Of_Line : String (1 .. 2) := ", ";
               It   : Node_Iterator;
               Member_Node : Node_Id;
            begin
               Init (It, Members (Node));
               while not Is_End (It) loop
                  Get_Next_Node (It, Member_Node);
                  declare
                     It2   : Node_Iterator;
                     Decl_Node : Node_Id;
                  begin
                     Init (It2, Decl (Member_Node));
                     while not Is_End (It2) loop
                        Get_Next_Node (It2, Decl_Node);
                        if Is_End (It) and Is_End (It2) then
                           End_Of_Line := ");";
                        end if;
                        PL (CU, Begin_Of_Line
                            & Ada_Name (Decl_Node)
                            & " => Result_"
                            & Ada_Name (Decl_Node)
                            & End_Of_Line);
                        if First_Member then
                           First_Member := False;
                           Begin_Of_Line := " ";
                        end if;
                     end loop;
                  end;
               end loop;
            end;
            DI (CU);
            DI (CU);
            PL (CU, "end From_Any;");

            --  to_any function
            NL (CU);
            Gen_To_Any_Profile (CU, Node);
            PL (CU, " is");
            II (CU);
            PL (CU, "Result : CORBA.Any := ");
            II (CU);
            PL (CU, "CORBA.Get_Empty_Any_Aggregate ("
                & Ada_TC_Name (Node)
                & ");");
            DI (CU);
            DI (CU);
            PL (CU, "begin");
            II (CU);
            declare
               It   : Node_Iterator;
               Member_Node : Node_Id;
            begin
               Init (It, Members (Node));
               while not Is_End (It) loop
                  Get_Next_Node (It, Member_Node);
                  declare
                     It2   : Node_Iterator;
                     Decl_Node : Node_Id;
                  begin
                     Init (It2, Decl (Member_Node));
                     while not Is_End (It2) loop
                        Get_Next_Node (It2, Decl_Node);
                        PL (CU, "CORBA.Add_Aggregate_Element");
                        II (CU);
                        PL (CU, "(Result, To_Any (Item."
                            & Ada_Name (Decl_Node)
                            & "));");
                        DI (CU);
                     end loop;
                  end;
               end loop;
            end;
            PL (CU, "return Result;");
            DI (CU);
            PL (CU, "end To_Any;");

            --  to fill in the typecode TC_<name of the type>
            Divert (CU, Elaboration);
            NL (CU);
            PL (CU, "declare");
            II (CU);
            PL (CU, "Name : CORBA.String := CORBA.To_CORBA_String ("""
                & Ada_Name (Node)
                & """);");
            PL (CU, "Id : CORBA.String := CORBA.To_CORBA_String ("""
                & Idl_Repository_Id (Node)
                & """);");
            declare
               It   : Node_Iterator;
               Member_Node : Node_Id;
            begin
               Init (It, Members (Node));
               while not Is_End (It) loop
                  Get_Next_Node (It, Member_Node);
                  declare
                     It2   : Node_Iterator;
                     Decl_Node : Node_Id;
                  begin
                     Init (It2, Decl (Member_Node));
                     while not Is_End (It2) loop
                        Get_Next_Node (It2, Decl_Node);
                        PL (CU, "Arg_Name_"
                            & Ada_Name (Decl_Node)
                            & " : CORBA.String := CORBA.To_CORBA_String ("""
                            & Ada_Name (Decl_Node)
                            & """);");
                     end loop;
                  end;
               end loop;
            end;
            DI (CU);
            PL (CU, "begin");
            II (CU);
            PL (CU, "CORBA.TypeCode.Add_Parameter ("
                & Ada_TC_Name (Node)
                & ", CORBA.To_Any (Name));");
            PL (CU, "CORBA.TypeCode.Add_Parameter ("
                & Ada_TC_Name (Node)
                & ", CORBA.To_Any (Id));");
            declare
               It   : Node_Iterator;
               Member_Node : Node_Id;
            begin
               Init (It, Members (Node));
               while not Is_End (It) loop
                  Get_Next_Node (It, Member_Node);
                  declare
                     It2   : Node_Iterator;
                     Decl_Node : Node_Id;
                  begin
                     Init (It2, Decl (Member_Node));
                     while not Is_End (It2) loop
                        Get_Next_Node (It2, Decl_Node);
                        PL (CU, "CORBA.TypeCode.Add_Parameter ("
                            & Ada_TC_Name (Node)
                            & ", CORBA.To_Any ("
                            & Ada_TC_Name (M_Type (Member_Node))
                            & "));");
                        PL (CU, "CORBA.TypeCode.Add_Parameter ("
                            & Ada_TC_Name (Node)
                            & ", CORBA.To_Any (Arg_Name_"
                            & Ada_Name (Decl_Node)
                            & "));");
                     end loop;
                  end;
               end loop;
            end;
            DI (CU);
            PL (CU, "end;");
            Divert (CU, Visible_Declarations);

         when others =>
            null;

      end case;
   end Gen_Node_Body;

   ----------------------------
   --  Gen_From_Any_Profile  --
   ----------------------------
   procedure Gen_From_Any_Profile
     (CU        : in out Compilation_Unit;
      Type_Node : in Node_Id) is
   begin
      Add_With (CU, "CORBA", Use_It => False);
      PL (CU, "function From_Any (Item : in CORBA.Any)");
      II (CU);
      Put (CU, "return "
           & Ada_Type_Name (Type_Node));
      DI (CU);
   end Gen_From_Any_Profile;

   --------------------------
   --  Gen_To_Any_Profile  --
   --------------------------
   procedure Gen_To_Any_Profile
     (CU        : in out Compilation_Unit;
      Type_Node : in Node_Id) is
   begin
      Add_With (CU, "CORBA", Use_It => False);
      PL (CU, "function To_Any (Item : in "
          & Ada_Type_Name (Type_Node)
          & ")");
      II (CU);
      Put (CU, "return CORBA.Any");
      DI (CU);
   end Gen_To_Any_Profile;

   -------------------
   --  Gen_Array_TC --
   -------------------

   procedure Gen_Array_TC
     (CU        : in out Compilation_Unit;
      Node      : in     Node_Id;
      Decl_Node : in     Node_Id) is

      procedure Rec_Gen_Array_TC
        (CU             : in out Compilation_Unit;
         It             : in out Node_Iterator;
         First_Bound    : in     Boolean;
         Index          : in     Integer;
         Node           : in     Node_Id;
         Decl_Node      : in     Node_Id);

      procedure Rec_Gen_Array_TC
        (CU             : in out Compilation_Unit;
         It             : in out Node_Iterator;
         First_Bound    : in     Boolean;
         Index          : in     Integer;
         Node           : in     Node_Id;
         Decl_Node      : in     Node_Id) is
         Bound_Node : Node_Id;
         Last_Bound : Boolean := False;
      begin
         Get_Next_Node (It, Bound_Node);
         if not Is_End (It) then
            Rec_Gen_Array_TC (CU, It, False, Index + 1, Node, Decl_Node);
         else
            Last_Bound := True;
         end if;
         Put (CU, "CORBA.TypeCode.Add_Parameter (");
         if First_Bound then
            Put (CU, Ada_TC_Name (Decl_Node));
         else
            Put (CU, "TC_" & Img (Index));
         end if;
         Put (CU, ", To_Any (Unsigned_Long (");
         Gen_Node_Stubs_Spec (CU, Bound_Node);
         PL (CU, ")));");
         Put (CU, "CORBA.TypeCode.Add_Parameter (");
         if First_Bound then
            Put (CU, Ada_TC_Name (Decl_Node));
         else
            Put (CU, "TC_" & Img (Index));
         end if;
         Put (CU, ", To_Any (");
         if Last_Bound then
            Put (CU, Ada_TC_Name (T_Type (Node)));
         else
            Put (CU, "TC_" & Img (Index + 1));
         end if;
         PL (CU, "));");
      end Rec_Gen_Array_TC;

      Bounds_It : Node_Iterator;
   begin
      Init (Bounds_It, Array_Bounds (Decl_Node));
      Rec_Gen_Array_TC (CU, Bounds_It, True, 0, Node, Decl_Node);
   end Gen_Array_TC;

   ------------------
   --  Ada_TC_Name --
   ------------------

   function Ada_TC_Name
     (Node : Node_Id)
     return String
   is
      NK : constant Node_Kind
        := Kind (Node);
      Prefix : constant String := "TC_";
   begin
      case NK is
         when
           K_Interface         |
           K_Forward_Interface =>
            --          K_ValueType         |
            --          K_Forward_ValueType =>
            return Prefix & Ada_Name (Node);

--          when K_Sequence_Instance =>
--             return Ada_Full_Name (Node) & ".Sequence";

--          when K_String_Instance =>
--             return Ada_Full_Name (Node) & ".Bounded_String";

         when
           K_Enum       |
           K_Union      |
           K_Struct     |
           K_Declarator =>
            return Prefix & Ada_Name (Node);

         when K_Scoped_Name =>
            return Ada_TC_Name (Value (Node));

         when K_Short =>
            return "CORBA." & Prefix & "Short";

         when K_Long =>
            return "CORBA." & Prefix & "Long";

         when K_Long_Long =>
            return "CORBA." & Prefix & "Long_Long";

         when K_Unsigned_Short =>
            return "CORBA." & Prefix & "Unsigned_Short";

         when K_Unsigned_Long =>
            return "CORBA." & Prefix & "Unsigned_Long";

         when K_Unsigned_Long_Long =>
            return "CORBA." & Prefix & "Unsigned_Long_Long";

         when K_Char =>
            return "CORBA." & Prefix & "Char";

         when K_Wide_Char =>
            return "CORBA." & Prefix & "Wide_Char";

         when K_Boolean =>
            return "CORBA." & Prefix & "Boolean";

         when K_Float =>
            return "CORBA." & Prefix & "Float";

         when K_Double =>
            return "CORBA." & Prefix & "Double";

         when K_Long_Double =>
            return "CORBA." & Prefix & "Long_Double";

         when K_String =>
            return "CORBA." & Prefix & "String";

         when K_Wide_String =>
            return "CORBA." & Prefix & "Wide_String";

         when K_Octet =>
            return "CORBA." & Prefix & "Octet";

         when K_Object =>
            return "CORBA." & Prefix & "Object.Ref";

         when K_Any =>
            return "CORBA." & Prefix & "Any";

         when others =>
            --  Improper use: node N is not
            --  mapped to an Ada type.

            Error
              ("A " & NK'Img
               & " does not denote a type. So there's no TypeCode"
               & " associated with it.",
               Fatal, Get_Location (Node));

            --  Keep the compiler happy.
            raise Program_Error;

      end case;
   end Ada_TC_Name;

end Ada_Be.Idl2Ada.Helper;

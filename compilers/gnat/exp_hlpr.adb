------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             E X P _ H L P R                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2003, Free Software Foundation, Inc.         --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT;  see file COPYING.  If not, write --
-- to  the Free Software Foundation,  59 Temple Place - Suite 330,  Boston, --
-- MA 02111-1307, USA.                                                      --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- It is now maintained by Ada Core Technologies Inc (http://www.gnat.com). --
--                                                                          --
------------------------------------------------------------------------------

with Atree;    use Atree;
with Einfo;    use Einfo;
with Exp_Tss;  use Exp_Tss;
with Exp_Util; use Exp_Util;
with Lib;      use Lib;
with Namet;    use Namet;
with Nlists;   use Nlists;
with Nmake;    use Nmake;
with Rtsfind;  use Rtsfind;
with Sinfo;    use Sinfo;
with Einfo;    use Einfo;
with Sem_Ch7;  use Sem_Ch7;
with Sem_Ch8;  use Sem_Ch8;
with Sem_Eval; use Sem_Eval;
with Sem_Util; use Sem_Util;
with Snames;   use Snames;
with Stand;    use Stand;
with Stringt;  use Stringt;
with Tbuild;   use Tbuild;
with Ttypes;   use Ttypes;
with Uintp;    use Uintp;

package body Exp_Hlpr is

   -----------------------
   -- Local Subprograms --
   -----------------------

   procedure Compile_Stream_Body_In_Scope
     (N     : Node_Id;
      Decl  : Node_Id;
      Arr   : Entity_Id;
      Check : Boolean);
   pragma Warnings (Off);
   pragma Unreferenced (Compile_Stream_Body_In_Scope);
   pragma Warnings (On);
   --  The body for a stream subprogram may be generated outside of the scope
   --  of the type. If the type is fully private, it may depend on the full
   --  view of other types (e.g. indices) that are currently private as well.
   --  We install the declarations of the package in which the type is declared
   --  before compiling the body in what is its proper environment. The Check
   --  parameter indicates if checks are to be suppressed for the stream body.
   --  We suppress checks for array/record reads, since the rule is that these
   --  are like assignments, out of range values due to uninitialized storage,
   --  or other invalid values do NOT cause a Constraint_Error to be raised.
   --  (copied from exp_attr.adb)

   function Find_Inherited_TSS
     (Typ : Entity_Id;
      Nam : Name_Id) return Entity_Id;
   --  A TSS reference for a representation aspect of a derived tagged type
   --  must take into account inheritance of that aspect from ancestor types.
   --  (copied from exp_attr.adb)

   function Find_Numeric_Representation
     (Typ : Entity_Id)
      return Entity_Id;
   --  Given a numeric type Typ, return the smallest integer or floarting point
   --  type from Standard, or the smallest unsigned (modular) type from
   --  System.Unsigned_Types, whose range  encompasses that of Typ.

   function Make_Stream_Procedure_Function_Name
     (Loc : Source_Ptr;
      Typ : Entity_Id;
      Nam : Name_Id)
      return Entity_Id;
   --  Return the name to be assigned for stream subprogram Nam of Typ.
   --  (copied from exp_strm.adb)

   ------------------------------------------------------------
   -- Common subprograms for building various tree fragments --
   ------------------------------------------------------------

   function Build_Get_Aggregate_Element
     (Loc : Source_Ptr;
      Any : Entity_Id;
      TC  : Node_Id;
      Idx : Node_Id)
     return Node_Id;
   --  Build a call to Get_Aggregate_Element on Any
   --  for typecode TC, returning the Idx'th element.

   generic
      Subprogram : Entity_Id;
      --  Reference location for constructed nodes.

      Arry : Entity_Id;
      --  For 'Range and Etype.

      Indices : List_Id;
      --  For the construction of the innermost element expression.

      with procedure Add_Process_Element
        (Stmts   : List_Id;
         Any     : Entity_Id;
         Counter : Entity_Id;
         Datum   : Node_Id);

   procedure Append_Array_Traversal
     (Stmts   : List_Id;
      Any     : Entity_Id;
      Counter : Entity_Id := Empty;
      Depth   : Pos       := 1);
   --  Build nested loop statements that iterate over the elements
   --  of an array Arry. The statement(s) built by Add_Process_Element
   --  are executed for each element; Indices is the list of indices to be
   --  used in the construction of the indexed component that denotes
   --  the current element. Subprogram is the entity for the subprogram
   --  for which this iterator is generated.
   --  The generated statements are appended to Stmts.

   generic
      Rec : Entity_Id;
      --  The record entity being dealt with.

      with procedure Add_Process_Element
        (Stmts     :        List_Id;
         Container :        Node_Or_Entity_Id;
         Counter   : in out Int;
         Rec       :        Entity_Id;
         Field     :        Node_Id);
      --  Rec is the instance of the record type, or Empty.
      --  Field is either the N_Defining_Identifier for a component,
      --  or an N_Variant_Part.

   procedure Append_Record_Traversal
     (Stmts     :        List_Id;
      Clist     :        Node_Id;
      Container :        Node_Or_Entity_Id;
      Counter   : in out Int);
   --  Process component list Clist. Individual fields are passed
   --  to Field_Processing. Each variant part is also processed.
   --  Container is the outer Any (for From_Any/To_Any),
   --  the outer typecode (for TC) to which the operation applies.

   -----------------------------
   -- Append_Record_Traversal --
   -----------------------------

   procedure Append_Record_Traversal
     (Stmts     :        List_Id;
      Clist     :        Node_Id;
      Container :        Node_Or_Entity_Id;
      Counter   : in out Int)
   is
      CI : constant List_Id := Component_Items (Clist);
      VP : constant Node_Id := Variant_Part (Clist);

      Item : Node_Id := First (CI);
      Def  : Entity_Id;

   begin
      while Present (Item) loop
         Def := Defining_Identifier (Item);
         if not Is_Internal_Name (Chars (Def)) then
            Add_Process_Element
              (Stmts, Container, Counter, Rec, Def);
         end if;
         Next (Item);
      end loop;

      if Present (VP) then
         Add_Process_Element (Stmts, Container, Counter, Rec, VP);
      end if;
   end Append_Record_Traversal;

   -------------------------
   -- Build_From_Any_Call --
   -------------------------

   function Build_From_Any_Call
     (Typ   : Entity_Id;
      N     : Node_Id;
      Decls : List_Id)
      return Node_Id
   is
      Loc : constant Source_Ptr := Sloc (N);
      U_Type : Entity_Id  := Underlying_Type (Typ);
      --  Rt_Type : constant Entity_Id  := Root_Type (U_Type);

      --  FST     : constant Entity_Id  := First_Subtype (U_Type);
      --  P_Size  : constant Uint       := Esize (FST);

      Fnam : Entity_Id := Empty;
      Lib_RE  : RE_Id := RE_Null;

   begin

      --  First simple case where the From_Any function is present
      --  in the type's TSS.

      Fnam := Find_Inherited_TSS (U_Type, Name_uFrom_Any);

      if Sloc (U_Type) <= Standard_Location then
         U_Type := Base_Type (U_Type);
      end if;

      --  Check first for Boolean and Character. These are enumeration types,
      --  but we treat them specially, since they may require special handling
      --  in the transfer protocol. However, this special handling only applies
      --  if they have standard representation, otherwise they are treated like
      --  any other enumeration type.

      if Present (Fnam) then
         null;

      elsif U_Type = Standard_Boolean then
         Lib_RE := RE_FA_B;

      elsif U_Type = Standard_Character then
         Lib_RE := RE_FA_C;

      elsif U_Type = Standard_Wide_Character then
         Lib_RE := RE_FA_WC;

      --  Floating point types

      elsif U_Type = Standard_Short_Float then
         Lib_RE := RE_FA_SF;

      elsif U_Type = Standard_Float then
         Lib_RE := RE_FA_F;

      elsif U_Type = Standard_Long_Float then
         Lib_RE := RE_FA_LF;

      elsif U_Type = Standard_Long_Long_Float then
         Lib_RE := RE_FA_LLF;

      --  Integer types

      elsif U_Type = Etype (Standard_Short_Short_Integer) then
            Lib_RE := RE_FA_SSI;

      elsif U_Type = Etype (Standard_Short_Integer) then
         Lib_RE := RE_FA_SI;

      elsif U_Type = Etype (Standard_Integer) then
         Lib_RE := RE_FA_I;

      elsif U_Type = Etype (Standard_Long_Integer) then
         Lib_RE := RE_FA_LI;

      elsif U_Type = Etype (Standard_Long_Long_Integer) then
         Lib_RE := RE_FA_LLI;

      --  Unsigned integer types

      elsif U_Type = RTE (RE_Short_Short_Unsigned) then
         Lib_RE := RE_FA_SSU;

      elsif U_Type = RTE (RE_Short_Unsigned) then
         Lib_RE := RE_FA_SU;

      elsif U_Type = RTE (RE_Unsigned) then
         Lib_RE := RE_FA_U;

      elsif U_Type = RTE (RE_Long_Unsigned) then
         Lib_RE := RE_FA_LU;

      elsif U_Type = RTE (RE_Long_Long_Unsigned) then
         Lib_RE := RE_FA_LLU;

      --  Access types

--        elsif Is_Access_Type (U_Type) then
--           if P_Size > System_Address_Size then
--              Lib_RE := RE_FA_AD;
--           else
--              Lib_RE := RE_FA_AS;
--           end if;

      elsif U_Type = Standard_String then
         Lib_RE := RE_FA_String;

      --  Other (non-primitive) types

      else
         declare
            Decl : Entity_Id;
         begin
            Build_From_Any_Function (Loc, U_Type, Decl, Fnam);
            Append_To (Decls, Decl);
         end;
      end if;

      --  Call the function

      if Lib_RE /= RE_Null then
         pragma Assert (No (Fnam));
         Fnam := RTE (Lib_RE);
      end if;

      return
          Make_Function_Call (Loc,
            Name => New_Occurrence_Of (Fnam, Loc),
            Parameter_Associations => New_List (N));
   end Build_From_Any_Call;

   -----------------------------
   -- Build_From_Any_Function --
   -----------------------------

   procedure Build_From_Any_Function
     (Loc  :     Source_Ptr;
      Typ  :     Entity_Id;
      Decl : out Node_Id;
      Fnam : out Entity_Id)
   is
      Spec : Node_Id;
      Decls : constant List_Id := New_List;
      Stms : constant List_Id := New_List;
      Any_Parameter : constant Entity_Id
        := Make_Defining_Identifier (Loc, New_Internal_Name ('A'));
   begin
      Fnam := Make_Stream_Procedure_Function_Name (Loc, Typ, Name_uFrom_Any);

      Spec :=
        Make_Function_Specification (Loc,
          Defining_Unit_Name => Fnam,
          Parameter_Specifications => New_List (
            Make_Parameter_Specification (Loc,
              Defining_Identifier =>
                Any_Parameter,
              Parameter_Type =>
                New_Occurrence_Of (RTE (RE_Any), Loc))),
          Subtype_Mark => New_Occurrence_Of (Typ, Loc));

      pragma Assert
        (not (Is_Remote_Access_To_Class_Wide_Type (Typ)));
      --  This is taken care of by Exp_Dist.Add_RACW_From_Any.

      if Is_Derived_Type (Typ)
        and then not Is_Tagged_Type (Typ)
      then
         Append_To (Stms,
           Make_Return_Statement (Loc,
             Expression =>
               OK_Convert_To (
                 Typ,
                 Build_From_Any_Call (
                   Root_Type (Typ),
                   New_Occurrence_Of (Any_Parameter, Loc),
                   Decls))));

      elsif Is_Record_Type (Typ)
        and then not Is_Derived_Type (Typ)
      then
         if Nkind (Declaration_Node (Typ)) = N_Subtype_Declaration then
            Append_To (Stms,
              Make_Return_Statement (Loc,
                Expression =>
                  OK_Convert_To (
                    Typ,
                    Build_From_Any_Call (
                      Etype (Typ),
                      New_Occurrence_Of (Any_Parameter, Loc),
                      Decls))));
         else
            declare
               Disc : Entity_Id := Empty;
               Discriminant_Associations : List_Id;
               Rdef : constant Node_Id :=
                 Type_Definition (Declaration_Node (Typ));
               Component_Counter : Int := 0;

               --  The returned object

               Res : constant Entity_Id :=
                 Make_Defining_Identifier (Loc,
                   New_Internal_Name ('R'));

               Res_Definition : Node_Id :=
                 New_Occurrence_Of (Typ, Loc);

               procedure FA_Rec_Add_Process_Element
                 (Stmts   : List_Id;
                  Any     : Entity_Id;
                  Counter : in out Int;
                  Rec     : Entity_Id;
                  Field   : Node_Id);

               procedure FA_Append_Record_Traversal is
                  new Append_Record_Traversal
                 (Rec                 => Res,
                  Add_Process_Element => FA_Rec_Add_Process_Element);

               procedure FA_Rec_Add_Process_Element
                 (Stmts   : List_Id;
                  Any     : Entity_Id;
                  Counter : in out Int;
                  Rec     : Entity_Id;
                  Field   : Node_Id)
               is
               begin
                  if Nkind (Field) = N_Defining_Identifier then

                     --  A regular component.

                     Append_To (Stmts,
                       Make_Assignment_Statement (Loc,
                         Name => Make_Selected_Component (Loc,
                           Prefix => New_Occurrence_Of (Rec, Loc),
                           Selector_Name => New_Occurrence_Of (Field, Loc)),
                         Expression =>
                           Build_From_Any_Call (Etype (Field),
                             Build_Get_Aggregate_Element (Loc,
                               Any => Any,
                               Tc  => Build_TypeCode_Call
                                        (Loc, Etype (Field), Decls),
                               Idx => Make_Integer_Literal (Loc, Counter)),
                             Decls)));
                  else

                     --  A variant part.

                     declare
                        Variant : Node_Id;
                        Struct_Counter : Int := 0;

                        Block_Decls : constant List_Id := New_List;
                        Block_Stmts : constant List_Id := New_List;
                        VP_Stmts : List_Id;

                        Alt_List : constant List_Id := New_List;
                        Choice_List : List_Id;

                        Struct_Any : constant Entity_Id
                          := Make_Defining_Identifier (Loc,
                               New_Internal_Name ('S'));
                     begin

                        Append_To (Decls,
                          Make_Object_Declaration (Loc,
                            Defining_Identifier =>
                              Struct_Any,
                            Constant_Present =>
                               True,
                            Object_Definition =>
                               New_Occurrence_Of (RTE (RE_Any), Loc),
                            Expression =>
                              Make_Function_Call (Loc,
                                Name => New_Occurrence_Of (
                                  RTE (RE_Extract_Union_Value), Loc),
                                Parameter_Associations => New_List (
                                  Build_Get_Aggregate_Element (Loc,
                                    Any => Any,
                                    Tc  => Make_Function_Call (Loc,
                                      Name => New_Occurrence_Of (
                                        RTE (RE_Any_Member_Type), Loc),
                                      Parameter_Associations => New_List (
                                        New_Occurrence_Of (Any, Loc),
                                        Make_Integer_Literal (Loc, Counter))),
                                    Idx => Make_Integer_Literal (Loc,
                                      Counter))))));

                        Append_To (Stmts,
                          Make_Block_Statement (Loc,
                            Declarations =>
                              Block_Decls,
                            Handled_Statement_Sequence =>
                              Make_Handled_Sequence_Of_Statements (Loc,
                                Statements => Block_Stmts)));

                        Append_To (Block_Stmts,
                          Make_Case_Statement (Loc,
                              Expression =>
                                Make_Selected_Component (Loc,
                                  Prefix =>
                                    New_Occurrence_Of (Rec, Loc),
                                  Selector_Name =>
                                    New_Occurrence_Of (
                                      Entity (Name (Field)), Loc)),
                              Alternatives =>
                                Alt_List));

                        Variant := First_Non_Pragma (Variants (Field));

                        while Present (Variant) loop
                           Choice_List := New_Copy_List_Tree
                             (Discrete_Choices (Variant));

                           VP_Stmts := New_List;
                           FA_Append_Record_Traversal (
                             Stmts     => VP_Stmts,
                             Clist     => Component_List (Variant),
                             Container => Struct_Any,
                             Counter   => Struct_Counter);

                           Append_To (Alt_List,
                             Make_Case_Statement_Alternative (Loc,
                               Discrete_Choices => Choice_List,
                               Statements =>
                                 VP_Stmts));
                           Next_Non_Pragma (Variant);
                        end loop;
                     end;
                  end if;
                  Counter := Counter + 1;
               end FA_Rec_Add_Process_Element;

            begin

               --  First all discriminants

               if Has_Discriminants (Typ) then
                  Disc := First_Discriminant (Typ);
                  Discriminant_Associations := New_List;

                  while Present (Disc) loop
                     declare
                        Disc_Var_Name : constant Entity_Id :=
                          Make_Defining_Identifier (Loc, Chars (Disc));
                        Disc_Type : constant Entity_Id :=
                          Etype (Disc);
                     begin
                        Append_To (Decls,
                          Make_Object_Declaration (Loc,
                            Defining_Identifier =>
                              Disc_Var_Name,
                            Constant_Present => True,
                            Object_Definition =>
                              New_Occurrence_Of (Disc_Type, Loc),
                            Expression =>
                              Build_From_Any_Call (Etype (Disc),
                                Build_Get_Aggregate_Element (Loc,
                                  Any => Any_Parameter,
                                  Tc  => Build_TypeCode_Call
                                           (Loc, Etype (Disc), Decls),
                                  Idx => Make_Integer_Literal
                                           (Loc, Component_Counter)),
                                Decls)));
                        Component_Counter := Component_Counter + 1;

                        Append_To (Discriminant_Associations,
                          Make_Discriminant_Association (Loc,
                            Selector_Names => New_List (
                              New_Occurrence_Of (Disc, Loc)),
                            Expression =>
                              New_Occurrence_Of (Disc_Var_Name, Loc)));
                     end;
                     Next_Discriminant (Disc);
                  end loop;

                  Res_Definition := Make_Subtype_Indication (Loc,
                    Subtype_Mark => Res_Definition,
                    Constraint   =>
                      Make_Index_Or_Discriminant_Constraint (Loc,
                        Discriminant_Associations));
               end if;

               --  Now we have all the discriminants in variables,
               --  we can declared a constrained object. Note that
               --  we are not initializing (non-discriminant) components
               --  directly in the object declarations, because which
               --  fields to initialize depends (at run time) on the
               --  discriminant values.

               Append_To (Decls,
                 Make_Object_Declaration (Loc,
                   Defining_Identifier =>
                     Res,
                   Object_Definition =>
                     Res_Definition));

               --  ... then all components

               FA_Append_Record_Traversal (Stms,
                 Clist     => Component_List (Rdef),
                 Container => Any_Parameter,
                 Counter   => Component_Counter);

               Append_To (Stms,
                 Make_Return_Statement (Loc,
                   Expression => New_Occurrence_Of (Res, Loc)));
            end;
         end if;

      elsif Is_Array_Type (Typ) then

         declare

            Constrained : constant Boolean := Is_Constrained (Typ);

            procedure FA_Ary_Add_Process_Element
              (Stmts   : List_Id;
               Any     : Entity_Id;
               Counter : Entity_Id;
               Datum   : Node_Id);
            --  Assign the current element (as identified by Counter) of
            --  Any to the variable denoted by name Datum, and advance Counter
            --  by 1.
            --  If Datum is not an Any, a call to From_Any for its type
            --  is inserted.

            procedure FA_Ary_Add_Process_Element
              (Stmts   : List_Id;
               Any     : Entity_Id;
               Counter : Entity_Id;
               Datum   : Node_Id)
            is
               Assignment : constant Node_Id :=
                 Make_Assignment_Statement (Loc,
                   Name       => Datum,
                   Expression => Empty);

               Element_Any : constant Node_Id :=
                 Build_Get_Aggregate_Element (Loc,
                   Any => Any,
                   Tc  => Build_TypeCode_Call (Loc, Etype (Datum), Decls),
                   Idx => New_Occurrence_Of (Counter, Loc));

            begin

               --  Note: here we *prepend* statements to Stmts, so
               --  we must do it in reverse order.

               Prepend_To (Stmts,
                 Make_Assignment_Statement (Loc,
                   Name =>
                     New_Occurrence_Of (Counter, Loc),
                   Expression =>
                     Make_Op_Add (Loc,
                       Left_Opnd =>
                         New_Occurrence_Of (Counter, Loc),
                       Right_Opnd =>
                         Make_Integer_Literal (Loc, 1))));

               if Nkind (Datum) /= N_Attribute_Reference then

                  --  We ignore the value of the length of each
                  --  dimension, since the target array has already
                  --  been constrained anyway.

                  if Etype (Datum) /= RTE (RE_Any) then
                     Set_Expression (Assignment,
                        Build_From_Any_Call (
                          Component_Type (Typ),
                          Element_Any,
                          Decls));
                  else
                     Set_Expression (Assignment, Element_Any);
                  end if;
                  Prepend_To (Stmts, Assignment);
               end if;
            end FA_Ary_Add_Process_Element;

            Counter : constant Entity_Id
              := Make_Defining_Identifier (Loc, Name_J);
            Initial_Counter_Value : Int := 0;
            Component_TC : constant Entity_Id
              := Make_Defining_Identifier (Loc, Name_T);

            Res : constant Entity_Id
              := Make_Defining_Identifier (Loc, Name_R);

            procedure Append_From_Any_Array_Iterator is
              new Append_Array_Traversal (
                Subprogram => Fnam,
                Arry       => Res,
                Indices    => New_List,
                Add_Process_Element => FA_Ary_Add_Process_Element);

            Res_Subtype_Indication : Node_Id
              := New_Occurrence_Of (Typ, Loc);

         begin
            if not Constrained then
               declare
                  Ndim : constant Int := Number_Dimensions (Typ);
                  Lnam : Name_Id;
                  Hnam : Name_Id;
                  Indx : Node_Id := First_Index (Typ);
                  Indt : Entity_Id;
                  Ranges : constant List_Id := New_List;
               begin
                  for J in 1 .. Ndim loop

                     Lnam := New_External_Name ('L', J);
                     Hnam := New_External_Name ('H', J);
                     Indt := Etype (Indx);

                     Append_To (Decls,
                       Make_Object_Declaration (Loc,
                         Defining_Identifier =>
                           Make_Defining_Identifier (Loc, Lnam),
                         Constant_Present    => True,
                         Object_Definition   => New_Occurrence_Of (Indt, Loc),
                         Expression          => Build_From_Any_Call (
                           Indt,
                           Build_Get_Aggregate_Element (Loc,
                             Any => Any_Parameter,
                             Tc  => Build_TypeCode_Call (Loc, Indt, Decls),
                             Idx => Make_Integer_Literal (Loc, J - 1)),
                           Decls)));

                     Append_To (Decls,
                       Make_Object_Declaration (Loc,
                         Defining_Identifier =>
                           Make_Defining_Identifier (Loc, Hnam),
                         Constant_Present    => True,
                         Object_Definition   => New_Occurrence_Of (Indt, Loc),
                         Expression =>
                           Make_Attribute_Reference (Loc,
                             Prefix         => New_Occurrence_Of (Indt, Loc),
                             Attribute_Name => Name_Val,
                             Expressions    => New_List (
                               Make_Op_Subtract (Loc,
                                 Left_Opnd =>
                                   Make_Op_Add (Loc,
                                     Left_Opnd =>
                                       Make_Attribute_Reference (Loc,
                                         Prefix         =>
                                           New_Occurrence_Of (Indt, Loc),
                                         Attribute_Name =>
                                           Name_Pos,
                                         Expressions    => New_List (
                                           Make_Identifier (Loc, Lnam))),
                                     Right_Opnd =>
                                       Make_Function_Call (Loc,
                                         Name =>
                                           New_Occurrence_Of (
                                             RTE (
                                               RE_Get_Nested_Sequence_Length),
                                               Loc),
                                         Parameter_Associations => New_List (
                                           New_Occurrence_Of (
                                             Any_Parameter, Loc),
                                           Make_Integer_Literal (Loc, J)))),
                                 Right_Opnd =>
                                   Make_Integer_Literal (Loc, 1))))));

                     Append_To (Ranges,
                       Make_Range (Loc,
                         Low_Bound  => Make_Identifier (Loc, Lnam),
                         High_Bound => Make_Identifier (Loc, Hnam)));

                     Next_Index (Indx);
                  end loop;

                  --  Now we have all the necessary bound information:
                  --  apply the set of range constraints to the (unconstrained)
                  --  nominal subtype of Res.

                  Initial_Counter_Value := Ndim;
                  Res_Subtype_Indication := Make_Subtype_Indication (Loc,
                    Subtype_Mark =>
                      Res_Subtype_Indication,
                    Constraint   =>
                      Make_Index_Or_Discriminant_Constraint (Loc,
                        Constraints => Ranges));
               end;
            end if;

            Append_To (Decls,
              Make_Object_Declaration (Loc,
                Defining_Identifier => Res,
                Object_Definition => Res_Subtype_Indication));
            Set_Etype (Res, Typ);

            Append_To (Decls,
              Make_Object_Declaration (Loc,
                Defining_Identifier => Counter,
                Object_Definition =>
                  New_Occurrence_Of (RTE (RE_Long_Unsigned), Loc),
                Expression =>
                  Make_Integer_Literal (Loc, Initial_Counter_Value)));

            Append_To (Decls,
              Make_Object_Declaration (Loc,
                Defining_Identifier => Component_TC,
                Constant_Present => True,
                Object_Definition =>
                  New_Occurrence_Of (RTE (RE_TypeCode), Loc),
                Expression =>
                  Build_TypeCode_Call (Loc, Component_Type (Typ), Decls)));

            Append_From_Any_Array_Iterator (Stms, Any_Parameter, Counter);

            Append_To (Stms,
              Make_Return_Statement (Loc,
                Expression => New_Occurrence_Of (Res, Loc)));
         end;

      elsif Is_Integer_Type (Typ) or else Is_Unsigned_Type (Typ) then
         Append_To (Stms,
           Make_Return_Statement (Loc,
             Expression =>
               Unchecked_Convert_To (
                 Typ,
                 Build_From_Any_Call (
                   Find_Numeric_Representation (Typ),
                   New_Occurrence_Of (Any_Parameter, Loc),
                   Decls))));

      else

         --  Default: type is represented as an opaque sequence of bytes.

         declare
            Strm : constant Entity_Id := Make_Defining_Identifier (Loc,
              New_Internal_Name ('S'));
            Res : constant Entity_Id := Make_Defining_Identifier (Loc,
              New_Internal_Name ('R'));

         begin

            --  Strm : Buffer_Stream_Type;

            Append_To (Decls,
              Make_Object_Declaration (Loc,
                Defining_Identifier =>
                  Strm,
                Aliased_Present     =>
                  True,
                Object_Definition   =>
                  New_Occurrence_Of (RTE (RE_Buffer_Stream_Type), Loc)));

            --  Any_To_BS (Strm, A);

            Append_To (Stms,
              Make_Procedure_Call_Statement (Loc,
                Name =>
                  New_Occurrence_Of (RTE (RE_Any_To_BS), Loc),
                Parameter_Associations => New_List (
                  New_Occurrence_Of (Any_Parameter, Loc),
                  New_Occurrence_Of (Strm, Loc))));

            --  declare
            --     Res : constant T := T'Input (Strm);
            --  begin
            --     Release_Buffer (Strm);
            --     return Res;
            --  end;

            Append_To (Stms, Make_Block_Statement (Loc,
              Declarations => New_List (
                Make_Object_Declaration (Loc,
                  Defining_Identifier => Res,
                  Constant_Present    => True,
                  Object_Definition   =>
                    New_Occurrence_Of (Typ, Loc),
                  Expression          =>
                      Make_Attribute_Reference (Loc,
                        Prefix         => New_Occurrence_Of (Typ, Loc),
                        Attribute_Name => Name_Input,
                        Expressions => New_List (
                          Make_Attribute_Reference (Loc,
                            Prefix => New_Occurrence_Of (Strm, Loc),
                            Attribute_Name => Name_Access))))),

              Handled_Statement_Sequence =>
                Make_Handled_Sequence_Of_Statements (Loc,
                  Statements => New_List (
                    Make_Procedure_Call_Statement (Loc,
                      Name => New_Occurrence_Of (RTE (RE_Release_Buffer), Loc),
                      Parameter_Associations => New_List (
                        New_Occurrence_Of (Strm, Loc))),
                    Make_Return_Statement (Loc,
                      Expression => New_Occurrence_Of (Res, Loc))))));

         end;
      end if;

      Decl :=
        Make_Subprogram_Body (Loc,
          Specification => Spec,
          Declarations => Decls,
          Handled_Statement_Sequence =>
            Make_Handled_Sequence_Of_Statements (Loc,
              Statements => Stms));
   end Build_From_Any_Function;

   ---------------------------------
   -- Build_Get_Aggregate_Element --
   ---------------------------------

   function Build_Get_Aggregate_Element
     (Loc : Source_Ptr;
      Any : Entity_Id;
      TC  : Node_Id;
      Idx : Node_Id)
      return Node_Id
   is
   begin
      return Make_Function_Call (Loc,
        Name =>
          New_Occurrence_Of (
            RTE (RE_Get_Aggregate_Element), Loc),
        Parameter_Associations => New_List (
          New_Occurrence_Of (Any, Loc),
          TC,
          Idx));
   end Build_Get_Aggregate_Element;

   -----------------------
   -- Build_To_Any_Call --
   -----------------------

   function Build_To_Any_Call
     (N     : Node_Id;
      Decls : List_Id)
      return Node_Id
   is
      Loc : constant Source_Ptr := Sloc (N);
      Typ : Entity_Id := Etype (N);
      U_Type  : Entity_Id;

      --  Rt_Type : constant Entity_Id  := Root_Type (U_Type);
      --  FST     : constant Entity_Id  := First_Subtype (U_Type);
      --  P_Size  : constant Uint       := Esize (FST);

      Fnam : Entity_Id := Empty;
      Lib_RE  : RE_Id := RE_Null;

   begin

      --  If N is a selected component, then maybe its Etype
      --  has not been set yet: try to use the Etype of the
      --  selector_name in that case.

      if No (Typ) and then Nkind (N) = N_Selected_Component then
         Typ := Etype (Selector_Name (N));
      end if;
      pragma Assert (Present (Typ));

      U_Type := Underlying_Type (Typ);
      --  The full view, if Typ is private; the completion,
      --  if Typ is incomplete.

      --  First simple case where the To_Any function is present
      --  in the type's TSS.

      Fnam := Find_Inherited_TSS (U_Type, Name_uTo_Any);

      --  Check first for Boolean and Character. These are enumeration types,
      --  but we treat them specially, since they may require special handling
      --  in the transfer protocol. However, this special handling only applies
      --  if they have standard representation, otherwise they are treated like
      --  any other enumeration type.

      if Sloc (U_Type) <= Standard_Location then
         U_Type := Base_Type (U_Type);
      end if;

      if Present (Fnam) then
         null;

      elsif U_Type = Standard_Boolean then
         Lib_RE := RE_TA_B;

      elsif U_Type = Standard_Character then
         Lib_RE := RE_TA_C;

      elsif U_Type = Standard_Wide_Character then
         Lib_RE := RE_TA_WC;

      --  Floating point types

      elsif U_Type = Standard_Short_Float then
         Lib_RE := RE_TA_SF;

      elsif U_Type = Standard_Float then
         Lib_RE := RE_TA_F;

      elsif U_Type = Standard_Long_Float then
         Lib_RE := RE_TA_LF;

      elsif U_Type = Standard_Long_Long_Float then
         Lib_RE := RE_TA_LLF;

      --  Integer types

      elsif U_Type = Etype (Standard_Short_Short_Integer) then
            Lib_RE := RE_TA_SSI;

      elsif U_Type = Etype (Standard_Short_Integer) then
         Lib_RE := RE_TA_SI;

      elsif U_Type = Etype (Standard_Integer) then
         Lib_RE := RE_TA_I;

      elsif U_Type = Etype (Standard_Long_Integer) then
         Lib_RE := RE_TA_LI;

      elsif U_Type = Etype (Standard_Long_Long_Integer) then
         Lib_RE := RE_TA_LLI;

      --  Unsigned integer types

      elsif U_Type = RTE (RE_Short_Short_Unsigned) then
         Lib_RE := RE_TA_SSU;

      elsif U_Type = RTE (RE_Short_Unsigned) then
         Lib_RE := RE_TA_SU;

      elsif U_Type = RTE (RE_Unsigned) then
         Lib_RE := RE_TA_U;

      elsif U_Type = RTE (RE_Long_Unsigned) then
         Lib_RE := RE_TA_LU;

      elsif U_Type = RTE (RE_Long_Long_Unsigned) then
         Lib_RE := RE_TA_LLU;

      --  Access types

--        elsif Is_Access_Type (U_Type) then
--           if P_Size > System_Address_Size then
--              Lib_RE := RE_TA_AD;
--           else
--              Lib_RE := RE_TA_AS;
--           end if;

      elsif U_Type = Standard_String then
         Lib_RE := RE_TA_String;

      elsif U_Type = Underlying_Type (RTE (RE_TypeCode)) then
         Lib_RE := RE_TA_TC;

      --  Other (non-primitive) types

      else
         declare
            Decl : Entity_Id;
         begin
            Build_To_Any_Function (Loc, U_Type, Decl, Fnam);
            Append_To (Decls, Decl);
         end;
      end if;

      --  Call the function

      if Lib_RE /= RE_Null then
         pragma Assert (No (Fnam));
         Fnam := RTE (Lib_RE);
      end if;

      return
          Make_Function_Call (Loc,
            Name => New_Occurrence_Of (Fnam, Loc),
            Parameter_Associations => New_List (N));
   end Build_To_Any_Call;

   ---------------------------
   -- Build_To_Any_Function --
   ---------------------------

   procedure Build_To_Any_Function
     (Loc  :     Source_Ptr;
      Typ  :     Entity_Id;
      Decl : out Node_Id;
      Fnam : out Entity_Id)
   is
      Spec : Node_Id;
      Decls : constant List_Id := New_List;
      Stms : constant List_Id := New_List;
      Expr_Parameter : constant Entity_Id
        := Make_Defining_Identifier (Loc, Name_E);
      Any : constant Entity_Id :=
        Make_Defining_Identifier (Loc, Name_A);
      Any_Decl : Node_Id;
      Result_TC : Node_Id := Build_TypeCode_Call (Loc, Typ, Decls);
   begin
      Fnam := Make_Stream_Procedure_Function_Name (Loc, Typ, Name_uTo_Any);

      Spec :=
        Make_Function_Specification (Loc,
          Defining_Unit_Name => Fnam,
          Parameter_Specifications => New_List (
            Make_Parameter_Specification (Loc,
              Defining_Identifier =>
                Expr_Parameter,
              Parameter_Type =>
                New_Occurrence_Of (Typ, Loc))),
          Subtype_Mark => New_Occurrence_Of (RTE (RE_Any), Loc));
      Set_Etype (Expr_Parameter, Typ);

      Any_Decl :=
        Make_Object_Declaration (Loc,
          Defining_Identifier =>
            Any,
          Object_Definition   =>
            New_Occurrence_Of (RTE (RE_Any), Loc));

      if Is_Derived_Type (Typ)
        and then not Is_Tagged_Type (Typ)
      then
         declare
            Rt_Type : constant Entity_Id
              := Root_Type (Typ);
            Expr : constant Node_Id
              := OK_Convert_To (
                   Rt_Type,
                   New_Occurrence_Of (Expr_Parameter, Loc));

         begin
            Set_Expression (Any_Decl, Build_To_Any_Call (Expr, Decls));
         end;
      elsif Is_Record_Type (Typ)
        and then not Is_Tagged_Type (Typ)
      then
         if Nkind (Declaration_Node (Typ)) = N_Subtype_Declaration then
            declare
               Rt_Type : constant Entity_Id
                 := Etype (Typ);
               Expr : constant Node_Id
                 := OK_Convert_To (
                      Rt_Type,
                      New_Occurrence_Of (Expr_Parameter, Loc));

            begin
               Set_Expression (Any_Decl, Build_To_Any_Call (Expr, Decls));
            end;
         else
            declare
               Disc : Entity_Id := Empty;
               Rdef : constant Node_Id :=
                 Type_Definition (Declaration_Node (Typ));
               Counter : Int := 0;
               Elements : List_Id := New_List;

               procedure TA_Rec_Add_Process_Element
                 (Stmts     :        List_Id;
                  Container :        Node_Or_Entity_Id;
                  Counter   : in out Int;
                  Rec       :        Entity_Id;
                  Field     :        Node_Id);

               procedure TA_Append_Record_Traversal is
                  new Append_Record_Traversal
                    (Rec                 => Expr_Parameter,
                     Add_Process_Element => TA_Rec_Add_Process_Element);

               procedure TA_Rec_Add_Process_Element
                 (Stmts     :        List_Id;
                  Container :        Node_Or_Entity_Id;
                  Counter   : in out Int;
                  Rec       :        Entity_Id;
                  Field     :        Node_Id)
               is
                  Field_Ref : Node_Id;
               begin
                  if Nkind (Field) = N_Defining_Identifier then

                     --  A regular component.

                     Field_Ref := Make_Selected_Component (Loc,
                       Prefix => New_Occurrence_Of (Rec, Loc),
                       Selector_Name => New_Occurrence_Of (Field, Loc));
                     Set_Etype (Field_Ref, Etype (Field));

                     Append_To (Stmts,
                       Make_Procedure_Call_Statement (Loc,
                         Name =>
                           New_Occurrence_Of (
                             RTE (RE_Add_Aggregate_Element), Loc),
                         Parameter_Associations => New_List (
                           New_Occurrence_Of (Any, Loc),
                           Build_To_Any_Call (Field_Ref, Decls))));

                  else

                     --  A variant part

                     declare
                        Variant : Node_Id;
                        Struct_Counter : Int := 0;

                        Block_Decls : constant List_Id := New_List;
                        Block_Stmts : constant List_Id := New_List;
                        VP_Stmts    : List_Id;

                        Alt_List : constant List_Id := New_List;
                        Choice_List : List_Id;

                        Union_Any : constant Entity_Id
                          := Make_Defining_Identifier (Loc,
                               New_Internal_Name ('U'));
                        Struct_Any : constant Entity_Id
                          := Make_Defining_Identifier (Loc,
                               New_Internal_Name ('S'));

                        function Make_Discriminant_Reference return Node_Id;
                        --  Build a selected component for the discriminant
                        --  of this variant part.

                        function Make_Discriminant_Reference return Node_Id is
                           Nod : constant Node_Id
                             := Make_Selected_Component (Loc,
                                  Prefix =>
                                    New_Occurrence_Of (Rec, Loc),
                                  Selector_Name =>
                                    New_Occurrence_Of (
                                      Entity (Name (Field)), Loc));
                        begin
                           Set_Etype (Nod, Name (Field));
                           return Nod;
                        end Make_Discriminant_Reference;

                     begin
                        Append_To (Stmts,
                          Make_Block_Statement (Loc,
                            Declarations =>
                              Block_Decls,
                            Handled_Statement_Sequence =>
                              Make_Handled_Sequence_Of_Statements (Loc,
                                Statements => Block_Stmts)));

                        Append_To (Block_Decls,
                          Make_Object_Declaration (Loc,
                            Defining_Identifier => Union_Any,
                            Object_Definition   =>
                              New_Occurrence_Of (RTE (RE_Any), Loc),
                            Expression =>
                              Make_Function_Call (Loc,
                                Name =>
                                  New_Occurrence_Of (RTE (RE_Create_Any), Loc),
                                Parameter_Associations => New_List (
                                  Make_Function_Call (Loc,
                                    Name =>
                                      New_Occurrence_Of (
                                        RTE (RE_Any_Member_Type), Loc),
                                    Parameter_Associations => New_List (
                                      New_Occurrence_Of (Container, Loc),
                                      Make_Integer_Literal (Loc,
                                        Counter)))))));

                        Append_To (Block_Decls,
                          Make_Object_Declaration (Loc,
                            Defining_Identifier => Struct_Any,
                            Object_Definition   =>
                              New_Occurrence_Of (RTE (RE_Any), Loc),
                            Expression =>
                              Make_Function_Call (Loc,
                                Name =>
                                  New_Occurrence_Of (RTE (RE_Create_Any), Loc),
                                Parameter_Associations => New_List (
                                  Make_Function_Call (Loc,
                                    Name =>
                                      New_Occurrence_Of (
                                        RTE (RE_Any_Member_Type), Loc),
                                    Parameter_Associations => New_List (
                                      New_Occurrence_Of (Union_Any, Loc),
                                      Make_Integer_Literal (Loc, Uint_0)))))));

                        Append_To (Block_Stmts,
                          Make_Case_Statement (Loc,
                              Expression =>
                                Make_Discriminant_Reference,
                              Alternatives =>
                                Alt_List));

                        Variant := First_Non_Pragma (Variants (Field));

                        while Present (Variant) loop
                           Choice_List := New_Copy_List_Tree
                             (Discrete_Choices (Variant));

                           VP_Stmts := New_List;
                           TA_Append_Record_Traversal (
                             Stmts     => VP_Stmts,
                             Clist     => Component_List (Variant),
                             Container => Struct_Any,
                             Counter   => Struct_Counter);

                           --  Append discriminant value and inner struct
                           --  to union aggregate.

                           Append_To (VP_Stmts,
                              Make_Procedure_Call_Statement (Loc,
                                Name =>
                                  New_Occurrence_Of (
                                    RTE (RE_Add_Aggregate_Element), Loc),
                                Parameter_Associations => New_List (
                                  New_Occurrence_Of (Union_Any, Loc),
                                    Build_To_Any_Call (
                                      Make_Discriminant_Reference,
                                      Block_Decls))));

                           Append_To (VP_Stmts,
                             Make_Procedure_Call_Statement (Loc,
                               Name =>
                                 New_Occurrence_Of (
                                   RTE (RE_Add_Aggregate_Element), Loc),
                               Parameter_Associations => New_List (
                                 New_Occurrence_Of (Union_Any, Loc),
                                 New_Occurrence_Of (Struct_Any, Loc))));

                           --  Append union to outer aggregate.

                           Append_To (VP_Stmts,
                             Make_Procedure_Call_Statement (Loc,
                               Name =>
                                 New_Occurrence_Of (
                                   RTE (RE_Add_Aggregate_Element), Loc),
                               Parameter_Associations => New_List (
                                 New_Occurrence_Of (Container, Loc),
                                 Make_Function_Call (Loc,
                                   Name => New_Occurrence_Of (
                                     RTE (RE_Any_Aggregate_Build), Loc),
                                   Parameter_Associations => New_List (
                                     New_Occurrence_Of (Union_Any, Loc))))));

                           Append_To (Alt_List,
                             Make_Case_Statement_Alternative (Loc,
                               Discrete_Choices => Choice_List,
                               Statements =>
                                 VP_Stmts));
                           Next_Non_Pragma (Variant);
                        end loop;
                     end;
                  end if;
               end TA_Rec_Add_Process_Element;

            begin

               --  First all discriminants

               if Has_Discriminants (Typ) then
                  Disc := First_Discriminant (Typ);

                  while Present (Disc) loop
                     Append_To (Elements,
                       Make_Component_Association (Loc,
                         Choices => New_List (
                           Make_Integer_Literal (Loc, Counter)),
                         Expression =>
                           Build_To_Any_Call (
                             Make_Selected_Component (Loc,
                               Prefix =>
                                 New_Occurrence_Of (Expr_Parameter, Loc),
                               Selector_Name =>
                                 New_Occurrence_Of (Disc, Loc)),
                             Decls)));
                     Counter := Counter + 1;
                     Next_Discriminant (Disc);
                  end loop;

               else

                  --  Make elements an empty array

                  declare
                     Dummy_Any : constant Entity_Id :=
                       Make_Defining_Identifier (Loc,
                         New_Internal_Name ('A'));
                  begin
                     Append_To (Decls,
                       Make_Object_Declaration (Loc,
                         Defining_Identifier => Dummy_Any,
                         Object_Definition   =>
                           New_Occurrence_Of (RTE (RE_Any), Loc)));

                     Append_To (Elements,
                       Make_Component_Association (Loc,
                         Choices => New_List (
                           Make_Range (Loc,
                             Low_Bound  =>
                               Make_Integer_Literal (Loc, 1),
                             High_Bound =>
                               Make_Integer_Literal (Loc, 0))),
                         Expression =>
                           New_Occurrence_Of (Dummy_Any, Loc)));
                  end;
               end if;

               Set_Expression (Any_Decl,
                 Make_Function_Call (Loc,
                   Name =>
                     New_Occurrence_Of (RTE (RE_Any_Aggregate_Build), Loc),
                   Parameter_Associations => New_List (
                     Result_TC,
                     Make_Aggregate (Loc,
                       Component_Associations => Elements))));
               Result_TC := Empty;

               --  ... then all components

               TA_Append_Record_Traversal (Stms,
                 Clist     => Component_List (Rdef),
                 Container => Any,
                 Counter   => Counter);
            end;
         end if;

      elsif Is_Array_Type (Typ) then

         declare

            Constrained : constant Boolean := Is_Constrained (Typ);

            procedure TA_Ary_Add_Process_Element
              (Stmts   : List_Id;
               Any     : Entity_Id;
               Counter : Entity_Id;
               Datum   : Node_Id);

            procedure TA_Ary_Add_Process_Element
              (Stmts   : List_Id;
               Any     : Entity_Id;
               Counter : Entity_Id;
               Datum   : Node_Id)
            is
               pragma Unreferenced (Counter);

               Element_Any : Node_Id;
            begin
               if Etype (Datum) = RTE (RE_Any) then
                  Element_Any := Datum;
               else
                  Element_Any := Build_To_Any_Call (Datum, Decls);
               end if;

               Append_To (Stmts,
                 Make_Procedure_Call_Statement (Loc,
                   Name =>
                     New_Occurrence_Of (RTE (RE_Add_Aggregate_Element), Loc),
                   Parameter_Associations => New_List (
                     New_Occurrence_Of (Any, Loc),
                     Element_Any)));
            end TA_Ary_Add_Process_Element;

            procedure Append_To_Any_Array_Iterator is
              new Append_Array_Traversal (
                Subprogram => Fnam,
                Arry       => Expr_Parameter,
                Indices    => New_List,
                Add_Process_Element => TA_Ary_Add_Process_Element);

            Index : Node_Id;
         begin
            Set_Expression (Any_Decl,
              Make_Function_Call (Loc,
                Name =>
                  New_Occurrence_Of (RTE (RE_Create_Any), Loc),
                Parameter_Associations => New_List (Result_TC)));
            Result_TC := Empty;

            if not Constrained then
               Index := First_Index (Typ);
               for J in 1 .. Number_Dimensions (Typ) loop
                  Append_To (Stms,
                    Make_Procedure_Call_Statement (Loc,
                      Name =>
                        New_Occurrence_Of (
                          RTE (RE_Add_Aggregate_Element), Loc),
                      Parameter_Associations => New_List (
                        New_Occurrence_Of (Any, Loc),
                        Build_To_Any_Call (
                          OK_Convert_To (Etype (Index),
                            Make_Attribute_Reference (Loc,
                              Prefix         =>
                                New_Occurrence_Of (Expr_Parameter, Loc),
                              Attribute_Name => Name_First,
                              Expressions    => New_List (
                                Make_Integer_Literal (Loc, J)))),
                          Decls))));
                  Next_Index (Index);
               end loop;
            end if;

            Append_To_Any_Array_Iterator (Stms, Any);

         end;
      elsif Is_Integer_Type (Typ) or else Is_Unsigned_Type (Typ) then
         Set_Expression (Any_Decl,
           Build_To_Any_Call (
             OK_Convert_To (
               Find_Numeric_Representation (Typ),
               New_Occurrence_Of (Expr_Parameter, Loc)),
             Decls));
      else

         --  Default: type is represented as an opaque sequence of bytes.

         declare
            Strm : constant Entity_Id := Make_Defining_Identifier (Loc,
              New_Internal_Name ('S'));
         begin

            --  Strm : aliased Buffer_Stream_Type;

            Append_To (Decls,
              Make_Object_Declaration (Loc,
                Defining_Identifier =>
                  Strm,
                Aliased_Present     =>
                  True,
                Object_Definition   =>
                  New_Occurrence_Of (RTE (RE_Buffer_Stream_Type), Loc)));

            --  Allocate_Buffer (Strm);

            Append_To (Stms,
              Make_Procedure_Call_Statement (Loc,
                Name =>
                  New_Occurrence_Of (RTE (RE_Allocate_Buffer), Loc),
                Parameter_Associations => New_List (
                  New_Occurrence_Of (Strm, Loc))));

            --  T'Output (Strm'Access, E);

            Append_To (Stms,
                Make_Attribute_Reference (Loc,
                  Prefix         => New_Occurrence_Of (Typ, Loc),
                  Attribute_Name => Name_Output,
                  Expressions => New_List (
                    Make_Attribute_Reference (Loc,
                      Prefix => New_Occurrence_Of (Strm, Loc),
                      Attribute_Name => Name_Access),
                    New_Occurrence_Of (Expr_Parameter, Loc))));

            --  BS_To_Any (Strm, A);

            Append_To (Stms,
              Make_Procedure_Call_Statement (Loc,
                Name =>
                  New_Occurrence_Of (RTE (RE_BS_To_Any), Loc),
                Parameter_Associations => New_List (
                  New_Occurrence_Of (Strm, Loc),
                  New_Occurrence_Of (Any, Loc))));

            --  Release_Buffer (Strm);

            Append_To (Stms,
              Make_Procedure_Call_Statement (Loc,
                Name =>
                  New_Occurrence_Of (RTE (RE_Release_Buffer), Loc),
                Parameter_Associations => New_List (
                  New_Occurrence_Of (Strm, Loc))));

         end;
      end if;

      Append_To (Decls, Any_Decl);

      if Present (Result_TC) then
         Append_To (Stms,
           Make_Procedure_Call_Statement (Loc,
             Name => New_Occurrence_Of (RTE (RE_Set_TC), Loc),
             Parameter_Associations => New_List (
               New_Occurrence_Of (Any, Loc),
               Result_TC)));
      end if;

      Append_To (Stms,
        Make_Return_Statement (Loc,
          Expression => New_Occurrence_Of (Any, Loc)));

      Decl :=
        Make_Subprogram_Body (Loc,
          Specification => Spec,
          Declarations => Decls,
          Handled_Statement_Sequence =>
            Make_Handled_Sequence_Of_Statements (Loc,
              Statements => Stms));
   end Build_To_Any_Function;

   -------------------------
   -- Build_TypeCode_Call --
   -------------------------

   function Build_TypeCode_Call
     (Loc   : Source_Ptr;
      Typ   : Entity_Id;
      Decls : List_Id)
      return Node_Id
   is
      U_Type  : Entity_Id  := Underlying_Type (Typ);
      --  The full view, if Typ is private; the completion,
      --  if Typ is incomplete.

      --  Rt_Type : constant Entity_Id  := Root_Type (U_Type);
      --  FST     : constant Entity_Id  := First_Subtype (U_Type);
      --  P_Size  : constant Uint       := Esize (FST);

      Fnam : Entity_Id := Empty;
      Tnam : Entity_Id := Empty;
      Pnam : Entity_Id := Empty;
      Args : List_Id := Empty_List;
      Lib_RE  : RE_Id := RE_Null;

      Expr : Node_Id;
   begin

      --  Special case System.PolyORB.Interface.Any: its primitives
      --  have not been set yet, so can't call Find_Inherited_TSS.

      if Typ = RTE (RE_Any) then
         Fnam := RTE (RE_TC_Any);
      else

         --  First simple case where the TypeCode is present
         --  in the type's TSS.

         Fnam := Find_Inherited_TSS (U_Type, Name_uTypeCode);

         if Present (Fnam) then
            --  When a TypeCode TSS exists, it has a single parameter
            --  that is an anonymous access to the corresponding type.
            --  This parameter is not used in any way; its purpose is
            --  solely to provide overloading of the TSS.

            Tnam := Make_Defining_Identifier (Loc,
                      New_Internal_Name ('T'));
            Pnam := Make_Defining_Identifier (Loc,
                      New_Internal_Name ('P'));

            Append_To (Decls,
              Make_Full_Type_Declaration (Loc,
                Defining_Identifier => Tnam,
                Type_Definition =>
                  Make_Access_To_Object_Definition (Loc,
                    Subtype_Indication =>
                      New_Occurrence_Of (U_Type, Loc))));
            Append_To (Decls,
              Make_Object_Declaration (Loc,
                Defining_Identifier => Pnam,
                Constant_Present    => True,
                Object_Definition   => New_Occurrence_Of (Tnam, Loc),
                Expression          => Make_Null (Loc)));
            --  Use a variable here to force proper freezing of Tnam.

            Args := New_List (New_Occurrence_Of (Pnam, Loc));
            --  Normally, calling _TypeCode with a null access parameter
            --  should raise Constraint_Error, but this check is suppressed
            --  for expanded code, and we do not care anyway because we do not
            --  actually ever use this value.


         end if;
      end if;

      if No (Fnam) then
         if Sloc (U_Type) <= Standard_Location then
            U_Type := Base_Type (U_Type);
            --  Do not try to build alias typecodes for subtypes from Standard.
         end if;

         if Is_Itype (U_Type) then
            return Build_TypeCode_Call
              (Loc, Associated_Node_For_Itype (U_Type), Decls);
         end if;

         if U_Type = Standard_Boolean then
            Lib_RE := RE_TC_B;

         elsif U_Type = Standard_Character then
            Lib_RE := RE_TC_C;

         elsif U_Type = Standard_Wide_Character then
            Lib_RE := RE_TC_WC;

         --  Floating point types

         elsif U_Type = Standard_Short_Float then
            Lib_RE := RE_TC_SF;

         elsif U_Type = Standard_Float then
            Lib_RE := RE_TC_F;

         elsif U_Type = Standard_Long_Float then
            Lib_RE := RE_TC_LF;

         elsif U_Type = Standard_Long_Long_Float then
            Lib_RE := RE_TC_LLF;

         --  Integer types (walk back to the base type)

         elsif U_Type = Etype (Standard_Short_Short_Integer) then
               Lib_RE := RE_TC_SSI;

         elsif U_Type = Etype (Standard_Short_Integer) then
            Lib_RE := RE_TC_SI;

         elsif U_Type = Etype (Standard_Integer) then
            Lib_RE := RE_TC_I;

         elsif U_Type = Etype (Standard_Long_Integer) then
            Lib_RE := RE_TC_LI;

         elsif U_Type = Etype (Standard_Long_Long_Integer) then
            Lib_RE := RE_TC_LLI;

         --  Unsigned integer types

         elsif U_Type = RTE (RE_Short_Short_Unsigned) then
            Lib_RE := RE_TC_SSU;

         elsif U_Type = RTE (RE_Short_Unsigned) then
            Lib_RE := RE_TC_SU;

         elsif U_Type = RTE (RE_Unsigned) then
            Lib_RE := RE_TC_U;

         elsif U_Type = RTE (RE_Long_Unsigned) then
            Lib_RE := RE_TC_LU;

         elsif U_Type = RTE (RE_Long_Long_Unsigned) then
            Lib_RE := RE_TC_LLU;

         --  Access types

   --        elsif Is_Access_Type (U_Type) then
   --           if P_Size > System_Address_Size then
   --              Lib_RE := RE_TC_AD;
   --           else
   --              Lib_RE := RE_TC_AS;
   --           end if;

         elsif U_Type = Standard_String then
            Lib_RE := RE_TC_String;

         --  Other (non-primitive) types

         else
            declare
               Decl : Entity_Id;
            begin
               Build_TypeCode_Function (Loc, U_Type, Decl, Fnam);
               Append_To (Decls, Decl);
            end;
         end if;

         if Lib_RE /= RE_Null then
            Fnam := RTE (Lib_RE);
         end if;
      end if;

      --  Call the function

      Expr := Make_Function_Call (Loc,
                Name => New_Occurrence_Of (Fnam, Loc),
                Parameter_Associations => Args);

      Set_Etype (Expr, RTE (RE_TypeCode));
      --  Allows Expr to be used as an argument to
      --  Build_To_Any_Call immediately.

      return Expr;
   end Build_TypeCode_Call;

   -----------------------------
   -- Build_TypeCode_Function --
   -----------------------------

   procedure Build_TypeCode_Function
     (Loc  :     Source_Ptr;
      Typ  :     Entity_Id;
      Decl : out Node_Id;
      Fnam : out Entity_Id)
   is
      Spec : Node_Id;
      Decls : constant List_Id := New_List;
      Stms : constant List_Id := New_List;

      TCNam : constant Entity_Id
        := Make_Stream_Procedure_Function_Name
        (Loc, Typ, Name_uTypeCode);


      Parameters : List_Id;

      procedure Add_String_Parameter
        (S              : String_Id;
         Parameter_List : List_Id);
      --  Add a literal for S to Parameters.

      procedure Add_TypeCode_Parameter
        (TC_Node        : Node_Id;
         Parameter_List : List_Id);
      --  Add the typecode for Typ to Parameters.

      procedure Add_Long_Parameter
        (Expr_Node      : Node_Id;
         Parameter_List : List_Id);
      --  Add a signed long integer expression to Parameters.

      procedure Initialize_Parameter_List
        (Name_String    : String_Id;
         Repo_Id_String : String_Id;
         Parameter_List : out List_Id);
      --  Return a list that contains the first two parameters
      --  for a parameterized typecode: name and repository id.

      function Make_Constructed_TypeCode
        (Kind : Entity_Id;
         Parameters : List_Id)
         return Node_Id;
      --  Call TC_Build with the given kind and parameters.

      procedure Return_Constructed_TypeCode (Kind : Entity_Id);
      --  Make a return statement that calls TC_Build with
      --  the given typecode kind, and the constructed parameters
      --  list.

      procedure Return_Alias_TypeCode
        (Base_TypeCode  : Node_Id);
      --  Return a typecode that is a TC_Alias for the given
      --  typecode.

      procedure Add_String_Parameter
        (S              : String_Id;
         Parameter_List : List_Id)
      is
      begin
         Append_To (Parameter_List,
           Make_Function_Call (Loc,
             Name =>
               New_Occurrence_Of (RTE (RE_TA_String), Loc),
             Parameter_Associations => New_List (
               Make_String_Literal (Loc, S))));
      end Add_String_Parameter;

      procedure Add_TypeCode_Parameter
        (TC_Node        : Node_Id;
         Parameter_List : List_Id)
      is
      begin
         Append_To (Parameter_List,
           Make_Function_Call (Loc,
             Name =>
               New_Occurrence_Of (RTE (RE_TA_TC), Loc),
             Parameter_Associations => New_List (
               TC_Node)));
      end Add_TypeCode_Parameter;

      procedure Add_Long_Parameter
        (Expr_Node      : Node_Id;
         Parameter_List : List_Id)
      is
      begin
         Append_To (Parameter_List,
           Make_Function_Call (Loc,
             Name =>
               New_Occurrence_Of (RTE (RE_TA_LI), Loc),
             Parameter_Associations => New_List (Expr_Node)));
      end Add_Long_Parameter;

      procedure Initialize_Parameter_List
        (Name_String    : String_Id;
         Repo_Id_String : String_Id;
         Parameter_List : out List_Id)
      is
      begin
         Parameter_List := New_List;
         Add_String_Parameter (Name_String, Parameter_List);
         Add_String_Parameter (Repo_Id_String, Parameter_List);
      end Initialize_Parameter_List;

      procedure Return_Alias_TypeCode
        (Base_TypeCode  : Node_Id)
      is
      begin
         Add_TypeCode_Parameter (Base_TypeCode, Parameters);
         Return_Constructed_TypeCode (RTE (RE_TC_Alias));
      end Return_Alias_TypeCode;

      function Make_Constructed_TypeCode
        (Kind : Entity_Id;
         Parameters : List_Id)
         return Node_Id
      is
         Constructed_TC : constant Node_Id :=
           Make_Function_Call (Loc,
             Name =>
               New_Occurrence_Of (RTE (RE_TC_Build), Loc),
             Parameter_Associations => New_List (
               New_Occurrence_Of (Kind, Loc),
               Make_Aggregate (Loc,
                  Expressions => Parameters)));
      begin
         Set_Etype (Constructed_TC, RTE (RE_TypeCode));
         return Constructed_TC;
      end Make_Constructed_TypeCode;

      procedure Return_Constructed_TypeCode (Kind : Entity_Id) is
      begin
         Append_To (Stms,
           Make_Return_Statement (Loc,
             Expression =>
                Make_Constructed_TypeCode (Kind, Parameters)));
      end Return_Constructed_TypeCode;

      ------------------
      -- Record types --
      ------------------

      procedure TC_Rec_Add_Process_Element
        (Params  : List_Id;
         Any     : Entity_Id;
         Counter : in out Int;
         Rec     : Entity_Id;
         Field   : Node_Id);

      procedure TC_Append_Record_Traversal is new Append_Record_Traversal (
        Rec                 => Empty,
        Add_Process_Element => TC_Rec_Add_Process_Element);

      procedure TC_Rec_Add_Process_Element
        (Params  : List_Id;
         Any     : Entity_Id;
         Counter : in out Int;
         Rec     : Entity_Id;
         Field   : Node_Id)
      is
         pragma Unreferenced (Any, Counter, Rec);
      begin
         if Nkind (Field) = N_Defining_Identifier then

            --  A regular component.

            Add_TypeCode_Parameter (
              Build_TypeCode_Call (Loc, Etype (Field), Decls), Params);
            Get_Name_String (Chars (Field));
            Add_String_Parameter (String_From_Name_Buffer, Params);

         else

            --  A variant part.

            declare
               Discriminant_Type : constant Entity_Id
                 := Etype (Name (Field));
               Is_Enum : constant Boolean
                 := Is_Enumeration_Type (Discriminant_Type);

               Union_TC_Params  : List_Id;
               U_Name : constant Name_Id
                 := New_External_Name (Chars (Typ), 'U', -1);
               Name_Str : String_Id;

               Struct_TC_Params : List_Id;

               Variant : Node_Id;
               Choice  : Node_Id;
               Default : constant Node_Id := Make_Integer_Literal (Loc, -1);
               Dummy_Counter : Int := 0;

               procedure Add_Params_For_Variant_Components;
               --  Add a struct TypeCode and a corresponding member name
               --  to the union parameter list.

               procedure Add_Params_For_Variant_Components
               is
                  S_Name : constant Name_Id
                    := New_External_Name (U_Name, 'S', -1);
               begin
                  Get_Name_String (S_Name);
                  Name_Str := String_From_Name_Buffer;
                  Initialize_Parameter_List
                    (Name_Str, Name_Str, Struct_TC_Params);

                  --  Build struct parameters

                  TC_Append_Record_Traversal (Struct_TC_Params,
                    Component_List (Variant),
                    Empty,
                    Dummy_Counter);

                  Add_TypeCode_Parameter
                    (Make_Constructed_TypeCode
                     (RTE (RE_TC_Struct), Struct_TC_Params),
                     Union_TC_Params);

                  Add_String_Parameter (Name_Str, Union_TC_Params);
               end Add_Params_For_Variant_Components;

            begin
               Get_Name_String (U_Name);
               Name_Str := String_From_Name_Buffer;

               Initialize_Parameter_List
                 (Name_Str, Name_Str, Union_TC_Params);

               Add_String_Parameter (Name_Str, Params);
               Add_TypeCode_Parameter
                 (Make_Constructed_TypeCode
                  (RTE (RE_TC_Union), Union_TC_Params),
                  Parameters);
               --  Add union in enclosing parameter list.

               --  Build union parameters

               Add_TypeCode_Parameter (Discriminant_Type, Union_TC_Params);
               Add_Long_Parameter (Default, Union_TC_Params);

               Variant := First_Non_Pragma (Variants (Field));
               while Present (Variant) loop
                  Choice := First (Discrete_Choices (Variant));
                  while Present (Choice) loop
                     case Nkind (Choice) is
                        when N_Range =>
                           declare
                              L : constant Uint
                                := Expr_Value (Low_Bound (Choice));
                              H : constant Uint
                                := Expr_Value (High_Bound (Choice));
                              J : Uint := L;
                              --  3.8.1(8) guarantees that the bounds of
                              --  this range are static.

                              Expr : Node_Id;
                           begin
                              while J <= H loop
                                 if Is_Enum then
                                    Expr := New_Occurrence_Of (
                                      Get_Enum_Lit_From_Pos (
                                        Discriminant_Type, J, Loc), Loc);
                                 else
                                    Expr := Make_Integer_Literal (Loc, J);
                                 end if;
                                 Append_To (Union_TC_Params,
                                   Build_To_Any_Call (Expr, Decls));
                                 Add_Params_For_Variant_Components;
                                 J := J + Uint_1;
                              end loop;
                           end;

                        when N_Others_Choice =>
                           Add_Long_Parameter (
                             Make_Integer_Literal (Loc, 0),
                             Union_TC_Params);
                           Add_Params_For_Variant_Components;

                        when others =>
                           Append_To (Union_TC_Params,
                             Build_To_Any_Call (Choice, Decls));
                           Add_Params_For_Variant_Components;

                     end case;

                  end loop;

                  Next_Non_Pragma (Variant);
               end loop;

            end;
         end if;
      end TC_Rec_Add_Process_Element;

      Type_Name_Str : String_Id;
   begin
      pragma Assert (not Is_Itype (Typ));
      Fnam := TCNam;

      Spec :=
        Make_Function_Specification (Loc,
          Defining_Unit_Name => Fnam,
          Parameter_Specifications => Empty_List,
          Subtype_Mark => New_Occurrence_Of (RTE (RE_TypeCode), Loc));

      Get_Name_String (Chars
        (Defining_Identifier (Declaration_Node (Typ))));
      Type_Name_Str := String_From_Name_Buffer;
      Initialize_Parameter_List (Type_Name_Str, Type_Name_Str, Parameters);
      --  XXX should compute a proper repository id!

      if Is_Derived_Type (Typ)
        and then not Is_Tagged_Type (Typ)
      then
         declare
            D_Node : constant Node_Id := Declaration_Node (Typ);
            Parent_Type : Entity_Id := Etype (Typ);
         begin

            if Is_Enumeration_Type (Typ)
              and then Nkind (D_Node) = N_Subtype_Declaration
              and then Nkind (Original_Node (D_Node))
              /= N_Subtype_Declaration
            then

               --  Parent_Type is the implicit intermediate base type
               --  created by Build_Derived_Enumeration_Type.

               Parent_Type := Etype (Parent_Type);
            end if;

            Return_Alias_TypeCode (
              Build_TypeCode_Call (Loc, Parent_Type, Decls));
         end;
      elsif Is_Integer_Type (Typ)
        or else Is_Unsigned_Type (Typ)
      then
         Return_Alias_TypeCode (
           Build_TypeCode_Call (Loc,
             Find_Numeric_Representation (Typ), Decls));

      elsif Is_Record_Type (Typ)
        and then not Is_Tagged_Type (Typ)
      then
         if Nkind (Declaration_Node (Typ)) = N_Subtype_Declaration then
            Return_Alias_TypeCode (
              Build_TypeCode_Call (Loc, Etype (Typ), Decls));
         else
            declare
               Disc : Entity_Id := Empty;
               Rdef : constant Node_Id :=
                 Type_Definition (Declaration_Node (Typ));
               Dummy_Counter : Int := 0;
            begin
               --  First all discriminants

               if Has_Discriminants (Typ) then
                  Disc := First_Discriminant (Typ);
               end if;
               while Present (Disc) loop
                  Add_TypeCode_Parameter (
                    Build_TypeCode_Call (Loc, Etype (Disc), Decls),
                    Parameters);
                  Get_Name_String (Chars (Disc));
                  Add_String_Parameter (
                    String_From_Name_Buffer,
                    Parameters);
                  Next_Discriminant (Disc);
               end loop;

               --  ... then all components

               TC_Append_Record_Traversal
                 (Parameters, Component_List (Rdef), Empty, Dummy_Counter);
               Return_Constructed_TypeCode (RTE (RE_TC_Struct));
            end;
         end if;

      elsif Is_Array_Type (Typ) then

         declare
            Ndim : constant Pos := Number_Dimensions (Typ);
            Inner_TypeCode : Node_Id;
            Constrained : constant Boolean := Is_Constrained (Typ);
            Indx : Node_Id := First_Index (Typ);
         begin
            Inner_TypeCode := Build_TypeCode_Call (Loc,
              Component_Type (Typ),
              Decls);

            for J in 1 .. Ndim loop
               if Constrained then
                  Inner_TypeCode := Make_Constructed_TypeCode
                    (RTE (RE_TC_Array), New_List (
                      Build_To_Any_Call (
                        OK_Convert_To (RTE (RE_Long_Unsigned),
                          Make_Attribute_Reference (Loc,
                            Prefix =>
                              New_Occurrence_Of (Typ, Loc),
                            Attribute_Name =>
                              Name_Length,
                            Expressions => New_List (
                              Make_Integer_Literal (Loc, Ndim - J + 1)))),
                        Decls),
                      Build_To_Any_Call (Inner_TypeCode, Decls)));
               else

                  --  Unconstrained case: add low bound for each dimension.

                  Add_TypeCode_Parameter
                    (Build_TypeCode_Call (Loc, Etype (Indx), Decls),
                     Parameters);
                  Get_Name_String (New_External_Name ('L', J));
                  Add_String_Parameter (
                    String_From_Name_Buffer,
                    Parameters);
                  Next_Index (Indx);

                  Inner_TypeCode := Make_Constructed_TypeCode
                    (RTE (RE_TC_Sequence), New_List (
                      Build_To_Any_Call (
                        OK_Convert_To (RTE (RE_Long_Unsigned),
                          Make_Integer_Literal (Loc, 0)),
                        Decls),
                      Build_To_Any_Call (Inner_TypeCode, Decls)));
               end if;
            end loop;

            if Constrained then
               Return_Alias_TypeCode (Inner_TypeCode);
            else
               Add_TypeCode_Parameter (Inner_TypeCode, Parameters);
               Start_String;
               Store_String_Char ('V');
               Add_String_Parameter (End_String, Parameters);
               Return_Constructed_TypeCode (RTE (RE_TC_Struct));
            end if;
         end;

      else

         --  Default: type is represented as an opaque sequence of bytes

         Return_Alias_TypeCode
           (New_Occurrence_Of (RTE (RE_TC_Opaque), Loc));

      end if;

      Decl :=
        Make_Subprogram_Body (Loc,
          Specification => Spec,
          Declarations => Decls,
          Handled_Statement_Sequence =>
            Make_Handled_Sequence_Of_Statements (Loc,
              Statements => Stms));
   end Build_TypeCode_Function;

   ----------------------------------
   -- Compile_Stream_Body_In_Scope --
   ----------------------------------

   procedure Compile_Stream_Body_In_Scope
     (N     : Node_Id;
      Decl  : Node_Id;
      Arr   : Entity_Id;
      Check : Boolean)
   is
      Installed : Boolean := False;
      Scop      : constant Entity_Id := Scope (Arr);
      Curr      : constant Entity_Id := Current_Scope;

   begin
      if Is_Hidden (Arr)
        and then not In_Open_Scopes (Scop)
        and then Ekind (Scop) = E_Package
      then
         New_Scope (Scop);
         Install_Visible_Declarations (Scop);
         Install_Private_Declarations (Scop);
         Installed := True;

         --  The entities in the package are now visible, but the generated
         --  stream entity must appear in the current scope (usually an
         --  enclosing stream function) so that itypes all have their proper
         --  scopes.

         New_Scope (Curr);
      end if;

      if Check then
         Insert_Action (N, Decl);
      else
         Insert_Action (N, Decl, All_Checks);
      end if;

      if Installed then

         --  Remove extra copy of current scope, and package itself

         Pop_Scope;
         End_Package_Scope (Scop);
      end if;
   end Compile_Stream_Body_In_Scope;

   ------------------------
   -- Find_Inherited_TSS --
   ------------------------

   function Find_Inherited_TSS
     (Typ : Entity_Id;
      Nam : Name_Id) return Entity_Id
   is
      P_Type : Entity_Id := Typ;
      Proc   : Entity_Id;

   begin
      Proc :=  TSS (Base_Type (Typ), Nam);

      --  Check first if there is a TSS given for the type itself.

      if Present (Proc) then
         return Proc;
      end if;

      --  If Typ is a derived type, it may inherit attributes from some
      --  ancestor which is not the ultimate underlying one.
      --  If Typ is a derived tagged type, the corresponding primitive
      --  operation has been created explicitly.

      if Is_Derived_Type (P_Type) then
         if Is_Tagged_Type (P_Type) then
            return Find_Prim_Op (P_Type, Nam);
         else
            while Is_Derived_Type (P_Type) loop
               Proc :=  TSS (Base_Type (Etype (Typ)), Nam);

               if Present (Proc) then
                  return Proc;
               else
                  P_Type := Base_Type (Etype (P_Type));
               end if;
            end loop;
         end if;
      end if;

      --  If nothing else, use the TSS of the root type.

      return TSS (Base_Type (Underlying_Type (Typ)), Nam);
   end Find_Inherited_TSS;

   ---------------------------------
   -- Find_Numeric_Representation --
   ---------------------------------

   function Find_Numeric_Representation
     (Typ : Entity_Id)
      return Entity_Id
   is
      FST : constant Entity_Id := First_Subtype (Typ);
      P_Size : constant Uint := Esize (FST);
   begin
      if Is_Unsigned_Type (Typ) then

         if P_Size <= Standard_Short_Short_Integer_Size then
            return RTE (RE_Short_Short_Unsigned);

         elsif P_Size <= Standard_Short_Integer_Size then
            return RTE (RE_Short_Unsigned);

         elsif P_Size <= Standard_Integer_Size then
            return RTE (RE_Unsigned);

         elsif P_Size <= Standard_Long_Integer_Size then
            return RTE (RE_Long_Unsigned);

         else
            return RTE (RE_Long_Long_Unsigned);
         end if;

      elsif Is_Integer_Type (Typ) then

         if P_Size <= Standard_Short_Short_Integer_Size then
            return Standard_Short_Short_Integer;

         elsif P_Size <= Standard_Short_Integer_Size then
            return Standard_Short_Integer;

         elsif P_Size <= Standard_Integer_Size then
            return Standard_Integer;

         elsif P_Size <= Standard_Long_Integer_Size then
            return Standard_Long_Integer;

         else
            return Standard_Long_Long_Integer;
         end if;

      elsif Is_Floating_Point_Type (Typ) then

         if P_Size <= Standard_Short_Float_Size then
            return Standard_Short_Float;

         elsif P_Size <= Standard_Float_Size then
            return Standard_Float;

         elsif P_Size <= Standard_Long_Float_Size then
            return Standard_Long_Float;

         else
            return Standard_Long_Long_Float;
         end if;

      else
         raise Program_Error;
      end if;

      --  XXX fixed point types??
      --  XXX numeric types with a biased representation??

   end Find_Numeric_Representation;

   ---------------------------
   -- Append_Array_Traversal --
   ---------------------------

   procedure Append_Array_Traversal
     (Stmts   : List_Id;
      Any     : Entity_Id;
      Counter : Entity_Id := Empty;
      Depth   : Pos       := 1)
   is
      Loc       : constant Source_Ptr := Sloc (Subprogram);
      Typ       : constant Entity_Id  := Etype (Arry);
      Constrained : constant Boolean := Is_Constrained (Typ);
      Ndim      : constant Pos        := Number_Dimensions (Typ);
      Inner_Any, Inner_Counter : Entity_Id;
      Loop_Stm  : Node_Id;
      Inner_Stmts : constant List_Id := New_List;
   begin
      if Depth > Ndim then

         --  Processing for one element of an array.

         declare
            Element_Expr : constant Node_Id
              := Make_Indexed_Component (Loc,
                   New_Occurrence_Of (Arry, Loc), Indices);
         begin
            Set_Etype (Element_Expr, Component_Type (Typ));
            Add_Process_Element (Stmts,
              Any     => Any,
              Counter => Counter,
              Datum   => Element_Expr);
         end;

         return;
      end if;

      Append_To (Indices,
        Make_Identifier (Loc, New_External_Name ('L', Depth)));

      if Constrained then
         Inner_Any := Any;
         Inner_Counter := Counter;
      else
         Inner_Any := Make_Defining_Identifier (Loc,
           New_External_Name ('A', Depth));
         Set_Etype (Inner_Any, RTE (RE_Any));

         if Present (Counter) then
            Inner_Counter := Make_Defining_Identifier (Loc,
              New_External_Name ('J', Depth));
         else
            Inner_Counter := Empty;
         end if;

      end if;

      Append_Array_Traversal (Inner_Stmts,
        Any     => Inner_Any,
        Counter => Inner_Counter,
        Depth   => Depth + 1);

      Loop_Stm :=
        Make_Implicit_Loop_Statement (Subprogram,
          Iteration_Scheme =>
            Make_Iteration_Scheme (Loc,
              Loop_Parameter_Specification =>
                Make_Loop_Parameter_Specification (Loc,
                  Defining_Identifier =>
                    Make_Defining_Identifier (Loc,
                      Chars => New_External_Name ('L', Depth)),

                  Discrete_Subtype_Definition =>
                    Make_Attribute_Reference (Loc,
                      Prefix         => New_Occurrence_Of (Arry, Loc),
                      Attribute_Name => Name_Range,

                      Expressions => New_List (
                        Make_Integer_Literal (Loc, Depth))))),
          Statements => Inner_Stmts);

      if Constrained then
         Append_To (Stmts, Loop_Stm);
         return;
      end if;

      declare
         Decls : constant List_Id := New_List;
         Dimen_Stmts : constant List_Id := New_List;
         Length_Node : Node_Id;
         Inner_Any_TypeCode : constant Entity_Id :=
           Make_Defining_Identifier (Loc,
             New_External_Name ('T', Depth));
         Inner_Any_TypeCode_Expr : Node_Id;
      begin

         if Depth = 1 then
            Inner_Any_TypeCode_Expr :=
              Make_Function_Call (Loc,
                Name =>
                  New_Occurrence_Of (RTE (RE_Any_Member_Type), Loc),
                Parameter_Associations => New_List (
                  New_Occurrence_Of (Any, Loc),
                  Make_Integer_Literal (Loc, Ndim)));
         else
            Inner_Any_TypeCode_Expr :=
              Make_Function_Call (Loc,
                Name =>
                  New_Occurrence_Of (RTE (RE_Content_Type), Loc),
                Parameter_Associations => New_List (
                  Make_Identifier (Loc, New_External_Name ('T', Depth - 1))));
         end if;

         Append_To (Decls,
           Make_Object_Declaration (Loc,
             Defining_Identifier => Inner_Any_TypeCode,
             Constant_Present    => True,
             Object_Definition   => New_Occurrence_Of (RTE (RE_TypeCode), Loc),
             Expression          => Inner_Any_TypeCode_Expr));
         Append_To (Decls,
           Make_Object_Declaration (Loc,
             Defining_Identifier => Inner_Any,
             Object_Definition   => New_Occurrence_Of (RTE (RE_Any), Loc),
             Expression          =>
               Make_Function_Call (Loc,
                 Name =>
                   New_Occurrence_Of (
                     RTE (RE_Create_Any), Loc),
                 Parameter_Associations => New_List (
                   New_Occurrence_Of (Inner_Any_TypeCode, Loc)))));

         if Present (Inner_Counter) then
            Append_To (Decls,
              Make_Object_Declaration (Loc,
                Defining_Identifier => Inner_Counter,
                Object_Definition   =>
                  New_Occurrence_Of (RTE (RE_Long_Unsigned), Loc),
                Expression          =>
                  Make_Integer_Literal (Loc, 0)));
         end if;

         Length_Node := Make_Attribute_Reference (Loc,
               Prefix         => New_Occurrence_Of (Arry, Loc),
               Attribute_Name => Name_Length,
               Expressions    =>
                 New_List (Make_Integer_Literal (Loc, Depth)));
         Set_Etype (Length_Node, RTE (RE_Long_Unsigned));

         Add_Process_Element (Dimen_Stmts,
           Datum   => Length_Node,
           Any     => Inner_Any,
           Counter => Inner_Counter);

         Append_To (Dimen_Stmts, Loop_Stm);
         --  Loop_Stm does approrpriate processing for each element
         --  of Inner_Any.

         Add_Process_Element (Dimen_Stmts,
           Any     => Any,
           Counter => Counter,
           Datum   => New_Occurrence_Of (Inner_Any, Loc));
         --  Link outer and inner any.

         Append_To (Stmts,
           Make_Block_Statement (Loc,
             Declarations =>
               Decls,
             Handled_Statement_Sequence =>
               Make_Handled_Sequence_Of_Statements (Loc,
                 Statements => Dimen_Stmts)));
      end;

   end Append_Array_Traversal;

   -----------------------------------------
   -- Make_Stream_Procedure_Function_Name --
   -----------------------------------------

   function Make_Stream_Procedure_Function_Name
     (Loc : Source_Ptr;
      Typ : Entity_Id;
      Nam : Name_Id)
      return Entity_Id
   is
   begin
      --  For tagged types, we use a canonical name so that it matches the
      --  primitive spec. For all other cases, we use a serialized name so
      --  that multiple generations of the same procedure do not clash.

      if Is_Tagged_Type (Typ) then
         return Make_Defining_Identifier (Loc, Nam);
      else
         return Make_Defining_Identifier (Loc,
             Chars =>
               New_External_Name (Nam, ' ', Increment_Serial_Number));
      end if;
   end Make_Stream_Procedure_Function_Name;

end Exp_Hlpr;

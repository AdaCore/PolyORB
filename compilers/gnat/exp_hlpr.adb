------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             E X P _ H L P R                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $LastChangedRevision$
--                                                                          --
--          Copyright (C) 1992-2002, Free Software Foundation, Inc.         --
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
with Sem;      use Sem;
with Sem_Ch7;  use Sem_Ch7;
with Sem_Ch8;  use Sem_Ch8;
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
   pragma Unreferenced (Compile_Stream_Body_In_Scope);
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

   -------------------------
   -- Build_From_Any_Call --
   -------------------------

   function Build_From_Any_Call
     (Typ : Entity_Id;
      N   : Node_Id)
      return Node_Id
   is
      Loc : constant Source_Ptr := Sloc (N);
      U_Type  : constant Entity_Id  := Underlying_Type (Typ);

      --  Rt_Type : constant Entity_Id  := Root_Type (U_Type);
      --  FST     : constant Entity_Id  := First_Subtype (U_Type);
      --  P_Size  : constant Uint       := Esize (FST);

      Fnam : Entity_Id := Empty;
      Lib_RE  : RE_Id := RE_Null;

   begin

      --  First simple case where the From_Any function is present
      --  in the type's TSS.

      Fnam := Find_Inherited_TSS (U_Type, Name_uFrom_Any);

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

      elsif U_Type = Standard_Short_Short_Integer then
            Lib_RE := RE_FA_SSI;

      elsif U_Type = Standard_Short_Integer then
         Lib_RE := RE_FA_SI;

      elsif U_Type = Standard_Integer then
         Lib_RE := RE_FA_I;

      elsif U_Type = Standard_Long_Integer then
         Lib_RE := RE_FA_LI;

      elsif U_Type = Standard_Long_Long_Integer then
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
            Append_Freeze_Action (U_Type, Decl);
            Analyze (Decl);
            --  Set_TSS (U_Type, Decl);
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
        := Make_Defining_Identifier (Loc, Name_A);
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
      --  This is supposed to be taken care of by Exp_Dist.Add_RACW_From_Any.

      if Is_Derived_Type (Typ)
        and then not Is_Tagged_Type (Typ)
      then
         declare
            Rt_Type : constant Entity_Id
              := Root_Type (Typ);
         begin
            Append_To (Stms,
              Make_Return_Statement (Loc,
                Expression =>
                  OK_Convert_To (
                    Typ,
                    Build_From_Any_Call (
                      Rt_Type,
                      New_Occurrence_Of (Any_Parameter, Loc)))));
         end;
      else
         declare
            Res_Parameter : constant Entity_Id
              := Make_Defining_Identifier (Loc,
                   New_Internal_Name ('R'));
         begin
            --  XXX dummy placeholder (the any is not initialised).
            Append_To (Decls,
             Make_Object_Declaration (Loc,
               Defining_Identifier =>
                 Res_Parameter,
               Aliased_Present     => False,
               Object_Definition   =>
                 New_Occurrence_Of (Typ, Loc)));
            Append_To (Stms,
              Make_Return_Statement (Loc,
                Expression => New_Occurrence_Of (Res_Parameter, Loc)));
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

   -----------------------
   -- Build_To_Any_Call --
   -----------------------

   function Build_To_Any_Call
     (N : Node_Id)
      return Node_Id
   is
      Loc : constant Source_Ptr := Sloc (N);
      Typ : constant Entity_Id := Etype (N);
      U_Type  : constant Entity_Id  := Underlying_Type (Typ);
      --  The full view, if Typ is private; the completion,
      --  if Typ is incomplete.

      --  Rt_Type : constant Entity_Id  := Root_Type (U_Type);
      --  FST     : constant Entity_Id  := First_Subtype (U_Type);
      --  P_Size  : constant Uint       := Esize (FST);

      Fnam : Entity_Id := Empty;
      Lib_RE  : RE_Id := RE_Null;

   begin

      --  First simple case where the To_Any function is present
      --  in the type's TSS.

      Fnam := Find_Inherited_TSS (U_Type, Name_uTo_Any);

      --  Check first for Boolean and Character. These are enumeration types,
      --  but we treat them specially, since they may require special handling
      --  in the transfer protocol. However, this special handling only applies
      --  if they have standard representation, otherwise they are treated like
      --  any other enumeration type.

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

      elsif U_Type = Standard_Short_Short_Integer then
            Lib_RE := RE_TA_SSI;

      elsif U_Type = Standard_Short_Integer then
         Lib_RE := RE_TA_SI;

      elsif U_Type = Standard_Integer then
         Lib_RE := RE_TA_I;

      elsif U_Type = Standard_Long_Integer then
         Lib_RE := RE_TA_LI;

      elsif U_Type = Standard_Long_Long_Integer then
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

      --  Other (non-primitive) types

      else
         declare
            Decl : Entity_Id;
         begin
            Build_To_Any_Function (Loc, U_Type, Decl, Fnam);
            Append_Freeze_Action (U_Type, Decl);
            Analyze (Decl);
            --  Set_TSS (U_Type, Decl);
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

      if Is_Derived_Type (Typ)
        and then not Is_Tagged_Type (Typ)
      then
         declare
            Rt_Type : constant Entity_Id
              := Root_Type (Typ);
            Any_Parameter : constant Entity_Id
              := Make_Defining_Identifier (Loc,
                   New_Internal_Name ('A'));
            Expr : constant Node_Id
              := OK_Convert_To (
                   Rt_Type,
                   New_Occurrence_Of (Expr_Parameter, Loc));

         begin
            Append_To (Decls,
             Make_Object_Declaration (Loc,
               Defining_Identifier =>
                 Any_Parameter,
               Aliased_Present     => False,
               Object_Definition   =>
                 New_Occurrence_Of (RTE (RE_Any), Loc),
               Expression =>
                 Build_To_Any_Call (Expr)));

            Append_To (Stms,
              Make_Procedure_Call_Statement (Loc,
                Name => New_Occurrence_Of (RTE (RE_Set_TC), Loc),
                Parameter_Associations => New_List (
                  New_Occurrence_Of (Any_Parameter, Loc),
                  Build_TypeCode_Call (Loc, Typ))));
            Append_To (Stms,
              Make_Return_Statement (Loc,
                Expression => New_Occurrence_Of (Any_Parameter, Loc)));
         end;
      else
         declare
            Any_Parameter : constant Entity_Id
              := Make_Defining_Identifier (Loc,
                   New_Internal_Name ('A'));
         begin
            --  XXX dummy placeholder (the any is not initialised).
            Append_To (Decls,
             Make_Object_Declaration (Loc,
               Defining_Identifier =>
                 Any_Parameter,
               Aliased_Present     => False,
               Object_Definition   =>
                 New_Occurrence_Of (RTE (RE_Any), Loc)));
            Append_To (Stms,
              Make_Procedure_Call_Statement (Loc,
                Name => New_Occurrence_Of (RTE (RE_Set_TC), Loc),
                Parameter_Associations => New_List (
                  New_Occurrence_Of (Any_Parameter, Loc),
                  Build_TypeCode_Call (Loc, Typ))));
            Append_To (Stms,
              Make_Return_Statement (Loc,
                Expression => New_Occurrence_Of (Any_Parameter, Loc)));
         end;
      end if;

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
     (Loc : Source_Ptr;
      Typ : Entity_Id)
      return Node_Id
   is
      U_Type  : constant Entity_Id  := Underlying_Type (Typ);
      --  The full view, if Typ is private; the completion,
      --  if Typ is incomplete.

      --  Rt_Type : constant Entity_Id  := Root_Type (U_Type);
      --  FST     : constant Entity_Id  := First_Subtype (U_Type);
      --  P_Size  : constant Uint       := Esize (FST);

      Fnam : Entity_Id := Empty;
      Lib_RE  : RE_Id := RE_Null;

   begin

      --  First simple case where the TypeCode is present
      --  in the type's TSS.

      Fnam := Find_Inherited_TSS (U_Type, Name_uTypeCode);

      --  Check first for Boolean and Character. These are enumeration types,
      --  but we treat them specially, since they may require special handling
      --  in the transfer protocol. However, this special handling only applies
      --  if they have standard representation, otherwise they are treated like
      --  any other enumeration type.

      if Present (Fnam) then
         null;

      elsif U_Type = Standard_Boolean then
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

      --  Integer types

      elsif U_Type = Standard_Short_Short_Integer then
            Lib_RE := RE_TC_SSI;

      elsif U_Type = Standard_Short_Integer then
         Lib_RE := RE_TC_SI;

      elsif U_Type = Standard_Integer then
         Lib_RE := RE_TC_I;

      elsif U_Type = Standard_Long_Integer then
         Lib_RE := RE_TC_LI;

      elsif U_Type = Standard_Long_Long_Integer then
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
            Insert_Action (Declaration_Node (Typ), Decl);
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
            Parameter_Associations => Empty_List);

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

      function Build_TC_Alias_Call
        (Loc            : Source_Ptr;
         Name_String    : String_Id;
         Repo_Id_String : String_Id;
         Base_TypeCode  : Node_Id)
         return Node_Id;
      --  Construct a call to TC_Alias to build a typecode
      --  with the specified name, repository id and base type.

      function Build_TC_Alias_Call
        (Loc            : Source_Ptr;
         Name_String    : String_Id;
         Repo_Id_String : String_Id;
         Base_TypeCode  : Node_Id)
         return Node_Id
      is
      begin
         return Make_Function_Call (Loc,
                  Name =>
                    New_Occurrence_Of (RTE (RE_TC_Build), Loc),
                  Parameter_Associations => New_List (
                    New_Occurrence_Of (RTE (RE_TC_Alias), Loc),
                    Make_Aggregate (Loc,
                      Expressions =>
                        New_List (
                          Make_Function_Call (Loc,
                            Name =>
                              New_Occurrence_Of (RTE (RE_TA_String), Loc),
                            Parameter_Associations => New_List (
                              Make_String_Literal (Loc, Name_String))),
                          Make_Function_Call (Loc,
                            Name =>
                              New_Occurrence_Of (RTE (RE_TA_String), Loc),
                            Parameter_Associations => New_List (
                              Make_String_Literal (Loc, Repo_Id_String))),
                          Make_Function_Call (Loc,
                            Name =>
                              New_Occurrence_Of (RTE (RE_TA_TC), Loc),
                            Parameter_Associations => New_List (
                              Base_TypeCode))))));
      end Build_TC_Alias_Call;

      Name_String : String_Id;
      Repo_Id_String : String_Id;

   begin
      Fnam := Make_Stream_Procedure_Function_Name (Loc, Typ, Name_uTypeCode);

      Spec :=
        Make_Function_Specification (Loc,
          Defining_Unit_Name => Fnam,
          Parameter_Specifications => Empty_List,
          Subtype_Mark => New_Occurrence_Of (RTE (RE_TypeCode), Loc));

      if Is_Derived_Type (Typ)
        and then not Is_Tagged_Type (Typ)
      then
         declare
            D_Node : constant Node_Id := Declaration_Node (Typ);
            Parent_Type : Entity_Id := Etype (Typ);
         begin
            Get_Name_String (Chars
              (Defining_Identifier (Declaration_Node (Typ))));
            Name_String := String_From_Name_Buffer;
            Repo_Id_String := Name_String;
            --  XXX should compute a proper repository id!

            if Is_Enumeration_Type (Typ)
              and then Nkind (D_Node) = N_Subtype_Declaration
              and then Nkind (Original_Node (D_Node))
              /= N_Subtype_Declaration
            then

               --  Parent_Type is the implicit intermediate base type
               --  created by Build_Derived_Enumeration_Type.

               Parent_Type := Etype (Parent_Type);
            end if;

            Append_To (Stms,
              Make_Return_Statement (Loc,
                Expression => Build_TC_Alias_Call (Loc,
                  Name_String,
                  Repo_Id_String,
                  Build_TypeCode_Call (Loc, Parent_Type))));
         end;
      elsif Is_Integer_Type (Typ)
        or else Is_Unsigned_Type (Typ)
      then
         Get_Name_String (Chars
           (Defining_Identifier (Declaration_Node (Typ))));
         Name_String := String_From_Name_Buffer;
         Repo_Id_String := Name_String;
         Append_To (Stms,
           Make_Return_Statement (Loc,
             Expression => Build_TC_Alias_Call (Loc,
               Name_String,
               Repo_Id_String,
               Build_TypeCode_Call (Loc,
                  Find_Numeric_Representation (Typ)))));
      else
         declare
            TypeCode_Parameter : constant Entity_Id
              := Make_Defining_Identifier (Loc,
                   New_Internal_Name ('T'));
         begin
            --  XXX dummy placeholder
            Append_To (Decls,
              Make_Object_Declaration (Loc,
               Defining_Identifier => TypeCode_Parameter,
               Object_Definition   =>
                 New_Occurrence_Of (RTE (RE_TypeCode), Loc)));
            Append_To (Stms,
              Make_Return_Statement (Loc,
                Expression =>
                  New_Occurrence_Of (TypeCode_Parameter, Loc)));
         end;
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

------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             E X P _ S T R M                              --
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
with Sem_Util; use Sem_Util;
with Snames;   use Snames;
with Stand;    use Stand;
with Stringt;  use Stringt;
with Tbuild;   use Tbuild;
with Ttypes;   use Ttypes;
with Exp_Tss;  use Exp_Tss;
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

   function Make_Stream_Procedure_Function_Name
     (Loc : Source_Ptr;
      Typ : Entity_Id;
      Nam : Name_Id)
      return Entity_Id;
   --  Return the name to be assigned for stream subprogram Nam of Typ.
   --  (copied from exp_strm.adb)

   -----------------------
   -- Build_To_Any_Call --
   -----------------------

   function Build_To_Any_Call (E : Entity_Id) return Node_Id is
   begin
      return Empty;
   end Build_To_Any_Call;

   -------------------------
   -- Build_TypeCode_Call --
   -------------------------

   function Build_TypeCode_Call (N : Node_Id) return Node_Id is
      Loc     : constant Source_Ptr := Sloc (N);
      P_Type  : constant Entity_Id  := Entity (N);
      U_Type  : constant Entity_Id  := Underlying_Type (P_Type);
      --  The full view, if P_Type is private; the completion,
      --  if P_Type is incomplete.

      --  Rt_Type : constant Entity_Id  := Root_Type (U_Type);
      FST     : constant Entity_Id  := First_Subtype (U_Type);
      P_Size  : constant Uint       := Esize (FST);

      Pnam : Entity_Id := Empty;
      Lib_RE  : RE_Id := RE_Null;

   begin

      --  First simple case where the TypeCode is present
      --  in the type's TSS.

      Pnam := Find_Helper (N, P_Type, Name_uTypeCode);

      --  Check first for Boolean and Character. These are enumeration types,
      --  but we treat them specially, since they may require special handling
      --  in the transfer protocol. However, this special handling only applies
      --  if they have standard representation, otherwise they are treated like
      --  any other enumeration type.

      if Present (Pnam) then
         null;

      elsif U_Type = Standard_Boolean then
         Lib_RE := RE_TC_B;

      elsif U_Type = Standard_Character then
         Lib_RE := RE_TC_C;

      elsif U_Type = Standard_Wide_Character then
         Lib_RE := RE_TC_WC;

      --  Floating point types

      elsif Is_Floating_Point_Type (U_Type) then

         if U_Type = Standard_Short_Float then
            Lib_RE := RE_TC_SF;

         elsif U_Type = Standard_Float then
            Lib_RE := RE_TC_F;

         elsif U_Type = Standard_Long_Float then
            Lib_RE := RE_TC_LF;

         else pragma Assert (U_Type = Standard_Long_Long_Float);
            Lib_RE := RE_TC_LLF;
         end if;

      --  Signed integer types. Also includes signed fixed-point types and
      --  enumeration types with a signed representation.

      --  Note on signed integer types. We do not consider types as signed for
      --  this purpose if they have no negative numbers, or if they have biased
      --  representation. The reason is that the value in either case basically
      --  represents an unsigned value.

      --  For example, consider:

      --     type W is range 0 .. 2**32 - 1;
      --     for W'Size use 32;

      --  This is a signed type, but the representation is unsigned, and may
      --  be outside the range of a 32-bit signed integer, so this must be
      --  treated as 32-bit unsigned.

      --  Similarly, if we have

      --     type W is range -1 .. +254;
      --     for W'Size use 8;

      --  then the representation is unsigned

      elsif not Is_Unsigned_Type (FST)
        and then
          (Is_Fixed_Point_Type (U_Type)
             or else
           Is_Enumeration_Type (U_Type)
             or else
           (Is_Signed_Integer_Type (U_Type)
              and then not Has_Biased_Representation (FST)))
      then
         if P_Size <= Standard_Short_Short_Integer_Size then
            Lib_RE := RE_TC_SSI;

         elsif P_Size <= Standard_Short_Integer_Size then
            Lib_RE := RE_TC_SI;

         elsif P_Size <= Standard_Integer_Size then
            Lib_RE := RE_TC_I;

         elsif P_Size <= Standard_Long_Integer_Size then
            Lib_RE := RE_TC_LI;

         else
            Lib_RE := RE_TC_LLI;
         end if;

      --  Unsigned integer types, also includes unsigned fixed-point types
      --  and enumeration types with an unsigned representation (note that
      --  we know they are unsigned because we already tested for signed).

      --  Also includes signed integer types that are unsigned in the sense
      --  that they do not include negative numbers. See above for details.

      elsif Is_Modular_Integer_Type    (U_Type)
        or else Is_Fixed_Point_Type    (U_Type)
        or else Is_Enumeration_Type    (U_Type)
        or else Is_Signed_Integer_Type (U_Type)
      then
         if P_Size <= Standard_Short_Short_Integer_Size then
            Lib_RE := RE_TC_SSU;

         elsif P_Size <= Standard_Short_Integer_Size then
            Lib_RE := RE_TC_SU;

         elsif P_Size <= Standard_Integer_Size then
            Lib_RE := RE_TC_U;

         elsif P_Size <= Standard_Long_Integer_Size then
            Lib_RE := RE_TC_LU;

         else
            Lib_RE := RE_TC_LLU;
         end if;

      else pragma Assert (Is_Access_Type (U_Type));
         if P_Size > System_Address_Size then
            Lib_RE := RE_TC_AD;
         else
            Lib_RE := RE_TC_AS;
         end if;
      end if;

      --  Call the function

      if Lib_RE /= RE_Null then
         pragma Assert (No (Pnam));
         Pnam := New_Occurrence_Of (RTE (Lib_RE), Loc);
      end if;

      return
          Make_Function_Call (Loc,
            Name => Pnam,
            Parameter_Associations => Empty_List);

   end Build_TypeCode_Call;

   -----------------------------
   -- Build_TypeCode_Function --
   -----------------------------

   procedure Build_TypeCode_Function
     (Loc : Source_Ptr;
      Typ : Entity_Id;
      Decl : out Node_Id;
      Fnam : out Entity_Id)
   is
      Spec : Node_Id;
      Stms : constant List_Id := New_List;
   begin
      Fnam := Make_Stream_Procedure_Function_Name (Loc, Typ, Name_uTypeCode);

      Spec :=
        Make_Function_Specification (Loc,
          Defining_Unit_Name => Fnam,
          Parameter_Specifications => Empty_List,
          Subtype_Mark => RTE (RE_TypeCode));

      if Is_Derived_Type (Typ)
        and then not Is_Tagged_Type (Typ)
      then
         Get_Name_String (Chars (Defining_Identifier (Typ)));
         declare
            Name_String : constant String_Id := String_From_Name_Buffer;
            Repo_Id_String : String_Id := Name_String;
         begin

            Append_To (Stms,
              Make_Return_Statement (Loc,
                Expression =>
                  Make_Function_Call (Loc,
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
                                Build_TypeCode_Call
                                  (Declaration_Node (Base_Type (Typ)))))))))));
         end;
      else
         Append_To (Stms, Make_Null_Statement (Loc));
      end if;

      Decl :=
        Make_Subprogram_Body (Loc,
          Specification => Spec,
          Declarations => Empty_List,
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

   -----------------
   -- Find_Helper --
   -----------------

   function Find_Helper
     (N : Node_Id;
      Typ : Entity_Id;
      Hnam : Name_Id)
      return Entity_Id
   is
      Loc : constant Source_Ptr := Sloc (N);
      Pname : Entity_Id;
      Decl : Node_Id;
   begin

      Pname := Find_Inherited_TSS (Typ, Hnam);

      if Present (Pname) then
         null;
      elsif Hnam = Name_uTypeCode then
         Build_TypeCode_Function (Loc, Typ, Decl, Pname);
         Insert_Action (N, Decl);
--           Compile_Stream_Body_In_Scope
--             (N, Decl, Typ, Check => True);

--        elsif Hnam = Name_uFrom_Any then
--           Build_From_Any_Function (Typ, Pname);
--        else
--           pragma Assert (Hnam = Name_uto_Any);
--           Build_To_Any_Function (Typ, Pname);
      end if;

      pragma Assert (Present (Pname));
      return Pname;
   end Find_Helper;

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

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

--  with Atree;    use Atree;
with Einfo;    use Einfo;
with Lib;      use Lib;
--  with Namet;    use Namet;
with Nlists;   use Nlists;
with Nmake;    use Nmake;
with Rtsfind;  use Rtsfind;
--  with Sinfo;    use Sinfo;
with Snames;   use Snames;
--  with Stand;    use Stand;
with Tbuild;   use Tbuild;
--  with Ttypes;   use Ttypes;
--  with Exp_Tss;  use Exp_Tss;
--  with Uintp;    use Uintp;

package body Exp_Hlpr is

   -----------------------
   -- Local Subprograms --
   -----------------------

   function Make_Stream_Procedure_Function_Name
     (Loc : Source_Ptr;
      Typ : Entity_Id;
      Nam : Name_Id)
      return Entity_Id;
   --  Return the name to be assigned for stream subprogram Nam of Typ.
   --  (copied from exp_strm)

   ---------------------------------
   -- Build_Record_Read_Procedure --
   ---------------------------------

   procedure Build_TypeCode_Function
     (Loc : Source_Ptr;
      Typ : Entity_Id;
      Decl : out Node_Id;
      Fnam : out Entity_Id)
   is
      Spec : Node_Id;
      Stms : List_Id;
   begin
      Fnam := Make_Stream_Procedure_Function_Name (Loc, Typ, Name_uTypeCode);

      Spec :=
        Make_Function_Specification (Loc,
          Defining_Unit_Name => Fnam,
          Parameter_Specifications => Empty_List,
          Subtype_Mark => RTE (RE_TypeCode));

      Stms := New_List (Make_Null_Statement (Loc));

      Decl :=
        Make_Subprogram_Body (Loc,
          Specification => Spec,
          Declarations => Empty_List,
          Handled_Statement_Sequence =>
            Make_Handled_Sequence_Of_Statements (Loc,
              Statements => Stms));
   end Build_TypeCode_Function;

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

------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             E X P _ S T R M                              --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                            $LastChangedRevision$
--                                                                          --
--          Copyright (C) 1992-1999 Free Software Foundation, Inc.          --
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

--  Routines to build distribtion helper subprograms for user-defined types

with Types; use Types;

package Exp_Hlpr is

   function Build_From_Any_Call
     (Typ : Entity_Id;
      N   : Node_Id)
      return Node_Id;
   --  Build call to From_Any attribute function of type Typ
   --  withf expression N as actual parameter.

   procedure Build_From_Any_Function
     (Loc  :     Source_Ptr;
      Typ  :     Entity_Id;
      Decl : out Node_Id;
      Fnam : out Entity_Id);
   --  Build From_Any attribute function for Typ.

   function Build_To_Any_Call (N : Node_Id) return Node_Id;
   --  Build call to To_Any attribute function with expression N
   --  as actual parameter.

   procedure Build_To_Any_Function
     (Loc  :     Source_Ptr;
      Typ  :     Entity_Id;
      Decl : out Node_Id;
      Fnam : out Entity_Id);
   --  Build To_Any attribute function for Typ.

   function Build_TypeCode_Call
     (Loc : Source_Ptr;
      Typ : Entity_Id)
      return Node_Id;
   --  Build call to TypeCode attribute function for Typ.

   procedure Build_TypeCode_Function
     (Loc  :     Source_Ptr;
      Typ  :     Entity_Id;
      Decl : out Node_Id;
      Fnam : out Entity_Id);
   --  Build TypeCode attribute function for Typ.

end Exp_Hlpr;

------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             E X P _ H L P R                              --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 2002-2004 Free Software Foundation, Inc.          --
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
--  For implementation of the Distributed systems annex (DSA) over the
--  PolyORB generic middleware components, it is necessary to generate
--  several supporting subprograms for each application data type used
--  in inter-partition communication. These subprograms are:
--    * a Typecode function returning a high-level description of the
--      type's structure;
--    * two conversion functions allowing conversion of values of the
--      type from and to the generic data containers used by PolyORB.
--      These generic containers are called 'Any' type values after
--      the CORBA terminology, and hence the conversion subprograms
--      are named To_Any and From_Any.

with Types; use Types;

package Exp_Hlpr is

   function Build_From_Any_Call
     (Typ   : Entity_Id;
      N     : Node_Id;
      Decls : List_Id) return Node_Id;
   --  Build call to From_Any attribute function of type Typ with expression N
   --  as actual parameter. Decls is the declarations list for an appropriate
   --  enclosing scope of the point where the call will be inserted; if the
   --  From_Any attribute for Typ needs to be generated at this point, its
   --  declaration is appended to Decls.

   procedure Build_From_Any_Function
     (Loc  : Source_Ptr;
      Typ  : Entity_Id;
      Decl : out Node_Id;
      Fnam : out Entity_Id);
   --  Build From_Any attribute function for Typ. Loc is the reference
   --  location for generated nodes, Typ is the type for which the conversion
   --  function is generated. On return, Decl and Fnam contain the declaration
   --  and entity for the newly-created function.

   function Build_To_Any_Call
     (N     : Node_Id;
      Decls : List_Id) return Node_Id;
   --  Build call to To_Any attribute function with expression N
   --  as actual parameter. Decls is the declarations list for an appropriate
   --  enclosing scope of the point where the call will be inserted; if the
   --  To_Any attribute for Typ needs to be generated at this point, its
   --  declaration is appended to Decls.

   procedure Build_To_Any_Function
     (Loc  : Source_Ptr;
      Typ  : Entity_Id;
      Decl : out Node_Id;
      Fnam : out Entity_Id);
   --  Build To_Any attribute function for Typ. Loc is the reference
   --  location for generated nodes, Typ is the type for which the conversion
   --  function is generated. On return, Decl and Fnam contain the declaration
   --  and entity for the newly-created function.

   function Build_TypeCode_Call
     (Loc   : Source_Ptr;
      Typ   : Entity_Id;
      Decls : List_Id) return Node_Id;
   --  Build call to TypeCode attribute function for Typ. Decls is the
   --  declarations list for an appropriate enclosing scope of the point where
   --  the call will be inserted; if the To_Any attribute for Typ needs to be
   --  generated at this point, its declaration is appended to Decls.

   procedure Build_TypeCode_Function
     (Loc  : Source_Ptr;
      Typ  : Entity_Id;
      Decl : out Node_Id;
      Fnam : out Entity_Id);
   --  Build TypeCode attribute function for Typ. Loc is the reference
   --  location for generated nodes, Typ is the type for which the conversion
   --  function is generated. On return, Decl and Fnam contain the declaration
   --  and entity for the newly-created function.

   procedure Build_Name_And_Repository_Id
     (E           : Entity_Id;
      Name_Str    : out String_Id;
      Repo_Id_Str : out String_Id);
   --  In the PolyORB distribution model, each distributed object type
   --  and each distributed operation has a globally unique identifier,
   --  its Repository Id. This subprogram builds and returns two strings
   --  for entity E (a distributed object type or operation): one
   --  containing the name of E, the second containing its repository id.

end Exp_Hlpr;

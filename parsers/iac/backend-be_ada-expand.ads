------------------------------------------------------------------------------
--                                                                          --
--                            POLYORB COMPONENTS                            --
--                                   IAC                                    --
--                                                                          --
--                B A C K E N D . B E _ A D A . E X P A N D                 --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                           Copyright (c) 2005                             --
--            Ecole Nationale Superieure des Telecommunications             --
--                                                                          --
-- IAC is free software; you  can  redistribute  it and/or modify it under  --
-- terms of the GNU General Public License  as published by the  Free Soft- --
-- ware  Foundation;  either version 2 of the liscence or (at your option)  --
-- any  later version.                                                      --
-- IAC is distributed  in the hope that it will be  useful, but WITHOUT ANY --
-- WARRANTY; without even the implied warranty of MERCHANTABILITY or        --
-- FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for --
-- more details.                                                            --
-- You should have received a copy of the GNU General Public License along  --
-- with this program; if not, write to the Free Software Foundation, Inc.,  --
-- 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.            --
--                                                                          --
------------------------------------------------------------------------------

with Types; use Types;

--  This package contains also routines to expand the IDL tree and generate an
--  intermediate IDL tree. In this tree, will be implemented :
--  * The implicit forward declarations (eg. when a type uses an interface
--    of the same scope). The implementation of this feature directly from
--    the original IDL tree to the Ada tree is very complex because we will
--    be obliged to revisit nodes we have already visited.
--  * The optimisations of the number of forward.
--  * The definition of nested structures types : nested structures anonymous
--    types are not deprecated.

--  This phase of the compilation is located in the Ada backend because the
--  the problems related to the forwards are Ada specific problems.
package Backend.BE_Ada.Expand is

   --  This function creates a new designator from the from the node N which
   --  may be :
   --  * a type declaration
   --  * a subprogram specification
   --  * an object declaration
   --  * a package specification
   --  * a package declaration

   --  The new created node is a designator having the same defining identifier
   --  as N. The parent unit name of the result is set basing on :
   --  * the Parent_Unit_Name of node N defining identifier, if we are handling
   --    an forward interface declaration.
   --  * the "Parent" field of N in the other cases.
   function Expand_Designator
     (N               : Node_Id;
      Add_With_Clause : Boolean := True)
     return Node_Id;

   procedure Expand (Entity : Node_Id);
   --  Note that this procedure modifies the IDL tree but this is not very
   --  dangerous since we are already in the Ada backend.
   --  NB : Iac may evolve to execute many backend on after the other. In this
   --  case the procedure above has to be replaced by a function which
   --  duplicates the IDL tree and keeps it intact for other backends.

end Backend.BE_Ada.Expand;

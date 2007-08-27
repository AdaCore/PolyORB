------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--          B A C K E N D . B E _ C O R B A _ A D A . E X P A N D           --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2005-2007, Free Software Foundation, Inc.          --
--                                                                          --
-- PolyORB is free software; you  can  redistribute  it and/or modify it    --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. PolyORB is distributed  in the hope that it will be  useful,    --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with PolyORB; see file COPYING. If    --
-- not, write to the Free Software Foundation, 51 Franklin Street, Fifth    --
-- Floor, Boston, MA 02111-1301, USA.                                       --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
--                  PolyORB is maintained by AdaCore                        --
--                     (email: sales@adacore.com)                           --
--                                                                          --
------------------------------------------------------------------------------

--  This package contains routines to expand the IDL tree and generate
--  an intermediate IDL tree. In this tree, will be implemented:

--  * The implicit forward declarations (eg. when a type uses an
--  interface of the same scope). The implementation of this feature
--  directly from the original IDL tree to the Ada tree is very
--  complex because we will be obliged to revisit nodes we have
--  already visited.

--  * The definition of nested structures types : nested structures
--  anonymous types are not deprecated.

--  * The expansion of IDL attributes into Get_/Set_ IDL subprograms

--  This phase of the compilation is located in the Ada backend
--  because the problems related to the forwards are Ada specific
--  problems.

package Backend.BE_CORBA_Ada.Expand is

   function Expand_Designator
     (N               : Node_Id;
      Add_With_Clause : Boolean := True)
     return Node_Id;
   --  This function creates a new designator from the node N which
   --  may be:

   --  * a type declaration
   --  * a subprogram specification
   --  * an object declaration
   --  * a package specification
   --  * a package declaration

   --  The new created node is a designator having the same defining
   --  identifier as N. The parent unit name of the result is set
   --  basing on:

   --  * the Parent_Unit_Name of node N defining identifier, if we are
   --  handling a forward interface declaration.

   --  * the "Parent" field of N in the other cases.

   procedure Expand (Entity : Node_Id);
   --  Note that this procedure modifies the IDL tree but this is not
   --  very dangerous since we are already in the Ada backend.

   --  NB: Iac may evolve to execute many backend one after the
   --  other. In this case the procedure above has to be replaced by a
   --  function which duplicates the IDL tree and keeps it intact for
   --  other backends.

end Backend.BE_CORBA_Ada.Expand;

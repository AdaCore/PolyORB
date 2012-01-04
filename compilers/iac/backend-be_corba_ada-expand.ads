------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--          B A C K E N D . B E _ C O R B A _ A D A . E X P A N D           --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2005-2012, Free Software Foundation, Inc.          --
--                                                                          --
-- This is free software;  you can redistribute it  and/or modify it  under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for  more details.                                               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
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

   procedure Expand (Entity : Node_Id);
   --  Note that this procedure modifies the IDL tree but this is not
   --  very dangerous since we are already in the Ada backend.

   --  NB: Iac may evolve to execute many backend one after the
   --  other. In this case the procedure above has to be replaced by a
   --  function which duplicates the IDL tree and keeps it intact for
   --  other backends.

end Backend.BE_CORBA_Ada.Expand;

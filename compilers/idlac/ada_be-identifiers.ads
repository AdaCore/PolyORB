------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                   A D A _ B E . I D E N T I F I E R S                    --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2001-2012, Free Software Foundation, Inc.          --
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

with Idl_Fe.Types; use Idl_Fe.Types;

package Ada_Be.Identifiers is

   function Ada_Name
     (Node : Node_Id)
     return String;
   --  The Ada name (unqualified) of K_Named node.

   function Ada_Full_Name
     (Node : Node_Id)
     return String;
   --  The Ada full name of K_Named Node.

   function Parent_Scope_Name
     (Node : Node_Id)
     return String;
   --  The Ada full name of the scope where K_Named
   --  Node is defined.

end Ada_Be.Identifiers;

------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                         I D L _ F E . U T I L S                          --
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

package Idl_Fe.Utils is

   --  Miscellaneous utilities for IDL tree manipulation

   procedure Add_Identifier_With_Renaming
     (Node       : Node_Id;
      Identifier : String;
      Scope      : Node_Id := No_Node;
      Is_Inheritable : Boolean := True);
   --  Assign Identifier to Node in Scope (or current scope if No_Node),
   --  possibly appending a numeric prefix if a conflict
   --  would otherwise be introduced. If Is_Inheritable is False, then
   --  this identifier will not be considered as conflicting when this scope
   --  is inherited by another.

end Idl_Fe.Utils;

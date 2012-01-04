------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                I D L _ F E . T R E E . L O W _ L E V E L                 --
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

package Idl_Fe.Tree.Low_Level is

   ----------------------------------------------------
   -- Low-level manipulations on the tree structure. --
   ----------------------------------------------------

   procedure Replace_Node
     (Old_Node : in out Node_Id;
      New_Node : in out Node_Id);
   --  Replaces Old_Node with New_Node in the hashtable.
   --  Sets the Origianal_Node attribute of New_Node to
   --  Old_Node.

   function Copy_Node
     (Old_Node : Node_Id)
     return Node_Id;
   --  Create a (shallow) copy of Node.

end Idl_Fe.Tree.Low_Level;

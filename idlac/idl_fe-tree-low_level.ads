------------------------------------------------------------------------------
--                                                                          --
--                          ADABROKER COMPONENTS                            --
--                                                                          --
--                I D L _ F E . T R E E . L O W _ L E V E L                 --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1999-2000 ENST Paris University, France.          --
--                                                                          --
-- AdaBroker is free software; you  can  redistribute  it and/or modify it  --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. AdaBroker  is distributed  in the hope that it will be  useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with AdaBroker; see file COPYING. If  --
-- not, write to the Free Software Foundation, 59 Temple Place - Suite 330, --
-- Boston, MA 02111-1307, USA.                                              --
--                                                                          --
--             AdaBroker is maintained by ENST Paris University.            --
--                     (email: broker@inf.enst.fr)                          --
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
     (Old_Node : in Node_Id)
     return Node_Id;
   --  Create a (shallow) copy of Node.

end Idl_Fe.Tree.Low_Level;

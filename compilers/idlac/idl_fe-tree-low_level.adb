------------------------------------------------------------------------------
--                                                                          --
--                          ADABROKER COMPONENTS                            --
--                                                                          --
--                I D L _ F E . T R E E . L O W _ L E V E L                 --
--                                                                          --
--                                 B o d y                                  --
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

package body Idl_Fe.Tree.Low_Level is

   procedure Replace_Node
     (Old_Node : in out Node_Id;
      New_Node : in out Node_Id)
   is
      Temp_Node : constant Node_Access := Nodes_Table.Table (Old_Node);
      Temp_Id   : constant Node_Id := Old_Node;
   begin
      Nodes_Table.Table (Old_Node) := Nodes_Table.Table (New_Node);
      Nodes_Table.Table (New_Node) := Temp_Node;
      Old_Node := New_Node;
      New_Node := Temp_Id;
      Set_Original_Node (New_Node, Old_Node);
   end Replace_Node;

   function Copy_Node
     (Old_Node : in Node_Id)
     return Node_Id
   is
      Node  : constant Node_Access := new Node_Type;
      Index : constant Node_Id     := Nodes_Table.Allocate;
   begin
      Node.all := Nodes_Table.Table (Old_Node).all;
      Nodes_Table.Table (Index) := Node;
      Set_Original_Node (Index, Old_Node);
      return Index;
   end Copy_Node;

end Idl_Fe.Tree.Low_Level;

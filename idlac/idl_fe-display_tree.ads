------------------------------------------------------------------------------
--                                                                          --
--                          ADABROKER COMPONENTS                            --
--                                                                          --
--                  I D L _ F E . D I S P L A Y _ T R E E                   --
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

with Idl_Fe.Types; use Idl_Fe.Types;

package Idl_Fe.Display_Tree is

   Offset : constant Natural := 2;

   procedure Disp_Tree (Tree : Node_Id);

   --  display a node list
   procedure Disp_List (List : Node_List; Indent : Natural; Full : Boolean);

private

   --  display the indentation
   procedure Disp_Indent (Indent : Natural; S : String := "");

   --  displays a binary operator
   procedure Disp_Binary (N : Node_Id;
                          Indent : Natural;
                          Full : Boolean;
                          Op : String);

   --  displays a unary operator
   procedure Disp_Unary (N : Node_Id;
                         Indent : Natural;
                         Full : Boolean;
                         Op : String);

end Idl_Fe.Display_Tree;

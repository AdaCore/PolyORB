------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                  I D L _ F E . D I S P L A Y _ T R E E                   --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--            Copyright (C) 2001 Free Software Foundation, Inc.             --
--                                                                          --
-- PolyORB is free software; you  can  redistribute  it and/or modify it    --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. PolyORB is distributed  in the hope that it will be  useful,    --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with PolyORB; see file COPYING. If    --
-- not, write to the Free Software Foundation, 59 Temple Place - Suite 330, --
-- Boston, MA 02111-1307, USA.                                              --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
--                PolyORB is maintained by ACT Europe.                      --
--                    (email: sales@act-europe.fr)                          --
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

   --  displays a constant value and its type
   procedure Disp_Const_Value (Expr : Constant_Value_Ptr;
                               Indent : Natural);

end Idl_Fe.Display_Tree;

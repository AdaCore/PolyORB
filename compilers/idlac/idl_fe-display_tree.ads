------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                  I D L _ F E . D I S P L A Y _ T R E E                   --
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

------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--          B A C K E N D . B E _ C O R B A _ A D A . C O M M O N           --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2005-2006, Free Software Foundation, Inc.          --
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

package Backend.BE_CORBA_Ada.Common is

   function Cast_Variable_From_PolyORB_Type
     (Var_Name : Name_Id; Var_Type : Node_Id)
     return Node_Id;
   --  This function builds a type conversion of a variable from a
   --  PolyORB type into a CORBA type.

   function Cast_Variable_To_PolyORB_Type
     (Var_Node : Node_Id; Var_Type : Node_Id)
     return Node_Id;
   --  This function builds a type conversion of a variable to a
   --  PolyORB type.

   function Is_In (Par_Mode : Mode_Id) return Boolean;
   --  This function tests whether the mode is IN or INOUT

   function Is_Out (Par_Mode : Mode_Id) return Boolean;
   --  This function tests whether the mode is OUT or INOUT

   function Contains_In_Parameters (E : Node_Id) return Boolean;
   --  Returun True if the operation E contains IN or INOUT parameters

   function Contains_Out_Parameters (E : Node_Id) return Boolean;
   --  Returun True if the operation E contains OUT or INOUT
   --  parameters.

   function Make_Type_Designator
     (N          : Node_Id;
      Declarator : Node_Id := No_Node)
     return Node_Id;
   --  This function builds a type conversion of a variable to a
   --  PolyORB aligned type (used for compiler alignment).

   function Cast_Variable_To_PolyORB_Aligned_Type
     (Var_Node : Node_Id; Var_Type : Node_Id)
     return Node_Id;

   procedure Marshall_Args
     (Stat     : List_Id;
      Var_Type : Node_Id;
      Var      : Node_Id;
      Var_Exp  : Node_Id := No_Node);

   procedure Get_Discriminants_Value
     (P      : Node_Id;
      N      : Node_Id;
      L      : List_Id;
      Ret    : Boolean := False);

private

   pragma Inline (Is_In);
   pragma Inline (Is_Out);
end Backend.BE_CORBA_Ada.Common;

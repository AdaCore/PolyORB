------------------------------------------------------------------------------
--                                                                          --
--                            POLYORB COMPONENTS                            --
--                                   IAC                                    --
--                                                                          --
--            B A C K E N D . B E _ C O R B A _ A D A . C O M M O N         --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                        Copyright (c) 2005 - 2006                         --
--            Ecole Nationale Superieure des Telecommunications             --
--                                                                          --
-- IAC is free software; you  can  redistribute  it and/or modify it under  --
-- terms of the GNU General Public License  as published by the  Free Soft- --
-- ware  Foundation;  either version 2 of the liscence or (at your option)  --
-- any  later version.                                                      --
-- IAC is distributed  in the hope that it will be  useful, but WITHOUT ANY --
-- WARRANTY; without even the implied warranty of MERCHANTABILITY or        --
-- FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for --
-- more details.                                                            --
-- You should have received a copy of the GNU General Public License along  --
-- with this program; if not, write to the Free Software Foundation, Inc.,  --
-- 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.            --
--                                                                          --
------------------------------------------------------------------------------


package Backend.BE_CORBA_Ada.Common is
   --  This function builds a type conversion of a variable from a PolyORB
   --  type into a CORBA type
   function Cast_Variable_From_PolyORB_Type
     (Var_Name : Name_Id; Var_Type : Node_Id)
     return Node_Id;

   --  This function builds a type conversion of a variable to a PolyORB
   --  type
   function Cast_Variable_To_PolyORB_Type
     (Var_Node : Node_Id; Var_Type : Node_Id)
     return Node_Id;

   --  This function tests wether the mode is IN or INOUT
   function Is_In (Par_Mode : Mode_Id) return Boolean;
   pragma Inline (Is_In);

   --  This function tests wether the mode is OUT or INOUT
   function Is_Out (Par_Mode : Mode_Id) return Boolean;
   pragma Inline (Is_Out);

   --  The two subprograms below use the two subprograms above to
   --  chack the parameter mode of an IDL operation
   function Contains_In_Parameters (E : Node_Id) return Boolean;
   function Contains_Out_Parameters (E : Node_Id) return Boolean;
end Backend.BE_CORBA_Ada.Common;

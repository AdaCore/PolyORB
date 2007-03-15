------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                A D A _ B E . I D L 2 A D A . H E L P E R                 --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2001-2006, Free Software Foundation, Inc.          --
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

private package Ada_Be.Idl2Ada.Helper is

   Suffix : constant String := ".Helper";

   procedure Gen_Node_Spec
     (CU   : in out Compilation_Unit;
      Node : Node_Id);
   procedure Gen_Node_Body
     (CU   : in out Compilation_Unit;
      Node : Node_Id);
   --  Generate an helper package

   procedure Add_Helper_Dependency
     (CU          : in out Compilation_Unit;
      Helper_Name :        String);
   --  Add a semantic dependency and an initialization dependency in CU
   --  upon Helper_Name.

   procedure Gen_Forward_Interface_Spec
     (CU        : in out Compilation_Unit;
      Node      : Node_Id);
   --  Generate the spec of the helper package for a forward interface
   --  declaration called directly by ada_be.idl2ada.gen_scope

   procedure Gen_Forward_Interface_Body
     (CU        : in out Compilation_Unit;
      Node      : Node_Id);
   --  Generate the body of the helper package for a forward interface
   --  declaration called directly by ada_be.idl2ada.gen_scope

   procedure Gen_Wrap_Call
     (CU   : in out Compilation_Unit;
      Typ  : Node_Id;
      Expr : String);
   --  Generate a call appropriate to wrap expression Expr (denoting some
   --  object to be pointed to) in a content wrapper for the given type.

end Ada_Be.Idl2Ada.Helper;

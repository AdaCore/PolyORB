------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                A D A _ B E . I D L 2 A D A . H E L P E R                 --
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

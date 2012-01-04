------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                  A D A _ B E . I D L 2 A D A . I M P L                   --
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

private package Ada_Be.Idl2Ada.Impl is

   Suffix : constant String := ".Impl";

   procedure Gen_Node_Spec
     (CU       : in out Compilation_Unit;
      Node     : Node_Id;
      Is_Delegate : Boolean := False);

   procedure Gen_Node_Body
     (CU   : in out Compilation_Unit;
      Node : Node_Id);
   --  Generate an implementation template. If Is_Delegate is True, the
   --  generated spec is a delegate's generic formal parameter.

end Ada_Be.Idl2Ada.Impl;

------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                              T E S T G E N                               --
--                                                                          --
--                                 B o d y                                  --
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

with GNAT.Command_Line;

with Idl_Fe.Types;
with Idl_Fe.Parser;

with Ada_Be.Expansion;
with Ada_Be.Idl2Ada;
with Ada_Be.Mappings.CORBA;

procedure Testgen is
   The_CORBA_Mapping : Ada_Be.Mappings.CORBA.CORBA_Mapping_Type;
   Rep : Idl_Fe.Types.Node_Id;
begin
   Idl_Fe.Parser.Initialize (GNAT.Command_Line.Get_Argument);

   Rep := Idl_Fe.Parser.Parse_Specification;

   Ada_Be.Expansion.Expand_Repository (Rep);

   Ada_Be.Idl2Ada.Generate
     (Use_Mapping => The_CORBA_Mapping,
      Node        => Rep,
      Implement   => True);

   Idl_Fe.Parser.Finalize;
end Testgen;

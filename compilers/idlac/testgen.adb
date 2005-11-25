------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                              T E S T G E N                               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2001-2005 Free Software Foundation, Inc.           --
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

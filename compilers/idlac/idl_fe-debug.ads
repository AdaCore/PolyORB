------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                         I D L _ F E . D E B U G                          --
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

--  This is a debugging package for IDL-Ada Compiler.
--
--  with Idl_Fe.Debug;
--  pragma Elaborate(Idl_Fe.Debug);
--
--  Flag : constant Natural := Idl_Fe.Debug.Is_Active ("specific_name");
--  procedure O is new Idl_Fe.Debug.Output (Flag);
--
--  and then :
--
--  pragma Debug (O ("debugging info"));
--
--  The output will be done if "idl_fe.opt" file contains
--  a line with "specific_name"

package Idl_Fe.Debug is

   pragma Elaborate_Body;

   function Is_Active (Flag : String) return Natural;
   --  Returns 0 when not active

   generic
      Flag : Natural;
   procedure Output (Message : String);
   --  Prints Message on standard output when Flag is not 0

end Idl_Fe.Debug;

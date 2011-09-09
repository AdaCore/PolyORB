------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--        P O L Y O R B . F I L T E R S . M I O P . M I O P _ O U T         --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2003-2010, Free Software Foundation, Inc.          --
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

--  MIOP filter for data which arrive from a GIOP Session

with PolyORB.Buffers;
with PolyORB.Components;

package PolyORB.Filters.MIOP.MIOP_Out is

   pragma Elaborate_Body;

   type MIOP_Out_Factory is new Factory with private;

   procedure Create
     (Fact     : access MIOP_Out_Factory;
      MIOP_Out :    out Filter_Access);

private

   type MIOP_Out_Factory is new Factory with null record;

   --  MIOP_OUT status
   type MIOP_Out_Filter is new Filter with record
      --  MIOP buffer
      MIOP_Buff : Buffers.Buffer_Access;
      --  Max size of MIOP packet
      Max_Size  : Types.Unsigned_Short;
   end record;

   function Handle_Message
     (F : not null access MIOP_Out_Filter;
      S : Components.Message'Class) return Components.Message'Class;

end PolyORB.Filters.MIOP.MIOP_Out;

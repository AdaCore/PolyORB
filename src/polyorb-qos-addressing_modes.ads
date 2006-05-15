------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--         P O L Y O R B . Q O S . A D D R E S S I N G _ M O D E S          --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--           Copyright (C) 2006, Free Software Foundation, Inc.             --
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

--  This parameter used for selection of GIOP addressing mode for request
--  marshalling. As client request parameter it define addressing mode which
--  should be used for request marshalling. As server request parameter it
--  specify which addressing mode have been used by client in request.

with PolyORB.Errors;

package PolyORB.QoS.Addressing_Modes is

   type QoS_GIOP_Addressing_Mode_Parameter is
     new QoS_Parameter (GIOP_Addressing_Mode) with
   record
      Mode : PolyORB.Errors.Addressing_Mode;
   end record;

   type QoS_GIOP_Addressing_Mode_Parameter_Access is
     access all QoS_GIOP_Addressing_Mode_Parameter'Class;

end PolyORB.QoS.Addressing_Modes;

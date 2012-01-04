------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--         P O L Y O R B . Q O S . A D D R E S S I N G _ M O D E S          --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2006-2012, Free Software Foundation, Inc.          --
--                                                                          --
-- This is free software;  you can redistribute it  and/or modify it  under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for  more details.                                               --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
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

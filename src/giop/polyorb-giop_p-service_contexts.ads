------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--      P O L Y O R B . G I O P _ P . S E R V I C E _ C O N T E X T S       --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2004-2012, Free Software Foundation, Inc.          --
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

--  Support package for GIOP Service Contexts

with PolyORB.Buffers;
with PolyORB.QoS.Service_Contexts;

package PolyORB.GIOP_P.Service_Contexts is

   package PRSC renames PolyORB.QoS.Service_Contexts;

   procedure Marshall_Service_Context_List
     (Buffer : access Buffers.Buffer_Type;
      SCP    : PRSC.QoS_GIOP_Service_Contexts_Parameter_Access);

   procedure Unmarshall_Service_Context_List
     (Buffer : access Buffers.Buffer_Type;
      SCP    :    out PRSC.QoS_GIOP_Service_Contexts_Parameter_Access);

end PolyORB.GIOP_P.Service_Contexts;

------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--     P O L Y O R B . G I O P _ P . S E R V I C E S _ C O N T E X T S      --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--            Copyright (C) 2004 Free Software Foundation, Inc.             --
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
--                PolyORB is maintained by ACT Europe.                      --
--                    (email: sales@act-europe.fr)                          --
--                                                                          --
------------------------------------------------------------------------------

--  Support package for GIOP Service Contexts

--  $Id$

with PolyORB.Buffers;
with PolyORB.Request_QoS;
with PolyORB.Types;

package PolyORB.GIOP_P.Service_Contexts is

   --  XXX For now, the data used to build the service contexts is
   --  carried by the QoS parameters associated with a request. This
   --  may be not sufficient for other service contexts to be
   --  implemented.

   procedure Marshall_Service_Context_List
     (Buffer : access Buffers.Buffer_Type;
      QoS    : in     PolyORB.Request_QoS.QoS_Parameter_Lists.List);

   procedure Unmarshall_Service_Context_List
     (Buffer : access Buffers.Buffer_Type;
      QoS    :    out PolyORB.Request_QoS.QoS_Parameter_Lists.List);

   --  List of supported Service Contexts

   RTCorbaPriority : constant PolyORB.Types.Unsigned_Long;

private

   RTCorbaPriority : constant PolyORB.Types.Unsigned_Long := 10;

end PolyORB.GIOP_P.Service_Contexts;

------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                  POLYORB.REQUEST_QOS.SERVICE_CONTEXTS                    --
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

--  $Id$

with PolyORB.Representations.CDR.Common;
with PolyORB.Types;
with PolyORB.Utils.Chained_Lists;

package PolyORB.Request_QoS.Service_Contexts is

   subtype Service_Id is Types.Unsigned_Long;

   --  List of supported Service Contexts

   CodeSets        : constant Service_Id;
   RTCorbaPriority : constant Service_Id;

   type Encapsulation_Access is
      access all PolyORB.Representations.CDR.Common.Encapsulation;

   type Service_Context is record
      Context_Id   : Service_Id;
      Context_Data : Encapsulation_Access;
   end record;

   package Service_Context_Lists is
     new Utils.Chained_Lists (Service_Context, "=", True);

   type QoS_GIOP_Service_Contexts_Parameter is
     new QoS_Parameter (GIOP_Service_Contexts) with
   record
      Service_Contexts : Service_Context_Lists.List;
   end record;

   type QoS_GIOP_Service_Contexts_Parameter_Access is
     access all QoS_GIOP_Service_Contexts_Parameter'Class;

   procedure Release_Contents
     (QoS : access QoS_GIOP_Service_Contexts_Parameter);

   procedure Rebuild_Request_Service_Contexts (Req : in PR.Request_Access);
   --  Reconstruct list of GIOP Service Contexts from the list of
   --  QoS Parameters.

   procedure Rebuild_Reply_Service_Contexts (Req : in PR.Request_Access);
   --  Reconstruct list of GIOP Service Contexts from the list of
   --  QoS Parameters.

   procedure Rebuild_Request_QoS_Parameters (Req : in PR.Request_Access);
   --  Reconstruct list of QoS Parameters from list of GIOP Service Contexts.

   procedure Rebuild_Reply_QoS_Parameters (Req : in PR.Request_Access);
   --  Reconstruct list of QoS Parameters from list of GIOP Service Contexts.

   type To_Service_Context is
      access function (QoS : in QoS_Parameter_Access) return Service_Context;

   type To_QoS_Parameter is
      access function (SC : in Service_Context) return QoS_Parameter_Access;

   procedure Register (QoS : in QoS_Kind; Converter : in To_Service_Context);

   procedure Register (Id : in Service_Id; Converter : in To_QoS_Parameter);

private

   CodeSets        : constant Service_Id := 1;
   RTCorbaPriority : constant Service_Id := 10;

end PolyORB.Request_QoS.Service_Contexts;

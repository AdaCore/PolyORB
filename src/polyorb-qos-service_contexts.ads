------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--         P O L Y O R B . Q O S . S E R V I C E _ C O N T E X T S          --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2004-2006, Free Software Foundation, Inc.          --
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

with PolyORB.Requests;
with PolyORB.Representations.CDR.Common;
with PolyORB.Types;
with PolyORB.Utils.Chained_Lists;

package PolyORB.QoS.Service_Contexts is

   subtype Service_Id is Types.Unsigned_Long;

   --  List of supported Service Contexts

   CodeSets                 : constant Service_Id;
   RTCorbaPriority          : constant Service_Id;
   FTGroupVersion           : constant Service_Id;
   FTRequest                : constant Service_Id;
   SecurityAttributeService : constant Service_Id;
   AdaExceptionInformation  : constant Service_Id;
   TMInfo                   : constant Service_Id;

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

   procedure Rebuild_Request_Service_Contexts
     (Req : PolyORB.Requests.Request_Access);
   --  Reconstruct list of GIOP Service Contexts from the list of
   --  QoS Parameters.

   procedure Rebuild_Reply_Service_Contexts
     (Req : PolyORB.Requests.Request_Access);
   --  Reconstruct list of GIOP Service Contexts from the list of
   --  QoS Parameters.

   procedure Rebuild_Request_QoS_Parameters
     (Req : PolyORB.Requests.Request_Access);
   --  Reconstruct list of QoS Parameters from list of GIOP Service Contexts.

   procedure Rebuild_Reply_QoS_Parameters
     (Req : PolyORB.Requests.Request_Access);
   --  Reconstruct list of QoS Parameters from list of GIOP Service Contexts.

   type To_Service_Context is
      access function (QoS : QoS_Parameter_Access) return Service_Context;

   type To_QoS_Parameter is
      access function (SC : Service_Context) return QoS_Parameter_Access;

   procedure Register (QoS : QoS_Kind; Converter : To_Service_Context);

   procedure Register (Id : Service_Id; Converter : To_QoS_Parameter);

private

   CodeSets                 : constant Service_Id := 1;
   RTCorbaPriority          : constant Service_Id := 10;
   FTGroupVersion           : constant Service_Id := 12;
   FTRequest                : constant Service_Id := 13;
   SecurityAttributeService : constant Service_Id := 15;
   AdaExceptionInformation  : constant Service_Id := 2005;
   TMInfo                   : constant Service_Id := 17;
end PolyORB.QoS.Service_Contexts;

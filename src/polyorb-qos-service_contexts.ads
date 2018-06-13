------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--         P O L Y O R B . Q O S . S E R V I C E _ C O N T E X T S          --
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

   overriding procedure Release_Contents
     (QoS : access QoS_GIOP_Service_Contexts_Parameter);

   procedure Rebuild_Request_Service_Contexts (Req : in out Requests.Request);
   --  Reconstruct list of GIOP Service Contexts from the list of QoS
   --  Parameters.

   procedure Rebuild_Reply_Service_Contexts (Req : in out Requests.Request);
   --  Reconstruct list of GIOP Service Contexts from the list of QoS
   --  Parameters.

   procedure Rebuild_Request_QoS_Parameters (Req : in out Requests.Request);
   --  Reconstruct list of QoS Parameters from list of GIOP Service Contexts

   procedure Rebuild_Reply_QoS_Parameters (Req : in out Requests.Request);
   --  Reconstruct list of QoS Parameters from list of GIOP Service Contexts

   type To_Service_Context is
      access function (QoS : QoS_Parameter_Access) return Service_Context;

   type To_QoS_Parameter is
      access function (SC : Service_Context) return QoS_Parameter_Access;

   procedure Register (QoS : QoS_Kind; Converter : To_Service_Context);

   procedure Register (Id : Service_Id; Converter : To_QoS_Parameter);

private

   use type Types.Unsigned_Long;

   --  Standard service tags

   CodeSets                 : constant Service_Id := 1;
   RTCorbaPriority          : constant Service_Id := 10;
   FTGroupVersion           : constant Service_Id := 12;
   FTRequest                : constant Service_Id := 13;
   SecurityAttributeService : constant Service_Id := 15;

   --  PolyORB-specific service tags (see docs/OMG_TAGS)

   PolyORB_First            : constant Service_Id := 16#504f0000#;
   --  "PO\x00\x00"

   AdaExceptionInformation  : constant Service_Id := PolyORB_First + 0;
   TMInfo                   : constant Service_Id := PolyORB_First + 1;

   PolyORB_Last             : constant Service_Id := 16#504f00ff#;
   --  "PO\x00\xff"

end PolyORB.QoS.Service_Contexts;

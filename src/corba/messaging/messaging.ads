------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                            M E S S A G I N G                             --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--            Copyright (C) 2004 Free Software Foundation, Inc.             --
--                                                                          --
-- This specification is derived from the CORBA Specification, and adapted  --
-- for use with PolyORB. The copyright notice above, and the license        --
-- provisions that follow apply solely to the contents neither explicitely  --
-- nor implicitely specified by the CORBA Specification defined by the OMG. --
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

with CORBA.Sequences.Unbounded;

package Messaging is

   --  Implementation Notes: this type temporary replaces CORBA::OctetSeq type.

   package IDL_Sequence_Octet is new CORBA.Sequences.Unbounded (CORBA.Octet);

   --  RebindMode type and constants

   type RebindMode is new CORBA.Short;

   Transparent  : constant RebindMode := 0;
   No_Rebind    : constant RebindMode := 1;
   No_Reconnect : constant RebindMode := 2;

   --  SyncScope type and constants

   type SyncScope is new CORBA.Short;

   Sync_None           : constant SyncScope := 0;
   Sync_With_Transport : constant SyncScope := 1;
   Sync_With_Server    : constant SyncScope := 2;
   Sync_With_Target    : constant SyncScope := 3;

   --  RoutingType type and constants

   type RoutingType is new CORBA.Short;

   Route_None              : constant RoutingType := 0;
   Route_Forward           : constant RoutingType := 1;
   Route_Store_And_Forward : constant RoutingType := 2;

   --  Priority type

   type Priority is new CORBA.Short;

   --  Ordering type and constants

   type Ordering is new CORBA.Unsigned_Short;

   Order_Any      : constant Ordering := 1;
   Order_Temporal : constant Ordering := 2;
   Order_Priority : constant Ordering := 4;
   Order_Deadline : constant Ordering := 8;

   --  Messaging related PolicyTypes

   Rebind_Policy_Type               : constant CORBA.PolicyType := 23;
   Sync_Scope_Policy_Type           : constant CORBA.PolicyType := 24;
   Request_Priority_Policy_Type     : constant CORBA.PolicyType := 25;
   Reply_Priority_Policy_Type       : constant CORBA.PolicyType := 26;
   Request_Start_Time_Policy_Type   : constant CORBA.PolicyType := 27;
   Request_End_Time_Policy_Type     : constant CORBA.PolicyType := 28;
   Reply_Start_Time_Policy_Type     : constant CORBA.PolicyType := 29;
   Reply_End_Time_Policy_Type       : constant CORBA.PolicyType := 30;
   Relative_Req_Timeout_Policy_Type : constant CORBA.PolicyType := 31;
   Relative_RT_Timeout_Policy_Type  : constant CORBA.PolicyType := 32;
   Routing_Policy_Type              : constant CORBA.PolicyType := 33;
   Max_Hops_Policy_Type             : constant CORBA.PolicyType := 34;
   Queue_Order_Policy_Type          : constant CORBA.PolicyType := 35;

   --  PriorityRange struct
   type PriorityRange is record
      Min : Priority;
      Max : Priority;
   end record;

   --  RoutingTypeRange struct

   type RoutingTypeRange is record
      Min : RoutingType;
      Max : RoutingType;
   end record;

   --  PolicyValue struct

   type PolicyValue is record
      PType  : CORBA.PolicyType;
      PValue : IDL_Sequence_Octet.Sequence;
   end record;

   --  PolicyValueSeq type

   package IDL_Sequence_Messaging_PolicyValue is
     new CORBA.Sequences.Unbounded (PolicyValue);

   type PolicyValueSeq is new IDL_Sequence_Messaging_PolicyValue.Sequence;

   --  native UserExceptionBase
   --  Not implemented

   --  Repository Ids

   Repository_Id : constant Standard.String
     := "IDL:omg.org/Messaging:1.0";

   Ordering_Repository_Id         : constant Standard.String
     := "IDL:omg.org/Messaging/Ordering:1.0";

   PolicyValue_Repository_Id : constant Standard.String
     := "IDL:omg.org/Messaging/PolicyValue:1.0";

   PolicyValueSeq_Repository_Id : constant Standard.String
     := "IDL:omg.org/Messaging/PolicyValueSeq:1.0";

   Priority_Repository_Id         : constant Standard.String
     := "IDL:omg.org/Messaging/Priority:1.0";

   PriorityRange_Repository_Id    : constant Standard.String
     := "IDL:omg.org/Messaging/PriorityRange:1.0";

   RebindMode_Repository_Id       : constant Standard.String
     := "IDL:omg.org/Messaging/RebindMode:1.0";

   RoutingType_Repository_Id      : constant Standard.String
     := "IDL:omg.org/Messaging/RoutingType:1.0";

   RoutingTypeRange_Repository_Id : constant Standard.String
     := "IDL:omg.org/Messaging/RoutingTypeRange:1.0";

   SyncScope_Repository_Id        : constant Standard.String
     := "IDL:omg.org/Messaging/SyncScope:1.0";

end Messaging;

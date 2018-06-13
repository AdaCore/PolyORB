------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                              R T C O R B A                               --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2004-2017, Free Software Foundation, Inc.          --
--                                                                          --
-- This specification is derived from the CORBA Specification, and adapted  --
-- for use with PolyORB. The copyright notice above, and the license        --
-- provisions that follow apply solely to the contents neither explicitly   --
-- nor implicitly specified by the CORBA Specification defined by the OMG.  --
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

-------------------------------------------------
--  This file has been generated automatically
--  by IDLAC version 2.3.0w.
--
--  Do NOT hand-modify this file, as your
--  changes will be lost when you re-run the
--  IDL to Ada compiler.
-------------------------------------------------
pragma Style_Checks ("NM32766");

with IOP;
with CORBA.Forward;
pragma Elaborate_All (CORBA.Forward);
with CORBA.Sequences.Unbounded;
pragma Elaborate_All (CORBA.Sequences.Unbounded);
with PolyORB.Std;
with CORBA;

with PolyORB.RT_POA_Policies.Priority_Model_Policy;

package RTCORBA is

   type NativePriority is
     new CORBA.Short;

   NativePriority_Repository_Id : constant PolyORB.Std.String
     := "IDL:omg.org/RTCORBA/NativePriority:1.0";

   type Priority is
     new CORBA.Short;

   Priority_Repository_Id : constant PolyORB.Std.String
     := "IDL:omg.org/RTCORBA/Priority:1.0";

   minPriority : constant RTCORBA.Priority
     := 0;

   maxPriority : constant RTCORBA.Priority
     := 32767;

   --  Type PriorityMapping is implementation defined;

   --  Type PriorityTransform is implementation defined;

   type ThreadpoolId is
     new CORBA.Unsigned_Long;

   ThreadpoolId_Repository_Id : constant PolyORB.Std.String
     := "IDL:omg.org/RTCORBA/ThreadpoolId:1.0";

   type ThreadpoolLane is record
      lane_priority : RTCORBA.Priority;
      static_threads : CORBA.Unsigned_Long;
      dynamic_threads : CORBA.Unsigned_Long;
   end record;

   ThreadpoolLane_Repository_Id : constant PolyORB.Std.String
     := "IDL:omg.org/RTCORBA/ThreadpoolLane:1.0";

   package IDL_SEQUENCE_RTCORBA_ThreadpoolLane is
     new CORBA.Sequences.Unbounded
       (RTCORBA.ThreadpoolLane);

   type ThreadpoolLanes is
     new RTCORBA.IDL_SEQUENCE_RTCORBA_ThreadpoolLane.Sequence;

   ThreadpoolLanes_Repository_Id : constant PolyORB.Std.String
     := "IDL:omg.org/RTCORBA/ThreadpoolLanes:1.0";

   PRIORITY_MODEL_POLICY_TYPE : constant CORBA.PolicyType
     := 40;

   type PriorityModel is
     new PolyORB.RT_POA_Policies.Priority_Model_Policy.Priority_Model;
   --  Implementation Note: this is equivalent to
   --  type PriorityModel is (CLIENT_PROPAGATED, SERVER_DECLARED);

   PriorityModel_Repository_Id : constant PolyORB.Std.String
     := "IDL:omg.org/RTCORBA/PriorityModel:1.0";

   --  Interface PriorityModelPolicy

   THREADPOOL_POLICY_TYPE : constant CORBA.PolicyType
     := 41;

   --  Interface ThreadpoolPolicy

   package ProtocolProperties_Forward is new CORBA.Forward;

   --  Interface ProtocolProperties

   type Protocol is record
      protocol_type : IOP.ProfileId;
      orb_protocol_properties : RTCORBA.ProtocolProperties_Forward.Ref;
      transport_protocol_properties : RTCORBA.ProtocolProperties_Forward.Ref;
   end record;

   Protocol_Repository_Id : constant PolyORB.Std.String
     := "IDL:omg.org/RTCORBA/Protocol:1.0";

   package IDL_SEQUENCE_RTCORBA_Protocol is
     new CORBA.Sequences.Unbounded
       (RTCORBA.Protocol);

   type ProtocolList is
     new RTCORBA.IDL_SEQUENCE_RTCORBA_Protocol.Sequence;

   ProtocolList_Repository_Id : constant PolyORB.Std.String
     := "IDL:omg.org/RTCORBA/ProtocolList:1.0";

   SERVER_PROTOCOL_POLICY_TYPE : constant CORBA.PolicyType
     := 42;

   --  Interface ServerProtocolPolicy

   CLIENT_PROTOCOL_POLICY_TYPE : constant CORBA.PolicyType
     := 43;

   --  Interface ClientProtocolPolicy

   PRIVATE_CONNECTION_POLICY_TYPE : constant CORBA.PolicyType
     := 44;

   --  Interface PrivateConnectionPolicy

   --  Interface TCPProtocolProperties

   type PriorityBand is record
      low : RTCORBA.Priority;
      high : RTCORBA.Priority;
   end record;

   PriorityBand_Repository_Id : constant PolyORB.Std.String
     := "IDL:omg.org/RTCORBA/PriorityBand:1.0";

   package IDL_SEQUENCE_RTCORBA_PriorityBand is
     new CORBA.Sequences.Unbounded
       (RTCORBA.PriorityBand);

   type PriorityBands is
     new RTCORBA.IDL_SEQUENCE_RTCORBA_PriorityBand.Sequence;

   PriorityBands_Repository_Id : constant PolyORB.Std.String
     := "IDL:omg.org/RTCORBA/PriorityBands:1.0";

   PRIORITY_BANDED_CONNECTION_POLICY_TYPE : constant CORBA.PolicyType
     := 45;

   --  Interface PriorityBandedConnectionPolicy

   --  Interface Current

   --  Interface Mutex

   --  Interface RTORB

   Repository_Id : constant PolyORB.Std.String
     := "IDL:omg.org/RTCORBA:1.0";

end RTCORBA;

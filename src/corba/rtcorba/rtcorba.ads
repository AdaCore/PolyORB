------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                              R T C O R B A                               --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--            Copyright (C) 2003 Free Software Foundation, Inc.             --
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

with PolyORB.RT_POA_Policies.Priority_Model_Policy;

package RTCORBA is

   type NativePriority is new CORBA.Short;

   type Priority is new CORBA.Short;

   MinPriority : constant RTCORBA.Priority := 0;
   MaxPriority : constant RTCORBA.Priority := 32767;

   type ThreadpoolId is new CORBA.Unsigned_Long;

   type ThreadpoolLane is record
      Lane_Priority   : RTCORBA.Priority;
      Static_Threads  : CORBA.Unsigned_Long;
      Dynamic_Threads : CORBA.Unsigned_Long;
   end record;

   package IDL_SEQUENCE_RTCORBA_ThreadpoolLane is
     new CORBA.Sequences.Unbounded (RTCORBA.ThreadpoolLane);

   type ThreadpoolLanes is
     new RTCORBA.IDL_SEQUENCE_RTCORBA_ThreadpoolLane.Sequence;

   --  Priority Model Policy

   PRIORITY_MODEL_POLICY_TYPE : constant CORBA.PolicyType := 40;

   type PriorityModel is
     new PolyORB.RT_POA_Policies.Priority_Model_Policy.Priority_Model;
   --  Implementation Note: this is equivalent to
   --   type PriorityModel is (CLIENT_PROPAGATED, SERVER_DECLARED);

   --  Threadpool Policy

   THREADPOOL_POLICY_TYPE : constant CORBA.PolicyType := 41;

   procedure Dummy;

end RTCORBA;

------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                        R T C O R B A . R T O R B                         --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2003-2004 Free Software Foundation, Inc.           --
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

with Ada.Exceptions;

with CORBA.Object;

with RTCORBA.Mutex;
with RTCORBA.PriorityModelPolicy;
with RTCORBA.ThreadpoolPolicy;

package RTCORBA.RTORB is

   type Ref is new CORBA.Object.Ref with private;

   function To_Ref (Self : CORBA.Object.Ref'Class) return Ref;

   function Create_Mutex (Self : in Ref) return RTCORBA.Mutex.Ref;

   procedure Destroy_Mutex (Self : in Ref; The_Mutex : in RTCORBA.Mutex.Ref);

   InvalidThreadpool : exception;

   function Create_Threadpool
     (Self                    : in Ref;
      Stacksize               : in CORBA.Unsigned_Long;
      Static_Threads          : in CORBA.Unsigned_Long;
      Dynamic_Threads         : in CORBA.Unsigned_Long;
      Default_Priority        : in RTCORBA.Priority;
      Allow_Request_Buffering : in CORBA.Boolean;
      Max_Buffered_Requests   : in CORBA.Unsigned_Long;
      Max_Request_Buffer_Size : in CORBA.Unsigned_Long)
     return RTCORBA.ThreadpoolId;

   function Create_Threadpool_With_Lanes
     (Self                    : in Ref;
      Stacksize               : in CORBA.Unsigned_Long;
      Lanes                   : in RTCORBA.ThreadpoolLanes;
      Allow_Borrowing         : in CORBA.Boolean;
      Allow_Request_Buffering : in CORBA.Boolean;
      Max_Buffered_Requests   : in CORBA.Unsigned_Long;
      Max_Request_Buffer_Size : in CORBA.Unsigned_Long)
     return RTCORBA.ThreadpoolId;

   procedure Destroy_Threadpool
     (Self       : in Ref;
      Threadpool : in RTCORBA.ThreadpoolId);

   function Create_Priority_Model_Policy
     (Self            : in Ref;
      Priority_Model  : in RTCORBA.PriorityModel;
      Server_Priority : in RTCORBA.Priority)
     return RTCORBA.PriorityModelPolicy.Ref;

   function Create_Threadpool_Policy
     (Self       : in Ref;
      Threadpool : in RTCORBA.ThreadpoolId)
     return RTCORBA.ThreadpoolPolicy.Ref;
   --  Implementation Note: RT-CORBA specifications (formal/03-11-01)
   --  defines no return exception for this function. However,
   --  creating a Threadpool_Policy from an invalid ThreadpoolId shall
   --  be an error, as the user has no control on generated
   --  ThreadpoolIds.

   -----------------------------------------
   -- RTCORBA.RTORB Exceptions Management --
   -----------------------------------------

   type InvalidThreadpool_Members is
     new CORBA.IDL_Exception_Members with null record;

   procedure Get_Members
     (From :     Ada.Exceptions.Exception_Occurrence;
      To   : out InvalidThreadpool_Members);

   procedure Raise_InvalidThreadpool
     (Excp_Memb : in InvalidThreadpool_Members);

private

   type Ref is new CORBA.Object.Ref with null record;

end RTCORBA.RTORB;

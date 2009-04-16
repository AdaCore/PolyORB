------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                        R T C O R B A . R T O R B                         --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2003-2009, Free Software Foundation, Inc.          --
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

with Ada.Exceptions;

with CORBA.Object;

with RTCORBA.Mutex;
with RTCORBA.PriorityModelPolicy;
with RTCORBA.ThreadpoolPolicy;

with PolyORB.Std;

with PolyORB.Smart_Pointers.Controlled_Entities;

package RTCORBA.RTORB is

   package PSPCE renames PolyORB.Smart_Pointers.Controlled_Entities;

   type Local_Ref is new CORBA.Object.Ref with private;

   function Create_Mutex
     (Self : Local_Ref)
     return RTCORBA.Mutex.Local_Ref;
   --  XXX for now, there is no priority inheritance mechanism in use.

   procedure Destroy_Mutex
     (Self      : Local_Ref;
      The_Mutex : RTCORBA.Mutex.Local_Ref);

   InvalidThreadpool : exception;

   function Create_Threadpool
     (Self                    : Local_Ref;
      Stacksize               : CORBA.Unsigned_Long;
      Static_Threads          : CORBA.Unsigned_Long;
      Dynamic_Threads         : CORBA.Unsigned_Long;
      Default_Priority        : RTCORBA.Priority;
      Allow_Request_Buffering : CORBA.Boolean;
      Max_Buffered_Requests   : CORBA.Unsigned_Long;
      Max_Request_Buffer_Size : CORBA.Unsigned_Long)
     return RTCORBA.ThreadpoolId;
   --  Implementation Note:
   --  * the parameter Max_Request_Buffer_Size is currently not
   --  handled. Setting it to a non-zero value will result in an
   --  invalid Threadpool configuration, and raise the CORBA.BAD_PARAM
   --  exception. The lane will buffer up to Max_Buffer_Requests
   --  requests, diregarding memory used.
   --
   --  * actual deallocation of dynamic threads is left as an
   --  implementation issue by the RT-CORBA specifications. PolyORB
   --  destroys dynamically allocated threads once the Threadpool has
   --  no queued job to process.

   function Create_Threadpool_With_Lanes
     (Self                    : Local_Ref;
      Stacksize               : CORBA.Unsigned_Long;
      Lanes                   : RTCORBA.ThreadpoolLanes;
      Allow_Borrowing         : CORBA.Boolean;
      Allow_Request_Buffering : CORBA.Boolean;
      Max_Buffered_Requests   : CORBA.Unsigned_Long;
      Max_Request_Buffer_Size : CORBA.Unsigned_Long)
     return RTCORBA.ThreadpoolId;
   --  Implementation Note:
   --  * the parameter Max_Request_Buffer_Size is currently not
   --  handled. Setting it to a non-zero value will result in an
   --  invalid Threadpool configuration, and raise the CORBA.BAD_PARAM
   --  exception. The lane will buffer up to Max_Buffer_Requests
   --  requests, diregarding memory used.
   --
   --  * the parameter Allow_Borrowing is not handled. Setting it to
   --  True will result in an invalid Threadpool configuration, and
   --  raise the CORBA.BAD_PARAM exception.
   --
   --  * actual deallocation of dynamic threads is left as an
   --  implementation issue by the RT-CORBA specifications. PolyORB
   --  destroys dynamically allocated threads once the Threadpool has
   --  no queued job to process.

   procedure Destroy_Threadpool
     (Self       : Local_Ref;
      Threadpool : RTCORBA.ThreadpoolId);
   --  Implementation Note: RT-CORBA specifications defines no return
   --  exception for this function. However, the user has no control
   --  on generated ThreadpoolIds, thus destroying a Threadpool_Policy
   --  from an invalid ThreadpoolId shall be an error. This function
   --  will raise InvalidThreadpool if Threadpool is not valid.

   function Create_Priority_Model_Policy
     (Self            : Local_Ref;
      Priority_Model  : RTCORBA.PriorityModel;
      Server_Priority : RTCORBA.Priority)
     return RTCORBA.PriorityModelPolicy.Local_Ref;

   function Create_Threadpool_Policy
     (Self       : Local_Ref;
      Threadpool : RTCORBA.ThreadpoolId)
     return RTCORBA.ThreadpoolPolicy.Local_Ref;
   --  Implementation Note: RT-CORBA specifications defines no return
   --  exception for this function. However, the user has no control
   --  on generated ThreadpoolIds, thus creating a Threadpool_Policy
   --  from an invalid ThreadpoolId shall be an error. This function
   --  will raise InvalidThreadpool if Threadpool is not valid.

   -----------------------------------------
   -- RTCORBA.RTORB Exceptions Management --
   -----------------------------------------

   type InvalidThreadpool_Members is
     new CORBA.IDL_Exception_Members with null record;

   procedure Get_Members
     (From :     Ada.Exceptions.Exception_Occurrence;
      To   : out InvalidThreadpool_Members);

   procedure Raise_InvalidThreadpool
     (Excp_Memb : InvalidThreadpool_Members);

   Repository_Id : constant PolyORB.Std.String :=
                     "IDL:omg.org/RTCORBA/RTORB:1.0";

private

   type Local_Ref is new CORBA.Object.Ref with null record;
   type RTORB_Object is new PSPCE.Entity with null record;

end RTCORBA.RTORB;

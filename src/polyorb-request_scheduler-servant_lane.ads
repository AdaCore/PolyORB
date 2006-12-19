------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                 POLYORB.REQUEST_SCHEDULER.SERVANT_LANE                   --
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

package PolyORB.Request_Scheduler.Servant_Lane is

   type Request_Scheduler_Servant_Lane is new Request_Scheduler with private;

   type Request_Scheduler_Servant_Lane_Access is
     access all Request_Scheduler_Servant_Lane;

   function Try_Queue_Request_Job
     (Self   : access Request_Scheduler_Servant_Lane;
      Job    :        PolyORB.Jobs.Job_Access;
      Target :        PolyORB.References.Ref)
     return Boolean;

   type Request_Scheduler_Servant_Lane_Factory is
     new Request_Scheduler_Factory with private;

   function Create
     (RCF : access Request_Scheduler_Servant_Lane_Factory)
     return Request_Scheduler_Access;

private

   type Request_Scheduler_Servant_Lane is
     new Request_Scheduler with null record;

   type Request_Scheduler_Servant_Lane_Factory is
     new Request_Scheduler_Factory with null record;

   RCF : constant Request_Scheduler_Factory_Access
     := new Request_Scheduler_Servant_Lane_Factory;

end PolyORB.Request_Scheduler.Servant_Lane;

------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--            P O L Y O R B . R E Q U E S T _ S C H E D U L E R             --
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

with PolyORB.Jobs;
with PolyORB.References;

package PolyORB.Request_Scheduler is

   -----------------------
   -- Request_Scheduler --
   -----------------------

   type Request_Scheduler is abstract tagged limited null record;

   type Request_Scheduler_Access is access all Request_Scheduler'Class;

   function Try_Queue_Request_Job
     (Self   : access Request_Scheduler;
      Job    :        PolyORB.Jobs.Job_Access;
      Target :        PolyORB.References.Ref)
     return Boolean
      is abstract;
   --  Try to have Job scheduled by Self, return False if the request
   --  scheduler refuses Job.

   -------------------------------
   -- Request_Scheduler_Factory --
   -------------------------------

   type Request_Scheduler_Factory is abstract tagged limited null record;

   type Request_Scheduler_Factory_Access is
     access all Request_Scheduler_Factory'Class;

   function Create
     (RSF : access Request_Scheduler_Factory)
     return Request_Scheduler_Access
      is abstract;
   --  Use factory to create a new Request_Scheduler

   procedure Register_Request_Scheduler_Factory
     (RSF : Request_Scheduler_Factory_Access);
   --  Register a Request_Scheduler factory

   procedure Create (RS : out Request_Scheduler_Access);
   --  Initialize a Request_Scheduler

end PolyORB.Request_Scheduler;

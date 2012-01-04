------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                 POLYORB.REQUEST_SCHEDULER.SERVANT_LANE                   --
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

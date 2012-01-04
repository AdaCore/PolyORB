------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--            P O L Y O R B . R E Q U E S T _ S C H E D U L E R             --
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

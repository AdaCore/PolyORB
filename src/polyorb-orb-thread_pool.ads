------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--              P O L Y O R B . O R B . T H R E A D _ P O O L               --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2001-2008, Free Software Foundation, Inc.          --
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

--  Implementation of thread pool architecture

package PolyORB.ORB.Thread_Pool is

   pragma Elaborate_Body;

   use PolyORB.Components;
   use PolyORB.Jobs;
   use PolyORB.Transport;

   ----------------------------------------------------
   -- Implementation of a thread-pool tasking policy --
   ----------------------------------------------------

   type Thread_Pool_Policy is new Tasking_Policy_Type with private;

   procedure Handle_New_Server_Connection
     (P   : access Thread_Pool_Policy;
      ORB :        ORB_Access;
      AC  :        Active_Connection);

   procedure Handle_Close_Connection
     (P   : access Thread_Pool_Policy;
      TE  :        Transport_Endpoint_Access);

   procedure Handle_New_Client_Connection
     (P   : access Thread_Pool_Policy;
      ORB :        ORB_Access;
      AC  :        Active_Connection);

   procedure Handle_Request_Execution
     (P   : access Thread_Pool_Policy;
      ORB :        ORB_Access;
      RJ  : access Request_Job'Class);

   procedure Idle
     (P         : access Thread_Pool_Policy;
      This_Task : in out PolyORB.Task_Info.Task_Info;
      ORB       :        ORB_Access);

   function Get_Minimum_Spare_Threads return Natural;
   function Get_Maximum_Spare_Threads return Natural;
   function Get_Maximum_Threads       return Natural;
   --  Return operational parameters of the thread pool

private

   type Thread_Pool_Policy is new Tasking_Policy_Type with null record;

end PolyORB.ORB.Thread_Pool;

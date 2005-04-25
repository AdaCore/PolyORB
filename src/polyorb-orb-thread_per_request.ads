------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--       P O L Y O R B . O R B . T H R E A D _ P E R _ R E Q U E S T        --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2002-2004 Free Software Foundation, Inc.           --
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

package PolyORB.ORB.Thread_Per_Request is

   pragma Elaborate_Body;

   use PolyORB.Components;
   use PolyORB.Jobs;
   use PolyORB.Transport;

   -----------------------------------------------------------
   -- Implementation of a thread-per-request tasking policy --
   -----------------------------------------------------------
   --  In this policy, a task is created for each request and the request
   --  is executed by this task

   type Thread_Per_Request_Policy is new Tasking_Policy_Type with private;

   procedure Handle_New_Server_Connection
     (P   : access Thread_Per_Request_Policy;
      ORB :        ORB_Access;
      C   :        Active_Connection);

   procedure Handle_Close_Connection
     (P   : access Thread_Per_Request_Policy;
      TE  :        Transport_Endpoint_Access);

   procedure Handle_New_Client_Connection
     (P   : access Thread_Per_Request_Policy;
      ORB :        ORB_Access;
      C   :        Active_Connection);

   procedure Handle_Request_Execution
     (P   : access Thread_Per_Request_Policy;
      ORB :        ORB_Access;
      RJ  : access Request_Job'Class);

   procedure Idle
     (P         : access Thread_Per_Request_Policy;
      This_Task :        PolyORB.Task_Info.Task_Info;
      ORB       :        ORB_Access);

   procedure Queue_Request_To_Handler
     (P   : access Thread_Per_Request_Policy;
      ORB :        ORB_Access;
      Msg :        Message'Class);

private

   type Thread_Per_Request_Policy is new Tasking_Policy_Type with null record;

end PolyORB.ORB.Thread_Per_Request;

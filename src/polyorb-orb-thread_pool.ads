------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--              P O L Y O R B . O R B . T H R E A D _ P O O L               --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                Copyright (C) 2001 Free Software Fundation                --
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
--              PolyORB is maintained by ENST Paris University.             --
--                                                                          --
------------------------------------------------------------------------------

--  Implementation of thread pool architecture

--  $Id$

package PolyORB.ORB.Thread_Pool is

   pragma Elaborate_Body;

   ----------------------------------------------------
   -- Implementation of a thread-pool tasking policy --
   ----------------------------------------------------

   type Thread_Pool_Policy is new Tasking_Policy_Type with private;

   procedure Handle_New_Server_Connection
     (P   : access Thread_Pool_Policy;
      ORB : ORB_Access;
      C   : Active_Connection);

   procedure Handle_New_Client_Connection
     (P   : access Thread_Pool_Policy;
      ORB : ORB_Access;
      C   : Active_Connection);

   procedure Handle_Request_Execution
     (P   : access Thread_Pool_Policy;
      ORB : ORB_Access;
      RJ  : access Jobs.Job'Class);

   procedure Idle
     (P : access Thread_Pool_Policy;
      ORB : ORB_Access);

   procedure Queue_Request_To_Handler
     (P   : access Thread_Pool_Policy;
      ORB : ORB_Access;
      Msg : Message'Class);

   procedure Initialize
     (Number_Of_Threads : Positive;
      Queue_Size        : Positive);
   --  This function must be called once before any other call in this package.
   --  Number_Of_Threads indicates how many threads will be created to deal
   --  with client requests.
   --  Queue_Size indicates how many requests can be queued while waiting for
   --  a thread to become available.
   --  When this queue is full, calls to Handle_Request_Execution become
   --  blocking.
   --  See package body for more information.

private

   type Thread_Pool_Policy is new Tasking_Policy_Type with null record;

end PolyORB.ORB.Thread_Pool;

------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--            P O L Y O R B . O R B . T A S K _ P O L I C I E S             --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                Copyright (C) 2001 Free Software Fundation                --
--                                                                          --
-- AdaBroker is free software; you  can  redistribute  it and/or modify it  --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. AdaBroker  is distributed  in the hope that it will be  useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with AdaBroker; see file COPYING. If  --
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

--  Tasking policy for the ORB core: 'No_Tasking'.

--  $Id$

package PolyORB.ORB.Task_Policies is

   pragma Elaborate_Body;

   ---------------------------------------------------------
   -- Simple policy for configuration without any tasking --
   ---------------------------------------------------------

   --  This policy may be used for the creation of a low-profile
   --  ORB that does not depend on the Ada tasking runtime library.
   --  It is suitable for use in a node that contains only an
   --  environment task.

   type No_Tasking is new Tasking_Policy_Type with private;

   procedure Handle_New_Server_Connection
     (P   : access No_Tasking;
      ORB : ORB_Access;
      C   : Active_Connection);

   procedure Handle_New_Client_Connection
     (P   : access No_Tasking;
      ORB : ORB_Access;
      C   : Active_Connection);

   procedure Handle_Request_Execution
     (P   : access No_Tasking;
      ORB : ORB_Access;
      RJ  : access Jobs.Job'Class);

   procedure Idle (P : access No_Tasking; ORB : ORB_Access);

   procedure Queue_Request_To_Handler
     (P   : access No_Tasking;
      ORB : ORB_Access;
      Msg : Message'Class);

private

   type No_Tasking is new Tasking_Policy_Type with null record;

end PolyORB.ORB.Task_Policies;

------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                 P O L Y O R B . Q O S . P R I O R I T Y                  --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2004-2005 Free Software Foundation, Inc.           --
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
--                  PolyORB is maintained by AdaCore                        --
--                     (email: sales@adacore.com)                           --
--                                                                          --
------------------------------------------------------------------------------

with PolyORB.Annotations;
with PolyORB.Tasking.Priorities;

package PolyORB.QoS.Priority is

   pragma Elaborate_Body;

   package PTP renames PolyORB.Tasking.Priorities;

   use PolyORB.Annotations;
   use PolyORB.Tasking.Priorities;

   type QoS_Static_Priority is new QoS_Parameter (Static_Priority) with record
      EP : PTP.External_Priority;
   end record;

   type Thread_Priority_Note is new Note with record
      Priority : External_Priority;
   end record;

   Default_Note : constant Thread_Priority_Note
     := Thread_Priority_Note'(Note with Priority => Invalid_Priority);

end PolyORB.QoS.Priority;

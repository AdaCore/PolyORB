------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                 P O L Y O R B . Q O S . P R I O R I T Y                  --
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

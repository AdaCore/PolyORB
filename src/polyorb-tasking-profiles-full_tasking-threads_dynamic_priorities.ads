------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--    POLYORB.TASKING.PROFILES.FULL_TASKING.THREADS_DYNAMIC_PRIORITIES      --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--            Copyright (C) 2003 Free Software Foundation, Inc.             --
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

--  This packages provides a thread library for an Ada full tasking
--  runtime, using Ada.Dynamic_Priorities.

with PolyORB.Tasking.Threads;
with PolyORB.Tasking.Profiles.Full_Tasking.Threads;

with System;

package PolyORB.Tasking.Profiles.Full_Tasking.Threads_Dynamic_Priorities is

   package PTPFT renames PolyORB.Tasking.Profiles.Full_Tasking.Threads;
   package PTT   renames PolyORB.Tasking.Threads;

   type Full_Tasking_DP_Thread_Factory_Type is
     new PTPFT.Full_Tasking_Thread_Factory_Type with private;

   type Full_Tasking_DP_Thread_Factory_Type_Access is
     access all Full_Tasking_DP_Thread_Factory_Type'Class;

   procedure Set_Priority
     (TF : access Full_Tasking_DP_Thread_Factory_Type;
      T  :        PTT.Thread_Id;
      P  :        System.Any_Priority);

   function Get_Priority
     (TF : access Full_Tasking_DP_Thread_Factory_Type;
      T  :        PTT.Thread_Id)
     return System.Any_Priority;

private

   type Full_Tasking_DP_Thread_Factory_Type is
     new PTPFT.Full_Tasking_Thread_Factory_Type with null record;

   The_Thread_Factory : constant Full_Tasking_DP_Thread_Factory_Type_Access
     := new Full_Tasking_DP_Thread_Factory_Type;

end PolyORB.Tasking.Profiles.Full_Tasking.Threads_Dynamic_Priorities;

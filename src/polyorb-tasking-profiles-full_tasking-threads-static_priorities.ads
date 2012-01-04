------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--     POLYORB.TASKING.PROFILES.FULL_TASKING.THREADS.STATIC_PRIORITIES      --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2003-2012, Free Software Foundation, Inc.          --
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

--  This packages provides a thread library for an Ada full tasking
--  runtime, using Ada.Dynamic_Priorities.

package PolyORB.Tasking.Profiles.Full_Tasking.Threads.Static_Priorities is

   procedure Set_Priority
     (TF : access Full_Tasking_Thread_Factory_Type;
      T  :        PTT.Thread_Id;
      P  :        System.Any_Priority);

   function Get_Priority
     (TF : access Full_Tasking_Thread_Factory_Type;
      T  :        PTT.Thread_Id)
     return System.Any_Priority;

end PolyORB.Tasking.Profiles.Full_Tasking.Threads.Static_Priorities;

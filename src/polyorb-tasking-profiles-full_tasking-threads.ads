------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--              POLYORB.TASKING.PROFILES.FULL_TASKING.THREADS               --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--             Copyright (C) 1999-2003 Free Software Fundation              --
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

--  This package provide an implementation for base types for tasking
--  using full Ada tasking, yet it does not allow for dynamic priority
--  modification of a running thread. For all comments, see
--  PolyORB.Tasking.Threads.

with Ada.Task_Identification;

with PolyORB.Tasking.Threads;

with System;

package PolyORB.Tasking.Profiles.Full_Tasking.Threads is

   package PTT renames PolyORB.Tasking.Threads;

   ------------------------------
   -- Full_Tasking_Thread Type --
   ------------------------------

   type Full_Tasking_Thread_Type is
     new PTT.Thread_Type with private;

   function Get_Thread_Id
     (T : access Full_Tasking_Thread_Type)
     return PTT.Thread_Id;

   type Full_Tasking_Thread_Access
      is access all Full_Tasking_Thread_Type'Class;

   ---------------------------------
   -- Full_Tasking_Thread_Factory --
   ---------------------------------

   type Full_Tasking_Thread_Factory_Type is
     new PTT.Thread_Factory_Type with private;

   type Full_Tasking_Thread_Factory_Access
     is access all Full_Tasking_Thread_Factory_Type'Class;

   The_Thread_Factory : constant Full_Tasking_Thread_Factory_Access;

   function Run_In_Task
     (TF               : access Full_Tasking_Thread_Factory_Type;
      Name             : String := "";
      Default_Priority : System.Any_Priority := System.Default_Priority;
      R                : PTT.Runnable_Access;
      C                : PTT.Runnable_Controller_Access)
     return PTT.Thread_Access;

   function Run_In_Task
     (TF               : access Full_Tasking_Thread_Factory_Type;
      Name             : String := "";
      Default_Priority : System.Any_Priority := System.Default_Priority;
      P                : PTT.Parameterless_Procedure)
     return PTT.Thread_Access;

   function Get_Current_Thread_Id
     (TF : access Full_Tasking_Thread_Factory_Type)
     return PTT.Thread_Id;

   function P_To_A_Task_Id (TID : PTT.Thread_Id)
     return Ada.Task_Identification.Task_Id;
   pragma Inline (P_To_A_Task_Id);
   --  Convert PolyORB Task_Id to Ada Task_Id.

   function Thread_Id_Image
     (TF : access Full_Tasking_Thread_Factory_Type;
      TID : PTT.Thread_Id)
     return String;

   procedure Set_Priority
     (TF : access Full_Tasking_Thread_Factory_Type;
      T  :        PTT.Thread_Id;
      P  :        System.Any_Priority);
   pragma No_Return (Set_Priority);
   --  Setting priority has no meaning under this profile,
   --  raise PolyORB.Tasking.Tasking_Profile_Error.

   function Get_Priority
     (TF : access Full_Tasking_Thread_Factory_Type;
      T  :        PTT.Thread_Id)
     return System.Any_Priority;
   --  XXX not implemented

private

   type Full_Tasking_Thread_Type is new PTT.Thread_Type with record
      Id        : PTT.Thread_Id;
      Priority  : System.Any_Priority;
   end record;

   type Full_Tasking_Thread_Factory_Type is
     new PTT.Thread_Factory_Type with null record;

   The_Thread_Factory : constant Full_Tasking_Thread_Factory_Access
     := new Full_Tasking_Thread_Factory_Type;

end PolyORB.Tasking.Profiles.Full_Tasking.Threads;

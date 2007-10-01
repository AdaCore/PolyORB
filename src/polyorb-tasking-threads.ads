------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--              P O L Y O R B . T A S K I N G . T H R E A D S               --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2002-2007, Free Software Foundation, Inc.          --
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

--  This package provides the base abstract interface for threads, as well
--  as clock time abstractions.
--  Real implementations for the different profiles are given by extending
--  Thread_Type and by registering the implementations of some procedures.

--  Some terminology issues:
--  in this package, and in its profile-specific implementations,
--  a task designates the common abstraction of processors as provided
--  by the Ada 95 language.
--  A Thread will only denote the type defined in this package,
--  which is only a container for the parameters of the task.

with System;

package PolyORB.Tasking.Threads is

   pragma Preelaborate;

   type Parameterless_Procedure is access procedure;

   Default_Storage_Size : constant := 262_144;

   --------------
   -- Runnable --
   --------------

   type Runnable is abstract tagged limited null record;
   --  Runnable is a type for elementary work units.

   type Runnable_Access is access all Runnable'Class;

   procedure Run (R : access Runnable) is abstract;
   --  Main procedure for the Runnable.

   type Runnable_Controller is tagged limited null record;
   --  A Runnable_Controller is used by the tasking runtime to
   --  control the deallocation of Runnables. This type should be
   --  extended to customize the deallocation policy for Runnable

   type Runnable_Controller_Access is access all Runnable_Controller'Class;

   procedure Free_Runnable
     (C : in out Runnable_Controller;
      R : in out Runnable_Access);
   --  Deallocate R.
   --  This default implementation assumes that the Runnable has
   --  been dynamically allocated, and performs an Unchecked_Deallocation.

   ----------------
   -- Thread Ids --
   ----------------

   type Thread_Id is private;
   --  Type used for identifying Threads.  An unique Thread_Id is
   --  assigned to each Thread at creation time.

   function Null_Thread_Id return Thread_Id;

   function Image (TID : Thread_Id) return String;
   --  Return a human-readable representation of TID.

   function Current_Task return Thread_Id;
   --  Return the Thread associated to the current task.

   function To_Address (TID : Thread_Id) return System.Address;
   pragma Inline (To_Address);

   function To_Thread_Id (A : System.Address) return Thread_Id;
   pragma Inline (To_Thread_Id);

   -----------------
   -- Thread Type --
   -----------------

   type Thread_Type is abstract tagged limited null record;
   --  This type is a task proxy. It should be given to a task creation
   --  procedure, so that its main procedure will be run in a different task.
   --  The difference between a Runnable and a Thread is that a thread has some
   --  information about the scheduling, such as its priority.  This type is
   --  derived by each concrete tasking profile.

   type Thread_Access is access all Thread_Type'Class;

   function Get_Thread_Id
     (T : access Thread_Type)
     return Thread_Id
      is abstract;
   --  Get the identifier of the Thread.

   --------------------
   -- Thread_Factory --
   --------------------

   type Thread_Factory_Type is abstract tagged limited null record;
   --  A factory of Thread_Type objects.
   --  This type is derived by each concrete tasking profile.

   type Thread_Factory_Access is access all Thread_Factory_Type'Class;

   procedure Create_Task
     (Main : Parameterless_Procedure);
   --  Call Run_In_Task with the Thread_Factory object registered in this
   --  package, to run the code in Main, with other parameters
   --  defaulted. The resulting Thread_Access is discarded.

   function Run_In_Task
     (TF               : access Thread_Factory_Type;
      Name             : String := "";
      Default_Priority : System.Any_Priority := System.Default_Priority;
      Storage_Size     : Natural := 0;
      R                : Runnable_Access;
      C                : Runnable_Controller_Access)
     return Thread_Access
     is abstract;
   --  Create a Thread according to the tasking profile. R is the
   --  Runnable that will be executed by the task associated to the
   --  created Thread. Name is used as a key to look up configuration
   --  information. Default_Priority will be the priority of the task
   --  if no priority is given in the configuration file. A
   --  Storage_Size of 0 means the Thread will use default value for
   --  its stack size, else it will use the value provided by
   --  Storage_Size.

   --  If a preallocated task with appropriate parameters exists, the
   --  Runnable is executed by that task.
   --  Otherwise, if the tasking profile allows dynamic task allocation,
   --  a new task is created and executes the Runnable.
   --  Otherwise, a profile-dependant exception is raised.
   --  The deallocation of R after completion is delegated to C.
   --  It is assumed that C is dynamically allocated.

   function Run_In_Task
     (TF               : access Thread_Factory_Type;
      Name             : String := "";
      Default_Priority : System.Any_Priority := System.Default_Priority;
      Storage_Size     : Natural := 0;
      P                : Parameterless_Procedure)
     return Thread_Access
     is abstract;
   --  This function plays the same role that the first one; the
   --  difference is that the code of the Thread is P.  In some
   --  profiles, this function ensure that no dynamic allocation is
   --  done. A Storage_Size of 0 means the Thread will use default
   --  value for its stack size, else it will use the value provided
   --  by Storage_Size.

   function Get_Current_Thread_Id
     (TF : access Thread_Factory_Type)
     return Thread_Id
      is abstract;
   --  Get the Thread object associated with the current task

   function Thread_Id_Image
     (TF  : access Thread_Factory_Type;
      TID :        Thread_Id)
     return String
     is abstract;
   --  Return a human-readable interpretation of TID.

   function Get_Thread_Factory
     return Thread_Factory_Access;
   pragma Inline (Get_Thread_Factory);
   --  Get the Thread_Factory object registered in this package.
   --  WARNING : if you make a call to this function at elaboration
   --  time, it will return null, as no Thread_Factory will be
   --  registered. So DO NOT call this function at elaboration time!

   procedure Register_Thread_Factory
     (TF : Thread_Factory_Access);
   --  Register the factory corresponding to the chosen tasking profile.
   --  Must be called exactly once per partition.

   procedure Set_Priority
     (TF : access Thread_Factory_Type;
      T  :        Thread_Id;
      P  :        System.Any_Priority) is abstract;
   --  Change the priority of the task T. Raise Tasking_Error if it is not
   --  permitted by the tasking profile in use.

   function Get_Priority
     (TF : access Thread_Factory_Type;
      T  :        Thread_Id)
     return System.Any_Priority is abstract;
   --  Return the priority of the task T. Raise Tasking_Error if it is not
   --  permitted by the tasking profile in use.

   procedure Relative_Delay (D : Duration);
   procedure Relative_Delay
     (TF : access Thread_Factory_Type; D : Duration) is abstract;
   --  Delay the calling task for duration D

   Node_Boot_Time : Duration;
   --  Node boot time as a duration elapsed since some unspecified epoch,
   --  set at initialization.

   -------------------
   -- Task Counting --
   -------------------

   function Awake_Count return Natural;
   function Independent_Count return Natural;
   --  Wrappers for the functions below

   function Awake_Count (TF : access Thread_Factory_Type)
     return Natural is abstract;
   --  Returns the number of awake tasks

   function Independent_Count (TF : access Thread_Factory_Type)
     return Natural is abstract;
   --  Returns the number of independent tasks

private
   type Thread_Id is new System.Address;
end PolyORB.Tasking.Threads;

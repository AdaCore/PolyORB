------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--              P O L Y O R B . T A S K I N G . T H R E A D S               --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--             Copyright (C) 1999-2002 Free Software Fundation              --
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

--  This package provides base types for tasking.
--  Real implementations for the different profiles are given by extending
--  Thread_Type and by registering the implementations of some procedures.

--  $Id$

--  Some terminology issues :
--  in this package, and in its profile specific implementations,
--  a task designate the common abstraction, which an abstraction
--  of processors. A Thread will only designate the type defined in
--  this package, which is just a record for the parameter of the task.

with System;

package PolyORB.Tasking.Threads is

   pragma Preelaborate;

   type Parameterless_Procedure is access procedure;

   --------------
   -- Runnable --
   --------------

   type Runnable is abstract tagged null record;
   --  Runnable is a type for elementary jobs.

   type Runnable_Access is access all Runnable'Class;

   procedure Run (R : access Runnable)
     is abstract;
   --  main procedure for the Runnable.

   ----------------
   -- Thread Ids --
   ----------------

   type Thread_Id is abstract tagged null record;
   --  Type used for identifying Threads.  An unique Thread_Id is
   --  given at each Thread at creation time; that means that, if we
   --  create two Threads and compare two copies of their Thread_Id
   --  using the "=" operator, the comparison must return False.  We
   --  should not use Ada.Task_Identification here, as in some high
   --  integrity runtimes it is not provided.
   --  A subclass of this type can be found for every tasking profile.

   type Thread_Id_Access is access all Thread_Id'Class;

   function "="
     (T1 : Thread_Id;
      T2 : Thread_Id)
     return Boolean
      is abstract;
   --  Compare the content of T1 and T2; return True if they are equals.

   function Image (T : Thread_Id) return String is abstract;

   -----------------
   -- Thread Type --
   -----------------

   type Thread_Type is abstract tagged limited null record;
   --  This type is a task proxy. It should be given to a task creation
   --  procedure, so that its main procedure will be runned in a
   --  different task.  The difference between a Runnable and a Thread
   --  is that a thread has some informations about the scheduling,
   --  such as its priority.

   type Thread_Access is access all Thread_Type'Class;

   procedure Run (T : access Thread_Type)
     is abstract;
   --  Main procedure of the thread. It should mainly be a call to Run
   --  of an internal Runnable.

   function Get_Thread_Id
     (T : access Thread_Type)
     return Thread_Id_Access
      is abstract;
   --  Get the identifier of the Thread.

   --------------------
   -- Thread_Factory --
   --------------------

   type Thread_Factory_Type is abstract tagged limited null record;
   --  This type is a factory for the Thread type.  A subclass of this
   --  factory exists for every tasking profile : Full Tasking,
   --  Ravenscar and No Tasking.  This type provides functionalities
   --  depending of the tasking profile, and it particularly provides
   --  functionnalities to create the Thread type corresponding to the
   --  chosen profile.

   type Thread_Factory_Access is access all Thread_Factory_Type'Class;

   procedure Copy_Thread_Id
     (TF     : access Thread_Factory_Type;
      Source : Thread_Id'Class;
      Target : Thread_Id_Access)
      is abstract;
   --  Copy Source in Target.all. It assume of course that the types of
   --  Target and Source are the same; if not, it raise an assertion failure.
   --  Indeed, their types depend on the profile under which they are created,
   --  and in a logic node every object is created under the same profile.

   function Create_Thread
     (TF               : access Thread_Factory_Type;
      Name             : String := "";
      Default_Priority : System.Any_Priority := System.Default_Priority;
      R                : access Runnable'Class)
     return Thread_Access
     is abstract;
   --  Create a Thread according to the tasking profile.  R is the
   --  Runnable that will be executed by the task associated to the
   --  created Thread.  Name will be the name of the type of thread.
   --  This name will be used to get the configuration of this thread
   --  from the configuration module. Default_Priority will be the priority
   --  of the task if no priority is given in the configuration file.
   --  Note that the main context is associated to a preallocated Thread
   --  at initialisation time, in order to be able to use PolyORB.Tasking
   --  API in this context.
   --  R is copied. It is the responsability of the caller to deallocate it.

   procedure Create_Task
     (TF : in out  Thread_Factory_Type;
      T  : access Thread_Type'Class)
     is abstract;
   --  Use the Thread given in paramater to create a new task, or to
   --  get a preallocated task; then it runs its code in this task.
   --  This procedure should not be called two times with the same T
   --  given in paramater, because in this case we would have the same
   --  Thread_Id for different tasks, which may be misleading.  So
   --  don't keep a access on a Thread_Type if you don't need it.
   --  A typical use would be :
   --     declare
   --        T : Thread_Access := Create_Thread (My_Thread_Factory,
   --                                            "My_Label",
   --                                            My_Runnable);
   --     begin
   --        Create_Task (My_Thread_Factory.all, T);
   --     end;

   function Get_Current_Thread_Id
     (TF : access Thread_Factory_Type)
     return Thread_Id'Class
      is abstract;
   --  If we are running in a task created using Create_Task,
   --  get the Thread object associated with the current task.
   --  If the current task was not created by this API but
   --  for example by a direct call to the Ada tasking facilities,
   --  this call will raise a PolyORB.Tasking.Tasking_Error.

   function Get_Thread_Factory
     return Thread_Factory_Access;
   pragma Inline (Get_Thread_Factory);
   --  Get the Thread_Factory object registered in this package.

   procedure Register_Thread_Factory
     (TF : Thread_Factory_Access);
   --  Register the factory corresponding to the chosen tasking profile.

end PolyORB.Tasking.Threads;

------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--            P O L Y O R B . R A V E N S C A R . T H R E A D S             --
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

--  Abstraction types for Ravenscar tasking.

--  Under the Ravenscar profile, "Threads" are
--  associated with an unique synchronisation
--  object on which is the only one they can wait on.
--  This assures that only one task wait on every
--  entry, as required in the Ravenscar profile.

--  The child packages of this package should only
--  be packages providing tasking facilities. Other
--  packages shoud not have access to "suspend" and "resume",
--  the procedures affecting the internal
--  synchronisation object.

--  $Id$

with PolyORB.Tasking.Threads;
with System;

package PolyORB.Ravenscar.Threads is
   pragma Elaborate_Body;

   use PolyORB.Tasking.Threads;

   --  Ravenscar tasking profile.
   --  The documentation for the following declarations can be
   --  found in PolyORB.Tasking.Threads.

   type Ravenscar_Thread_Id is new Thread_Id with private;

   type Ravenscar_Thread_Id_Access is access all Ravenscar_Thread_Id'Class;

   function "="
     (T1 : Ravenscar_Thread_Id;
      T2 : Ravenscar_Thread_Id)
     return Boolean;

   function Image (T : Ravenscar_Thread_Id) return String;

   type Ravenscar_Thread_Type is new Thread_Type with private;

   type Ravenscar_Thread_Access is access all Ravenscar_Thread_Type'Class;

   procedure Run (T : access Ravenscar_Thread_Type);

   function Get_Thread_Id (T : access Ravenscar_Thread_Type)
     return Thread_Id_Access;

   type Ravenscar_Thread_Factory_Type is
     new Thread_Factory_Type with private;

   type Ravenscar_Thread_Factory_Access is
     access all Ravenscar_Thread_Factory_Type'Class;

   The_Thread_Factory : constant Ravenscar_Thread_Factory_Access;
   --  The thread factory for this profile.

   function Create_Thread
     (TF   : access Ravenscar_Thread_Factory_Type;
      Name : String := "";
      Default_Priority : System.Any_Priority := System.Default_Priority;
      R    : access Runnable'Class)
     return Thread_Access;

   procedure Create_Task
     (TF : in out  Ravenscar_Thread_Factory_Type;
      T  : access Thread_Type'Class);

   function Get_Current_Thread_Id
     (TF : access Ravenscar_Thread_Factory_Type)
     return Thread_Id'Class;

   procedure Copy_Thread_Id
     (TF     : access Ravenscar_Thread_Factory_Type;
      Source : Thread_Id'Class;
      Target : Thread_Id_Access);

   --  The following procedures make access to the
   --  internal synchronisation object, so it should
   --  only be used by other packages that thread pool ones,
   --  and synchronisations.

   procedure Suspend (T : Ravenscar_Thread_Id);
   --  Calling this procedure, the current task await on the internal
   --  synchronisation.
   --  The calling task MUST be the one which abstraction
   --  is "T". Else, it would wait on a synchronisation
   --  object that doesn't belong to her, which would
   --  raise a Program_Error if another task call
   --  "Suspend" on the same synchronisation object.

   procedure Resume (T : Ravenscar_Thread_Id);
   --  The call to this procedure free the task waiting
   --  on the internal synchronisation object of "T".

   function Get_Thread_Index (T : Ravenscar_Thread_Id)
                             return Integer;
   --  return a different integer for each Thread_Id.

   procedure Initialize;
   --  Initialize the package.

private

   type Ravenscar_Thread_Id is new Thread_Id with record
      Id : Integer;
   end record;

   type Ravenscar_Thread_Factory_Type is new Thread_Factory_Type with record
        null;
   end record;

   The_Thread_Factory : constant Ravenscar_Thread_Factory_Access
     := new Ravenscar_Thread_Factory_Type;

   type Ravenscar_Thread_Type is new Thread_Type with record
      Id   : aliased Ravenscar_Thread_Id;
      --  Id of the Thread.

      Run  : Runnable_Access;
      --  Runnable that contains the code that will be executed
      --  by the task associated to the Thread object.

   end record;

end PolyORB.Ravenscar.Threads;

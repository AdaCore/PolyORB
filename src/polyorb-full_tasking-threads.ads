------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--         P O L Y O R B - F U L L _ T A S K I N G - T H R E A D S          --
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

--  This package provide a real implementation for base types for tasking
--  using full Ada tasking. For all comments, see PolyORB.Tasking.Threads.

--  $Id$

with Ada.Task_Identification;
with PolyORB.Tasking.Threads;
with System;

package PolyORB.Full_Tasking.Threads is

   package PTT renames PolyORB.Tasking.Threads;

   ----------------------------
   -- Full_Tasking_Thread_Id --
   ----------------------------

   type Full_Tasking_Thread_Id is new PTT.Thread_Id with private;

   type Full_Tasking_Thread_Id_Access is
     access all Full_Tasking_Thread_Id'Class;

   function "="
     (T1 : Full_Tasking_Thread_Id;
      T2 : Full_Tasking_Thread_Id)
     return Boolean;

   function Image (T : Full_Tasking_Thread_Id) return String;

   ------------------------------
   -- Full_Tasking_Thread Type --
   ------------------------------

   type Full_Tasking_Thread_Type is
     new PTT.Thread_Type with private;

   procedure Run (T : access Full_Tasking_Thread_Type);

   function Get_Thread_Id
     (T : access Full_Tasking_Thread_Type)
     return PTT.Thread_Id_Access;


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

   procedure Copy_Thread_Id
     (TF     : access Full_Tasking_Thread_Factory_Type;
      Source : PTT.Thread_Id'Class;
      Target : PTT.Thread_Id_Access);

   function Create_Thread
     (TF               : access Full_Tasking_Thread_Factory_Type;
      Name             : String := "";
      Default_Priority : System.Any_Priority := System.Default_Priority;
      R                : access PTT.Runnable'Class)
     return PTT.Thread_Access;

   procedure Create_Task
     (TF : in out  Full_Tasking_Thread_Factory_Type;
      T  : access PTT.Thread_Type'Class);

   function Get_Current_Thread_Id
     (TF : access Full_Tasking_Thread_Factory_Type)
     return PTT.Thread_Id'Class;

private

   type Full_Tasking_Thread_Id is new PTT.Thread_Id with record
      Tid : Ada.Task_Identification.Task_Id;
   end record;

   type Full_Tasking_Thread_Type is new PTT.Thread_Type with record
      Id        : PTT.Thread_Id_Access;
      Run       : PTT.Runnable_Access;
      Priority  : System.Any_Priority;
   end record;

   type Full_Tasking_Thread_Factory_Type is
     new PTT.Thread_Factory_Type with record
        null;
     end record;

   The_Thread_Factory : constant Full_Tasking_Thread_Factory_Access
     := new Full_Tasking_Thread_Factory_Type;

end PolyORB.Full_Tasking.Threads;

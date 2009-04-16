------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--               POLYORB.TASKING.PROFILES.NO_TASKING.THREADS                --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2002-2008, Free Software Foundation, Inc.          --
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

--  Implementation of PolyORB.Tasking.Threads for the No_Tasking profile.

with System;

with PolyORB.Tasking.Threads;

package PolyORB.Tasking.Profiles.No_Tasking.Threads is

   package PTT renames PolyORB.Tasking.Threads;

   ----------------------------
   -- No_Tasking_Thread Type --
   ----------------------------

   type No_Tasking_Thread_Type is
     new PTT.Thread_Type with private;

   type No_Tasking_Thread_Access is
     access all No_Tasking_Thread_Type'Class;

   function Get_Thread_Id
     (T : access No_Tasking_Thread_Type)
     return PTT.Thread_Id;
   --  Under No_Tasking profile, this function simply return
   --  Null_Thread_Id.

   -------------------------------
   -- No_Tasking_Thread_Factory --
   -------------------------------

   type No_Tasking_Thread_Factory_Type is
     new PTT.Thread_Factory_Type with private;

   type No_Tasking_Thread_Factory_Access
     is access all No_Tasking_Thread_Factory_Type'Class;

   The_Thread_Factory : constant No_Tasking_Thread_Factory_Access;

   function Run_In_Task
     (TF               : access No_Tasking_Thread_Factory_Type;
      Name             : String := "";
      Default_Priority : System.Any_Priority := System.Default_Priority;
      Storage_Size     : Natural := 0;
      R                : PTT.Runnable_Access) return PTT.Thread_Access;
   --  This function has no sense in No_Tasking profile.
   --  It simply raises a Tasking_Error.

   function Run_In_Task
     (TF               : access No_Tasking_Thread_Factory_Type;
      Name             : String := "";
      Default_Priority : System.Any_Priority := System.Default_Priority;
      Storage_Size     : Natural := 0;
      P                : PTT.Parameterless_Procedure) return PTT.Thread_Access;
   --  This function has no sense in No_Tasking profile.
   --  It simply raises a Tasking_Error.

   function Get_Current_Thread_Id
     (TF : access No_Tasking_Thread_Factory_Type)
     return PTT.Thread_Id;
   --  Under No_Tasking profile, this function simply return
   --  Null_Thread_Id.

   function Thread_Id_Image
     (TF  : access No_Tasking_Thread_Factory_Type;
      TID : PTT.Thread_Id)
     return String;
   --  Under No_Tasking profile, this function simply return
   --  "main_task".

   procedure Set_Priority
     (TF : access No_Tasking_Thread_Factory_Type;
      T  :        PTT.Thread_Id;
      P  :        System.Any_Priority);
   pragma No_Return (Set_Priority);
   --  Setting priority has no meaning under this profile, raise Tasking_Error

   function Get_Priority
     (TF : access No_Tasking_Thread_Factory_Type;
      T  :        PTT.Thread_Id)
     return System.Any_Priority;
   --  Getting priority has no meaning under this profile, raise Tasking_Error

   procedure Relative_Delay
     (TF : access No_Tasking_Thread_Factory_Type; D : Duration);

   function Awake_Count (TF : access No_Tasking_Thread_Factory_Type)
     return Natural;
   --  This function always return 1 under No_Tasking profile

   function Independent_Count (TF : access No_Tasking_Thread_Factory_Type)
     return Natural;
   --  This function always return 0 under No_Tasking profile

private

   type No_Tasking_Thread_Type is new PTT.Thread_Type with null record;

   type No_Tasking_Thread_Factory_Type is
     new PTT.Thread_Factory_Type with null record;

   The_Thread_Factory : constant No_Tasking_Thread_Factory_Access
     := new No_Tasking_Thread_Factory_Type;

end PolyORB.Tasking.Profiles.No_Tasking.Threads;

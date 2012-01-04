------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                      R T C O R B A . C U R R E N T                       --
--                                                                          --
--                                 B o d y                                  --
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

with CORBA.Object;
with RTCORBA.PriorityMapping;

with PolyORB.CORBA_P.Initial_References;
with PolyORB.RTCORBA_P.Setup;

with PolyORB.Annotations;
with PolyORB.Initialization;
with PolyORB.QoS.Priority;
with PolyORB.Tasking.Priorities;
with PolyORB.Tasking.Threads.Annotations;
with PolyORB.Utils.Strings.Lists;

package body RTCORBA.Current is

   use PolyORB.Annotations;
   use PolyORB.Tasking.Priorities;
   use PolyORB.Tasking.Threads.Annotations;
   use PolyORB.QoS.Priority;

   function Create return CORBA.Object.Ref;
   --  Create a RTCORBA.Current.Ref

   ------------
   -- Create --
   ------------

   function Create return CORBA.Object.Ref is
      Result : Local_Ref;
      Current : constant PolyORB.Smart_Pointers.Entity_Ptr :=
                  new Current_Object;
   begin
      Set (Result, Current);
      return CORBA.Object.Ref (Result);
   end Create;

   ----------------------
   -- Get_The_Priority --
   ----------------------

   function Get_The_Priority
     (Self : Local_Ref)
     return RTCORBA.Priority
   is
      pragma Unreferenced (Self);

      Note : Thread_Priority_Note;
   begin
      Get_Note (Get_Current_Thread_Notepad.all, Note, Default_Note);

      if Note.Priority = Invalid_Priority then
         CORBA.Raise_Initialize (CORBA.Default_Sys_Member);
      end if;

      return RTCORBA.Priority (Note.Priority);
   end Get_The_Priority;

   ----------------------
   -- Set_The_Priority --
   ----------------------

   procedure Set_The_Priority
     (Self : Local_Ref;
      To   : RTCORBA.Priority)
   is
      pragma Unreferenced (Self);

      use type PolyORB.RTCORBA_P.Setup.PriorityMapping_Access;

      Success : CORBA.Boolean;
      New_Priority : RTCORBA.NativePriority;

      Priority_Mapping : constant
        PolyORB.RTCORBA_P.Setup.PriorityMapping_Access
        := PolyORB.RTCORBA_P.Setup.Get_Priority_Mapping;

   begin
      --  Consistency check: To is in range 0 .. 32767

      if To < 0 then
         --  Implementation Note: To is a CORBA.Short, thus To < 32767
         --  is always true.

         CORBA.Raise_Bad_Param (CORBA.Default_Sys_Member);
      end if;

      --  Compute new priority

      if Priority_Mapping = null then
         CORBA.Raise_Internal (CORBA.Default_Sys_Member);
      end if;

      RTCORBA.PriorityMapping.To_Native
        (Priority_Mapping.all,
         To,
         New_Priority,
         Success);

      if not Success then
         CORBA.Raise_Data_Conversion
           (CORBA.System_Exception_Members'(Minor     => 2,
                                            Completed => CORBA.Completed_No));
      end if;

      declare

         use PolyORB.Tasking.Threads;

         Note : Thread_Priority_Note;
         Notepad : constant Notepad_Access := Get_Current_Thread_Notepad;

      begin
         Get_Note (Notepad.all, Note, Default_Note);

         --  Modify priority

         if Note.Priority /= External_Priority (New_Priority)
           or else Get_Priority (Get_Thread_Factory, Current_Task)
           /= Integer (New_Priority)
         then
            Set_Priority
              (Get_Thread_Factory,
               Current_Task,
               Integer (New_Priority));
         end if;

         --  Update current object

         Note.Priority := External_Priority (To);
         Set_Note (Notepad.all, Note);

      end;
   end Set_The_Priority;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize;

   procedure Initialize is
      use PolyORB.CORBA_P.Initial_References;

   begin
      Register_Initial_Reference ("RTCurrent", Create'Access);
   end Initialize;

   use PolyORB.Initialization;
   use PolyORB.Utils.Strings;
   use PolyORB.Utils.Strings.Lists;

begin
   Register_Module
     (Module_Info'
      (Name      => +"rtcorba.current",
       Conflicts => Empty,
       Depends   => +"corba.initial_references"
       & "tasking.annotations",
       Provides  => Empty,
       Implicit  => False,
       Init      => Initialize'Access,
       Shutdown  => null));
end RTCORBA.Current;

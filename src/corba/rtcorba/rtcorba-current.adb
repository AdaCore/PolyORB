------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                      R T C O R B A . C U R R E N T                       --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2003-2004 Free Software Foundation, Inc.           --
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

with CORBA.Object;
with RTCORBA.PriorityMapping;

with PolyORB.CORBA_P.Initial_References;
with PolyORB.RTCORBA_P.Setup;

with PolyORB.Annotations;
with PolyORB.Initialization;
pragma Elaborate_All (PolyORB.Initialization); --  WAG:3.15

with PolyORB.Smart_Pointers;
with PolyORB.Tasking.Priorities;
with PolyORB.Tasking.Threads.Annotations;
with PolyORB.Utils.Strings.Lists;
with PolyORB.Request_QoS.Priority;

package body RTCORBA.Current is

   use PolyORB.Annotations;
   use PolyORB.Tasking.Priorities;
   use PolyORB.Tasking.Threads.Annotations;
   use PolyORB.Request_QoS.Priority;

   function Create return CORBA.Object.Ref;
   --  Create a RTCORBA.Current.Ref

   ------------
   -- Create --
   ------------

   function Create return CORBA.Object.Ref is
      Result : Local_Ref;

      Current : constant PolyORB.Smart_Pointers.Entity_Ptr
        := new Current_Object;

   begin
      Set (Result, Current);

      return CORBA.Object.Ref (Result);
   end Create;

   ----------------------
   -- Get_The_Priority --
   ----------------------

   function Get_The_Priority
     (Self : in Local_Ref)
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
     (Self : in Local_Ref;
      To   : in RTCORBA.Priority)
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
       Init      => Initialize'Access));
end RTCORBA.Current;


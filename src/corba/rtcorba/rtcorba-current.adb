------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                      R T C O R B A . C U R R E N T                       --
--                                                                          --
--                                 B o d y                                  --
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

--  $Id$

with CORBA.Object;
with RTCORBA.PriorityMapping;

with PolyORB.CORBA_P.Initial_References;
with PolyORB.RTCORBA_P.Setup;

with PolyORB.Initialization;
pragma Elaborate_All (PolyORB.Initialization); --  WAG:3.15

with PolyORB.Smart_Pointers;
with PolyORB.Tasking.Threads;
with PolyORB.Utils.Strings.Lists;

package body RTCORBA.Current is

   type Current_Object is new PolyORB.Smart_Pointers.Entity with record
      Id             : PolyORB.Tasking.Threads.Thread_Id;
      Last_Value_Set : RTCORBA.Priority := Unset_Priority;
   end record;

   function Create return CORBA.Object.Ref;
   --  Create a RTCORBA.Current.Ref

   ------------
   -- Create --
   ------------

   function Create return CORBA.Object.Ref
   is
      Result : Ref;

      Current : constant PolyORB.Smart_Pointers.Entity_Ptr
        := new Current_Object;

   begin
      Current_Object (Current.all).Id
        := PolyORB.Tasking.Threads.Current_Task;

      Set (Result, Current);

      return CORBA.Object.Ref (Result);
   end Create;

   ------------
   -- To_Ref --
   ------------

   function To_Ref
     (Self : CORBA.Object.Ref'Class)
     return Ref
   is
      Result : Ref;

   begin
      if CORBA.Object.Entity_Of (Self).all
        not in Current_Object'Class then
         CORBA.Raise_Bad_Param (CORBA.Default_Sys_Member);
      end if;

      Set (Result, CORBA.Object.Entity_Of (Self));

      return Result;
   end To_Ref;

   ----------------------
   -- Get_The_Priority --
   ----------------------

   function Get_The_Priority
     (Self : in Ref)
     return RTCORBA.Priority
   is
      Returns : RTCORBA.Priority;
   begin
      Returns := Current_Object (Entity_Of (Self).all).Last_Value_Set;

      if Returns = Unset_Priority then
         CORBA.Raise_Initialize (CORBA.Default_Sys_Member);
      end if;

      return Returns;
   end Get_The_Priority;

   ----------------------
   -- Set_The_Priority --
   ----------------------

   procedure Set_The_Priority
     (Self : in Ref;
      To   : in RTCORBA.Priority)
   is
      Success : CORBA.Boolean;
      New_Priority : RTCORBA.NativePriority;

   begin
      --  Consistency check: To is in range 0 .. 32767

      if To < 0 then
         --  Note: To is a CORBA.Short, thus To < 32767 is always true.

         CORBA.Raise_Bad_Param (CORBA.Default_Sys_Member);
      end if;

      --  Compute new priority

      RTCORBA.PriorityMapping.To_Native
        (PolyORB.RTCORBA_P.Setup.Get_Priority_Mapping,
         To,
         New_Priority,
         Success);

      if False then
         CORBA.Raise_Data_Conversion
           (CORBA.System_Exception_Members'(Minor     => 2,
                                            Completed => CORBA.Completed_No));
      end if;

      --  Update current object

      declare
         use PolyORB.Tasking.Threads;

         Current : PolyORB.Smart_Pointers.Entity_Ptr;
      begin
         Current := Entity_Of (Self);

         Current_Object (Current.all).Last_Value_Set := To;

         --  Modify priority

         Set_Priority
           (Get_Thread_Factory,
            Current_Object (Current.all).Id,
            Integer (New_Priority));
      end;
   end Set_The_Priority;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize;

   procedure Initialize
   is
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
       Depends   => +"corba.initial_references",
       Provides  => Empty,
       Init      => Initialize'Access));

end RTCORBA.Current;


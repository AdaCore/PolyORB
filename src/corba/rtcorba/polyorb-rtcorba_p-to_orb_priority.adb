------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                      T O _ O R B _ P R I O R I T Y                       --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--            Copyright (C) 2004 Free Software Foundation, Inc.             --
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

with CORBA;
with RTCORBA.PriorityMapping;
with PolyORB.RTCORBA_P.Setup;
with PolyORB.Tasking.Priorities;

function PolyORB.RTCORBA_P.To_ORB_Priority
  (From : RTCORBA.Priority)
  return PolyORB.Tasking.Priorities.ORB_Priority
is
   use type PolyORB.RTCORBA_P.Setup.PriorityMapping_Access;
   use PolyORB.Tasking.Priorities;

   Priority_Mapping : constant
     PolyORB.RTCORBA_P.Setup.PriorityMapping_Access
     := PolyORB.RTCORBA_P.Setup.Get_Priority_Mapping;

   Success : CORBA.Boolean;
   New_Priority : RTCORBA.NativePriority;

begin
   --  Compute new priority

   if Priority_Mapping = null then
      CORBA.Raise_Internal (CORBA.Default_Sys_Member);
   end if;

   RTCORBA.PriorityMapping.To_Native
     (Priority_Mapping.all,
      From,
      New_Priority,
      Success);

   if not Success then
      CORBA.Raise_Data_Conversion
        (CORBA.System_Exception_Members'(Minor     => 2,
                                         Completed => CORBA.Completed_No));
   end if;

   return ORB_Priority (New_Priority);
end PolyORB.RTCORBA_P.To_ORB_Priority;

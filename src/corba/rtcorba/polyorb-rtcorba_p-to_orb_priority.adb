------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--    P O L Y O R B . R T C O R B A _ P . T O _ O R B _ P R I O R I T Y     --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2004-2012, Free Software Foundation, Inc.          --
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

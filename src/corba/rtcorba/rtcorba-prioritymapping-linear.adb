------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--              R T C O R B A . P R I O R I T Y M A P P I N G               --
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

with PolyORB.Tasking.Priorities;

package body RTCORBA.PriorityMapping.Linear is

   use PolyORB.Tasking.Priorities;

   --------------
   -- To_CORBA --
   --------------

   procedure To_CORBA
     (Self            : in     Object;
      Native_Priority : in     RTCORBA.NativePriority;
      CORBA_Priority  :    out RTCORBA.Priority;
      Returns         :    out CORBA.Boolean)
   is
      pragma Warnings (Off); --  WAG:3.15
      pragma Unreferenced (Self);
      pragma Warnings (On); --  WAG:3.15

      Temp : constant Long_Integer
        := (Long_Integer (Native_Priority) * Long_Integer (MaxPriority))
        / Long_Integer (ORB_Component_Priority'Last);
      --  XXX to be checked ...

   begin
      CORBA_Priority := Priority (Temp);
      Returns := True;
   end To_CORBA;

   ---------------
   -- To_Native --
   ---------------

   procedure To_Native
     (Self            : in     Object;
      CORBA_Priority  : in     RTCORBA.Priority;
      Native_Priority :    out RTCORBA.NativePriority;
      Returns         :    out CORBA.Boolean)
   is
      pragma Warnings (Off); --  WAG:3.15
      pragma Unreferenced (Self);
      pragma Warnings (On); --  WAG:3.15

      Temp : constant Long_Integer
        := (Long_Integer (CORBA_Priority)
            * Long_Integer (ORB_Component_Priority'Last))
        / Long_Integer (MaxPriority);
      --  XXX to be checked ...

   begin
      Native_Priority := NativePriority (Temp);
      Returns := True;
   end To_Native;

end RTCORBA.PriorityMapping.Linear;


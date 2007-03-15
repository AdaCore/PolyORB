------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--       R T C O R B A . P R I O R I T Y M A P P I N G . D I R E C T        --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2003-2006, Free Software Foundation, Inc.          --
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

with System;

package body RTCORBA.PriorityMapping.Direct is

   --------------
   -- To_CORBA --
   --------------

   procedure To_CORBA
     (Self            : Object;
      Native_Priority : RTCORBA.NativePriority;
      CORBA_Priority  :    out RTCORBA.Priority;
      Returns         :    out CORBA.Boolean)
   is
      pragma Unreferenced (Self);

   begin
      if Native_Priority >= NativePriority (System.Priority'First)
        and then Native_Priority <= NativePriority (System.Priority'Last)
      then
         CORBA_Priority := Priority (Native_Priority);
         Returns := True;
      else
         Returns := False;
      end if;
   end To_CORBA;

   ---------------
   -- To_Native --
   ---------------

   procedure To_Native
     (Self            : Object;
      CORBA_Priority  : RTCORBA.Priority;
      Native_Priority :    out RTCORBA.NativePriority;
      Returns         :    out CORBA.Boolean)
   is
      pragma Unreferenced (Self);

   begin
      if CORBA_Priority >= Priority (System.Priority'First)
        and then CORBA_Priority <= Priority (System.Priority'Last)
      then
         Native_Priority := NativePriority (CORBA_Priority);
         Returns := True;
      else
         Returns := False;
      end if;

   end To_Native;

end RTCORBA.PriorityMapping.Direct;

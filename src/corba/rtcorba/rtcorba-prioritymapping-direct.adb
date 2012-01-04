------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--       R T C O R B A . P R I O R I T Y M A P P I N G . D I R E C T        --
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

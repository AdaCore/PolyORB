------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--  P O L Y O R B . T A S K I N G . T H R E A D S . A N N O T A T I O N S   --
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

package body PolyORB.Tasking.Threads.Annotations is

   Current_TAF : TAF_Access;

   --------------
   -- Register --
   --------------

   procedure Register (TAF : TAF_Access) is
   begin
      pragma Assert (Current_TAF = null);
      Current_TAF := TAF;
   end Register;

   --------------------------------
   -- Get_Current_Thread_Notepad --
   --------------------------------

   function Get_Current_Thread_Notepad
     return PolyORB.Annotations.Notepad_Access
   is
   begin
      return Get_Current_Thread_Notepad (Current_TAF);
   end Get_Current_Thread_Notepad;

end PolyORB.Tasking.Threads.Annotations;

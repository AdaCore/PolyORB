------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--              P O L Y O R B . T A S K I N G . T H R E A D S               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--             Copyright (C) 1999-2002 Free Software Fundation              --
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
--              PolyORB is maintained by ENST Paris University.             --
--                                                                          --
------------------------------------------------------------------------------

--  $Id$

package body PolyORB.Tasking.Threads is

   My_Thread_Factory : Thread_Factory_Access;
   --  Thread_Factory of the chosen profile.

   Initialised       : Boolean := False;

   ------------------------
   -- Get_Thread_Factory --
   ------------------------

   function Get_Thread_Factory
     return Thread_Factory_Access is
   begin
      pragma Assert (Initialised);
      return My_Thread_Factory;
   end Get_Thread_Factory;

   -----------------------------
   -- Register_Thread_Factory --
   -----------------------------

   procedure Register_Thread_Factory
     (TF : Thread_Factory_Access) is
   begin
      pragma Assert (not Initialised);

      if not Initialised then
         My_Thread_Factory := TF;
         Initialised := True;
      end if;
   end Register_Thread_Factory;

end PolyORB.Tasking.Threads;

------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--             P O L Y O R B . T A S K I N G . M O N I T O R S              --
--                                                                          --
--                                 S p e c                                  --
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

--  Implementation of monitors. Monitors are objects which can only be accessed
--  by one task at the time. In these objects, you can wait for some conditions
--  to be fulfilled; The evaluation of these conditions are done when the
--  monitor is signaled.

--  A complete implementation of this package is provided for all
--  tasking profiles.

--  $Id$

package body PolyORB.Tasking.Monitors is

   My_Factory : Monitor_Factory_Access;
   --  Real factory, corresponding to the chosen tasking profile.

   Initialized : Boolean := False;
   --  Set to True when this package is initialized.

   -------------------------
   -- Get_Monitor_Factory --
   -------------------------

   function Get_Monitor_Factory
     return Monitor_Factory_Access is
   begin
      pragma Assert (Initialized);
      return My_Factory;
   end Get_Monitor_Factory;

   ------------------------------
   -- Register_Monitor_Factory --
   ------------------------------

   procedure Register_Monitor_Factory
     (MF : Monitor_Factory_Access) is
   begin
      pragma Assert (not Initialized);

      if not Initialized then
         My_Factory := MF;
         Initialized := True;
      end if;
   end Register_Monitor_Factory;

end PolyORB.Tasking.Monitors;

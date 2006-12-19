------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--         POLYORB.TASKING.PROFILES.RAVENSCAR.THREADS.ANNOTATIONS           --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2004-2006, Free Software Foundation, Inc.          --
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

with PolyORB.Annotations;
with PolyORB.Initialization;
with PolyORB.Tasking.Threads.Annotations;

generic
package PolyORB.Tasking.Profiles.Ravenscar.Threads.Annotations is

   type Ravenscar_TAF is
     new PolyORB.Tasking.Threads.Annotations.Thread_Annotations_Factory
     with private;

   type Ravenscar_TAF_Access is access all Ravenscar_TAF;

   function Get_Current_Thread_Notepad
     (TAF : access Ravenscar_TAF)
     return PolyORB.Annotations.Notepad_Access;

private
   type Ravenscar_TAF is
     new PolyORB.Tasking.Threads.Annotations.Thread_Annotations_Factory
     with null record;

   procedure Initialize;

   Initializer : constant PolyORB.Initialization.Initializer :=
                   Initialize'Access;

end PolyORB.Tasking.Profiles.Ravenscar.Threads.Annotations;

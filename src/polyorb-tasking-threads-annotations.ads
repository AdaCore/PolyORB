------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--  P O L Y O R B . T A S K I N G . T H R E A D S . A N N O T A T I O N S   --
--                                                                          --
--                                 S p e c                                  --
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

--  This package provides mechanisms to attach a PolyORB's Notepad to
--  a thread.

with PolyORB.Annotations;

package PolyORB.Tasking.Threads.Annotations is

   type Thread_Annotations_Factory is abstract tagged limited null record;

   type TAF_Access is access all Thread_Annotations_Factory'Class;

   procedure Register (TAF : TAF_Access);

   function Get_Current_Thread_Notepad
     (TAF : access Thread_Annotations_Factory)
     return PolyORB.Annotations.Notepad_Access
      is abstract;
   --  Return the annotation object associated with current thread. If no
   --  object associated with current thread, allocate new object.

   function Get_Current_Thread_Notepad
     return PolyORB.Annotations.Notepad_Access;

end PolyORB.Tasking.Threads.Annotations;

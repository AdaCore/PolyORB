------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--  P O L Y O R B . T A S K I N G . T H R E A D S . A N N O T A T I O N S   --
--                                                                          --
--                                 S p e c                                  --
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

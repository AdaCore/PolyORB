------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                             S V C . I M P L                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2008-2012, Free Software Foundation, Inc.          --
--                                                                          --
-- This is free software;  you can redistribute it  and/or modify it  under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for  more details.                                               --
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

with Svc.Skel;
pragma Unreferenced (Svc.Skel);

with Ada.Text_IO; use Ada.Text_IO;

with Conditions; use Conditions;

with PolyORB.Tasking.Condition_Variables;
use  PolyORB.Tasking.Condition_Variables;

with PolyORB.Tasking.Mutexes;
use  PolyORB.Tasking.Mutexes;

package body Svc.Impl is

   ----------
   -- Wait --
   ----------

   procedure Wait
     (Self : not null access Object;
      Cond_Id : CORBA.Short)
   is
      pragma Unreferenced (Self);
   begin
      Put_Line ("Wait: enter, Cond_Id =" & Cond_Id'Img);
      Enter (Mutex);
      Wait (Condition_Variables (Natural (Cond_Id)), Mutex);
      Leave (Mutex);
      Put_Line ("Wait: leave, Cond_Id =" & Cond_Id'Img);
   end Wait;

end Svc.Impl;

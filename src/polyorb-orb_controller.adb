------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--               P O L Y O R B . O R B _ C O N T R O L L E R                --
--                                                                          --
--                                 B o d y                                  --
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

--  $Id$

package body PolyORB.ORB_Controller is

   My_Factory : ORB_Controller_Factory_Access;

   ------------
   -- Create --
   ------------

   procedure Create (O : out ORB_Controller_Access) is
   begin
      pragma Assert (My_Factory /= null);

      O := Create (My_Factory);
   end Create;

   -------------------------------------
   -- Register_ORB_Controller_Factory --
   -------------------------------------

   procedure Register_ORB_Controller_Factory
     (OCF : ORB_Controller_Factory_Access)
   is
   begin
      pragma Assert (My_Factory = null);
      My_Factory := OCF;
   end Register_ORB_Controller_Factory;

   ------------
   -- Status --
   ------------

   function Status (O : access ORB_Controller) return String is
   begin
      return "Tot:" & Natural'Image (O.Registered_Tasks)
        & " U:" & Natural'Image (O.Unscheduled_Tasks)
        & " R:" & Natural'Image (O.Running_Tasks)
        & " B:" & Natural'Image (O.Blocked_Tasks)
        & " I:" & Natural'Image (O.Idle_Tasks)
        & "| PJ:" & Natural'Image (O.Number_Of_Pending_Jobs)
        & " AES:" & Natural'Image (O.Number_Of_AES);
   end Status;

end PolyORB.ORB_Controller;

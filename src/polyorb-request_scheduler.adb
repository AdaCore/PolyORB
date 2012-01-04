------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--            P O L Y O R B . R E Q U E S T _ S C H E D U L E R             --
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

package body PolyORB.Request_Scheduler is

   My_Factory : Request_Scheduler_Factory_Access;

   ------------
   -- Create --
   ------------

   procedure Create (RS : out Request_Scheduler_Access) is
   begin
      if My_Factory /= null then
         RS := Create (My_Factory);
      end if;
   end Create;

   ----------------------------------------
   -- Register_Request_Scheduler_Factory --
   ----------------------------------------

   procedure Register_Request_Scheduler_Factory
     (RSF : Request_Scheduler_Factory_Access)
   is
   begin
      pragma Assert (My_Factory = null);
      My_Factory := RSF;
   end Register_Request_Scheduler_Factory;

end PolyORB.Request_Scheduler;

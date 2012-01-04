------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--  P O L Y O R B . T A S K I N G . C O N D I T I O N _ V A R I A B L E S   --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2002-2012, Free Software Foundation, Inc.          --
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

--  A complete implementation of this package is provided for all
--  tasking profiles.

with PolyORB.Log;

package body PolyORB.Tasking.Condition_Variables is

   use PolyORB.Log;

   package L is new PolyORB.Log.Facility_Log
     ("polyorb.tasking.condition_variables");
   procedure O (Message : String; Level : Log_Level := Debug)
     renames L.Output;
   function C (Level : Log_Level := Debug) return Boolean
     renames L.Enabled;

   My_Factory : Condition_Factory_Access;
   --  Real factory, corresponding to the chosen tasking profile.

   ------------
   -- Create --
   ------------

   procedure Create (Cond : out Condition_Access; Name : String := "") is
   begin
      pragma Debug (C, O ("Create: enter"));
      pragma Assert (My_Factory /= null);
      Cond := Create (My_Factory, Name);
      pragma Debug (C, O ("Create: leave"));
   end Create;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Cond : in out Condition_Access) is
   begin
      pragma Debug (C, O ("Destroy: enter"));
      pragma Assert (My_Factory /= null);
      Destroy (My_Factory, Cond);
      pragma Debug (C, O ("Destroy: leave"));
   end Destroy;

   --------------------------------
   -- Register_Condition_Factory --
   --------------------------------

   procedure Register_Condition_Factory
     (MF : Condition_Factory_Access) is
   begin
      pragma Assert (My_Factory = null);
      My_Factory := MF;
   end Register_Condition_Factory;

end PolyORB.Tasking.Condition_Variables;

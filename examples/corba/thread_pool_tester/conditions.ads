------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                           C O N D I T I O N S                            --
--                                                                          --
--                                 S p e c                                  --
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

with PolyORB.Tasking.Mutexes;
use  PolyORB.Tasking.Mutexes;

with PolyORB.Tasking.Condition_Variables;
use  PolyORB.Tasking.Condition_Variables;

package Conditions is

   type Condition_Array is array (Natural range <>) of Condition_Access;
   type Condition_Array_Access is access all Condition_Array;
   Condition_Variables : Condition_Array_Access;
   Mutex : Mutex_Access;

   procedure Create_Conditions (Count : Natural);
   --  Initialize Condition_Variables by creating Count condition variables

end Conditions;

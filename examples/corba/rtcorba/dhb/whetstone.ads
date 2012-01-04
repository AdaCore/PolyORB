------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                            W H E T S T O N E                             --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2006-2012, Free Software Foundation, Inc.          --
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

--  XXX: propagate Copyright notice wherever required

package Whetstone is

   procedure Small_Whetstone (Kilo_Whets : Positive);
   pragma Inline (Small_Whetstone);
   --  Performs the computational workload of a Hartstone task. The
   --  computation is a scaled-down version of the one performed by the
   --  full Whetstone benchmark program.  An exception is raised if the
   --  computation fails to satisfy an internal consistency check.
   --  This procedure does not return any "result" from its
   --  computation; its sole function is to give a Hartstone task
   --  something to do.

   function Compute_KWIPS return Positive;
   --  Computes the raw speed of the Small_Whetstone benchmark, in the
   --  absence of tasking, by determining how many thousands of
   --  Whetstone instructions (Kilo-Whetstones) per second it can
   --  execute.  Raw speed is expressed in Kilo-Whetstone Instructions
   --  Per Second (KWIPS).  The performance of the Hartstone task set
   --  will be measured against this non-tasking computation.
   --
   --  Note: the result is cached, any successive call will directly
   --  return the result computed at the first call.

end Whetstone;

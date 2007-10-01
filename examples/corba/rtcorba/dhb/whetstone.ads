------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                            W H E T S T O N E                             --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--           Copyright (C) 2006, Free Software Foundation, Inc.             --
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

--  XXX: propagate Copyright notice wherever required

package Whetstone is

   procedure Small_Whetstone (Kilo_Whets : in Positive);
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

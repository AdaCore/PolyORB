------------------------------------------------------------------------------
--                                                                          --
--                            GLADE COMPONENTS                              --
--                                                                          --
--            S Y S T E M . G A R L I C . T E R M I N A T I O N             --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                            $Revision$                             --
--                                                                          --
--         Copyright (C) 1996,1997 Free Software Foundation, Inc.           --
--                                                                          --
-- GARLIC is free software;  you can redistribute it and/or modify it under --
-- terms of the  GNU General Public License  as published by the Free Soft- --
-- ware Foundation;  either version 2,  or (at your option)  any later ver- --
-- sion.  GARLIC is distributed  in the hope that  it will be  useful,  but --
-- WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHANTABI- --
-- LITY or  FITNESS FOR A PARTICULAR PURPOSE.  See the  GNU General Public  --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License  distributed with GARLIC;  see file COPYING.  If  --
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
--               GLADE  is maintained by ACT Europe.                        --
--               (email: glade-report@act-europe.fr)                        --
--                                                                          --
------------------------------------------------------------------------------

with System.RPC;

package System.Garlic.Termination is

   --  The termination algorithm tries to determine how many tasks are
   --  active (i.e. in a non-terminating state) on the system.
   --  It works by propagating two messages. The first one sends a Stamp
   --  to all known partitions, the second one asks all the partitions
   --  to know if it's OK for them to shutdown (i.e. there has been no
   --  activity since the timestamp was set). The answers arrive in
   --  an asynchronous fashion, and if, in a bounded time, not all the
   --  answers have arrived, we will retry and hope that the Send message
   --  will fail. Anyway, if a partition is completely dead and this fact
   --  is undetectable will lead to a non-terminating program. The algorithm
   --  may be improved provided that the interface of this package is
   --  not changed.

   procedure Add_Non_Terminating_Task;
   pragma Inline (Add_Non_Terminating_Task);
   --  Let Garlic know that a task is not going to terminate and that
   --  it should not be taken into account during distributed termination.

   procedure Sub_Non_Terminating_Task;
   pragma Inline (Add_Non_Terminating_Task);
   --  Let Garlic know that a task is no longer a non terminating task.

   procedure Shutdown;
   --  Shutdown any active task.

   procedure Initialize;
   --  Initialization.

   procedure Activity_Detected;
   pragma Inline (Activity_Detected);
   --  Some activity has been detected. This means that the current
   --  shutdown procedure (if any) must be terminated.

end System.Garlic.Termination;

------------------------------------------------------------------------------
--                                                                          --
--                            GLADE COMPONENTS                              --
--                                                                          --
--             S Y S T E M . G A R L I C . P R I O R I T I E S              --
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
--            (email:distribution@act-europe.gnat.com).                     --
--                                                                          --
------------------------------------------------------------------------------

package System.Garlic.Priorities is

   --  This package defines priority constants which will be used
   --  throughout the whole PCS. If you change them and experiment better
   --  results (and can explain why), please report it to the maintenance
   --  team which will adjust future distributions.
   --  All the priorities are defined as if the application's main program
   --  was using System.Default_Priority as the base priority.

   Master_Termination_Priority : constant Priority := Priority'First;
   --  The main termination algorithm needs to run only when no other
   --  task is running and so runs at the lowest priority.

   RPC_Priority : constant Priority := Priority'Last;
   --  The RPC may be considered as an internal mechanism which will take
   --  place as soon as possible. However, since there is a good chance
   --  RPCs are slowed down because of communication speed, this shouldn't
   --  take too much time.

   Polling_Priority : constant Priority := RPC_Priority;
   --  When polling is being used, it needs to be executed as a high
   --  priority.

end System.Garlic.Priorities;

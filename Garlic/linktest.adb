------------------------------------------------------------------------------
--                                                                          --
--                            GLADE COMPONENTS                              --
--                                                                          --
--                             L I N K T E S T                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision$
--                                                                          --
--         Copyright (C) 1996-2001 Free Software Foundation, Inc.           --
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

with System.RPC.Server;
pragma Warnings (Off, System.RPC.Server);
with System.Garlic.Storages.Dsm;
pragma Warnings (Off, System.Garlic.Storages.Dsm);
with System.Garlic.Storages.Dfs;
pragma Warnings (Off, System.Garlic.Storages.Dfs);
with System.Garlic.Protocols.Replay.Server;
pragma Warnings (Off, System.Garlic.Protocols.Replay.Server);
with System.Garlic.Protocols.Tcp.Server;
pragma Warnings (Off, System.Garlic.Protocols.Tcp.Server);
with System.Garlic.Tasking;
pragma Warnings (Off, System.Garlic.Tasking);
with System.Garlic.No_Tasking;
pragma Warnings (Off, System.Garlic.No_Tasking);
with System.Garlic.Termination;
pragma Warnings (Off, System.Garlic.Termination);
with System.Partition_Interface;
pragma Warnings (Off, System.Partition_Interface);
with System.RPC.Stream_IO;
pragma Warnings (Off, System.RPC.Stream_IO);

procedure Linktest is

   --  This test is not expected to run, it is only a test that will try to
   --  link against System.Partition_Interface which in turns drags all
   --  the Garlic stuff through System.RPC. It will help to catch circular
   --  dependencies when in developper mode.

begin
   null;
end Linktest;

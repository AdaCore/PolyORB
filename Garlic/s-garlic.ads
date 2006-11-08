------------------------------------------------------------------------------
--                                                                          --
--                            GLADE COMPONENTS                              --
--                                                                          --
--                        S Y S T E M . G A R L I C                         --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 1996-2006 Free Software Foundation, Inc.           --
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

package System.Garlic is

   pragma Pure;

   --  GARLIC is organized around several modules, which form a coherent
   --  set:
   --    System.Garlic
   --       This is the top-level package for all the PCS.
   --    System.Garlic.Protocols
   --       This package defines the Protocol_Type type and its primitive
   --       operations. An instance of Protocol_Type'Class defines a
   --       communication mean (e.g. Tcp) and performs some basic operations.
   --    System.Garlic.Options
   --       This package gets the Boot_Location and other informations from
   --       the command line.
   --    System.Garlic.Startup
   --       This package 'withes' the different protocols to link them into
   --       the runtime and do the job of registering them. It also offers
   --       services to initialize the PCS.
   --    System.Garlic.Physical_Location
   --       This package defines a Location type which will represents the
   --       way to use to find a protocol. This type is usable for
   --       inter-partitions communications and will be used to spread
   --       the knowledge into the PCS.
   --    System.Garlic.Heart
   --       This is the heart of the system. It defines callable operations
   --       to initialize the system and send Params_Stream_Type instances
   --       accross partitions, and thus may be called by System.RPC to
   --       perform the remote calls.
   --    System.Garlic.Remote
   --       This package provides the necessary procedures needed to launch
   --       the slave partitions with the right options.
   --    System.Garlic.Replay
   --       This package provides the user with facilities to record and
   --       replay the execution of a distributed program.
   --    System.Garlic.Filters
   --       This package implements data filtering.

   Communication_Error : exception;

end System.Garlic;

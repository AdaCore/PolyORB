------------------------------------------------------------------------------
--                                                                          --
--                            GLADE COMPONENTS                              --
--                                                                          --
--                  S Y S T E M . G A R L I C . T Y P E S                   --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                            $Revision$
--                                                                          --
--         Copyright (C) 1996-1999 Free Software Foundation, Inc.           --
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

with System.Garlic.Streams;

package System.Garlic.Types is

   pragma Elaborate_Body;

   type Status_Type is (None, Busy, Done, Dead);

   Null_Partition_ID : constant := 0;
   Last_Partition_ID : constant := 1024;

   type Partition_ID is range Null_Partition_ID .. Last_Partition_ID;

   Null_PID : constant Partition_ID := Null_Partition_ID;

   First_PID : constant Partition_ID := Null_PID + 1;
   Last_PID  : constant Partition_ID := Last_Partition_ID;
   Boot_PID  : Partition_ID := First_PID;
   Self_PID  : Partition_ID := Null_PID;

   subtype Valid_Partition_ID is Partition_ID range First_PID .. Last_ID;
   --  A partition whose ID fits in Valid_Partition_ID is a real partition

   Partition_ID_Increment : constant Valid_Partition_ID := 10;

   type Version_Type is new String (1 .. 8);
   Null_Version : constant Version_Type := (others => ' ');

   type Shutdown_Access is access procedure;

   --  This package defines basic types that are used throughout Garlic
   --  as well as commonly used deallocation and conversion subprograms.

   type Word is mod 2 ** 32;
   --  Unsigned 32-bit integer

   type Portable_Address is mod 2 ** 64;
   --  This type can contain an object of type System.Address on any platform
   --  where GNAT is supported. It is made public on purpose so that it is
   --  possible to take a 'Image of it.

   function To_Address (Addr : Portable_Address) return Address;
   function To_Portable_Address (Addr : Address) return Portable_Address;
   --  Conversion routines

   type Shutdown_Type is (Shutdown_On_Any_Partition_Error,
                          Shutdown_On_Boot_Partition_Error,
                          Never_Shutdown_On_Partition_Error);
   --  Three ways of terminating Garlic

   type Termination_Type is (Local_Termination,
                             Global_Termination,
                             Deferred_Termination);
   --  Three ways of terminating a partition

   type Reconnection_Type is (Rejected_On_Restart,
                              Blocked_Until_Restart,
                              Failed_Until_Restart);
   --  Three ways of reconnecting to a partition

   type Execution_Mode_Type is (Trace_Mode,
                                Replay_Mode,
                                Normal_Mode);
   --  Trace_Mode will record all the traces in a file, Replay_Mode will
   --  replay a distributed execntion and Normal_Mode does nothing regarding
   --  tracing or replay.

   type RPC_Receiver is
      access procedure (Params : access Streams.Params_Stream_Type;
                        Result : access Streams.Params_Stream_Type);
   --  Similar to System.RPC.RPC_Receiver

private

   pragma Inline (To_Address);
   pragma Inline (To_Portable_Address);

end System.Garlic.Types;

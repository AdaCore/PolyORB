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
--         Copyright (C) 1996-2000 Free Software Foundation, Inc.           --
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

with Ada.Streams;

package System.Garlic.Types is

   type Status_Type is (None, Busy, Done, Dead);
   --  General status type for automaton

   Partition_ID_Byte   : constant := 4;
   Partition_ID_Size   : constant := 8 * Partition_ID_Byte;
   Null_Partition_ID   : constant := 0;
   First_Partition_ID  : constant := Null_Partition_ID + 1;
   Last_Partition_ID   : constant := 2 ** Partition_ID_Size - 1;

   type Partition_ID is range Null_Partition_ID .. Last_Partition_ID;

   procedure Read
     (S : access Ada.Streams.Root_Stream_Type'Class;
      X : out Partition_ID);

   procedure Write
     (S : access Ada.Streams.Root_Stream_Type'Class;
      X : in Partition_ID);

   for Partition_ID'Read  use Read;
   for Partition_ID'Write use Write;

   Null_PID  : constant Partition_ID := Null_Partition_ID;
   First_PID : constant Partition_ID := First_Partition_ID;
   Last_PID  : constant Partition_ID := Last_Partition_ID;

   Boot_PID  : Partition_ID := First_PID;
   Self_PID  : Partition_ID := Null_PID;

   subtype Valid_Partition_ID is Partition_ID range First_PID .. Last_PID;
   --  A partition whose ID fits in Valid_Partition_ID is a real partition

   subtype Partition_ID_SEA is
     Ada.Streams.Stream_Element_Array (1 .. Partition_ID_Byte);

   function Read  (S : Partition_ID_SEA) return Partition_ID;
   function Write (P : Partition_ID) return Partition_ID_SEA;

   Partition_ID_Increment : constant := 10;

   type Partition_List is array (Natural range <>) of Partition_ID;
   Null_Partition_List : constant Partition_List (1 .. 0)
     := (others => Null_PID);

   type Unit_Id is new Natural;
   Null_Unit_Id  : constant Unit_Id := 0;
   First_Unit_Id : constant Unit_Id := 1_000_000;
   Unit_Id_Increment : constant := 10;

   type Version_Id is mod 2 ** 8;
   No_Version : constant Version_Id := 0;

   function "<" (L, R : Version_Id) return Boolean;

   type Version_Type is new String (1 .. 8);
   Null_Version : constant Version_Type := (others => ' ');

   --  This package defines basic types that are used throughout Garlic
   --  as well as commonly used deallocation and conversion subprograms.

   type Word is mod 2 ** 32;
   --  Unsigned 32-bit integer

   type Shutdown_Type is (Shutdown_On_Any_Partition_Error,
                          Shutdown_On_Boot_Partition_Error,
                          Never_Shutdown_On_Partition_Error);
   --  Three ways of terminating Garlic

   type Termination_Type is (Local_Termination,
                             Global_Termination,
                             Deferred_Termination);
   --  Three ways of terminating a partition

   type Reconnection_Type is (Reject_On_Restart,
                              Block_Until_Restart,
                              Fail_Until_Restart);
   --  Three ways of reconnecting to a partition

   type Execution_Mode_Type is (Trace_Mode,
                                Replay_Mode,
                                Normal_Mode);
   --  Trace_Mode will record all the traces in a file, Replay_Mode will
   --  replay a distributed execntion and Normal_Mode does nothing regarding
   --  tracing or replay.

end System.Garlic.Types;

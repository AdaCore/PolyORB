------------------------------------------------------------------------------
--                                                                          --
--                            GLADE COMPONENTS                              --
--                                                                          --
--                  S Y S T E M . G A R L I C . T R A C E                   --
--                                                                          --
--                                 S p e c                                  --
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

with Ada.Streams;
with System.Garlic.Streams;
with System.Garlic.Types;

package System.Garlic.Trace is

   procedure Initialize;
   --  Initialize trace / replay stuff. In trace mode, create the trace
   --  file. Read partition id and force it.

   procedure Trace_Received_Data
     (Partition : in Types.Partition_ID;
      Filtered  : access Ada.Streams.Stream_Element_Array;
      Offset    : in  Ada.Streams.Stream_Element_Count);
   --  Trace the message Data (and the time that has passed since the
   --  previous recording) and record it in the partition trace file.

   procedure Shutdown;
   --  Close trace file in trace mode

   type Trace_Type is record
      Time : Duration;
      Data : Streams.Stream_Element_Access;
      PID  : Types.Partition_ID := Types.Null_PID;
   end record;

   procedure Read (S : access Ada.Streams.Root_Stream_Type'Class;
                   T : out Trace_Type);
   procedure Write (S : access Ada.Streams.Root_Stream_Type'Class;
                    T : in Trace_Type);
   for Trace_Type'Read use Read;
   for Trace_Type'Write use Write;

end System.Garlic.Trace;



------------------------------------------------------------------------------
--                                                                          --
--                            GLADE COMPONENTS                              --
--                                                                          --
--                  S Y S T E M . G A R L I C . T R A C E                   --
--                                                                          --
--                                 B o d y                                  --
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

with Ada.Real_Time;         use Ada.Real_Time;
with Ada.Streams.Stream_IO; use Ada.Streams.Stream_IO;

with System.Garlic.Options; use System.Garlic.Options;
with System.Garlic.Debug;   use System.Garlic.Debug;
with System.Garlic.Heart;   use System.Garlic.Heart;

with System.RPC;

package body System.Garlic.Trace is

   Private_Debug_Key : constant Debug_Key :=
     Debug_Initialize ("GARTRA", "(s-gartra): ");
   procedure D
     (Level   : in Debug_Level;
      Message : in String;
      Key     : in Debug_Key := Private_Debug_Key)
     renames Print_Debug_Info;

   type Trace_Type
     (Length : Ada.Streams.Stream_Element_Count) is
      record
         Time : Ada.Real_Time.Time_Span;
         Data : Ada.Streams.Stream_Element_Array (1 .. Length);
         PID  : System.RPC.Partition_ID := Null_Partition_ID;
      end record;

   Trace_File : File_Type;
   --  Where to file in the traces.

   Trace_Time : Time;
   --  When did the last trace occur.

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      pragma Debug
        (D (D_Debug,
            "Initializing trace / replay in mode " & Execution_Mode'Img));

      if Execution_Mode = Trace_Mode then
         Trace_Time := Clock;
         pragma Debug
           (D (D_Debug, "Creating trace file " & Trace_File_Name.all));
            Create (Trace_File, Out_File, Trace_File_Name.all);
      end if;
   end Initialize;

   ----------------
   -- Trace_Data --
   ----------------

   procedure Trace_Data
     (Partition : in System.RPC.Partition_ID;
      Data      : in Ada.Streams.Stream_Element_Array) is
      Trace : Trace_Type (Data'Length);
      Date  : Time;

   begin
      --  Compute the period between the arrival of this message and
      --  the arrival of the previous message.

      Date := Clock;
      Trace.Time := Date - Trace_Time;
      Trace_Time := Date;

      Trace.Data := Data;
      Trace.PID  := Partition;

      pragma Debug
        (D (D_Debug,
            "Writing trace from partition" & Trace.PID'Img &
            " of length" & Trace.Length'Img));

      Trace_Type'Output (Stream (Trace_File), Trace);
   end Trace_Data;

   ------------------------
   -- Trace_Partition_ID --
   ------------------------

   procedure Trace_Partition_ID (Partition : in System.RPC.Partition_ID) is
   begin
      --  The partition ID is the first thing written to the
      --  trace file (not for the boot partition though).
      --  We can be sure of this since the partition has to have
      --  an ID before any message reception can take place.

      System.RPC.Partition_ID'Write (Stream (Trace_File), Partition);
   end Trace_Partition_ID;

   --------------
   -- Shutdown --
   --------------

   procedure Shutdown is
   begin
      if Execution_Mode = Trace_Mode then
         pragma Debug (D (D_Debug, "Closing trace file"));
         Close (Trace_File);
      end if;
   end Shutdown;

end System.Garlic.Trace;






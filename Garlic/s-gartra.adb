------------------------------------------------------------------------------
--                                                                          --
--                            GLADE COMPONENTS                              --
--                                                                          --
--                  S Y S T E M . G A R L I C . T R A C E                   --
--                                                                          --
--                                 B o d y                                  --
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

with Ada.Streams.Stream_IO; use Ada.Streams.Stream_IO;

with Ada.Calendar;          use Ada.Calendar;
with System.Garlic.Options; use System.Garlic.Options;
with System.Garlic.Debug;   use System.Garlic.Debug;

package body System.Garlic.Trace is

   Private_Debug_Key : constant Debug_Key :=
     Debug_Initialize ("S_GARTRA", "(s-gartra): ");
   procedure D
     (Message : in String;
      Key     : in Debug_Key := Private_Debug_Key)
     renames Print_Debug_Info;

   use Ada.Streams, System.Garlic.Streams, System.Garlic.Types;

   Trace_File : File_Type;
   --  File containing the traces

   Trace_Time : Time;
   --  Date of the last trace

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      pragma Debug
        (D ("Initializing trace / replay in mode " & Execution_Mode'Img));

      if Execution_Mode = Trace_Mode then
         Trace_Time := Clock;
         pragma Debug (D ("Creating trace file " & Trace_File_Name.all));
            Create (Trace_File, Out_File, Trace_File_Name.all);
      end if;
   end Initialize;

   ----------
   -- Read --
   ----------

   procedure Read (S : access Root_Stream_Type'Class;
                   T : out Trace_Type)
   is
      Count : Stream_Element_Count;
   begin
      Duration'Read (S, T.Time);
      Stream_Element_Count'Read (S, Count);
      T.Data := new Stream_Element_Array (1 .. Count);
      for I in T.Data'Range loop
         Stream_Element'Read (S, T.Data (I));
      end loop;
      Partition_ID'Read (S, T.PID);
   end Read;

   ----------------
   -- Trace_Data --
   ----------------

   procedure Trace_Data
     (Partition : in Types.Partition_ID;
      Filtered  : access Ada.Streams.Stream_Element_Array;
      Offset    : in  Ada.Streams.Stream_Element_Count)
   is
      Trace : Trace_Type;
      Date  : Time;
      First : constant Stream_Element_Count := Filtered'First + Offset;
      Last  : constant Stream_Element_Count := Filtered'Last;
   begin
      --  Compute the period between the arrival of this message and
      --  the arrival of the previous message.

      Date := Clock;
      Trace.Time := Date - Trace_Time;
      Trace_Time := Date;

      Trace.Data     := new Stream_Element_Array (First .. Last);
      Trace.Data.all := Filtered (First .. Last);
      Trace.PID      := Partition;

      pragma Debug
        (D ("Writing trace from partition" & Trace.PID'Img &
            " of length" & Trace.Data'Length'Img));

      Trace_Type'Output (Stream (Trace_File), Trace);
      Free (Trace.Data);
   end Trace_Data;

   ------------------------
   -- Trace_Partition_ID --
   ------------------------

   procedure Trace_Partition_ID (Partition : in Types.Partition_ID) is
   begin
      --  The partition ID is the first thing written to the
      --  trace file (not for the boot partition though).
      --  We can be sure of this since the partition has to have
      --  an ID before any message reception can take place.

      Types.Partition_ID'Write (Stream (Trace_File), Partition);
   end Trace_Partition_ID;

   --------------
   -- Shutdown --
   --------------

   procedure Shutdown is
   begin
      if Execution_Mode = Trace_Mode
        and then Is_Open (Trace_File)
      then
         pragma Debug (D ("Closing trace file"));
         Close (Trace_File);
      end if;
   end Shutdown;

   -----------
   -- Write --
   -----------

   procedure Write (S : access Root_Stream_Type'Class;
                    T : in Trace_Type)
   is
   begin
      pragma Assert (T.Data /= null);
      Duration'Write (S, T.Time);
      Stream_Element_Count'Write (S, T.Data'Length);
      for I in T.Data'Range loop
         Stream_Element'Write (S, T.Data (I));
      end loop;
      Partition_ID'Write (S, T.PID);
   end Write;

end System.Garlic.Trace;






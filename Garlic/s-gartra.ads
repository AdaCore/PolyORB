--  $Id$

with Ada.Streams;
with System.RPC;
with System.Garlic.Heart;

package System.Garlic.Trace is

   procedure Record_Trace
     (Partition : in System.RPC.Partition_ID;
      Data      : in Ada.Streams.Stream_Element_Array);
   --  Trace the message Data (and the time that has passed since the
   --  previous recording) and record it in the partition's trace file.

   procedure Deliver_Next_Trace (Last : out Boolean);
   --  Read the next trace from the partition's trace file, sleep for the
   --  recorded amount of time and deliver the message to Heart.Has_Arrived.

   procedure Initialize;
   --  Initialize trace/replay stuff.

   procedure Save_Partition_ID (Partition : in System.RPC.Partition_ID);
   --  Save our partition ID to a file named ''Command_Name & ".pid"''
   --  N.B. This is ugly -- too many files -- it'd be nice save the
   --  partition ID in the trace file instead.

   function Load_Partition_ID return System.RPC.Partition_ID;
   --  Load and return partition ID from the above mentioned file.

   function Get_Current_Execution_Mode return Heart.Execution_Mode_Type;
   --  So we won't have to rescan the argument list every time we need
   --  the execution mode.

end System.Garlic.Trace;

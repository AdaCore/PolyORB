--  $Id$

with Ada.Streams.Stream_IO; use Ada.Streams.Stream_IO;
with Ada.Real_Time; use Ada.Real_Time;
with System.Garlic.Options; use System.Garlic.Options;
with System.Garlic.Debug; use System.Garlic.Debug;
with System.Garlic.Replay;
with Ada.Command_Line; use Ada.Command_Line;
with System.RPC;
with System.Garlic.Heart; use System.Garlic.Heart;

package body System.Garlic.Trace is

   Private_Debug_Key : constant Debug_Key :=
     Debug_Initialize ("GARTRA", "(s-gartra): ");
   procedure D
     (Level   : in Debug_Level;
      Message : in String;
      Key     : in Debug_Key := Private_Debug_Key)
     renames Print_Debug_Info;

   type Trace_Entry_Type
     (Length : Ada.Streams.Stream_Element_Count)
   is record
      Arrival_Delay : Ada.Real_Time.Time_Span;
      Data          : Ada.Streams.Stream_Element_Array (1 .. Length);
      Partition     : System.RPC.Partition_ID := Null_Partition_ID;
   end record;

   Trace_File : File_Type;

   Last_Trace_Time : Time;

   --  Trace file format:
   --    <Length (Ada.Streams.Stream_Element_Count)>
   --    <Trace_Entry (Trace_Entry_Type)>
   --    ...

   procedure Record_Trace
     (Partition : in System.RPC.Partition_ID;
      Data      : in Ada.Streams.Stream_Element_Array)
   is
      Trace_Entry : Trace_Entry_Type (Data'Length);
      Tmp_Time    : Time;
   begin
      Tmp_Time := Clock;
      Trace_Entry.Arrival_Delay := Tmp_Time - Last_Trace_Time;
      Last_Trace_Time := Tmp_Time;

      --  Write length.
      Ada.Streams.Stream_Element_Count'Write
        (Stream (Trace_File), Data'Length);

      Trace_Entry.Data := Data;
      Trace_Entry.Partition := Partition;

      --  Write entry.
      pragma Debug (D (D_Debug, "Writing trace of message from partition" &
                       Partition'Img & " of length" & Data'Length'Img));
      Trace_Entry_Type'Write (Stream (Trace_File), Trace_Entry);
   end Record_Trace;

   procedure Deliver_Next_Trace (Last : out Boolean) is
      Length      : Ada.Streams.Stream_Element_Count;
   begin
      if End_Of_File (Trace_File) then
         Last := True;
         return;
      end if;

      --  Read length.
      Ada.Streams.Stream_Element_Count'Read
        (Stream (Trace_File), Length);

      declare
         Trace_Entry : Trace_Entry_Type (Length);
      begin
         --  Read entry.
         Trace_Entry_Type'Read (Stream (Trace_File), Trace_Entry);
         pragma Debug
           (D (D_Debug, "Read trace of message from partition" &
               Trace_Entry.Partition'Img & " of length" & Length'Img));

         --  We'd want the message to arrive at about the same time as
         --  during the recorded execution.
         pragma Debug (D (D_Debug, "-> Start delay"));
         delay To_Duration (Trace_Entry.Arrival_Delay);
         pragma Debug (D (D_Debug, "<- End delay"));

         --  Deliver message.
         pragma Debug
           (D (D_Debug, "Calling Has_Arrived, partition"
               & Trace_Entry.Partition'Img));
         Has_Arrived (Trace_Entry.Partition, Trace_Entry.Data);
      end;

      Last := False;
   end Deliver_Next_Trace;

   procedure Set_Current_Execution_Mode
     (Mode : in Execution_Mode_Type);

   procedure Initialize is
   begin
      pragma Debug (D (D_Debug, "Initializing trace/replay"));

      Set_Current_Execution_Mode (Options.Get_Execution_Mode);
      pragma Debug (D (D_Debug, "Execution mode is "
                       & Get_Current_Execution_Mode'Img));

      if Get_Current_Execution_Mode = Trace_Mode or
        Get_Current_Execution_Mode = Replay_Mode then
         Last_Trace_Time := Clock;
         declare
            Trace_File_Name : String := Get_Trace_File_Name;
         begin
            pragma Debug (D (D_Debug, "Using trace file " & Trace_File_Name));
            case Get_Current_Execution_Mode is
               when Trace_Mode =>
                  pragma Debug (D (D_Debug, "Creating trace file"));
                  Create (File => Trace_File,
                          Mode => Out_File,
                          Name => Trace_File_Name);

               when Replay_Mode =>
                  pragma Debug (D (D_Debug, "Opening trace file"));
                  Open (File => Trace_File,
                        Mode => In_File,
                        Name => Trace_File_Name);

                  if not Is_Boot_Partition then
                     pragma Debug
                       (D (D_Debug, "Loading and setting my partition ID"));
                     Set_My_Partition_ID (Load_Partition_ID);
                  end if;

                  Replay.Recorded_Data_Reader.Start;

               when Normal_Mode =>
                  null;
            end case;
         end;
      end if;

   end Initialize;

   procedure Save_Partition_ID (Partition : in System.RPC.Partition_ID) is
      File_Name : String := Command_Name & ".pid";
      File : File_Type;
   begin
      Create (File => File, Mode => Out_File, Name => File_Name);
      System.RPC.Partition_ID'Write (Stream (File), Partition);
      Close (File);
   end Save_Partition_ID;

   function Load_Partition_ID return System.RPC.Partition_ID is
      File_Name : String := Command_Name & ".pid";
      File : File_Type;
      Partition : System.RPC.Partition_ID;
   begin
      Open (File => File, Mode => In_File, Name => File_Name);
      System.RPC.Partition_ID'Read (Stream (File), Partition);
      Close (File);
      pragma Debug (D (D_Debug, "Loaded partition ID is" & Partition'Img));
      return Partition;
   end Load_Partition_ID;

   Execution_Mode : Execution_Mode_Type;
   --  Current execution mode (so we don't have to rescan
   --  Ada.Command_Line.Argument every time we need this).

   function Get_Current_Execution_Mode
     return Execution_Mode_Type is
   begin
      return Execution_Mode;
   end Get_Current_Execution_Mode;
   pragma Inline (Get_Current_Execution_Mode);

   procedure Set_Current_Execution_Mode
     (Mode : in Execution_Mode_Type) is
   begin
      Execution_Mode := Mode;
   end Set_Current_Execution_Mode;
   pragma Inline (Set_Current_Execution_Mode);

end System.Garlic.Trace;

------------------------------------------------------------------------------
--                                                                          --
--                            GLADE COMPONENTS                              --
--                                                                          --
--                 S Y S T E M . G A R L I C . R E P L A Y                  --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision$                             --
--                                                                          --
--         Copyright (C) 1996-1998 Free Software Foundation, Inc.           --
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

with Ada.Real_Time;                   use Ada.Real_Time;
with Ada.Streams.Stream_IO;           use Ada.Streams.Stream_IO;

with System.Garlic.Debug;             use System.Garlic.Debug;
with System.Garlic.Heart;             use System.Garlic.Heart;
with System.Garlic.Options;           use System.Garlic.Options;
with System.Garlic.Physical_Location; use System.Garlic.Physical_Location;
with System.Garlic.Protocols;         use System.Garlic.Protocols;
with System.Garlic.Types;             use System.Garlic.Types;

package body System.Garlic.Replay is

   --  This package should be modified not to require the Storage_Size
   --  pragma on the Engine_Type task ???

   use type Types.Partition_ID;

   Private_Debug_Key : constant Debug_Key :=
     Debug_Initialize ("GARREP", "(s-garrep): ");

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
         PID  : Types.Partition_ID := Null_Partition_ID;
      end record;

   Trace_File : File_Type;
   --  Where to read the traces

   task type Engine_Type is
      pragma Storage_Size (3_000_000);
   end Engine_Type;
   type Engine_Type_Access is access Engine_Type;
   --  Reads and delivers the messages from the trace file

   ------------
   -- Create --
   ------------

   function Create return Protocols.Protocol_Access is
      Self : Protocol_Access := new Replay_Protocol;

   begin
      Register_Protocol (Self);
      return Self;
   end Create;

   -----------------
   -- Engine_Type --
   -----------------

   task body Engine_Type is
   begin
      pragma Debug (D (D_Debug, "Replay engine started"));

      while not End_Of_File (Trace_File) loop

         --  Read a new trace from file (new incoming message)

         declare
            Trace : constant Trace_Type :=
              Trace_Type'Input (Stream (Trace_File));

         begin
            pragma Debug
              (D (D_Debug,
                  "Read trace from partition" & Trace.PID'Img &
                  " of length" & Trace.Data'Length'Img));

            --  The message should arrive at about the same time as
            --  during the recorded execution.

            declare
               Latency : Duration := To_Duration (Trace.Time);
            begin
               pragma Debug
                 (D (D_Debug, "Replay network latency" & Latency'Img));
               delay Latency;
            end;

            --  Deliver message

            Has_Arrived (Trace.PID, Trace.Data);

            pragma Debug (D (D_Debug, "Message delivered"));
         end;
      end loop;

      --  When the partition is the boot partition, the trace file
      --  receives no QUIT request and exits the loop above when no
      --  incoming message is available. Thus, the replay has to
      --  initiate the shutdown itself.

      if Is_Boot_Partition then
         Soft_Shutdown;
      end if;
   end Engine_Type;

   --------------
   -- Get_Name --
   --------------

   function Get_Name (P : access Replay_Protocol) return String is
   begin
      return "replay";
   end Get_Name;

   ----------
   -- Send --
   ----------

   procedure Send
     (Protocol  : access Replay_Protocol;
      Partition : in Types.Partition_ID;
      Data      : access Ada.Streams.Stream_Element_Array) is
   begin
      pragma Debug
         (D (D_Debug, "Send (but do nothing)" & Data'Length'Img & " bytes"));

      --  Send is a no-op since every partition gets its messages from
      --  the trace file.

      null;
   end Send;

   -------------------
   -- Set_Boot_Data --
   -------------------

   procedure Set_Boot_Data
     (Protocol         : access Replay_Protocol;
      Is_Boot_Protocol : in Boolean := False;
      Boot_Data        : in String  := "";
      Is_Master        : in Boolean := False)
   is
      Engine    : Engine_Type_Access;
      Partition : Types.Partition_ID;
   begin
      --  Replay protocol is always loaded because its activation
      --  is determined at run-time. It should be activated here when
      --  we are sure that the boot server is replay.

      if Options.Execution_Mode = Replay_Mode then

         --  Boot data provides a way to give an alternate trace file name

         if Boot_Data /= ""  then
            Set_Trace_File_Name (Boot_Data);
         end if;

         Open (Trace_File, In_File, Trace_File_Name.all);

         --  A non boot server partition won't be able to find its
         --  partition id. Moreover, if this was possible, it can very
         --  well end up with a partition id different from the one obtained
         --  during the traced execution.

         if not Is_Master then
            pragma Debug
              (D (D_Debug, "Force partition ID read from trace file"));
            Types.Partition_ID'Read (Stream (Trace_File), Partition);
            Set_My_Partition_ID (Partition);
         end if;

         --  We create an unnamed task on which we keep no reference

         Engine := new Engine_Type;

      end if;

   end Set_Boot_Data;

   --------------
   -- Shutdown --
   --------------

   procedure Shutdown (Protocol : access Replay_Protocol) is
   begin
      if Execution_Mode = Replay_Mode then
         pragma Debug (D (D_Debug, "Closing trace file"));
         Close (Trace_File);
      end if;
   end Shutdown;

end System.Garlic.Replay;


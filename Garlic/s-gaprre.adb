------------------------------------------------------------------------------
--                                                                          --
--                            GLADE COMPONENTS                              --
--                                                                          --
--       S Y S T E M . G A R L I C . P R O T O C O L S . R E P L A Y        --
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

with Ada.Streams.Stream_IO;           use Ada.Streams.Stream_IO;
with Ada.Calendar;                    use Ada.Calendar;

with System.Garlic.Debug;             use System.Garlic.Debug;
with System.Garlic.Exceptions;        use System.Garlic.Exceptions;
with System.Garlic.Heart;             use System.Garlic.Heart;
with System.Garlic.Options;           use System.Garlic.Options;
with System.Garlic.Physical_Location; use System.Garlic.Physical_Location;
with System.Garlic.Protocols;         use System.Garlic.Protocols;
with System.Garlic.Streams;           use System.Garlic.Streams;
with System.Garlic.Trace;             use System.Garlic.Trace;
with System.Garlic.Types;             use System.Garlic.Types;
with System.Garlic.Utils;             use System.Garlic.Utils;

package body System.Garlic.Protocols.Replay is

   Private_Debug_Key : constant Debug_Key :=
     Debug_Initialize ("S_GARREP", "(s-garrep): ");

   procedure D
     (Message : in String;
      Key     : in Debug_Key := Private_Debug_Key)
     renames Print_Debug_Info;

   Trace_File : File_Type;
   --  Where to read the traces

   Trace_Start : Time;

   task type Engine_Type is
      pragma Priority (System.Priority'Last);
   end Engine_Type;
   type Engine_Type_Access is access Engine_Type;
   Engine : Engine_Type_Access;
   --  Reads and delivers the messages from the trace file

   Self_Reference : Protocol_Access;

   --------------
   -- Activate --
   --------------

   procedure Activate
     (Protocol : access Replay_Protocol;
      Error    : in out Error_Type)
   is
      pragma Unreferenced (Protocol);
      pragma Unreferenced (Error);
   begin
      if Options.Execution_Mode = Replay_Mode
        and then Engine = null
      then
         Engine := new Engine_Type;
      end if;

      pragma Debug (D ("Activate replay protocol"));
   end Activate;

   ------------
   -- Create --
   ------------

   function Create return Protocol_Access is
   begin
      if Self_Reference /= null then
         return null;
      end if;

      Self_Reference := new Replay_Protocol;

      return Self_Reference;
   end Create;

   -----------------
   -- Engine_Type --
   -----------------

   task body Engine_Type is
      PID   : Partition_ID := Null_PID;
      Code  : Any_Opcode;
      Data  : Stream_Element_Access;
      Error : Error_Type;

   begin
      pragma Debug (D ("Replay engine started"));

      Trace_Start := Clock;

      while not End_Of_File (Trace_File) loop

         --  Read a new trace from file (new incoming message)

         declare
            Trace : Trace_Type :=
              Trace_Type'Input (Stream (Trace_File));

         begin
            pragma Debug
              (D ("Read trace from partition" & Trace.PID'Img &
                  " of length" & Trace.Data'Length'Img));

            --  The message should arrive at about the same time as
            --  during the recorded execution.

            pragma Debug (D ("Replay network latency" & Trace.Time'Img));
            delay until Trace_Start + Trace.Time;

            --  Deliver message

            Analyze_Stream
              (PID, Self_Reference, Code, Data, Trace.Data, 0, Error);
            Free (Trace.Data);

            exit when Found (Error);

            Process_Stream (PID, Code, Data, Error);
            Free (Data);

            exit when Found (Error);

            pragma Debug (D ("Message delivered"));
         end;
      end loop;

      --  When the partition is the boot partition, the trace file
      --  receives no QUIT request and exits the loop above when no
      --  incoming message is available. Thus, the replay has to
      --  initiate the shutdown itself.

      if Is_Boot_Server then
         Activate_Shutdown;
      end if;
   end Engine_Type;

   --------------
   -- Get_Data --
   --------------

   function Get_Data
     (Protocol : access Replay_Protocol)
     return String_Array_Access
   is
      pragma Unreferenced (Protocol);

      Result : String_Array_Access;

   begin
      if Options.Execution_Mode /= Replay_Mode then
         return null;
      end if;

      Result := new String_Array (1 .. 1);
      Result (1) := new String'(Trace_File_Name.all);

      return Result;
   end Get_Data;

   --------------
   -- Get_Name --
   --------------

   function Get_Name
     (Protocol : access Replay_Protocol)
     return String
   is
      pragma Unreferenced (Protocol);
   begin
      return "replay";
   end Get_Name;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Protocol  : access Replay_Protocol;
      Self_Data : in String;
      Required  : in Boolean;
      Performed : out Boolean;
      Error     : in out Error_Type)
   is
      pragma Unreferenced (Protocol);
      pragma Unreferenced (Self_Data);
      pragma Unreferenced (Required);
      pragma Unreferenced (Error);
   begin
      Performed := False;
   end Initialize;

   -------------
   -- Receive --
   -------------

   function Receive
     (Protocol  : access Replay_Protocol;
      Timeout   : Duration)
     return Boolean
   is
      pragma Unreferenced (Protocol);
      pragma Unreferenced (Timeout);
   begin
      return True;
   end Receive;

   ----------
   -- Send --
   ----------

   procedure Send
     (Protocol  : access Replay_Protocol;
      Partition : in Partition_ID;
      Data      : access Ada.Streams.Stream_Element_Array;
      Error     : in out Error_Type)
   is
      pragma Unreferenced (Protocol);
      pragma Unreferenced (Partition);
      pragma Unreferenced (Error);
   begin
      pragma Debug
         (D ("Send (but do nothing)" & Data'Length'Img & " bytes"));

      --  Send is a no-op since every partition gets its messages from
      --  the trace file.

      null;
   end Send;

   -------------------
   -- Set_Boot_Data --
   -------------------

   procedure Set_Boot_Data
     (Protocol  : access Replay_Protocol;
      Boot_Data : in String;
      Error     : in out Error_Type)
   is
      pragma Unreferenced (Protocol);
   begin
      --  Replay protocol is always loaded because its initialization
      --  is decided at run-time. It should be initialized here when
      --  we are sure that the boot server is replay.

      if Options.Boot_Location'Length /= 1 then
         return;
      end if;

      if Boot_Data'Length /= 0 then
         Set_Trace_File_Name (Boot_Data);
      end if;

      begin
         Open (Trace_File, In_File, Trace_File_Name.all);
         Set_Execution_Mode (Replay_Mode);
         Set_Boot_Location (To_Location (Self_Reference, Trace_File_Name.all));
      exception when others =>
         Throw (Error, "Cannot open " & Trace_File_Name.all);
      end;
   end Set_Boot_Data;

   --------------
   -- Shutdown --
   --------------

   procedure Shutdown (Protocol : access Replay_Protocol)
   is
      pragma Unreferenced (Protocol);
   begin
      if Execution_Mode = Replay_Mode
        and then Is_Open (Trace_File)
      then
         pragma Debug (D ("Closing trace file"));
         Close (Trace_File);
      end if;
   end Shutdown;

end System.Garlic.Protocols.Replay;


--  $Id$

with Ada.Streams;
with System.Garlic.Heart;
with System.Garlic.Physical_Location; use System.Garlic.Physical_Location;
with System.Garlic.Protocols; use System.Garlic.Protocols;
with System.Garlic.Debug; use System.Garlic.Debug;
with System.Garlic.Trace; use System.Garlic.Trace;

package body System.Garlic.Replay is

   use type System.RPC.Partition_ID;

   Private_Debug_Key : constant Debug_Key :=
     Debug_Initialize ("GARREP", "(s-garrep): ");
   procedure D
     (Level   : in Debug_Level;
      Message : in String;
      Key     : in Debug_Key := Private_Debug_Key)
     renames Print_Debug_Info;

   ------------
   -- Create --
   ------------

   function Create return System.Garlic.Protocols.Protocol_Access is
      Self : Protocol_Access := new Replay_Protocol;
   begin
      Register_Protocol (Self);
      return Self;
   end Create;

   --------------
   -- Get_Info --
   --------------

   function Get_Info (P : access Replay_Protocol) return String is
   begin
      return "";
   end Get_Info;

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
      Partition : in System.RPC.Partition_ID;
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
   begin
      pragma Assert (Boot_Data = "");
      null;
   end Set_Boot_Data;

   --------------
   -- Shutdown --
   --------------

   procedure Shutdown (Protocol : access Replay_Protocol) is
   begin
      null;
   end Shutdown;

   task body Recorded_Data_Reader is
   begin

      select
         accept Start;
      or
         terminate;
      end select;

      pragma Debug (D (D_Debug, "Task Recorded_Data_Reader started"));

      select
         Heart.Shutdown_Keeper.Wait;
         pragma Debug
           (D (D_Debug, "Recorded_Data_Reader exiting because of " &
               "Shutdown_Keeper"));
         --  Is this an error?
      then abort
         declare
            Is_Last : Boolean;
         begin
            Reader : loop
               --  Read next trace from trace file, sleep
               --  until it's time to deliver and then call
               --  Heart.Has_Arrived with the data read.
               Deliver_Next_Trace (Is_Last);
               exit Reader when Is_Last;
            end loop Reader;
         end;
      end select;

      pragma Debug (D (D_Debug, "End of task Recorded_Data_Reader"));

   end Recorded_Data_Reader;

end System.Garlic.Replay;

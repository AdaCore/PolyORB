--  $Id$

with Ada.Streams;
with System.Garlic.Protocols;
with System.RPC;

package System.Garlic.Replay is

   type Replay_Protocol is
      new System.Garlic.Protocols.Protocol_Type with private;

   function Create return System.Garlic.Protocols.Protocol_Access;

   function Get_Name (P : access Replay_Protocol) return String;

   procedure Set_Boot_Data
     (Protocol         : access Replay_Protocol;
      Is_Boot_Protocol : in Boolean := False;
      Boot_Data        : in String  := "";
      Is_Master        : in Boolean := False);

   function Get_Info (P : access Replay_Protocol) return String;

   procedure Send
      (Protocol  : access Replay_Protocol;
       Partition : in System.RPC.Partition_ID;
       Data      : access Ada.Streams.Stream_Element_Array);

   procedure Shutdown (Protocol : access Replay_Protocol);

   task Recorded_Data_Reader is
      pragma Storage_Size (300_000);
      entry Start;
   end Recorded_Data_Reader;
   --  Reads and delivers the messages from the trace file.

private

   type Replay_Protocol is new System.Garlic.Protocols.Protocol_Type
     with null record;

end System.Garlic.Replay;

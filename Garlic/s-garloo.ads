--
--  $Id$
--

with Ada.Streams;
with System.Garlic.Protocols;
with System.RPC;

package System.Garlic.Loopback is

   type Loopback_Protocol is
     new System.Garlic.Protocols.Protocol_Type with private;
   --  Loopback protocol.

   function Create return System.Garlic.Protocols.Protocol_Access;

   function Get_Name (P : access Loopback_Protocol) return String;
   --  Get name.

   procedure Set_Boot_Data
     (Protocol         : access Loopback_Protocol;
      Is_Boot_Protocol : in Boolean := False;
      Boot_Data        : in String  := "";
      Is_Master        : in Boolean := False);
   --  Set boot data.

   function Get_Info (P : access Loopback_Protocol) return String;
   --  Get protocol specific info.

   procedure Send
     (Protocol  : access Loopback_Protocol;
      Partition : in System.RPC.Partition_ID;
      Data      : access Ada.Streams.Stream_Element_Array);
   --  Send data.

   procedure Shutdown (Protocol : access Loopback_Protocol);
   --  Shutdown a protocol.

private

   type Loopback_Protocol is
      new System.Garlic.Protocols.Protocol_Type with null record;

end System.Garlic.Loopback;

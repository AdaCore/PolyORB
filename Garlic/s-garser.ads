--
--  $Id$
--

with Ada.Streams;
with System.Garlic.Protocols;
with System.RPC;

package System.Garlic.Serial is

   --  This package implements a protocol which exchanges data over
   --  a serial peripheral like a wire or a radio device. Its main use
   --  is for embedded systems.

   type Serial_Protocol is
      new System.Garlic.Protocols.Protocol_Type with private;

   function Create return System.Garlic.Protocols.Protocol_Access;

   function Get_Name (P : access Serial_Protocol) return String;

   procedure Set_Boot_Data
     (Protocol         : access Serial_Protocol;
      Is_Boot_Protocol : in Boolean := False;
      Boot_Data        : in String  := "";
      Is_Master        : in Boolean := False);

   function Get_Info (P : access Serial_Protocol) return String;

   procedure Send
     (Protocol  : access Serial_Protocol;
      Partition : in System.RPC.Partition_ID;
      Data      : access Ada.Streams.Stream_Element_Array);

   procedure Shutdown (Protocol : access Serial_Protocol);

private

   type Serial_Protocol is new System.Garlic.Protocols.Protocol_Type
     with null record;

end System.Garlic.Serial;

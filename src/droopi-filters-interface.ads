--  Messages exchanged by Filter components.

--  $Id$

with Ada.Streams; use Ada.Streams;

with Droopi.Buffers; use Droopi.Buffers;
with Droopi.Schedulers; use Droopi.Schedulers;

package Droopi.Filters.Interface is

   --------------------------------------------------
   -- Filters communicate by exchanging Interface --
   --------------------------------------------------

   type Root_Data_Unit is abstract new Message with null record;
   subtype Data_Unit is Root_Data_Unit'Class;

   type Set_Server is new Root_Data_Unit with record
      Server : Schedulers.Server_Access;
   end record;

   type Set_Buffer is new Root_Data_Unit with record
      Buffer : Buffer_Access;
   end record;
   --  Direction: from upper to lower.
   --  Semantics: Buffer is to be used by filters along the
   --  chain to hold received data contents.

   type Connect_Indication is new Root_Data_Unit with null record;
   --  Direction: from lower to upper.
   --  Semantics: a new incoming transport connection is
   --  being initiated.

   type Disconnect_Indication is new Root_Data_Unit with null record;
   --  Direction: from lower to upper.
   --  Semantics: a transport endpoint has been closed.
   --    upper layers must release all associated resources.

   type Data_Indication is new Root_Data_Unit with null record;
   --  Direction: from lower to upper.
   --  Semantics: data has been received and must be handled.

   type Data_Expected is new Root_Data_Unit with record
      --  Direction: from upper to lower.
      --  Semantics: prepare for reception of a message.

      In_Buf : Buffer_Access;
      --  Where to store the data when it arrives.

      Max : Stream_Element_Count;
      --  The maximum amount of data to be received.
   end record;

   type Data_Out is new Root_Data_Unit with record
      --  Direction: from upper to lower.
      --  Semantics: send data out.

      Out_Buf : Buffer_Access;
      --  The data to be sent down.
   end record;

end Droopi.Filters.Interface;


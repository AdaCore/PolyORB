--  Messages exchanged by Filter components.

--  $Id$

with Ada.Streams; use Ada.Streams;

with Droopi.Buffers; use Droopi.Buffers;
with Droopi.Components; use Droopi.Components;

package Droopi.Filters.Interface is

   -----------------------------
   -- Filter_Factory messages --
   -----------------------------

   type Create_Filter_Chain is new Message with null record;

   type Created_Filter_Chain is new Message with record
      Filter_Chain : Filter_Access;
   end record;

   ---------------------
   -- Filter messages --
   ---------------------

   type Root_Data_Unit is abstract new Message with null record;
   subtype Data_Unit is Root_Data_Unit'Class;

   type Set_Server is new Root_Data_Unit with record
      Server : Components.Component_Access;
   end record;
   --  Direction: from lower to upper.
   --  Semantics: inform stacks participants of the ORB
   --  component they are working for.

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

   type Connect_Confirmation is new Root_Data_Unit with null record;
   --  Direction: from lower to upper.
   --  Semantics: a new client transport connection has been established.

   type Disconnect_Indication is new Root_Data_Unit with null record;
   --  Direction: from lower to upper.
   --  Semantics: a transport endpoint has been closed.
   --    upper layers must release all associated resources.

   type Data_Indication is new Root_Data_Unit with null record;
   --  Direction: from lower to upper.
   --  Semantics: Data has been received and must be handled.

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


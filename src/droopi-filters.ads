--  A communication filter (a transport Data_Unit handler/forwarder).

--  $Id$

with Ada.Streams; use Ada.Streams;

with Droopi.Buffers;    use Droopi.Buffers;
with Droopi.Components; use Droopi.Components;
with Droopi.Schedulers;

package Droopi.Filters is

   pragma Elaborate_Body;

   ----------------------------------------------------
   -- A Filter is a component that forwards messages --
   -- across a stack.                                --
   ----------------------------------------------------

   type Filter is abstract new Component with private;
   type Filter_Access is access all Filter'Class;

   procedure Connect_Lower (F : access Filter; Lower : Component_Access);
   function Lower (F : access Filter) return Component_Access;

   --------------------------------------------------
   -- Filters communicate by exchanging Data_Units --
   --------------------------------------------------

   type Root_Data_Unit is abstract new Message with null record;
   subtype Data_Unit is Root_Data_Unit'Class;

   package Data_Units is

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

      type Data_Expected is new Root_Data_Unit
      --  Direction: from upper to lower.
      --  Semantics: prepare for reception of a message.
        with record
           In_Buf : Buffer_Access;
           --  Where to store the data when it arrives.

           Max : Stream_Element_Count;
           --  The maximum amount of data to be received.
        end record;

      type Data_Out is new Root_Data_Unit
      --  Direction: from upper to lower.
      --  Semantics: send data out.
        with record
           Out_Buf : Buffer_Access;
           --  The data to be sent down.
        end record;
   end Data_Units;

   ---------------------------------------------------
   -- Filters can be chained. A chain of filters is --
   -- created from a chain of filter factories.     --
   ---------------------------------------------------

   type Factory is abstract tagged limited private;
   type Factory_Access is access all Factory'Class;

   procedure Create
     (Fact : access Factory;
      Filt : out Filter_Access)
      is abstract;

   type Factory_Chain;
   type Factory_Chain_Access is access all Factory_Chain;

   type Factory_Chain is record
      This  : Factory_Access;
      Upper : Factory_Chain_Access;
   end record;

   function Create_Filter_Chain (FChain : Factory_Chain_Access)
     return Filter_Access;

private

   type Filter is abstract new Component with record
      Lower  : Component_Access;
      Upper  : Component_Access;
   end record;

   type Factory is abstract tagged limited null record;

end Droopi.Filters;

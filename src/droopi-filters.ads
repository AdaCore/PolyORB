--  A communication filter (a transport SDU handler/forwarder).

--  $Id$

with Ada.Streams; use Ada.Streams;

with Droopi.Buffers; use Droopi.Buffers;
with Droopi.Servers;

package Droopi.Filters is

   -------------------------------------------------
   -- A Filter is a protocol entity that forwards --
   -- data units from a lower layer to an upper   --
   -- layer and back.                             --
   -------------------------------------------------

   type Filter is abstract tagged limited private;
   type Filter_Access is access all Filter'Class;

   function Lower (F : access Filter) return Filter_Access;
   function Server_Of (F : access Filter) return Servers.Server_Access;

   -------------------------------------------------
   -- Filters exchange service data units (SDUs). --
   -------------------------------------------------

   type SDU_Kind is
     (Connect_Indication,
      --  Direction: from lower to upper.
      --  Semantics: a new incoming transport connection is
      --  being initiated.

      Disconnect_Indication,
      --  Direction: from lower to upper.
      --  Semantices: a transport endpoint has been closed.
      --    upper layers must release all associated resources.

      Data_Indication,
      --  Direction: from lower to upper.
      --  Semantics: data has been received and must be handled.

      Data_Expected,
      --  Direction: from upper to lower.
      --  Semantics: prepare for reception of a message.

      Data_Out
      --  Direction: from upper to lower.
      --  Semantics: send data out.
      );

   type SDU (Kind : SDU_Kind) is record
      case Kind is
         when Data_Expected =>
            In_Buf : Buffer_Access;
            --  Where to store the data when it arrives.

            Max : Stream_Element_Count;
            --  The maximum amount of data to be received.

         when Data_Out =>
            Out_Buf : Buffer_Access;
            --  The data to be sent down.

         when others =>
            null;
      end case;
   end record;

   ---------------------------------------------------
   -- Filter primitives (interface to upper layer) --
   ---------------------------------------------------

   procedure Handle_SDU
     (F : access Filter;
      S : SDU) is abstract;
   --  Ask the filter to forward SDU S appropriately.

   ---------------------------------------------------
   -- Filters can be chained. A chain of filters is --
   -- created from a chain of filter factories.     --
   ---------------------------------------------------

   type Factory is abstract tagged limited private;
   type Factory_Access is access all Factory'Class;

   procedure Create
     (Fact    : access Factory;
      Lower   : Filter_Access;
      Upper   : out Filter_Access) is abstract;

   type Factory_Chain;
   type Factory_Chain_Access is access all Factory_Chain;

   type Factory_Chain is record
      This  : Factory_Access;
      Upper : Factory_Chain_Access;
   end record;

   procedure Create_Filter_Chain
     (Lower  : Filter_Access;
      FChain : Factory_Chain_Access);

private

   type Filter is abstract tagged limited record
      Server : Servers.Server_Access;
      Lower  : Filter_Access;
      Upper  : Filter_Access;
   end record;

   type Factory is abstract tagged limited null record;

end Droopi.Filters;

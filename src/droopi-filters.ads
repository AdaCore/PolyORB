--  A communication filter (a transport Data_Unit handler/forwarder).

--  $Id$

with Ada.Streams; use Ada.Streams;

with Droopi.Buffers; use Droopi.Buffers;
with Droopi.Schedulers;

package Droopi.Filters is

   pragma Elaborate_Body;

   -------------------------------------------------
   -- A Filter is a protocol entity that forwards --
   -- data units from a lower layer to an upper   --
   -- layer and back.                             --
   -------------------------------------------------

   type Filter is abstract tagged limited private;
   type Filter_Access is access all Filter'Class;

   function Lower (F : access Filter) return Filter_Access;
   function Server_Of (F : access Filter) return Schedulers.Server_Access;

   --------------------------------------------------
   -- Filters communicate by exchanging Data_Units --
   --------------------------------------------------

   type Root_Data_Unit is abstract tagged null record;
   subtype Data_Unit is Root_Data_Unit'Class;

   package Data_Units is

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
   -- Filter primitives (interface to upper layer) --
   ---------------------------------------------------

   procedure Handle_Data_Unit
     (F : access Filter;
      S : Data_Unit) is abstract;
   --  Ask the filter to forward Data_Unit S appropriately.

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

   procedure Create_Filter_Chain
     (Lower  : Filter_Access;
      FChain : Factory_Chain_Access);

private

   type Filter is abstract tagged limited record
      Server : Schedulers.Server_Access;
      Lower  : Filter_Access;
      Upper  : Filter_Access;
   end record;

   type Factory is abstract tagged limited null record;

end Droopi.Filters;

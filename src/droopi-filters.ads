--  A communication filter (a transport Data_Unit handler/forwarder).

--  A protocol implementation can be considered as a stack of layers.
--  Each layer exchanges messages with its immediate neighbours.
--  Variations on the protocol, and adaptation and optimizations
--  according to various environment an application constraints,
--  can be implemented in a very modular and configurable fasion
--  by adding or removing layers in the stack. This design was
--  used for protocol implementation in the x-kernel. This unit
--  defines abstract 'filter' components that can be used in such
--  a stacked configuration.

--  For further information, see:
--    N. C. Hutchinson and L. L. Peterson
--    "The x-kernel: An Architecture for Implementing Network Protocols",
--    IEEE Transactions on Software Engineering,
--    vol. 17, pp. 64--76, January 1991.

--  $Id$

with Droopi.Components; use Droopi.Components;

package Droopi.Filters is

   --  Body requires child unit Droopi.Filters.Interface:
   --  no elab control pragma.

   ----------------------------------------------------
   -- A Filter is a component that forwards messages --
   -- across a stack.                                --
   ----------------------------------------------------

   type Filter is abstract new Component with private;
   type Filter_Access is access all Filter'Class;

   procedure Connect_Lower (F : access Filter; Lower : Component_Access);
   function Lower (F : access Filter) return Component_Access;

   --------------------------------------------------------
   -- Filters communicate by exchanging Data_Units,      --
   -- defined in child package Droopi.Filters.Interface. --
   --                                                    --
   -- Filters can be chained. A chain of filters is      --
   -- created from a chain of filter factories.          --
   --------------------------------------------------------

   type Factory is abstract new Filter with private;
   type Factory_Access is access all Factory'Class;

   type Factory_Array is array (Integer range <>)
     of Factory_Access;

   procedure Create
     (Fact : access Factory;
      Filt : out Filter_Access)
      is abstract;
   --  Each filter factory implements a Create operation that
   --  instanciates the corresponding filter.

   function Handle_Message
     (F : access Factory;
      Msg : Message'Class)
     return Message'Class;

   procedure Chain_Factories (Factories : Factory_Array);
   --  Chain Factories into a Factory_Chain.

   function Create_Filter_Chain (FChain : access Factory)
     return Filter_Access;
   --  Invoke the factory chain starting with Head, to create
   --  a chain of filters. The head of the created filter chain
   --  is returned.

private

   type Filter is abstract new Component with record
      Lower  : Component_Access;
      Upper  : Component_Access;
   end record;

   type Factory is abstract new Filter with null record;

end Droopi.Filters;

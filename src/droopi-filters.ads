--  A communication filter (a transport Data_Unit handler/forwarder).

--  $Id$

with Droopi.Components; use Droopi.Components;

package Droopi.Filters is

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

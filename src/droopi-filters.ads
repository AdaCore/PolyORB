--  A communication filter (a transport Data_Unit handler/forwarder).

--  $Id$

with Droopi.Components; use Droopi.Components;

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

   --------------------------------------------------------
   -- Filters communicate by exchanging Data_Units,      --
   -- defined in child package Droopi.Filters.Interface. --
   --------------------------------------------------------

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

------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                      P O L Y O R B . F I L T E R S                       --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2001-2007, Free Software Foundation, Inc.          --
--                                                                          --
-- PolyORB is free software; you  can  redistribute  it and/or modify it    --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. PolyORB is distributed  in the hope that it will be  useful,    --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with PolyORB; see file COPYING. If    --
-- not, write to the Free Software Foundation, 51 Franklin Street, Fifth    --
-- Floor, Boston, MA 02111-1301, USA.                                       --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
--                  PolyORB is maintained by AdaCore                        --
--                     (email: sales@adacore.com)                           --
--                                                                          --
------------------------------------------------------------------------------

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

with PolyORB.Components;

package PolyORB.Filters is

   --  Body requires child unit PolyORB.Filters.Interface:
   --  no elab control pragma.

   package PC renames PolyORB.Components;

   ----------------------------------------------------
   -- A Filter is a component that forwards messages --
   -- across a stack.                                --
   ----------------------------------------------------

   type Filter is abstract new PC.Component with private;
   type Filter_Access is access all Filter'Class;

   function Handle_Message
     (F   : access Filter;
      Msg : Components.Message'Class) return Components.Message'Class;
   --  Implement default propagation: just transmit message to the appropriate
   --  neighbour (lower or upper, depending on message type, as documented in
   --  Filters.Iface).

   procedure Connect_Lower (F : access Filter; Lower : PC.Component_Access);
   function Lower (F : access Filter) return PC.Component_Access;

   function Upper (F : access Filter) return PC.Component_Access;

   --  Filters communicate by exchanging Data_Units, defined in child package
   --  PolyORB.Filters.Interface.

   --  Filters are associated in a stack, each one having a lower and an
   --  upper neighbour. A stack is created from a list of factories.

   type Factory is abstract tagged limited private;
   type Factory_Access is access all Factory'Class;

   type Factory_Array is array (Integer range <>) of Factory_Access;
   type Factories_Access is access all Factory_Array;

   procedure Create
     (Fact : access Factory;
      Filt : out Filter_Access) is abstract;
   --  Each filter factory implements a Create operation that instantiates
   --  the corresponding filter.

   procedure Destroy (F : in out Filter);

   procedure Create_Filter_Chain
     (Factories :     Factory_Array;
      Bottom    : out Filter_Access;
      Top       : out Filter_Access);
   --  Invoke the factory chain Factories, to create a stack of filters whose
   --  bottom and top elements are returned.

private

   type Filter is abstract new PC.Component with record
      Lower  : PC.Component_Access;
      Upper  : PC.Component_Access;
   end record;

   type Factory is abstract tagged limited null record;

end PolyORB.Filters;

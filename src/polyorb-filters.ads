------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                      P O L Y O R B . F I L T E R S                       --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                Copyright (C) 2001 Free Software Fundation                --
--                                                                          --
-- AdaBroker is free software; you  can  redistribute  it and/or modify it  --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. AdaBroker  is distributed  in the hope that it will be  useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with AdaBroker; see file COPYING. If  --
-- not, write to the Free Software Foundation, 59 Temple Place - Suite 330, --
-- Boston, MA 02111-1307, USA.                                              --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
--              PolyORB is maintained by ENST Paris University.             --
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

--  $Id$

with PolyORB.Components; use PolyORB.Components;

package PolyORB.Filters is

   --  Body requires child unit PolyORB.Filters.Interface:
   --  no elab control pragma.

   ----------------------------------------------------
   -- A Filter is a component that forwards messages --
   -- across a stack.                                --
   ----------------------------------------------------

   type Filter is abstract new Component with private;
   type Filter_Access is access all Filter'Class;

   procedure Connect_Lower (F : access Filter; Lower : Component_Access);
   function Lower (F : access Filter) return Component_Access;

   function Upper (F : access Filter) return Component_Access;

   procedure Finalize (F : in out Filter);
   --  Destroy Filter and all of its UPPER components in the stack.

   --------------------------------------------------------
   -- Filters communicate by exchanging Data_Units,      --
   -- defined in child package PolyORB.Filters.Interface. --
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

end PolyORB.Filters;

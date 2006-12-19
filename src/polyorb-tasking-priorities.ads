------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--           P O L Y O R B . T A S K I N G . P R I O R I T I E S            --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2003-2006, Free Software Foundation, Inc.          --
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

--  This package defines priority ranges available within PolyORB

with System;
with PolyORB.Types;

package PolyORB.Tasking.Priorities is

   ------------------
   -- ORB Priority --
   ------------------

   --  ORB priorities are derived from Ada native priorities. We
   --  define ORB_Core and ORB_Component priority levels, so we make a
   --  distinction between ORB Core entities that require high
   --  priority to process some information and other components.

   subtype ORB_Priority is System.Priority range
     System.Priority'First .. System.Priority'Last;
   --  ORB priority range

   ORB_Core_Levels : constant Natural := 1;
   --  Number of priority levels affected to the ORB Core

   subtype ORB_Component_Priority is ORB_Priority range
     ORB_Priority'First .. ORB_Priority'Last - ORB_Core_Levels;
   --  ORB_Component_Priority defines the priority an ORB component
   --  may have. This range usually applies to most components,
   --  including user components.

   Default_Component_Priority : constant ORB_Component_Priority;
   --  Default priority for ORB Components

   subtype ORB_Core_Priority is System.Priority range
     ORB_Priority'Last - ORB_Core_Levels + 1 .. ORB_Priority'Last;
   --  ORB_Core_Priority defines the priority of some ORB key
   --  components. It is reserved to high priority loops, such as
   --  PolyORB.ORB main loop.

   Default_Core_Priority : constant ORB_Core_Priority;
   --  Default priority for ORB Core

   -----------------------
   -- External Priority --
   -----------------------

   --  External priorities are derived from integer. They represent
   --  priority levels as defined by PolyORB's personalities.

   type External_Priority is new Integer;

   Invalid_Priority : constant External_Priority;

   ----------------------
   -- Priority mapping --
   ----------------------

   --  These funcitons define mapping between ORB_Priority and
   --  External_Priority. When False, Returns indicate the mapping was
   --  not possible.

   type To_External_Priority_T is access procedure
     (Value    : ORB_Priority;
      Result   : out External_Priority;
      Returns  : out PolyORB.Types.Boolean);

   type To_ORB_Priority_T is access procedure
     (Value    : External_Priority;
      Result   : out ORB_Priority;
      Returns  : out PolyORB.Types.Boolean);

   To_External_Priority : To_External_Priority_T;

   To_ORB_Priority : To_ORB_Priority_T;

private
   Default_Component_Priority : constant ORB_Component_Priority
     := ORB_Component_Priority (System.Default_Priority);

   Default_Core_Priority : constant ORB_Core_Priority
     := ORB_Core_Priority'First;

   Invalid_Priority : constant External_Priority := External_Priority'Last;

end PolyORB.Tasking.Priorities;

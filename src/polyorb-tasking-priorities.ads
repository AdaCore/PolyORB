------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--           P O L Y O R B . T A S K I N G . P R I O R I T I E S            --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--            Copyright (C) 2003 Free Software Foundation, Inc.             --
--                                                                          --
-- PolyORB is free software; you  can  redistribute  it and/or modify it    --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. PolyORB is distributed  in the hope that it will be  useful,    --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with PolyORB; see file COPYING. If    --
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
--                PolyORB is maintained by ACT Europe.                      --
--                    (email: sales@act-europe.fr)                          --
--                                                                          --
------------------------------------------------------------------------------

--  This package defines priority management within PolyORB.

--  This package defines priority ranges available within PolyORB for
--  execution threads. It also defines priority mapping between entity
--  priority (e.g. as request, message, object) that impact the
--  execution priority level and PolyORB's internal priorities set.

--  $Id$

with System;

with PolyORB.Types;

package PolyORB.Tasking.Priorities is

   ------------------
   -- ORB Priority --
   ------------------

   --  ORB internal priorities are derived from Ada native priorities.
   --  We define ORB_Core and ORB_Component priority levels, so we
   --  make a distinction between ORB Core entities that require high
   --  priority to process some information and other components.

   subtype ORB_Priority is System.Priority range
     System.Priority'First .. System.Priority'Last;
   --  ORB priority range.

   ORB_Core_Levels : constant Natural := 1;
   --  Number of priority levels affected to the ORB Core.

   subtype ORB_Component_Priority is ORB_Priority range
     ORB_Priority'First .. ORB_Priority'Last - ORB_Core_Levels;
   --  ORB_Component_Priority defines the priority an ORB component
   --  may have. This range usually applies to most components,
   --  including user components.

   Default_Component_Priority : constant ORB_Component_Priority;
   --  Default priority for ORB Components.

   subtype ORB_Core_Priority is System.Priority range
     ORB_Priority'Last - ORB_Core_Levels + 1 .. ORB_Priority'Last;
   --  ORB_Core_Priority defines the priority of some ORB key
   --  components. It is reserved to high priority loops, such as
   --  PolyORB.ORB main loop.

   Default_Core_Priority : constant ORB_Core_Priority;
   --  Default priority for ORB Core.

   ----------------------
   -- Priority mapping --
   ----------------------

   --  Priority mapping policies define how a number representing the
   --  priority of an entity is mapped into an ORB_Priority.
   --  This definition is inspired by the RT-CORBA specification.

   --  Note that per construction, these functions cannot map an
   --  entity priority in the ORB_Core_Priority range.

   type Priority_Mapping is abstract tagged null record;

   procedure To_PolyORB_Priority
     (Self              : in  Priority_Mapping;
      External_Priority : in  Integer;
      PolyORB_Priority  : out ORB_Component_Priority;
      Returns           : out PolyORB.Types.Boolean) is abstract;
   --  Map an 'Eternal_Priority' into an ORB_Component_Priority.
   --  'Returns' is set to true if the operation was succesful.

   procedure To_External_Priority
     (Self              : in  Priority_Mapping;
      PolyORB_Priority  : in  ORB_Component_Priority;
      External_Priority : out Integer;
      Returns           : out PolyORB.Types.Boolean) is abstract;
   --  Map an ORB_Component_Priority into an 'External_Priority'.
   --  'Returns' is set to true if the operation was succesful.

private
   Default_Component_Priority : constant ORB_Component_Priority
     := ORB_Component_Priority (System.Default_Priority);

   Default_Core_Priority : constant ORB_Core_Priority
     := ORB_Core_Priority'First;

end PolyORB.Tasking.Priorities;

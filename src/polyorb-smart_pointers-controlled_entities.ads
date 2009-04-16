------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--               POLYORB.SMART_POINTERS.CONTROLLED_ENTITIES                 --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2001-2009, Free Software Foundation, Inc.          --
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

package PolyORB.Smart_Pointers.Controlled_Entities is

   pragma Preelaborate;

   ---------------------------------
   -- Controlled task-safe entity --
   ---------------------------------

   type Entity is abstract new Non_Controlled_Entity with private;
   procedure Initialize (X : in out Entity) is null;
   function Is_Controlled (X : Entity) return Boolean;

private

   ---------------------------------
   -- Task-safe controlled entity --
   ---------------------------------

   type Entity_Controller (E : access Entity'Class)
      is new Ada.Finalization.Limited_Controlled with null record;

   procedure Initialize (X : in out Entity_Controller);
   procedure Finalize   (X : in out Entity_Controller);

   type Entity is abstract new Non_Controlled_Entity with record
      Controller : Entity_Controller (Entity'Access);
      --  Controller component used to trigger a call to the Entity's
      --  Finalize primitive operation when it is Finalized (note that
      --  Entity itself is not a controlled type).
   end record;

end PolyORB.Smart_Pointers.Controlled_Entities;

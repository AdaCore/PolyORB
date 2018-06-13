------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--               POLYORB.SMART_POINTERS.CONTROLLED_ENTITIES                 --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2001-2014, Free Software Foundation, Inc.          --
--                                                                          --
-- This is free software;  you can redistribute it  and/or modify it  under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for  more details.                                               --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
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
   overriding function Is_Controlled (X : Entity) return Boolean;

private

   ---------------------------------
   -- Task-safe controlled entity --
   ---------------------------------

   type Entity_Controller (E : access Entity'Class)
      is new Ada.Finalization.Limited_Controlled with null record;

   overriding procedure Initialize (X : in out Entity_Controller);
   overriding procedure Finalize   (X : in out Entity_Controller);

   type Entity is abstract new Non_Controlled_Entity with record
      Controller : Entity_Controller (Entity'Access);
      --  Controller component used to trigger a call to the Entity's
      --  Finalize primitive operation when it is Finalized (note that
      --  Entity itself is not a controlled type).
   end record;

end PolyORB.Smart_Pointers.Controlled_Entities;

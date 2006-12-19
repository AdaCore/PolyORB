------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--            POLYORB.TASKING.PROFILES.RAVENSCAR.INDEX_MANAGER              --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2002-2006, Free Software Foundation, Inc.          --
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

--  This package provide a thread safe management of a pool of identifiers.

generic
   Number_Of_Indices : Natural;
package PolyORB.Tasking.Profiles.Ravenscar.Index_Manager is

   pragma Preelaborate;

   subtype Index_Type is Integer range 0 .. Number_Of_Indices - 1;
   --  Type of the identifiers that are managed by this package

   procedure Get (Id : out Index_Type);
   --  Get a unique identifier. No other call to Get will return this
   --  identifier until this identifier is released. Raise a
   --  No_More_Indentifier if all identifier are used.
   --  This procedure is executed in mutual exclusion, so that two tasks
   --  that make a call on Get will get two different identifiers.

   procedure Release (Id : Index_Type);
   --  Release the given identifier. Id will now be available and is
   --  eligible to be return by Get. Raise a Identifier_Already_Released
   --  when its is called on a free identifier, that do not need to be
   --  released.

   procedure Initialize (Error_On_Initialize : Boolean := True);
   --  Initialize this package.
   --  If Error_On_Initialise is set to false, we can call initialize
   --  several times. In this case, if the package was already
   --  initialized no initialization is done.

end PolyORB.Tasking.Profiles.Ravenscar.Index_Manager;

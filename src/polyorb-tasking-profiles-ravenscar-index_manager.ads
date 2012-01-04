------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--            POLYORB.TASKING.PROFILES.RAVENSCAR.INDEX_MANAGER              --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2002-2012, Free Software Foundation, Inc.          --
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

------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--      P O L Y O R B . R A V E N S C A R . I N D E X _ M A N A G E R       --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--             Copyright (C) 1999-2002 Free Software Fundation              --
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
--              PolyORB is maintained by ENST Paris University.             --
--                                                                          --
------------------------------------------------------------------------------

--  This package provide a thread safe management of a pool of identifiers.

--  $Id$

generic
   Number_Of_Indices : Natural;
package PolyORB.Ravenscar.Index_Manager is

   pragma Preelaborate;

   subtype Index_Type is Integer range 0 .. Number_Of_Indices - 1;
   --  Type of the identifiers that are managed by this package.

   function Modular (I : Integer) return Index_Type;
   pragma Inline (Modular);
   --  Convert an Integer to an Index_Type, returning
   --  I mod Number_Of_Indices

   procedure Get (Id : out Index_Type);
   --  Get a unique identifier. No other call to Get will return this
   --  identifier until this identifier is released. Raise a
   --  No_More_Indentifier if all identifier are used.
   --  This procedure is executed in mutual exclusion, so that two tasks
   --  that make a call on Get will get two different identifiers.

   procedure Release (Id : in Index_Type);
   --  Release the given identifier. Id will now be available and is
   --  eligible to be return by Get. Raise a Identifier_Already_Released
   --  when its is called on a free identifier, that do not need to be
   --  released.

   No_More_Identifier          : exception;

   Identifier_Already_Released : exception;

   procedure Initialize;
   --  initialize this package.

end PolyORB.Ravenscar.Index_Manager;

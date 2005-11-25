------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--    P O L Y O R B . I N I T I A L I Z A T I O N . E X C E P T I O N S     --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--            Copyright (C) 2005 Free Software Foundation, Inc.             --
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

--  This package provides a way for the initialization subsystem to raise
--  Program_Error with a meaningful exception message in case of failure.
--  Ada.Exceptions cannot be used directly, as it is not preelaborated in
--  Ada 95.

--  This is a compiler-specific implementation for GNAT.

with System.Pure_Exceptions;

package body PolyORB.Initialization.Exceptions is

   procedure Raise_Program_Error (Msg : String) is
      use System.Pure_Exceptions;
   begin
      Raise_Exception (PE, Msg);
   end Raise_Program_Error;

end PolyORB.Initialization.Exceptions;

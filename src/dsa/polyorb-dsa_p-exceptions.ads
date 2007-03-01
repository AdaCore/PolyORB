------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--             P O L Y O R B . D S A _ P . E X C E P T I O N S              --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2003-2007, Free Software Foundation, Inc.          --
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

--  Errors management for the DSA Application Personality of PolyORB.

with PolyORB.Any;
with PolyORB.Errors;

package PolyORB.DSA_P.Exceptions is

   function Exception_Repository_Id (Name, Version : String) return String;
   --  Build a repository ID from an Ada exception name and unit version

   procedure Raise_From_Error
     (Error : in out PolyORB.Errors.Error_Container);
   pragma No_Return (Raise_From_Error);
   --  Raise a DSA specific exception from the data in 'Error'

   procedure Raise_From_Any
     (Occurrence : Any.Any;
      Msg        : String := "<remote exception>");
   pragma No_Return (Raise_From_Any);
   --  Raise a DSA specific exception from the data in Occurrence, with an
   --  optional Exception_Message.

end PolyORB.DSA_P.Exceptions;

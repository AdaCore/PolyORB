------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--            P O L Y O R B . G I O P _ P . E X C E P T I O N S             --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2002-2007, Free Software Foundation, Inc.          --
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

--  Exceptions management for the GIOP Protocol Personality of PolyORB.

with PolyORB.Any;

package PolyORB.GIOP_P.Exceptions is

   function Is_System_Exception (Name : String) return Boolean;

   function To_CORBA_Exception
     (Exc : PolyORB.Any.Any)
     return PolyORB.Any.Any;
   --  Convert PolyORB Exc exception typecode to CORBA exception typecode

   function Extract_System_Exception_Name
     (Name : Standard.String)
     return Standard.String;
   --  Extract the name of the system exception found in Name.

   function System_Exception_TypeCode
     (Name : Standard.String)
     return PolyORB.Any.TypeCode.Local_Ref;
   --  Return the TypeCode corresponding to the indicated
   --  system exception name.

end PolyORB.GIOP_P.Exceptions;

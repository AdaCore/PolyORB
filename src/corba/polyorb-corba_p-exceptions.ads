------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--           P O L Y O R B . C O R B A _ P . E X C E P T I O N S            --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                Copyright (C) 2001 Free Software Fundation                --
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

--  Exceptions management for the CORBA Application Personality of PolyORB.

with Ada.Exceptions;

with PolyORB.Any;
with PolyORB.Exceptions;

package PolyORB.CORBA_P.Exceptions is

   procedure Raise_From_Any (Occurrence : PolyORB.Any.Any);
   pragma No_Return (Raise_From_Any);
   --  Raise CORBA exception.

   procedure Raise_From_Error
     (Error : in out PolyORB.Exceptions.Error_Container);
   pragma No_Return (Raise_From_Error);
   --  Raise a CORBA specific exception from the data in 'Error'

   function System_Exception_To_Any
     (E : Ada.Exceptions.Exception_Occurrence)
     return PolyORB.Any.Any;

   type Raise_From_Error_Hook is access
     procedure (Error : in out PolyORB.Exceptions.Error_Container);

   CORBA_Raise_From_Error : Raise_From_Error_Hook := null;
   POA_Raise_From_Error : Raise_From_Error_Hook := null;
   POAManager_Raise_From_Error : Raise_From_Error_Hook := null;


end PolyORB.CORBA_P.Exceptions;

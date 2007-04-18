------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--           P O L Y O R B . C O R B A _ P . E X C E P T I O N S            --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2001-2007, Free Software Foundation, Inc.          --
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

--  Exceptions management for the CORBA application personality of PolyORB

with Ada.Exceptions;

with CORBA;

with PolyORB.Any;
with PolyORB.Errors;
with PolyORB.Requests;

package PolyORB.CORBA_P.Exceptions is

   procedure Request_Raise_Occurrence (R : in out Requests.Request_Access);
   --  If R has non-empty exception information, call Raise_From_Any
   --  with an appropriate information message after having destroyed R.

   procedure Raise_From_Any
     (Occurrence : PolyORB.Any.Any;
      Message    : String := "");
   pragma No_Return (Raise_From_Any);
   --  Raise CORBA exception from data in 'Occurrence'

   function System_Exception_To_Any
     (E : Ada.Exceptions.Exception_Occurrence) return PolyORB.Any.Any;
   function System_Exception_To_Any
     (E : Ada.Exceptions.Exception_Occurrence) return CORBA.Any;
   --  Convert a CORBA System Exception into a Any

   procedure Raise_From_Error
     (Error   : in out PolyORB.Errors.Error_Container;
      Message : String := "");
   pragma No_Return (Raise_From_Error);
   --  Raise a CORBA specific exception from the data in 'Error'

   --  Exceptions classification

   function Is_Forward_Request
     (Occurrence : PolyORB.Any.Any)
     return Boolean;
   --  Return True iff Occurrence is a PolyORB forward request exception

   function Is_Needs_Addressing_Mode
     (Occurrence : PolyORB.Any.Any)
     return Boolean;
   --  Returns True iff Occurrence is a PolyORB style addressing mode change
   --  request.

   function Is_System_Exception
     (Occurrence : PolyORB.Any.Any)
      return Boolean;
   --  Return True iff Occurrence is an ORB system exception

   ----------------------------
   -- Raise_From_Error Hooks --
   ----------------------------

   --  The CORBA aplication personality may raise exceptions found in
   --  different packages. Hooks are set up to work around circular
   --  references problems.

   type Raise_From_Error_Hook is access
     procedure
       (Error   : in out PolyORB.Errors.Error_Container;
        Message : String);

   CORBA_Raise_From_Error      : Raise_From_Error_Hook := null;
   --  Raise CORBA.* exceptions

   POA_Raise_From_Error        : Raise_From_Error_Hook := null;
   --  Raise PortableServer.POA.* exceptions

   POAManager_Raise_From_Error : Raise_From_Error_Hook := null;
   --  Raise PortableServer.POAManager.* exceptions

end PolyORB.CORBA_P.Exceptions;

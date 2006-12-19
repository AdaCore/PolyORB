------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--              P O L Y O R B . P R O T O C O L S . I F A C E               --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2001-2005 Free Software Foundation, Inc.           --
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
--                  PolyORB is maintained by AdaCore                        --
--                     (email: sales@adacore.com)                           --
--                                                                          --
------------------------------------------------------------------------------

--  Interface for Sessions

with PolyORB.Any.NVList;
with PolyORB.Components;

package PolyORB.Protocols.Iface is

   --  When a Session receives a method invocation request,
   --  it is not always possible to determine the signature
   --  for the called method immediately; it may be necessary
   --  to wait until the servant has started handling the
   --  request, and performs a call to ServerRequest.Arguments.

   --  In that case, where unmarshalling is deferred until request
   --  execution commences, the message Unmarshall_Arguments must
   --  be sent to the Session with a properly-type NVList in it
   --  so the unmarshalling can take place. An Unmarshalled_Arguments
   --  message is returned.

   --  If an error is dectected when unmarshalling, then
   --  Arguments_Error is returned.

   type Unmarshall_Arguments is new Components.Message with record
      Args : Any.NVList.Ref;
   end record;

   type Unmarshalled_Arguments is new Components.Message with record
      Args : Any.NVList.Ref;
   end record;

   type Arguments_Error is new Components.Message with record
      Error : Errors.Error_Container;
   end record;

   --  The Flush message reinitializes the session object.

   type Flush is new Components.Message with null record;

end PolyORB.Protocols.Iface;

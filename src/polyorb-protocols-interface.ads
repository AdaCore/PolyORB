------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--          P O L Y O R B . P R O T O C O L S . I N T E R F A C E           --
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

--  Interface for Sessions.

--  $Id$

with PolyORB.Any.NVList;
with PolyORB.Components;

package PolyORB.Protocols.Interface is

   type Unmarshall_Arguments is new Components.Message with record
      Args : Any.NVList.Ref;
   end record;

   type Unmarshalled_Arguments is new Components.Message with record
      Args : Any.NVList.Ref;
   end record;

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

end PolyORB.Protocols.Interface;

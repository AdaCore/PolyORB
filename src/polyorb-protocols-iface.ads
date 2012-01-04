------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--              P O L Y O R B . P R O T O C O L S . I F A C E               --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2001-2012, Free Software Foundation, Inc.          --
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

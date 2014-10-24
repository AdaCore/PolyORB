------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                    P O L Y O R B . O R B . I F A C E                     --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2001-2013, Free Software Foundation, Inc.          --
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

--  The messages supported by ORBs (middleware core module).

with PolyORB.Components;
with PolyORB.Requests;
with PolyORB.Transport;

package PolyORB.ORB.Iface is

   type Queue_Request is new Components.Message with record
      Request   : Requests.Request_Access;
      Requestor : Components.Component_Access;
   end record;
   --  Queue method invocation request Req for execution by Server
   --  on behalf of a remote caller. No reply expected.
   --  When the request is executed, a message will be sent
   --  back to Requestor (asynchronously). If this request comes
   --  from a Session, Requestor must be set to that Session.
   --  If the request is submitted directly by a local client task,
   --  Requestor must be set to null.
   --  The client the responsible of the destruction of
   --  the Request after its execution is completed.

   type Unregister_Endpoint is new Components.Message with record
      TE : Transport.Transport_Endpoint_Access;
   end record;
   --  Request that TE be removed from the set of endpoints
   --  managed by the ORB.

   type Monitor_Access_Point is new Components.Message with record
      TAP : Transport.Transport_Access_Point_Access;
   end record;
   --  A binding object requests that the designated transport
   --  access point be monitored for incoming data.

   type Monitor_Endpoint is new Components.Message with record
      TE : Transport.Transport_Endpoint_Access;
   end record;
   --  A binding object requests that the designated transport
   --  endpoint be monitored for incoming data.

   type Validate_Endpoint is new Components.Message with record
      TE : Transport.Transport_Endpoint_Access;
   end record;
   --  Force a validity check on TE if needed (see ORB.Handle_Validate_TE)

end PolyORB.ORB.Iface;

------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                    P O L Y O R B . T R A N S P O R T                     --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                Copyright (C) 2001 Free Software Fundation                --
--                                                                          --
-- AdaBroker is free software; you  can  redistribute  it and/or modify it  --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. AdaBroker  is distributed  in the hope that it will be  useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with AdaBroker; see file COPYING. If  --
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

--  Abstract transport service access points and
--  communication endpoints.

--  $Id$

with Ada.Streams; use Ada.Streams;

with PolyORB.Annotations;
with PolyORB.Asynch_Ev;
with PolyORB.Buffers;
with PolyORB.Components;

package PolyORB.Transport is

   --  Package body needs PolyORB.ORB.Interface, which has an
   --  indirect dependency on Transport: no pramga Elaborate_Body.

   use PolyORB.Asynch_Ev;

   -------------------------------------------------------------
   -- A transport service access point:                       --
   --                                                         --
   -- an object that has an address within a communcation     --
   -- domain, on which connections can be established by      --
   -- remote entities that wish to communicate with this ORB. --
   -------------------------------------------------------------

   type Transport_Access_Point
      is abstract new Components.Component with private;
   type Transport_Access_Point_Access is
     access all Transport_Access_Point'Class;
   --  A listening transport service access point.

   function Notepad_Of (TAP : Transport_Access_Point_Access)
     return Annotations.Notepad_Access;
   pragma Inline (Notepad_Of);
   --  A TAP is an annotable object (cf. PolyORB.Annotations),
   --  so clients can associate it with any information they see fit.
   --  This functions returns an access to TAP's Notepad component.

   function Create_Event_Source
     (TAP : Transport_Access_Point)
     return Asynch_Ev_Source_Access
      is abstract;
   --  Create a view of TAP as an asyncrhonous event source.

   function Handle_Message
     (TAP : access Transport_Access_Point;
      Msg : Components.Message'Class)
     return Components.Message'Class;

   ----------------------------------------------------------------
   -- A transport service endpoint:                              --
   --                                                            --
   -- an object that represent a connection that was established --
   -- when a transport access point was contacted.               --
   ----------------------------------------------------------------

   type Transport_Endpoint
      is abstract new Components.Component with private;

   procedure Connect_Upper
     (TE    : access Transport_Endpoint;
      Upper : Components.Component_Access);
   --  Connect the "upper layer" signal of TE to Upper.

   type Transport_Endpoint_Access is access all Transport_Endpoint'Class;
   --  An opened transport endpoint.

   function Notepad_Of (TE : Transport_Endpoint_Access)
     return Annotations.Notepad_Access;
   pragma Inline (Notepad_Of);

   procedure Destroy (TE : in out Transport_Endpoint_Access);
   --  Destroy a transport endpoint and the associated protocol stack.

   function Handle_Message
     (TE  : access Transport_Endpoint;
      Msg : Components.Message'Class)
     return Components.Message'Class;

   ----------------------------------------------------
   -- Primitive operations of Transport_Access_Point --
   -- and Transport_Endpoint.                        --
   -- To be overridden by concrete implementations.  --
   ----------------------------------------------------

   --  These primitives are invoked from event-driven ORB
   --  threads, and /must not/ be blocking.

   procedure Accept_Connection
     (TAP : Transport_Access_Point;
      TE  : out Transport_Endpoint_Access)
      is abstract;
   --  Accept a pending new connection on TAP and create
   --  a new associated TE.

   --  function Address (TAP : Transport_Access_Point)
   --    return Binding_Data is abstract;

   Connection_Closed : exception;

   function Create_Event_Source
     (TE : Transport_Endpoint)
     return Asynch_Ev_Source_Access
      is abstract;
   --  Create a view of TE as an asyncrhonous event source.

   procedure Read
     (TE     : in out Transport_Endpoint;
      Buffer : Buffers.Buffer_Access;
      Size   : in out Stream_Element_Count)
      is abstract;
   --  Receive data from TE into Buffer. When Read is Called,
   --  Size is set to the maximum size of the data to be received.
   --  On return, Size is set to the effective amount of data received.

   procedure Write
     (TE     : in out Transport_Endpoint;
      Buffer : Buffers.Buffer_Access)
      is abstract;
   --  Write out the contents of Buffer onto TE.

   procedure Close (TE : in out Transport_Endpoint) is abstract;

private

   type Transport_Access_Point
      is abstract new Components.Component with record
         Notepad : aliased Annotations.Notepad;
      end record;

   type Transport_Endpoint
      is abstract new Components.Component with record
         Notepad : aliased Annotations.Notepad;

         Server : Components.Component_Access;
         --  Communication signal to ORB core.

         Upper  : Components.Component_Access;
         --  Communication signal to upper layer.

         In_Buf : Buffers.Buffer_Access;
         Max    : Ada.Streams.Stream_Element_Count;
      end record;

end PolyORB.Transport;

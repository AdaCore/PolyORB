------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                    P O L Y O R B . T R A N S P O R T                     --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2001-2003 Free Software Foundation, Inc.           --
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
--                PolyORB is maintained by ACT Europe.                      --
--                    (email: sales@act-europe.fr)                          --
--                                                                          --
------------------------------------------------------------------------------

--  Abstract transport service access points and transport endpoints.

--  $Id$

with Ada.Streams;

with PolyORB.Annotations;
with PolyORB.Asynch_Ev;
with PolyORB.Buffers;
with PolyORB.Components;
with PolyORB.Exceptions;

package PolyORB.Transport is

   --  Package body needs PolyORB.ORB.Interface, which has an
   --  indirect dependency on Transport: no pramga Elaborate_Body.

   -------------------------------------------------------------
   -- A transport service access point:                       --
   -- An object that has an address within a communication    --
   -- domain, to which connections can be established by      --
   -- remote entities that wish to communicate with this ORB. --
   -------------------------------------------------------------

   type Transport_Access_Point
      is abstract new Components.Component with private;
   type Transport_Access_Point_Access is
     access all Transport_Access_Point'Class;
   --  A listening transport service access point

   function Notepad_Of
     (TAP : Transport_Access_Point_Access)
     return Annotations.Notepad_Access;
   pragma Inline (Notepad_Of);
   --  A TAP is an annotable object (cf. PolyORB.Annotations),
   --  so clients can associate it with any information they see fit.
   --  This functions returns an access to TAP's Notepad component.

   function Create_Event_Source
     (TAP : Transport_Access_Point)
     return Asynch_Ev.Asynch_Ev_Source_Access
      is abstract;
   --  Create a view of TAP as an asyncrhonous event source.
   --  This function MUST create an Event Handler for this acces point.
   --  The Event Handler will be referenced by the AES.Handler Note.

   function Handle_Message
     (TAP : access Transport_Access_Point;
      Msg :        Components.Message'Class)
     return Components.Message'Class;

   -----------------------------------------------------------------
   -- A transport service endpoint:                               --
   -- An object that represents a connection that was established --
   -- when a transport access point was contacted.                --
   -----------------------------------------------------------------

   type Transport_Endpoint
      is abstract new Components.Component with private;

   procedure Connect_Upper
     (TE    : access Transport_Endpoint;
      Upper :        Components.Component_Access);
   --  Connect the "upper layer" signal of TE to Upper.

   type Transport_Endpoint_Access is access all Transport_Endpoint'Class;
   --  An opened transport endpoint.

   function Notepad_Of
     (TE : Transport_Endpoint_Access)
     return Annotations.Notepad_Access;
   pragma Inline (Notepad_Of);

   procedure Destroy (TE : in out Transport_Endpoint_Access);
   --  Destroy a transport endpoint and the associated protocol stack.

   function Handle_Message
     (TE  : access Transport_Endpoint;
      Msg :        Components.Message'Class)
     return Components.Message'Class
     is abstract;

   function Upper
     (TE : Transport_Endpoint_Access)
     return Components.Component_Access;
   --  Return a component access to the upper layer of TE

   ----------------------------------------------------
   -- Primitive operations of Transport_Access_Point --
   -- and Transport_Endpoint.                        --
   -- To be overridden by concrete implementations.  --
   ----------------------------------------------------

   --  These primitives are invoked from event-driven ORB
   --  threads, and /must not/ be blocking.

   function Create_Event_Source
     (TE : Transport_Endpoint)
     return Asynch_Ev.Asynch_Ev_Source_Access
      is abstract;
   --  Create a view of TE as an asyncrhonous event source.
   --  This function MUST create an Event Handler for this acces point.
   --  The Event Handler will be referenced by the AES.Handler Note.

   procedure Read
     (TE     : in out Transport_Endpoint;
      Buffer :        Buffers.Buffer_Access;
      Size   : in out Ada.Streams.Stream_Element_Count;
      Error  :    out Exceptions.Error_Container)
      is abstract;
   --  Receive data from TE into Buffer. When Read is Called,
   --  Size is set to the maximum size of the data to be received.
   --  On return, Size is set to the effective amount of data received.

   procedure Write
     (TE     : in out Transport_Endpoint;
      Buffer :        Buffers.Buffer_Access;
      Error  :    out Exceptions.Error_Container)
      is abstract;
   --  Write out the contents of Buffer onto TE.

   procedure Close (TE : in out Transport_Endpoint) is abstract;

   --  Handler for AES associated with a Transport Access Point
   --  is defined in polyorb-binding_data.ads

   type TE_AES_Event_Handler
      is abstract new PolyORB.Asynch_Ev.AES_Event_Handler with record
         TE : PolyORB.Transport.Transport_Endpoint_Access;
      end record;
   --  Handler for AES associated with a Transport End Point

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

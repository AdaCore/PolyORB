------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                    P O L Y O R B . T R A N S P O R T                     --
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

--  Abstract transport service access points and transport endpoints.

with Ada.Streams;

with PolyORB.Annotations;
with PolyORB.Asynch_Ev;
with PolyORB.Buffers;
with PolyORB.Components;
with PolyORB.Errors;
with PolyORB.Smart_Pointers;

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
     (TAP : access Transport_Access_Point)
     return Asynch_Ev.Asynch_Ev_Source_Access
      is abstract;
   --  Create a view of TAP as an asynchronous event source. The Handler
   --  of the newly-created event source is TAP.Handler.

   function Handle_Message
     (TAP : not null access Transport_Access_Point;
      Msg : Components.Message'Class) return Components.Message'Class;

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
     (TE : Transport_Endpoint_Access) return Annotations.Notepad_Access;
   pragma Inline (Notepad_Of);

   function Handle_Message
     (TE  : not null access Transport_Endpoint;
      Msg : Components.Message'Class) return Components.Message'Class;

   function Upper
     (TE : Transport_Endpoint_Access) return Components.Component_Access;
   --  Return a component access to the upper layer of TE

   ----------------------------------------------------
   -- Primitive operations of Transport_Access_Point --
   -- and Transport_Endpoint.                        --
   -- To be overridden by concrete implementations.  --
   ----------------------------------------------------

   --  These primitives are invoked from event-driven ORB
   --  threads, and /must not/ be blocking.

   function Create_Event_Source
     (TE : access Transport_Endpoint)
     return Asynch_Ev.Asynch_Ev_Source_Access
      is abstract;
   --  Create a view of TE as an asynchronous event source. The Handler
   --  of the newly-created event source is TE.Handler.

   procedure Read
     (TE     : in out Transport_Endpoint;
      Buffer : Buffers.Buffer_Access;
      Size   : in out Ada.Streams.Stream_Element_Count;
      Error  : out Errors.Error_Container)
      is abstract;
   --  Receive data from TE into Buffer at the current position. On entry,
   --  called, Size is set to the maximum size of the data to be received.
   --  On return, Size is set to the effective amount of data received.
   --  The current position in Buffer remains unchanged.

   procedure Write
     (TE     : in out Transport_Endpoint;
      Buffer :        Buffers.Buffer_Access;
      Error  :    out Errors.Error_Container)
      is abstract;
   --  Write out the contents of Buffer onto TE

   procedure Close (TE : access Transport_Endpoint);
   --  Dissociate the transport endpoint from any communication resource

   procedure Destroy (TE : in out Transport_Endpoint);
   --  Destroy any resources allocated to TE

   procedure Destroy (TE : in out Transport_Endpoint_Access);
   --  Destroy TE and the protocol stack built upon it, recursively.
   --  Deallocate TE. On return, TE is null.

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

         Binding_Object : Smart_Pointers.Entity_Ptr;
         --  Enclosing binding object entity (not reference counted, set for
         --  both client side and server side TEs).

         Dependent_Binding_Object : Smart_Pointers.Ref;
         --  For server-side transport endpoints, keep a reference to the
         --  associated binding object as long as the transport endpoint is
         --  is alive. Note: when Dependent_Binding_Object is set, its
         --  entity is always equal to the above Binding_Object.

         Closed : Boolean := False;
         --  Set to True once Close has been called on this endpoint.

         In_Buf : Buffers.Buffer_Access;
         Max    : Ada.Streams.Stream_Element_Count;
      end record;

   procedure Check_Validity (TE : access Transport_Endpoint);
   --  Check whether TE (which must not be closed) is still valid, and if not,
   --  close it. Used for handling of Check_Validity filter message.

end PolyORB.Transport;

--  Abstract transport service access points and
--  communication endpoints.

--  $Id$

with Ada.Streams; use Ada.Streams;

with Droopi.Asynch_Ev; use Droopi.Asynch_Ev;
with Droopi.Buffers; use Droopi.Buffers;
with Droopi.Components; use Droopi.Components;

package Droopi.Transport is

   type Transport_Access_Point
      is abstract tagged limited private;
   type Transport_Access_Point_Access is
     access all Transport_Access_Point'Class;
   --  A listening transport service access point.

   function Create_Event_Source
     (TAP : Transport_Access_Point)
     return Asynch_Ev_Source_Access
      is abstract;
   --  Create a view of TAP as an asyncrhonous event source.

   type Transport_Endpoint is abstract new Component with private;

   procedure Connect_Upper
     (TE    : access Transport_Endpoint;
      Upper : Components.Component_Access);
   --  Connect the "upper layer" signal of TE to Upper.

   type Transport_Endpoint_Access is access all Transport_Endpoint'Class;
   --  An opened transport endpoint.

   function Handle_Message
     (TE  : access Transport_Endpoint;
      Msg : Components.Message'Class)
     return Boolean;

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
     (TE     : Transport_Endpoint;
      Buffer : Buffer_Access;
      Size   : in out Stream_Element_Count)
      is abstract;
   --  Receive data from TE into Buffer. When Read is Called,
   --  Size is set to the maximum size of the data to be received.
   --  On return, Size is set to the effective amount of data received.

   procedure Write
     (TE     : Transport_Endpoint;
      Buffer : Buffer_Access)
      is abstract;
   --  Write out the contents of Buffer onto TE.

   procedure Close (TE : Transport_Endpoint) is abstract;

private

   type Transport_Access_Point
      is abstract tagged limited null record;

   type Transport_Endpoint
      is abstract new Components.Component with record
         Upper  : Components.Component_Access;
         --  Communication signal to upper layer.

         In_Buf : Buffers.Buffer_Access;
         Max    : Ada.Streams.Stream_Element_Count;
      end record;

end Droopi.Transport;

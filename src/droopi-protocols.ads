--  Support for object method invocation protocols.

--  $Id$

with Ada.Streams;

with Droopi.Buffers;
with Droopi.Components;
with Droopi.Filters; use Droopi.Filters;
with Droopi.Requests; use Droopi.Requests;

package Droopi.Protocols is

   pragma Elaborate_Body;

   --  A protocol is a factory of sessions. Each session corresponds
   --  to a connection to a remote protocol entity.

   type Protocol is abstract new Filters.Factory with private;
   type Protocol_Access is access all Protocol'Class;

   type Session is abstract new Filters.Filter with private;
   type Session_Access is access all Session'Class;

   procedure Create
     (Proto   : access Protocol;
      Session : out Filter_Access)
      is abstract;
   --  Create a Session for protocol Proto using filter Lower.

   procedure Destroy_Session (S : in out Session_Access);
   --  Destroy the session associated with S, return any associated
   --  resources to the system, and assign null to S.

   -----------------------------------------------------
   -- Protocol primitives (interface to upper layers) --
   -----------------------------------------------------

   procedure Invoke_Request (S : access Session; R : Request)
      is abstract;
   procedure Abort_Request (S : access Session; R : Request)
      is abstract;

   --  XXX
   --  Primitives of Session might be derived from the primitives
   --  of AdaBroker type Broca.IOP.Connection_Type.

--     -----------------------------------
--     -- Abstract GIOP connection type --
--     -----------------------------------
--
--     type Connection_Type is abstract tagged private;
--     type Connection_Ptr is access all Connection_Type'Class;
--
--     function Get_Request
--       (Connection : access Connection_Type)
--       return CORBA.Unsigned_Long;
--     --  Get a new request id for this connection.
--
--     procedure Release
--       (Connection : access Connection_Type) is abstract;
--     --  Release a previously suspended connection.
--
--     procedure Send
--       (Connection : access Connection_Type;
--        Buffer     : access Buffers.Buffer_Type) is abstract;
--     --  Send a buffer to a connection. Raise Comm_Failure on error.
--
--     function Receive
--       (Connection : access Connection_Type;
--        Length     : Opaque.Index_Type)
--       return Opaque.Octet_Array_Ptr is abstract;
--     --  Receive data from a connection. Raise Comm_Failure on error.

   ------------------------------------------------
   -- Callback point (interface to lower layers) --
   ------------------------------------------------

   function Handle_Message
     (Sess : access Session;
      S : Components.Message'Class)
     return Boolean;
   --  Demultiplex Data_Units to specialized operations.

   procedure Handle_Connect (S : access Session) is abstract;
   --  Invoked when a new incoming connection has been accepted
   --  as session S.

   procedure Handle_Data_Indication (S : access Session) is abstract;
   --  Invoked when some data arrives for session S.

   procedure Handle_Disconnect (S : access Session) is abstract;
   --  Invoked when the underlying connection is closed.

private

   type Protocol is abstract new Filters.Factory with null record;

   type Session is abstract new Filters.Filter with null record;

   procedure Expect_Data
     (S      : access Session;
      In_Buf : Buffers.Buffer_Access;
      Max    : Ada.Streams.Stream_Element_Count);

   --  XXX remove
--     with record
--       Server  : Schedulers.Server_Access;
--       Channel : Channels.Channel_Access;
--     end record;

--     type Session_Channel is new Channels.Channel with record
--        Session : Session_Access;
--        --  Uplink to the associated session.
--     end record;
--
--     procedure Signal_Data (SC : access Session_Channel);
--     procedure Signal_Connection_Closed (SC : access Session_Channel);

end Droopi.Protocols;

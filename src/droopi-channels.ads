--  A communication channel (a transport service protocol entity).

-- $Id$

with Droopi.ORB;
with Droopi.Protocols;

package Droopi.Channels is

  type Channel is abstract tagged limited private;
  type Channel_Access is access all Channel'Class;
  --  One communication channel.

  ---------------------------------------------------
  -- Channel primitives (interface to upper layer) --
  ---------------------------------------------------

  procedure Attach
    (C : access Channel;
     S : Droopi.Protocols.Session_Access);
  --  Attach C to upper protocol session S.

  procedure Send_Data (C : access Channel) is abstract;
  --  Send data onto the channel.

  procedure Expect_Data (C : access Channel; Size : Natural)
    is abstract;
  --  Signal C that data is expected. If Size = 0,
  --  a callback to the attached session will be made as soon
  --  as data is received; if Size > 0, a callback
  --  will be made when exactly Size bytes of data have
  --  been received.

  -----------------------------------------------
  -- Callback point (interface to lower layer) --
  -----------------------------------------------

  procedure Handle_Data
    (C : access Channel;
     AS : Droopi.ORB.Active_Socket) is abstract;
  --  Interface to lower layer: handle a 'Data present' event.

private

   type Channel is abstract tagged limited record
      Session : Session_Access;
   end record;

end Droopi.Channels;



--  A communication channel (a transport service protocol entity).

-- $Id$

with Droopi.ORB;

package Droopi.Channels is

  type Channel is abstract tagged limited private;
  type Channel_Access is access all Channel'Class;
  --  One communication channel.
  
  procedure Handle_Data
    (C : access Channel;
     AS : Droopi.ORB.Active_Socket) is abstract;
  --  Interface to lower layer: handle a 'Data present' event.
  
private
   
   type Channel is abstract tagged limited null record;
   
end Droopi.Channels;



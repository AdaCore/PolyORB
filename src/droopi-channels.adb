--  A communication channel (a transport service protocol entity).

-- $Id$

package body Droopi.Channels is

  procedure Attach
    (C : access Channel;
     S : Droopi.Protocols.Session_Access) is
  begin
     C.Session := S;
  end Attach;
  
end Droopi.Channels;



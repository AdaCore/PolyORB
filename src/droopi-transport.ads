--  Communication channels.

--  $Id$

with Droopi.Sockets;

package Droopi.Transport is

   pragma Preelaborate;

   subtype Channel is Droopi.Sockets.Socket_Type;

end Droopi.Transport;

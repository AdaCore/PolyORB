--  A filter that wraps a Socket.

--  $Id$

with Droopi.ORB; use Droopi.ORB;

package Droopi.Filters.Sockets is

   pragma Elaborate_Body;

   procedure Create (Sock : in out Active_Socket);
   --  Create a Socket_Filter associated with Sock.
   --  On output, Sock.Channel is set to the newly-created
   --  filter.

   Connection_Closed : exception;
   --  Raised by Handle_SDU when a disconnect is detected.

private

   type Socket_Filter is new Filter with record
      Sock : Active_Socket;

      In_Buf : Buffer_Access;
      Max : Stream_Element_Count;
   end record;

   procedure Handle_SDU
     (SF : access Socket_Filter;
      S  :  SDU);

end Droopi.Filters.Sockets;


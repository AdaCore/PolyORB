with Broca.Opaque;
with Interfaces.C;
with System;

package Broca.Sockets is

   pragma Elaborate_Body;

   function Receive
     (Socket : Interfaces.C.int;
      Buffer : access Broca.Opaque.Octet_Array;
      Length : Broca.Opaque.Index_Type)
     return Broca.Opaque.Index_Type;
   --  Call C recv() until everything has been read or an error is returned.
   --  Returns the number of bytes read before the error.

   function Receive
     (Socket : Interfaces.C.int;
      Buffer : System.Address;
      Length : Broca.Opaque.Index_Type)
    return Broca.Opaque.Index_Type;
   --  Similar function but with a System.Address instead

end Broca.Sockets;

--  Interface for Sessions.

--  $Id$

with PolyORB.Any.NVList;
with PolyORB.Components;

package PolyORB.Protocols.Interface is

   type Unmarshall_Arguments is new Components.Message with record
      Args : Any.NVList.Ref;
   end record;

   type Unmarshalled_Arguments is new Components.Message with record
      Args : Any.NVList.Ref;
   end record;

   --  When a Session receives a method invocation request,
   --  it is not always possible to determine the signature
   --  for the called method immediately; it may be necessary
   --  to wait until the servant has started handling the
   --  request, and performs a call to ServerRequest.Arguments.

   --  In that case, where unmarshalling is deferred until request
   --  execution commences, the message Unmarshall_Arguments must
   --  be sent to the Session with a properly-type NVList in it
   --  so the unmarshalling can take place. An Unmarshalled_Arguments
   --  message is returned.

end PolyORB.Protocols.Interface;

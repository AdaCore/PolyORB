--  The messages supported by Servants (object implementations).

--  $Id$

with PolyORB.Components;
with PolyORB.Requests;

package PolyORB.Objects.Interface is

   type Execute_Request is new Components.Message with record
      Req : Requests.Request_Access;
   end record;
   --  Request the receiving Servant to execute Req.
   --  The expected reply is Executed_Request. Null_Message
   --  can also be returned if the request was not processed
   --  immediately.

   type Executed_Request is new Components.Message with record
      Req : Requests.Request_Access;
   end record;
   --  Returned by a servant after Req has been executed.

end PolyORB.Objects.Interface;

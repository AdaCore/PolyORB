--  The messages supported by Servants (object implementations).

--  $Id$

with Droopi.Components;
with Droopi.Requests;

package Droopi.Objects.Interface is

   pragma Elaborate_Body;

   type Execute_Request is new Components.Message with record
      Req : Requests.Request_Access;
   end record;
   --  Request the receiving Servant to execute Req.
   --  The expected reply is Executed_Request.

   type Executed_Request is new Components.Message with record
      Req : Requests.Request_Access;
   end record;
   --  Returned by a servant after Req has been executed.

   procedure Emit_Execute_Request
     (S : Servant_Access;
      R : Requests.Request_Access);
   pragma Inline (Emit_Execute_Request);
   --  Ask S to process R, and get back the processed request.

end Droopi.Objects.Interface;

--  The messages supported by Servants (object implementations).

--  $Id$

package body Droopi.Objects.Interface is

   procedure Emit_Execute_Request
     (S : Servant_Access;
      R : Requests.Request_Access)
   is
      Result : constant Components.Message'Class
        := Components.Emit (Components.Component_Access (S),
                            Execute_Request'(Req => R));
   begin
      if not (Result in Executed_Request) then
         raise Components.Unhandled_Message;
      end if;
   end Emit_Execute_Request;

end Droopi.Objects.Interface;

--  A simple test server object.

--  $Id$

with Ada.Text_IO;

with Droopi.Components;
with Droopi.Objects.Interface;
with Droopi.Requests;

package body Droopi.Test_Object is

   use Ada.Text_IO;

   use Droopi.Objects.Interface;
   use Droopi.Requests;

   function Echo_String
     (O : My_Object;
      S : String)
     return String is
   begin
      return S;
   end Echo_String;

   function Echo_Integer
     (O : My_Object;
      I : Integer)
     return Integer is
   begin
      return I;
   end Echo_Integer;

   function Handle_Message
     (O   : access My_Object;
      Msg : Components.Message'Class)
     return Components.Message'Class is
   begin
      if Msg in Execute_Request then
         declare
            Req : Request_Access renames Execute_Request (Msg).Req;
         begin
            Put_Line ("The server is executing the request:"
                      & Droopi.Requests.Image (Req.all));
            return Executed_Request'(Req => Req);
         end;
      else
         raise Droopi.Components.Unhandled_Message;
      end if;
   end Handle_Message;


end Droopi.Test_Object;


with Droopi.Requests;

package Droopi.Servers is

   type Exit_Condition_Access is access all Boolean;

   type Server_Type is abstract tagged limited private;
   type Server_Access is access all Server_Type'Class;

   procedure Queue_Request
     (Server : access Server_Type;
      Req    : Requests.Request_Access)
     is abstract;
   --  An ORB can execute requests on behalf of callers.
   --  The ORB Destroys Req after executing it.

private

   type Server_Type is abstract tagged limited null record;

end Droopi.Servers;

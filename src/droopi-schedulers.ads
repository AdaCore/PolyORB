--  Abstract interface to a middleware's work scheduling facility.

with Droopi.Jobs;
with Droopi.Requests;

package Droopi.Schedulers is

   type Exit_Condition_Access is access all Boolean;

   type Server_Type is abstract tagged limited private;
   type Server_Access is access all Server_Type'Class;

   procedure Queue_Job
     (Server : access Server_Type;
      Job    : Droopi.Jobs.Job_Access)
      is abstract;
   --  Queue any job for execution by Server.

   procedure Queue_Request
     (Server : access Server_Type;
      Req    : Requests.Request_Access)
     is abstract;
   --  Queue method invocation request Req for execution by Server
   --  on behalf of a remote caller.
   --  Server destroys Req after executing it.

private

   type Server_Type is abstract tagged limited null record;

end Droopi.Schedulers;

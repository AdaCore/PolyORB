--  $Id$

package body Droopi.Tasking_Policies.Multitask is
   
   ----------------------
   -- Task_Per_Session --
   ----------------------
   
   procedure Handle_New_Connection
     (P : access Task_Per_Session;
      C : Channel) is
   begin
      Create_Session_Task (C);
   end Handle_New_Connection;
   
   procedure Handle_Request
     (P : access Task_Per_Session;
      R : Request) is
   begin
      
      --  In the task per session policy, requests
      --  are executed by the task handling the session,
      --  /not/ by general purpose ORB tasks. This procedure
      --  is thus never called.
      
      pragma Assert (False);
      null;
   end Handle_Request;
   
   ----------------------
   -- Task_Per_Request --
   ----------------------
   
   procedure Handle_Request
     (P : access Task_Per_Request;
      R : Request) is
   begin
      Create_Request_Task (R);
   end Handle_Request;

   ---------------
   -- Task_Pool --
   ---------------

   procedure Handle_Request
     (P : access Task_Pool;
      R : Request) is
   begin
      Handle_Request (No_Tasking (P.all)'Access, R);
      --  Inherited behaviour.
      
      --  Check task pool. Create a new task
      --  if necessary.
      
      raise Not_Implemented;
   end Handle_Request;

end Droopi.Tasking_Policies.Multitask;

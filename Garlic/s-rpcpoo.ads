--
--  $Id$
--

with System.RPC.Util;

private package System.RPC.Pool is

   --  This package handles a pool of anonymous tasks which will be used
   --  by System.RPC to handle incoming calls.

   task type Anonymous_Task (Partition    : Partition_ID;
                             Id           : Request_Id;
                             Params       : Params_Stream_Access;
                             Asynchronous : Boolean) is
      pragma Storage_Size (300_000);
   end Anonymous_Task;
   type Anonymous_Task_Access is access Anonymous_Task;
   --  An anonymous task will serve a request.

   procedure Abort_Task (Partition : in Partition_ID;
                         Id        : in Request_Id);

   procedure Shutdown;
   --  Called on shutdown.

end System.RPC.Pool;

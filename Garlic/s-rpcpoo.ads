--
--  $Id$
--

with System.RPC.Util;

private package System.RPC.Pool is

   --  This package handles a pool of anonymous tasks which will be used
   --  by System.RPC to handle incoming calls.

   procedure Allocate_Task (Partition    : in Partition_ID;
                            Id           : in Request_Id;
                            Params       : in Params_Stream_Access;
                            Asynchronous : in Boolean);
   --  Start a new anonymous task to handle the request.

   procedure Abort_Task (Partition : in Partition_ID;
                         Id        : in Request_Id);
   --  Abort a running task.

   procedure Shutdown;
   --  Called on shutdown.

end System.RPC.Pool;

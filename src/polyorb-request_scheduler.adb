package body PolyORB.Request_Scheduler is

   My_Factory : Request_Scheduler_Factory_Access;

   ------------
   -- Create --
   ------------

   procedure Create (RS : out Request_Scheduler_Access) is
   begin
      if My_Factory /= null then
         RS := Create (My_Factory);
      end if;
   end Create;

   -------------------------------------
   -- Register_Request_Scheduler_Factory --
   -------------------------------------

   procedure Register_Request_Scheduler_Factory
     (RSF : Request_Scheduler_Factory_Access)
   is
   begin
      pragma Assert (My_Factory = null);
      My_Factory := RSF;
   end Register_Request_Scheduler_Factory;

end PolyORB.Request_Scheduler;

with MOMA.Connections.Queues;

package MOMA.Connection_Factories.Queues is

   -------------------
   --  Object Queue --
   -------------------

   type Queue is new Connection_Factory with null record;

   -----------------------
   --  Functions Create --
   -----------------------

   function Create return Connections.Queues.Queue;

   function Create (Username : String; Password : String)
                   return Connections.Queues.Queue;

end MOMA.Connection_Factories.Queues;

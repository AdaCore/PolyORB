with MOMA.Connections.Topics;

package MOMA.Connection_Factories.Topics is

   -------------------
   --  Object Topic --
   -------------------

   type Topic is new Connection_Factory with null record;

   -----------------------
   --  Functions Create --
   -----------------------

   function Create return Connections.Topics.Topic;

   function Create (Username : String; Password : String)
                   return Connections.Topics.Topic;

end MOMA.Connection_Factories.Topics;

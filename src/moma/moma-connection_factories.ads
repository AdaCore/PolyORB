with MOMA.Connections;

package MOMA.Connection_Factories is

   -----------------------------------------
   --  Abstract Object Connection_Factory --
   -----------------------------------------

   type Connection_Factory is abstract tagged private;

private

   type Connection_Factory is abstract tagged null record;

   --------------------------------
   --  Abstract Create Functions --
   --------------------------------

   function Create return Connections.Connection is abstract;

   function Create (Username : String; Password : String)
                   return Connections.Connection
      is abstract;

end MOMA.Connection_Factories;

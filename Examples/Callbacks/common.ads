with Types; use Types;
package Common is
   pragma Remote_Types;

   type Callback is
      access procedure (Q : Query; R : Reply; P : Natural);
   pragma Asynchronous (Callback);

   type Worker is abstract tagged limited private;
   procedure Work (W : access Worker;
                   Q : Query;
                   C : Callback) is abstract;

private
   type Worker is abstract tagged limited null record;
end Common;

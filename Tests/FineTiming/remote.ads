--
--  $Id$
--

package Remote is

   pragma Remote_Call_Interface;

   procedure Synchronous_Empty_Test;

   procedure Asynchronous_Empty_Test;
   pragma Asynchronous (Asynchronous_Empty_Test);

   type T is array (1 .. 1_000) of Natural;

   procedure Synchronous_Test (A : in T);

   procedure Asynchronous_Test (A : in T);
   pragma Asynchronous (Asynchronous_Test);

end Remote;

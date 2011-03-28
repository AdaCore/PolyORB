--
--  $Id$
--

package Remote is

   pragma Remote_Call_Interface;

   procedure Synchronous_Empty_Test;

   procedure Asynchronous_Empty_Test;
   pragma Asynchronous (Asynchronous_Empty_Test);

   type T is array (1 .. 1_000) of Natural;

   procedure Synchronous_Test (A : in out T);

   procedure Asynchronous_Test (A : in T);
   pragma Asynchronous (Asynchronous_Test);

   procedure Enter_Test_Mode
     (Partition : in Natural;
      Port      : in Positive;
      Tries     : in Natural);
   pragma Asynchronous (Enter_Test_Mode);

end Remote;

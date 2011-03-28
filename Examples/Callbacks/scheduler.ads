with Common; use Common;
with Types;  use Types;
package Scheduler is

   pragma Remote_Call_Interface;

   type Any_Worker is access all Worker'Class;
   pragma Asynchronous (Any_Worker);

   procedure Save (Q : Query; R : Reply; P : Natural);

   function Continue return Boolean;
   procedure Push (W : Any_Worker);
   procedure Pop  (W : out Any_Worker; Q : out Query);

end Scheduler;

--  Taking policies that support multiple tasks, and depend on the
--  Ada tasks runtime library.

--  $Id$

package Droopi.Tasking_Policies.Multitask is

   pragma Preelaborate;

   type Task_Per_Session is new No_Tasking with private;

   procedure Handle_New_Connection
     (P : access Task_Per_Session;
      C : Channel);

   procedure Handle_Request
     (P : access Task_Per_Session;
      R : Request);

   type Task_Per_Request is new No_Tasking with private;

   --  procedure Handle_New_Connection
   --    (P : access Task_Per_Request;
   --     C : Channel);
   --  is inerited from No_Tasking.

   procedure Handle_Request
     (P : access Task_Per_Request;
      R : Request);

   type Task_Pool is new No_Tasking with private;

   --  procedure Handle_New_Connection
   --    (P : access Task_Per_Request;
   --     C : Channel);
   --  is inerited from No_Tasking.

   procedure Handle_Request
     (P : access Task_Pool;
      R : Request);

private

   type Task_Per_Session is new Tasking_Policy with null record;

   type Task_Per_Request is new Tasking_Policy with null record;

   type Task_Pool is new Tasking_Policy with record

      --  Configuration parameters

      Min_Tasks      : Positive;
      Max_Tasks      : Positive;
      Max_Idle_Tasks : Positive;

      --  Current state of the task pool

      Current_Tasks      : Positive;
      Current_Idle_Tasks : Positive;
   end record;

end Droopi.Tasking_Policies.Multitask;

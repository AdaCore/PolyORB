--  $Id$

--  This package implements a basic thread-safe bounded queue.

generic

   type Queue_Element is private;

package Locked_Queue is

   pragma Elaborate_Body;

   type Queue is limited private;

   procedure Create
     (Result    :    out Queue;
      Max_Count : in     Positive);
   --  Creates a queue with a limitation of Max_Count individuals.

   procedure Add
     (Q : in out Queue;
      E : in     Queue_Element);
   --  Appends an element to the end of the queue.
   --  This call is blocking when the queue is full.
   --
   --  ??? : This is a function that needs to be modified when
   --        adding the notion of priority to queue : request should be
   --        inserted in the queue with regard to their priority and
   --        not necessarily at the end.
   --
   --  ??? : A function Priority_Of (Element) return Natural should
   --        also be added to the generic part.

   procedure Get_Head
     (Q : in out Queue;
      E :    out Queue_Element);
   --  Removes the first element from the queue and returns it.
   --  This call is blocking when the queue is empty.

private

   type Queue_Element_Access is access Queue_Element;

   type Queue_Node;

   type Queue_Node_Access is access Queue_Node;

   type Queue_Node is record
      Element : Queue_Element_Access;
      Next    : Queue_Node_Access;
   end record;

   protected type Lock is
      entry Enter;
      procedure Leave;
   private
      Taken : Boolean := False;
   end Lock;


   type Queue is record
      Max_Count  : Positive;

      State_Lock : Lock;
      --  This locks the global state of the queue, and should be
      --  taken when modifying First, Last and Count fields.

      Full_Lock  : Lock;
      --  This lock is taken when the queue is full.

      Empty_Lock : Lock;
      --  This lock is taken when the queue is empty.

      First      : Queue_Node_Access := null;
      Last       : Queue_Node_Access := null;
      Count      : Natural := 0;
   end record;

end Locked_Queue;









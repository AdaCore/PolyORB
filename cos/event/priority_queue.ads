with CORBA;
with PolyORB.Tasking.Semaphores;

package Priority_Queue is

   type Priority_Queue_Type (MaxElt : Integer) is private;
   type Priority_Queue_Access is access all Priority_Queue_Type;

   subtype Priority_Value is Integer range -32767 .. 32767;

   type Discard_Policy_Values is (Fifo, Lifo, Priority);
   type Order_Policy_Values is (Fifo, Priority);

   subtype Factor_Range is Integer range 0 .. 16;

   --  Create a priority queue
   procedure Create (PQ : out Priority_Queue_Access;
      The_Factor : in Factor_Range);

   --  Destroy a priority queue
   procedure Destroy (PQ : in out Priority_Queue_Access);

   --  Insert a new element in the queue
   procedure Insert
    (PQ : in out Priority_Queue_Access;
     Data : in CORBA.Any;
     Prio : in Priority_Value);

   --  Retrieve the highest-priority element in the queue
   procedure Pop (PQ : in out Priority_Queue_Access; Data : out CORBA.Any);

   --  Set the Order policy
   procedure Set_OrderPolicy (PQ : in out Priority_Queue_Access;
      Policy : in Order_Policy_Values);

   --  Set the Maximum Queue Size
   procedure Set_MaxQueueSize (PQ : in out Priority_Queue_Access;
      Size : in Integer);

private
   use PolyORB.Tasking.Semaphores;

   type Queue_Elt;
   type Queue_Elt_Ptr is access Queue_Elt;
   type Queue_Elt is record
      Next : Queue_Elt_Ptr;
      Last : Queue_Elt_Ptr;
      Prev : Queue_Elt_Ptr;
      NextTime : Queue_Elt_Ptr;
      PrevTime : Queue_Elt_Ptr;
      Last_Higher : Queue_Elt_Ptr;
      Data : CORBA.Any;
      P : Priority_Value;
   end record;

   type Priority_Queue_Array is array (Integer range <>) of Queue_Elt_Ptr;

   type Priority_Queue_Type (MaxElt : Integer) is record
      Priority_Queues : Priority_Queue_Array (0 .. MaxElt);
      Highest_Priority : Integer := 0;
      Lowest_Priority : Integer := MaxElt;
      Queue_Lock : Semaphore_Access := null;
      Factor : Factor_Range := 0;
      MaxSize : Integer := -1;
      FirstTime : Queue_Elt_Ptr := null;
      LastTime : Queue_Elt_Ptr := null;
      Discard : Discard_Policy_Values := Lifo;
      Order : Order_Policy_Values := Fifo;
   end record;

   procedure Discard (PQ : in out Priority_Queue_Access; Result : out Boolean);

end Priority_Queue;

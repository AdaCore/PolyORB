with Ada.Unchecked_Deallocation;
with PolyORB.Tasking.Semaphores; use PolyORB.Tasking.Semaphores;
with PolyORB.Tasking.Soft_Links; use PolyORB.Tasking.Soft_Links;

package body Priority_Queue is

   procedure Free_Queue_Elt is
      new Ada.Unchecked_Deallocation (Queue_Elt, Queue_Elt_Ptr);

   procedure Free_Queue is
      new Ada.Unchecked_Deallocation
          (Priority_Queue_Type, Priority_Queue_Access);

   ------------------------------
   --           Create         --
   ------------------------------

   procedure Create (PQ : out Priority_Queue_Access;
      The_Factor : in Factor_Range) is
   begin
      PQ := new Priority_Queue_Type ((2 ** The_Factor) - 1);
      PQ.Highest_Priority := 0;
      PQ.Lowest_Priority := 2**The_Factor-1;
      PQ.Factor := The_Factor;
      PQ.MaxSize := -1;
      PQ.Order := Fifo;
      PQ.FirstTime := null;
      PQ.LastTime := null;
      Create (PQ.Queue_Lock);
   end Create;

   ------------------------------
   --           Destroy        --
   ------------------------------

   procedure Destroy (PQ : in out Priority_Queue_Access)
   is
         Event : Queue_Elt_Ptr;
         Temp : Queue_Elt_Ptr;
   begin
      if PQ /= null then
         --  how should we treat elements still in
         --  the queue upon destruction of the queue
         --  for now, these elements are destroyed
         --  (future QoS parameter ?)
         for i in 0 .. 2 ** (PQ.Factor) - 1 loop
            if PQ.Priority_Queues (i) /= null then
               Event := PQ.Priority_Queues (i);
               while Event.Next /= null loop
                  Temp := Event;
                  Event := Event.Next;
                  Free_Queue_Elt (Temp);
               end loop;
               Free_Queue_Elt (Event);
            end if;
         end loop;
         Destroy (PQ.Queue_Lock);
         Free_Queue (PQ);
      end if;
   end Destroy;

   ------------------------------
   --           Insert         --
   ------------------------------

   procedure Insert
    (PQ : in out Priority_Queue_Access;
     Data : in CORBA.Any;
     Prio : in Priority_Value)
         is
      Queuenum : Integer;
      Elt : Queue_Elt_Ptr;
      Event : Queue_Elt_Ptr;
      Result : Boolean;
   begin
      Queuenum := (Prio + 32767) / (2**(16-PQ.Factor));

      if State (PQ.Queue_Lock) = PQ.MaxSize and PQ.MaxSize > -1 then
         Discard (PQ, Result);
         if Result = False then
            return;
         end if;
      end if;

      Event := new Queue_Elt;

      --  Event was allocated properly,
      --  and there is room in the queue to place it
      Event.Data := Data;
      Event.P := Prio;
      Event.Last := Event;
      Enter_Critical_Section;
      if PQ.Priority_Queues (Queuenum) = null then
         --  this queue is empty
         PQ.Priority_Queues (Queuenum) := Event;
         Event.Next := null;
         Event.Prev := null;
         Event.Last_Higher := null;
      else
         --  this queue already has elements
         Elt := PQ.Priority_Queues (Queuenum);
         if Elt.P < Prio then
            --  All current elements are of lesser priority
            --  insert at head of queue
            Event.Next := Elt;
            Event.Prev := null;
            Event.Last_Higher := null;
            Elt.Last_Higher := Event;
            Elt.Last.Last_Higher := Event;
         else
            --  There are elements of equal or higher priority in this queue
            Elt := Elt.Last;
            while Elt.Next /= null and Elt.Next.P >= Prio loop
               Elt := Elt.Next.Last;
            end loop;
            --  Elt is now positioned at the last of element
            --  of priority higher or equal to Prio
            Event.Next := Elt.Next;
            Event.Prev := Elt;
            Elt.Next := Event;
            if Elt.P = Prio then
               --  Event is of the same priority as Elt
               Elt.Last := Event;
               Event.Last_Higher := Elt.Last_Higher;
               if Elt.Last_Higher = null then
                  --  Elt is of the highest priority in this queue
                  PQ.Priority_Queues (Queuenum).Last := Event;
               else
                  Elt.Last_Higher.Next.Last := Event;
               end if;
            else
               --  Event is of a different (and therefor new) priority
               Event.Last_Higher := Elt;
            end if;
            if Event.Next /= null then
               --  there are elements following Event
               Event.Next.Last_Higher := Event;
               Event.Last.Last_Higher := Event;
               Event.Next.Prev := Event;
            end if;
         end if;
      end if;

      --  Update TimeOrder pointers
      Event.NextTime := null; --  Event is the most recent element
      Event.PrevTime := PQ.LastTime;
      PQ.LastTime := Event;
      if PQ.FirstTime = null then
         --  Event is the first element in the queue
         PQ.FirstTime := Event;
      end if;

      --  Update Highest_Priority
      if Queuenum > PQ.Highest_Priority then
         PQ.Highest_Priority := Queuenum;
      end if;

      --  Update Lowest_Priority
      if Queuenum < PQ.Lowest_Priority then
         PQ.Highest_Priority := Queuenum;
      end if;

      Leave_Critical_Section;
      --  Signal that there is another event waiting
      V (PQ.Queue_Lock);
   exception
      when Storage_Error =>
         --  There is no memory available to allocate
         --  new Events in the queue, so reject the event
         return;
   end Insert;

   ------------------------------
   --           Pop            --
   ------------------------------

   procedure Pop (PQ : in out Priority_Queue_Access;
      Data : out CORBA.Any)
   is
      Event : Queue_Elt_Ptr;
   begin
      P (PQ.Queue_Lock);
      --  if you get to here, there is something in the queue
      Enter_Critical_Section;
      Event := PQ.Priority_Queues (PQ.Highest_Priority);
      Data := Event.Data;
      PQ.Priority_Queues (PQ.Highest_Priority) := Event.Next;
      if Event.Next = null then
         --  oh no ! there are no other elements of this priority
         --  so find the next priority, if such a thing exists
         if State (PQ.Queue_Lock) > 0 then
            --  the queue isn't empty yet
            while PQ.Highest_Priority > 0 and
               PQ.Priority_Queues (PQ.Highest_Priority) = null loop
                  PQ.Highest_Priority := PQ.Highest_Priority - 1;
            end loop;
            --  we should now have found the next highest priority element
         end if;
      else
         --  there are other elements left in the queue
         Event.Next.Last_Higher := null;
         Event.Next.Prev := null;
         if Event.Next.P = Event.P then
            Event.Next.Last := Event.Last;
         else
            Event.Next.Last.Last_Higher := null;
         end if;
      end if;

      --  Update Time pointers
      if Event.NextTime /= null then
         --  Event wasn't the most recent element
         Event.NextTime.PrevTime := Event.PrevTime;
      else
         PQ.LastTime := Event.PrevTime;
      end if;
      if Event.PrevTime /= null then
         --  Event wasn't the oldest element
         Event.PrevTime.NextTime := Event.NextTime;
      else
         PQ.FirstTime := Event.NextTime;
      end if;

      --  Update  Lowest priority if the list is now empty
      if State (PQ.Queue_Lock) = 0 then
         PQ.Lowest_Priority := 2 ** PQ.Factor - 1;
      end if;

      Leave_Critical_Section;
      Free_Queue_Elt (Event);
   end Pop;

   ------------------------------
   --      Set_OrderPolicy     --
   ------------------------------

   procedure Set_OrderPolicy
      (PQ : in out Priority_Queue_Access;
      Policy : in Order_Policy_Values)
         is
   begin
      Enter_Critical_Section;
      PQ.Order := Policy;
      Leave_Critical_Section;
   end Set_OrderPolicy;


   ------------------------------
   --     Set_MaxQueueSize     --
   ------------------------------

   procedure Set_MaxQueueSize (PQ : in out Priority_Queue_Access;
      Size : in Integer)
         is
   begin
      Enter_Critical_Section;
      PQ.MaxSize := Size;
      Leave_Critical_Section;
   end Set_MaxQueueSize;

   ------------------------------
   --          Discard         --
   ------------------------------

   procedure Discard (PQ : in out Priority_Queue_Access; Result : out Boolean)
         is
      Event : Queue_Elt_Ptr;
      QueueNum : Integer;
      Data : CORBA.Any;
   begin
      if State (PQ.Queue_Lock) <= 0 then
         --  there is a problem : Discard was called on an empty queue
         --  so return false so as not to raise false hopes
         Result := False;
         return;
      elsif PQ.Discard = Fifo and PQ.Order = Fifo then
         Pop (PQ, Data);
      elsif PQ.Discard = Fifo and PQ.Order = Priority then
         --  Get rid of oldest element
         Event := PQ.LastTime;
      elsif PQ.Discard = Lifo then
         Result := False;
         return;
      elsif PQ.Discard = Priority then
         --  get rid of lowest-priority element
         Event := PQ.Priority_Queues (PQ.Lowest_Priority).Last;
         while Event.Next /= null loop
            Event := Event.Next.Last;
         end loop;
      else
         --  Unlikely, but you never know : return false
         Result := False;
         return;
      end if;
      --  we now have an Event to delete

      QueueNum := (Event.P + 32767) / (2 ** (16 - PQ.Factor));

      if Event.PrevTime = null then
         PQ.FirstTime := Event.NextTime;
      else
         Event.PrevTime.NextTime := Event.NextTime;
      end if;

      if Event.NextTime = null then
         PQ.LastTime := Event.PrevTime;
      else
         Event.NextTime.PrevTime := Event.PrevTime;
      end if;

      --  Update Last pointers
      if Event.Prev = null or Event.Prev.P > Event.P then
         if Event.Next /= null and Event.Next.P = Event.P then
            Event.Next.Last := Event.Last;
         end if;
      elsif Event.Next = null or Event.Next.P < Event.P then
         if Event.Prev /= null and Event.Prev.P = Event.P then
            Event.Prev.Last := Event.Prev;
         end if;
         if Event.Last_Higher /= null then
            Event.Last_Higher.Next.Last := Event.Prev.Last;
         end if;
      end if;

      if Event.Prev /= null then
         Event.Prev.Next := Event.Next;
      else
         PQ.Priority_Queues (QueueNum) := Event.Next;
      end if;
      if Event.Next /= null then
         Event.Next.Prev := Event.Prev;
      end if;

      if PQ.Priority_Queues (QueueNum) = null then
         --  we emptied that queue
         if QueueNum = PQ.Highest_Priority then
            while PQ.Highest_Priority > 0 and
               PQ.Priority_Queues (PQ.Highest_Priority) = null loop
                  PQ.Highest_Priority := PQ.Highest_Priority - 1;
            end loop;
         end if;
         if QueueNum = PQ.Lowest_Priority then
            while PQ.Highest_Priority < 2 ** PQ.Factor - 1 and
                  PQ.Priority_Queues (PQ.Lowest_Priority) = null loop
                     PQ.Highest_Priority := PQ.Highest_Priority - 1;
            end loop;
         end if;
      end if;
      Free_Queue_Elt (Event);
      Result := True;
   end Discard;

end Priority_Queue;
--  $Id$

with Unchecked_Deallocation;

package body Locked_Queue is

   use Droopi.Locks;

   procedure Free is new Unchecked_Deallocation
     (Queue_Node, Queue_Node_Access);

   ------------
   -- Create --
   ------------

   procedure Create
     (Result    :    out Queue;
      Max_Count : in     Positive)
   is
   begin
      Result.Max_Count := Max_Count;

      Create (Result.State_Lock);
      Create (Result.Full_Lock);
      Create (Result.Empty_Lock);

      Lock_W (Result.Empty_Lock);
   end Create;

   ---------
   -- Add --
   ---------

   procedure Add
     (Q : in out Queue;
      E : in     Queue_Element)
   is
   begin
      Lock_W (Q.Full_Lock);

      Lock_W (Q.State_Lock);
      if Q.Last = null then
         Q.Last := new Queue_Node'
           (Element => new Queue_Element'(E),
            Next    => null);
         Q.First := Q.Last;
      else
         Q.Last.Next := new Queue_Node'
           (Element => new Queue_Element'(E),
            Next    => null);
         Q.Last := Q.Last.Next;
      end if;
      Q.Count := Q.Count + 1;
      Unlock_W (Q.State_Lock);

      if Q.Count = 1 then
         Unlock_W (Q.Empty_Lock);
      end if;

      if Q.Count /= Q.Max_Count then
         Unlock_W (Q.Full_Lock);
      end if;
   end Add;

   --------------
   -- Get_Head --
   --------------

   procedure Get_Head
     (Q : in out Queue;
      E :    out Queue_Element)
   is
   begin
      Lock_W (Q.Empty_Lock);

      --  When execution reaches this, necessarily Q.First /= null.

      Lock_W (Q.State_Lock);

      declare
         --  Old_First : Queue_Node_Access := Q.First;
      begin
         E := Q.First.Element.all;
         Q.First := Q.First.Next;

         --  ??? Should free old elements !!
         --  Free (Old_First);
      end;

      Q.Count := Q.Count - 1;
      Unlock_W (Q.State_Lock);

      --  When execution reaches this, necessarily the queue is not full.

      if Q.Count = Q.Max_Count - 1 then
         Unlock_W (Q.Full_Lock);
      end if;

      if Q.Count > 0 then
         Unlock_W (Q.Empty_Lock);
      end if;

   end Get_Head;

end Locked_Queue;

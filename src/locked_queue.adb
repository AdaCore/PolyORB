--  $Id$

with Unchecked_Deallocation;

with PolyORB.Log;
pragma Elaborate_All (PolyORB.Log);

package body Locked_Queue is

   use PolyORB.Log;
   use PolyORB.Soft_Links;

   package L is new PolyORB.Log.Facility_Log
     ("locked_queue");
   procedure O (Message : in String; Level : Log_Level := Debug)
     renames L.Output;

   procedure Free is new Unchecked_Deallocation
     (Queue_Node, Queue_Node_Access);
   procedure Free is new Unchecked_Deallocation
     (Queue_Element, Queue_Element_Access);

   ---------------
   -- Get_Count --
   ---------------

   function Get_Count
     (Q : Queue)
     return Natural
   is
      N : Natural;
   begin
      Enter (Q.State_Lock);
      N := Q.Count;
      Leave (Q.State_Lock);
      return N;
   end Get_Count;

   -------------------
   -- Get_Max_Count --
   -------------------

   function Get_Max_Count
     (Q : Queue)
     return Positive
   is
   begin
      return Q.Max_Count;
   end Get_Max_Count;

   -------------
   -- Destroy --
   -------------

   procedure Destroy
     (Q : in out Queue)
   is
      Next : Queue_Node_Access;
   begin
      Enter (Q.State_Lock);
      while Q.First /= null loop
         Next := Q.First.Next;
         Free (Q.First);
         Q.First := Next;
      end loop;
      Leave (Q.State_Lock);

      Destroy (Q.State_Lock);
      Destroy (Q.Full_Lock);
      Destroy (Q.Empty_Lock);
   end Destroy;

   ------------
   -- Create --
   ------------

   procedure Create
     (Q         :    out Queue;
      Max_Count : in     Positive)
   is
   begin
      Create (Q.State_Lock);

      Enter (Q.State_Lock);
      Q.Max_Count := Max_Count;
      Q.Count := 0;
      Create (Q.Empty_Lock);
      Create (Q.Full_Lock);
      Create (Q.State_Lock);
      Leave (Q.State_Lock);

      Enter (Q.Empty_Lock);
   end Create;

   ---------
   -- Add --
   ---------

   procedure Add
     (Q : in out Queue;
      E : in     Queue_Element)
   is
   begin
      Enter (Q.Full_Lock);

      Enter (Q.State_Lock);
      if Q.First = null then
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
      Leave (Q.State_Lock);

      if Q.Count = 1 then
         Leave (Q.Empty_Lock);
      end if;

      if Q.Count /= Q.Max_Count then
         Leave (Q.Full_Lock);
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
      Enter (Q.Empty_Lock);

      --  When execution reaches this, necessarily Q.First /= null.

      Enter (Q.State_Lock);

      declare
         Old_First : Queue_Node_Access := Q.First;
      begin
         E := Q.First.Element.all;
         Q.First := Q.First.Next;
         Free (Old_First);
      end;

      Q.Count := Q.Count - 1;
      Leave (Q.State_Lock);

      --  When execution reaches this, necessarily the queue is not full.

      if Q.Count = Q.Max_Count - 1 then
         Leave (Q.Full_Lock);
      end if;

      if Q.Count > 0 then
         Leave (Q.Empty_Lock);
      end if;

   end Get_Head;

end Locked_Queue;
